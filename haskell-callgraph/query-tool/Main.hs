{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  ) where

import CallGraph (fromStream)
import CallGraph.Kythe
import Conduit
import Control.Monad
import qualified Data.IntMap as IntMap
import Data.Graph
import Data.Graph.Sparse
import qualified Data.IntMap as IntMap
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as T
import Language.Haskell.Source
import RIO
import System.Exit (die)
import Options.Applicative
import Options.Applicative.Types (readerAsk)
import qualified RIO.Map as M
import qualified RIO.Vector as V

data Options = Options
  { optsDir :: FilePath
  , optsCommand :: Command
  }

data Command
  = Paths Text Text
  | Stats

opts = info (options <**> helper)
       ( fullDesc
       <> progDesc "Output Kythe proto entries file as JSON"
       <> header "entries-to-json - json export of kythe entries" )
  where
    options = Options
      <$> strOption
          (  long "directory"
          <> short 'd'
          <> metavar "DIRECTORY"
          <> help "Directory with input Kythe entries files")
      <*> cmd
    cmd = subparser $
      command "paths" (info paths (progDesc "Paths between functions"))
      <>
      command "stats" (info (pure Stats) (progDesc "Call graph stats"))
    paths = Paths
      <$> textOption
          (  long "source"
          <> short 's'
          <> metavar "SRC"
          <> help "Source graph node")
      <*> textOption
          (  long "target"
          <> short 't'
          <> metavar "TGT"
          <> help "Target graph node")
    textOption = option (T.pack <$> readerAsk)

main :: IO ()
main = do
  Options{..} <- execParser opts
  si <- readSourceInfo optsDir
  when (V.null . assocTable $ calls si) $
    die "No call graph information was found"
  case optsCommand of
    Paths sK tK -> do
      s <- findVertex "source node" sK si
      t <- findVertex "target node" tK si
      findPaths (calls si) s t
    Stats -> do
      let graph = calls si
      putStrLn $ "Number of vertices:" ++ show (length $ assocTable graph) ++ "\n" ++
        "Number of edges:" ++ show (V.sum $ V.map (IntMap.size . snd) $ assocTable graph)

readSourceInfo :: FilePath -> IO SourceInfo
readSourceInfo d = fmap fromKCallGraph $ runConduitRes $ entriesDir d .| fromStream

findVertex :: String -> Text -> SourceInfo -> IO Vertex
findVertex needle funTxt SourceInfo {..} = dieIfNothing lookedUp
  where
    dieIfNothing = maybe (die $ "Could not find " ++ needle ++ " in the graph") pure
    lookedUp = do
      let colon = ':'
      fun <- case T.split (== colon) funTxt of
        [p, m, f] -> pure $ FunctionDefinition p m f
        _ ->  Nothing
      M.lookup fun (nodeIndices calls)

findPaths :: FunctionCallGraph -> Int -> Int -> IO ()
findPaths g x y = do
  let paths = pathsBetween g x y
      showDefinition fun =
        functionName fun <> "(module " <> module_ fun <> ")"
  case paths of
    [] -> putStrLn "No paths between nodes"
    _ -> do
      putStrLn $ "Found " ++ show (length paths) ++ " path(s):"
      start <- case assocTable g V.!? x of
        Nothing -> die "Start node not present in the graph"
        Just (s, _) -> pure s
      putStrLn . T.unpack $ showDefinition start
      forM_ paths $ \path ->
        forM_ (zip [1..] path) $ \(ind, (n, fCalls)) ->
          case assocTable g V.!? n of
            Nothing ->
              putStrLn . T.unpack $ T.replicate ind "  " <> "<missing node>"
            Just (fun, _) -> do
              putStrLn . T.unpack $ T.replicate ind "  " <> showDefinition fun
              forM_ fCalls $ \(FunctionCall (SourceLocation offset)) ->
                putStrLn . T.unpack $ T.replicate (ind + 1) "  " <>
                  "Byte offset " <> T.pack (show offset)
