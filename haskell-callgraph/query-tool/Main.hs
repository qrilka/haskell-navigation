{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  ) where

import Control.Monad
import qualified Data.IntMap as IntMap
import Data.Graph
import Data.Graph.Sparse
import Data.Text (Text)
import qualified Data.Text as T
import Language.Haskell.Source
import Language.Haskell.Source.IO
import qualified Lens.Family2 as F2
import qualified Proto.Kythe.Proto.Storage_Fields as K
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
  | List Text

opts :: ParserInfo Options
opts = info (options <**> helper)
       ( fullDesc
       <> progDesc "Navigation source code graph from Kythe entries"
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
      command "list" (info list (progDesc "List module contents (function definitions and calls)"))
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
    list = List
      <$> textOption
          (  long "module"
          <> short 'm'
          <> metavar "MOD"
          <> help "Module to list")
    textOption = option (T.pack <$> readerAsk)

main :: IO ()
main = do
  Options{..} <- execParser opts
  si <- loadFromDir optsDir
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
    List t -> do
      case T.split (==':') t of
        [pkg, mod]| not (T.null pkg) && not (T.null mod) ->
           listModule si pkg mod
        _ ->
           die $ "Bad module name " ++ show t

findVertex :: String -> Text -> SourceInfo -> IO Vertex
findVertex needle funTxt BaseSourceInfo {..} = dieIfNothing lookedUp
  where
    dieIfNothing =
      maybe (die $ "Could not find " ++ needle ++ " in the graph") pure
    lookedUp = do
      let colon = ':'
      fun <- case T.split (== colon) funTxt of
        [p, m, f] -> pure $ FunctionDefinition p m f
        _ ->  Nothing
      M.lookup (ResolvedRef fun) (nodeIndices calls)

findPaths :: FunctionCallGraph -> Int -> Int -> IO ()
findPaths g x y =
  case pathsBetween g x y of
    [] -> putStrLn "No paths between nodes"
    paths -> do
      putStrLn $ "Found " ++ show (length paths) ++ " path(s):"
      start <- case assocTable g V.!? x of
        Nothing -> die "Start node not present in the graph"
        Just (s, _) -> pure s
      putStrLn . T.unpack $ showRef start
      forM_ paths $ \p ->
        forM_ (zip [1..] p) $ \(ind, (n, fCalls)) ->
          case assocTable g V.!? n of
            Nothing ->
              putStrLn . T.unpack $ T.replicate ind "  " <> "<missing node>"
            Just (fun, _) -> do
              putStrLn . T.unpack $ T.replicate ind "  " <> showRef fun
              forM_ fCalls $ \(FunctionCall (SourceLocation offset)) ->
                putStrLn . T.unpack $ T.replicate (ind + 1) "  " <>
                  "Byte offset " <> T.pack (show offset)

showRef :: FunctionRef -> Text
showRef (ResolvedRef fun) =
  functionName fun <> "(module " <> module_ fun <> ")"
showRef (UnresolvedRef vname) =
  "Unresolved " <> (vname F2.^. K.signature)

listModule :: SourceInfo -> PackageName -> ModuleName -> IO ()
listModule BaseSourceInfo{packages=ps, calls=cs} pname mname = do
  case M.lookup mname =<< M.lookup pname ps of
    Nothing -> die "Module not found"
    Just ModuleInfo{modFunctions=fs} ->
      forM_ fs $ \case
        srcRef@(ResolvedRef source) -> do
          putStrLn . T.unpack $ functionName source
          case snd <$> lookupNode srcRef cs of
            Just outRefs ->
              forM_ (IntMap.toList outRefs) $ \(v, fCalls) -> do
                ref <- case assocTable cs V.!? v of
                  Nothing ->
                    die "Inconsistent graph: referenced node was not found"
                  Just (x, _) -> pure x
                putStrLn . T.unpack $ "  " <> showRef ref
                forM_ fCalls $ \(FunctionCall (SourceLocation offset)) ->
                  putStrLn . T.unpack $ "    " <>
                    "Byte offset " <> T.pack (show offset)
            Nothing -> return ()
        UnresolvedRef vname ->
          putStrLn $ "Unresolved " ++ show vname
