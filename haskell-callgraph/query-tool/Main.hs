{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  ) where

import CallGraph
import CallGraph.Kythe
import Conduit
import Control.Monad
import Data.List (intercalate)
import Data.Monoid ((<>))
import Data.Graph
import Data.Text (Text)
import qualified Data.Text as T
import System.Exit (die)
import Options.Applicative
import Options.Applicative.Types (readerAsk)

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
  g <- readCallGraph optsDir
  when (null . vertices $ cgGraph g) $
    die "No call graph information was found"
  case optsCommand of
    Paths sK tK -> do
      s <- findVertex "source node" sK g
      t <- findVertex "target node" tK g
      findPaths g s t
    Stats -> do
      let graph = cgGraph g
      putStrLn $ "Number of vertices:" ++ show (length $ vertices graph) ++ "\n" ++
        "Number of edges:" ++ show (length $ edges graph)

readCallGraph :: FilePath -> IO KCallGraph
readCallGraph d = runConduitRes $ entriesDir d .| fromStream

findVertex :: String -> Text -> KCallGraph -> IO Vertex
findVertex needle sign CallGraph {..}
  | Just v <- cgVertexFromKey ("", sign) = pure v
  | otherwise = die $ "Could not find " ++ needle ++ " in the graph"

findPaths :: KCallGraph -> Vertex -> Vertex -> IO ()
findPaths g x y = do
  let paths = pathsBetween g x y
  case paths of
    [] -> putStrLn "No paths between nodes"
    _ -> do
      putStrLn $ "Found " ++ show (length paths) ++ " path(s):"
      forM_ paths $ \p ->
        putStrLn . T.unpack $ T.intercalate " -> " $ map (flip nodeLabel g) p
    
  
