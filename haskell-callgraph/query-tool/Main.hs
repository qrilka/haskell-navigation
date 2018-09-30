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
import Data.Store (decode)
import Data.Text (Text)
import qualified Data.Text as T
import Language.Haskell.Source
import RIO
import System.Exit (die)
import Options.Applicative
import Options.Applicative.Types (readerAsk)
import qualified RIO.ByteString as B
import qualified RIO.Map as M
import qualified RIO.Vector as V

data Options = Options
  { optsGraphPath :: FilePath
  , optsCommand :: Command
  }

data Collapse = Collapse | DontCollapse

data Command
  = Paths Text Text Collapse
  | Stats
  | ListPackages
  | ListModule Text

opts :: ParserInfo Options
opts = info (options <**> helper)
       ( fullDesc
       <> progDesc "Source code graph query tool using Kythe entries"
       <> header "query-tool - querying call graph from kythe entries" )
  where
    options = Options
      <$> strOption
          (  long "graph"
          <> short 'g'
          <> metavar "GRAPH"
          <> help "File with call graph information")
      <*> cmd
    cmd = subparser $
      command "paths" (info paths (progDesc "Paths between functions"))
      <>
      command "list-module"
      (info listModuleCmd (progDesc "List module contents (function definitions and calls)"))
      <>
      command "list-packages"
      (info (pure ListPackages) (progDesc "List packages and their modules"))
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
      <*> flag DontCollapse Collapse
          (  long "collapse"
          <> short 'c'
          <> help "Collapse call path prefixes")
    listModuleCmd = ListModule
      <$> textOption
          (  long "module"
          <> short 'm'
          <> metavar "MOD"
          <> help "Module to list")
    textOption = option (T.pack <$> readerAsk)

main :: IO ()
main = do
  Options{..} <- execParser opts
  decoded <- decode <$> B.readFile optsGraphPath
  si <- case decoded of
    Left err -> die $ "Could not decode call graph, error: " ++ show err
    Right x -> pure x
  when (V.null . assocTable $ calls si) $
    die "No call graph information was found"
  case optsCommand of
    Paths sK tK collapse -> do
      s <- findVertex "source node" sK si
      t <- findVertex "target node" tK si
      case collapse of
        Collapse -> findPathsCollapsed (calls si) s t
        DontCollapse -> findPaths (calls si) s t
    Stats -> do
      let graph = calls si
      putStrLn $ "Number of packages: " ++ show (M.size $ packages si)
      putStrLn $ "Number of modules: " ++ show (sum $ M.map M.size $ packages si)
      putStrLn $ "Number of call graph vertices: " ++ show (length $ assocTable graph)
      putStrLn $ "Number of call graph edges: " ++
        show (V.sum $ V.map (IntMap.size . snd) $ assocTable graph)
    ListPackages -> do
      listPackages si
    ListModule t -> do
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
    lookedUp = lookupUnresolved <|> lookupResolved
    termPrefix = "haskell:term:"
    lookupUnresolved =
      M.lookup (UnresolvedRef $ termPrefix <> funTxt) (nodeIndices calls)
    lookupResolved = do
      let colon = ':'
      ref <-
        case T.split (== colon) funTxt of
          [p, m, f] -> pure $ ResolvedRef (FunctionDefinition p m f)
          _ -> Nothing
      M.lookup ref (nodeIndices calls)

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
                putStrLn . T.unpack $ T.replicate ind "  " <>
                  "<Byte offset " <> T.pack (show offset) <> ">"

countPaths :: PathForest e -> Int
countPaths xs = sum $ map go xs
 where
  go PathEnd = 1
  go (PathTree subforest) = sum $ map (go . snd) subforest

findPathsCollapsed :: FunctionCallGraph -> Int -> Int -> IO ()
findPathsCollapsed g x y =
  case pathsForestBetween g x y of
    [] -> putStrLn "No paths between nodes"
    paths -> do
      putStrLn $ "Found " ++ show (countPaths paths) ++ " path(s):"
      start <- case assocTable g V.!? x of
        Nothing -> die "Start node not present in the graph"
        Just (s, _) -> pure s
      putStrLn . T.unpack $ showRef start
      forM_ paths $ showPath 1
  where
    showPath _indent PathEnd = pure ()
    showPath indent (PathTree subforest) =
      forM_ subforest $ \((n, fCalls), rest) -> do
        case assocTable g V.!? n of
            Nothing ->
              putStrLn . T.unpack $ T.replicate indent "  " <> "<missing node>"
            Just (fun, _) -> do
              putStrLn . T.unpack $ T.replicate indent "  " <> showRef fun
              forM_ fCalls $ \(FunctionCall (SourceLocation offset)) ->
                putStrLn . T.unpack $ T.replicate indent "  " <>
                  "<Byte offset " <> T.pack (show offset) <> ">"
              showPath (indent+1) rest

showRef :: FunctionRef -> Text
showRef (ResolvedRef fun) =
  functionName fun <> " (package " <> package fun <> ", module " <> module_ fun <> ")"
showRef (UnresolvedRef urname) =
  "Unresolved " <> urname

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
                    "<Byte offset " <> T.pack (show offset) <> ">"
            Nothing -> return ()
        UnresolvedRef vname ->
          putStrLn $ "Unresolved " ++ show vname

listPackages :: SourceInfo -> IO ()
listPackages BaseSourceInfo{packages=ps} =
 forM_ (M.toList ps) $ \(pn, modules) -> do
   putStrLn . T.unpack $ pn
   forM_ (M.keys modules) $ \mn -> do
     putStrLn . T.unpack $ "  " <> mn
