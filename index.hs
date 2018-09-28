#!/usr/bin/env stack
-- stack --resolver lts-11.22 script
{-# LANGUAGE OverloadedStrings #-}
import Turtle
import Prelude hiding (FilePath)
import qualified Control.Foldl as Fold
import qualified Data.Text as T

parseOpts :: Parser (FilePath, FilePath)
parseOpts = (,) <$> argPath "dir"  "The directory with the project to parse"
             <*> argPath "dest" "The destination file"

main = do
  (prjDir, dest) <- options "Haskell source code indexer" parseOpts
  destTxt <- case toText dest of
    Left _ -> die "Could not decode destination path"
    Right t -> pure t
  ghc <- stackPath "--compiler-exe"
  export "REALGHC" ghc
  root <- pwd
  mpath <- need "PATH"
  path <- case mpath of
    Nothing -> die "PATH was not found"
    Just p -> pure p
  wrapperDir <- case toText (root </> "wrappers" </> "stack") of
    Left _ -> die "Can't form wrapper dir"
    Right d -> pure d
  compilerBin <- stackPath "--compiler-bin"
  installBin <- (<> "/bin") <$> stackPath "--local-install-root"
  export "PATH" $ T.intercalate ":" [wrapperDir, compilerBin, path, installBin]
  cd prjDir
  prjRoot <- pwd
  with (mktempdir prjRoot "idx") $ \tmpDir -> do
    exportPath "STACK_ROOT" (tmpDir </> "root")
    let indexDir = tmpDir </> "index"
    indexDirTxt <- case toText indexDir of
      Left appr -> die $ "Could not decode index dir " <> appr
      Right t -> pure t
    mkExportPath "INDEXER_OUTPUT_DIR" indexDir
    with (mktempdir prjRoot "idx-work") $ \tmpWork -> do
      exportPath "STACK_WORK" (filename tmpWork)
      echo "-------Starting build-------"
      stdout (inproc "stack" ["--system-ghc", "build"] empty)
    cd root
    unset "STACK_ROOT"
    unset "STACK_WORK"
    echo "-------Creating call graph from Kythe entries-------"
    stdout (inproc "stack" ["exec", "kythe-to-call-graph"
                           , "--", "-d", indexDirTxt
                           , "-o", destTxt] empty)

mkExportPath var d = do
  mkdir d
  exportPath var d

stackPath flag = do
  mp <- fold (inproc "stack" ["path", flag] empty) Fold.head
  case mp of
    Nothing -> die $ "Stack returned nothing for " <> flag
    Just x -> pure $ lineToText x

exportPath var p =
  case toText p of
    Left appr -> die $ "Could not decode path" <> appr
    Right t -> export var t
