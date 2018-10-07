{-# LANGUAGE OverloadedStrings #-}
module Options
  ( Options(..)
  , opts
  ) where

import Data.Semigroup ((<>))
import qualified Data.Text as T
import Data.Text (Text)
import Options.Applicative
import Options.Applicative.Types (readerAsk)

data Options = Options
  { optsDir  :: FilePath
  , optsRoot :: Text
  }

opts = info (options <**> helper)
       ( fullDesc
       <> progDesc "CLI UI tool to navigate raw Kythe call graph"
       <> header "kythe-navigator - navigate call graph of kythe entries" )
  where
    options = Options
      <$> strOption
          (  long "directory"
          <> short 'd'
          <> metavar "DIRECTORY"
          <> help "Directory with input Kythe entries files")
      <*> option (T.pack <$> readerAsk)
          (  long "root"
          <> short 'r'
          <> metavar "ROOT"
          <> help "Root graph not to start from")
