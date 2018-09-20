{-# LANGUAGE RecordWildCards #-}
module Main
  ( main
  ) where

import Data.Store
import Language.Haskell.Source.IO
import Options.Applicative
import RIO
import RIO.ByteString as B

data Options = Options
  { optsDir :: FilePath
  , optsOutput :: FilePath
  }

opts :: ParserInfo Options
opts = info (options <**> helper)
       ( fullDesc
       <> progDesc "Source code graph importer from Kythe entries"
       <> header "import-kythe - import code graph from kythe entries" )
  where
    options = Options
      <$> strOption
          (  long "directory"
          <> short 'd'
          <> metavar "DIRECTORY"
          <> help "Directory with input Kythe entries files")
      <*> strOption
          (  long "output"
          <> short 'o'
          <> metavar "OUTPUT"
          <> help "Output file name to store source code call graph")

main :: IO ()
main = do
  Options{..} <- execParser opts
  si <- loadFromDir optsDir
  B.writeFile optsOutput (encode si)
