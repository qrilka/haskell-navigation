module Language.Haskell.Source.IO
  ( loadFromDir
  ) where

import CallGraph
import CallGraph.Kythe
import Conduit
import qualified Data.Conduit.List as CL
import Language.Haskell.Source
import RIO
import RIO.List (isSuffixOf)

loadFromDir :: FilePath -> IO SourceInfo
loadFromDir dir = fromRaw <$> do
  entryFiles <- runConduitRes $
    sourceDirectory dir .| filterC (".entries" `isSuffixOf`) .| CL.consume
  foldMapM processFile entryFiles

processFile :: FilePath -> IO RawSourceInfo
processFile fp =
  fmap fromKCallGraph . runConduitRes $ protoFile fp .| fromStream
