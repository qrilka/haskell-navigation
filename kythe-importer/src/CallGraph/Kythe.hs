{-# LANGUAGE BangPatterns #-}
module CallGraph.Kythe
  ( protoFile
  , entriesDir
  ) where

import Conduit
import Control.Monad
import qualified Data.Attoparsec.ByteString as AB
import Data.Bits
import Data.ByteString (ByteString)
import Data.Conduit.Attoparsec (conduitParser)
import Data.Conduit.Combinators (sourceFile)
import Data.ProtoLens.Encoding (decodeMessage)
import qualified Proto.Kythe.Proto.Storage as K
import System.FilePath.Glob (compile, match)

protoFile ::
     (MonadResource m, MonadThrow m) => FilePath -> ConduitT i K.Entry m ()
protoFile f = sourceFile f .| toEntries
  where
    toEntries :: (Monad m, MonadThrow m) => ConduitT ByteString K.Entry m ()
    toEntries =
      mapOutput snd . conduitParser $ do
        len <- varInt
        bs <- AB.take len
        case decodeMessage bs of
          Right entry -> return entry
          Left err -> fail err
    varInt :: AB.Parser Int
    varInt = loop 1 0
      where
        loop !s !n = do
          b <- AB.anyWord8
          let n' = n + s * fromIntegral (b .&. 127)
          if (b .&. 128) == 0
            then return $! n'
            else loop (128 * s) n'

entriesDir ::
     (MonadResource m, MonadThrow m) => FilePath -> ConduitT () K.Entry m ()
entriesDir f = sourceDirectory f .| pathEntries -- sequence_ (mapM protoFile)
  where
    pathEntries :: (MonadResource m, MonadThrow m) => ConduitT FilePath K.Entry m ()
    pathEntries = do
      let entries = compile "**/*.entries"
      mp <- await
      case mp of
        Nothing -> return ()
        Just fp -> do
          when (match entries fp) $ protoFile fp
          pathEntries
