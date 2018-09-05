{-# LANGUAGE OverloadedStrings #-}
import CallGraph.Kythe
import Conduit
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Lazy (toStrict)
import qualified Data.Conduit.Combinators as C
import Data.Semigroup ((<>))
import qualified Data.Text.Encoding as T
import Lens.Family2 ((^.))
import Options.Applicative
import qualified Proto.Kythe.Proto.Storage as K
import qualified Proto.Kythe.Proto.Storage_Fields as K

data Options = Options
  { optsFile :: FilePath
  }

opts = info (options <**> helper)
       ( fullDesc
       <> progDesc "Output Kythe proto entries file as JSON"
       <> header "entries-to-json - json export of kythe entries" )
  where
    options = Options <$> strArgument
              (  metavar "FILENAME"
              <> help "Input Kythe entries file")

main :: IO ()
main = do
  options <- execParser opts
  runConduitRes $ protoToJsonOut (optsFile options)

protoToJsonOut :: FilePath -> ConduitT () Void (ResourceT IO) ()
protoToJsonOut f = protoFile f .| outputJson .| C.stdout
  where
    outputJson :: Monad m => ConduitT K.Entry ByteString m ()
    outputJson = C.map $ \e->
      B.concat [toStrict $ encode e, "\n"]

instance ToJSON K.Entry where
  toJSON k =
    object [ "source" .= fromVName (k ^. K.source)
           , "edgeKind" .= (k ^. K.edgeKind)
           , "target" .= fromVName (k ^. K.target)
           , "factName" .= (k ^. K.factName)
           , "factValue" .= (T.decodeUtf8 $ k ^. K.factValue)
           ]
    where
      fromVName :: K.VName -> Value
      fromVName vn =
        object [ "signature".= (vn ^. K.signature)
               , "corpus"   .= (vn ^. K.corpus)
               , "root"     .= (vn ^. K.root)
               , "path"     .= (vn ^. K.path)
               , "language" .= (vn ^. K.language)
               ]
