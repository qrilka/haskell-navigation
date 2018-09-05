{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
import Brick.AttrMap (attrMap)
import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core ((<+>), str, withAttr)
import qualified Brick.Widgets.List as L
import Brick.Types (Widget)
import qualified Brick.Types as T
import CallGraph
import CallGraph.Kythe
import Conduit
import Control.Monad
import Data.Graph
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Lens.Micro ((^.), (&), (?~))
import qualified Graphics.Vty as Vty
import Options
import Options.Applicative (execParser)
import System.Exit (die)

main :: IO ()
main = do
  Options{..} <- execParser opts
  g <- readCallGraph optsDir
  when (null . vertices $ cgGraph g) $
    die "No call graph information was found"
  root <- case cgVertexFromKey g ("", optsRoot) of
    Just v -> pure v
    Nothing -> die "Root node not found in the graph"
  let l = listChildren g root 0
      initialState = AppState g root [] l
  void $ M.defaultMain app initialState

readCallGraph :: FilePath -> IO KCallGraph
readCallGraph d = runConduitRes $ entriesDir d .| fromStream

data AppState = AppState
  { graph :: KCallGraph
  , currentNode :: Vertex
  , history :: [(Int, Vertex)]
  , list :: L.List () Text
  }

app :: M.App AppState e ()
app =
      M.App
      { M.appDraw = drawUi
      , M.appStartEvent = return
      , M.appHandleEvent = appEvent
      , M.appAttrMap = const $ attrMap Vty.defAttr []
      , M.appChooseCursor = M.neverShowCursor
      }

visitChild :: AppState -> Vertex -> AppState
visitChild s@AppState {..} v =
  s
  { currentNode = v
  , list = listChildren graph v 0
  , history = (fromMaybe 0 $ list ^. L.listSelectedL,  currentNode) : history
  }

goBack :: AppState -> AppState
goBack s@AppState {..}
  | null history = s
  | otherwise =
    let (pos, prev):rest = history
    in s {currentNode = prev, list = listChildren graph prev pos, history = rest}

listChildren :: KCallGraph -> Vertex -> Int -> L.List () Text
listChildren g v pos = list & L.listSelectedL ?~ pos
  where
    list = L.list () (V.fromList labels) 1 
    labels = [ nodeLabel c g | c <- children v g]

drawUi :: AppState  -> [Widget ()]
drawUi AppState {..} = [ui]
  where
    ui = B.borderWithLabel label $ L.renderList listDrawElement True list
    label = str "Item " <+> cur <+> str " of " <+> total
    cur =
      case list ^. (L.listSelectedL) of
        Nothing -> str "-"
        Just i -> str (show (i + 1))
    total = str $ show $ V.length $ list ^. (L.listElementsL)

listDrawElement :: Bool -> Text -> Widget ()
listDrawElement sel a =
    let selStr t = if sel
                   then withAttr customAttr (str $ "<" <> T.unpack t <> ">")
                   else str $ T.unpack t
    in C.hCenter $ selStr a

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"

appEvent :: AppState -> T.BrickEvent () e -> T.EventM () (T.Next AppState)
appEvent s@AppState{..} (T.VtyEvent e)  =
  case e of
    Vty.EvKey Vty.KEsc [] -> M.halt s
    Vty.EvKey Vty.KEnter [] -> do
      let (_, _, children) = cgNodeFromVertex graph currentNode
          Just v = case list ^. L.listSelectedL of
                Just i -> cgVertexFromKey graph (children !! i)
                Nothing -> Just currentNode
      M.continue $ visitChild s v
    Vty.EvKey Vty.KLeft [] -> M.continue $ goBack s
    _ -> do
      list' <- L.handleListEvent e list
      M.continue s{list=list'}
appEvent s _ = M.continue s
