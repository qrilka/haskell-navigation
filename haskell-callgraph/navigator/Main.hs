{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
import Brick.AttrMap (attrMap)
import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import qualified Brick.Widgets.Border as B
import Brick.Widgets.Core ((<+>), str, txt, withAttr, emptyWidget)
import qualified Brick.Widgets.List as L
import Brick.Types (Widget)
import qualified Brick.Types as T
import Control.Monad
import Data.Graph.Sparse
import qualified Data.IntMap as IntMap
import Data.Store (decode)
import Data.Text (Text)
import qualified Data.Vector as V
import Language.Haskell.Source
import Lens.Micro ((^.), (&), (?~))
import qualified Graphics.Vty as Vty
import qualified Options.Applicative as O
import RIO
import qualified RIO.ByteString as B
import qualified RIO.Map as M
import qualified RIO.Set as S
import qualified RIO.Text as T
import System.Exit (die)

main :: IO ()
main = do
  Options{..} <- O.execParser opts
  decoded <- decode <$> B.readFile optsGraphPath
  si <- case decoded of
    Left err -> die $ "Could not decode call graph, error: " ++ show err
    Right x -> pure x
  when (V.null . assocTable $ calls si) $
    die "No call graph information was found"
  (fstPackage, exps) <- case M.keys (packages si) of
    [] -> die "No packages were found"
    ps@(nm:_) -> return (Path nm Nothing, map (const PkgCollapsed) ps)
  let l = listData si exps 0
      initialState = AppState si exps fstPackage l
  void $ M.defaultMain app initialState

data Options = Options
  { optsGraphPath  :: FilePath
  }

opts :: O.ParserInfo Options
opts = O.info (options O.<**> O.helper)
       ( O.fullDesc
       <> O.progDesc "CLI UI tool to navigate Haskell call graph"
       <> O.header "navigator - navigate Haskell call graph" )
  where
    options = Options
      <$> O.argument O.str 
          (  O.metavar "GRAPH"
          <> O.help "Path to a call graph file to navigate")

data Path =
  Path PackageName
       (Maybe ModulePath)
  deriving (Eq)

data ModulePath =
  ModulePath ModuleName
             (Maybe FunctionPath)
  deriving (Eq)

data FunctionPath =
  FunctionPath FunctionRef
               (Maybe RefPath)
  deriving (Eq)

data RefPath =
  RefPath FunctionRef
          (Maybe CallPath)
  deriving (Eq)
                                     
data CallPath =
  CallPath SourceLocation
  deriving (Eq)

data PkgExpansion = PkgCollapsed | PkgExpansion [ModuleExpansion]

data ModuleExpansion = ModuleCollapsed | ModuleExpansion [FunctionExpansion]

data FunctionExpansion = FunctionCollapsed | FunctionExpansion [RefExpansion]

data RefExpansion = RefCollapsed | RefExpansion [()]

data AppState = AppState
  { sourceInfo :: SourceInfo
  , expansion :: [PkgExpansion]
  , currentPath :: Path
  , list :: L.List () Path
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

expand :: AppState -> Path -> [PkgExpansion]
expand AppState{..} p = map expandPkg $ zip (M.toList $ packages sourceInfo) expansion
  where
    expandPkg ((pname, modules), ex) =
      let path = Path pname
      in case (isCurrent $ path Nothing, ex) of
        (True, PkgCollapsed) ->
          PkgExpansion $ map (expandMod $ path . Just) $
          zip (M.toList modules) (repeat ModuleCollapsed)
        (False, PkgExpansion modExps) ->
          PkgExpansion $ map (expandMod $ path . Just) $
          zip (M.toList modules) modExps
        (_, e) -> e
    isCurrent = (p ==)
    expandMod path ((mname, modInfo), ex) =
      let modPath = path . ModulePath mname
          funs = modFunctions modInfo
      in case (isCurrent $ modPath Nothing, ex) of
        (True, ModuleCollapsed) ->
          ModuleExpansion $ map (expandFun $ modPath . Just) $
          zip (S.toList funs) (repeat FunctionCollapsed)
        (False, ModuleExpansion funExps) ->
          ModuleExpansion $ map (expandFun $ modPath . Just) $
          zip (S.toList funs) funExps
        (_, e) -> e
    expandFun path (fname, ex) = 
      let funPath = path . FunctionPath fname
          refs = refsFrom fname (calls sourceInfo)
      in case (isCurrent $ funPath Nothing, ex) of
        (True, FunctionCollapsed) ->
          FunctionExpansion $ map (expandRef $ funPath . Just) $
          zip refs (repeat RefCollapsed)
        (False, FunctionExpansion refExps) ->
          FunctionExpansion $ map (expandRef $ funPath . Just) $
          zip refs refExps
        (_, e) -> e
    expandRef path ((ref, locs), ex) = RefCollapsed

collapse :: AppState -> Path -> [PkgExpansion]
collapse AppState{..} p = map collapsePkg $ zip (M.toList $ packages sourceInfo) expansion
  where
    collapsePkg ((pname, modules), ex) =
      case (isCurrent $ Path pname Nothing, ex) of
        (True, PkgExpansion _) ->
          PkgCollapsed
        (_, e) -> e
    isCurrent = (p ==)
    collapseMod ((_mname, _), _) = ModuleCollapsed

goBack :: AppState -> AppState
goBack s@AppState {..} = undefined

refsFrom fname graph =
  case lookupNode fname graph of
    Nothing -> []
    Just (_, refEdges) ->
      catMaybes [(,es) <$> maybeFun n | (n, es) <- IntMap.toList refEdges]
  where
    maybeFun :: Int -> Maybe FunctionRef
    maybeFun n = fmap fst (assocTable graph V.!? n)
              
listData :: SourceInfo -> [PkgExpansion] -> Int -> L.List () Path
listData si exps pos = list & L.listSelectedL ?~ pos
  where
    list = L.list () (V.fromList elems) 1 
    elems = concatMap listPkg $ zip (M.toList $ packages si) exps
    listPkg ((pname, _mods), PkgCollapsed) = [Path pname Nothing]
    listPkg ((pname, mods), PkgExpansion mexps) =
      (Path pname Nothing):
      concatMap (map (Path pname . Just) . listMod) (zip (M.toList mods) mexps)
    listMod ((mname, _), ModuleCollapsed) = [ModulePath mname Nothing]
    listMod ((mname, modInfo), ModuleExpansion fexps) =
      (ModulePath mname Nothing):
      concatMap (map (ModulePath mname . Just) . listFun)
      (zip (S.toList $ modFunctions modInfo) fexps)
    listFun (fname, FunctionCollapsed) = [FunctionPath fname Nothing]
    listFun (fname, FunctionExpansion refexps) =
      let refs = refsFrom fname (calls si)
      in (FunctionPath fname Nothing):
         concatMap (map (FunctionPath fname . Just) . listRef)
         (zip refs refexps)
    listRef ((ref, locs), RefCollapsed) = [RefPath ref Nothing]

drawUi :: AppState  -> [Widget ()]
drawUi AppState {..} = [ui]
  where
    ui = B.borderWithLabel label $ L.renderList listDrawElement True list
    label = txt "Item " <+> cur <+> txt " of " <+> total
    cur =
      case list ^. (L.listSelectedL) of
        Nothing -> txt "-"
        Just i -> str (show (i + 1))
    total = str  $ show $ V.length $ list ^. (L.listElementsL)

listDrawElement :: Bool -> Path -> Widget ()
listDrawElement sel p =
    let (indent, path) = renderPath p
        selTxt t = if sel
                   then withAttr customAttr (txt $ "<" <> t <> ">")
                   else txt t
    in maybe emptyWidget txt indent <+> selTxt path

renderPath :: Path -> (Maybe Text, Text)
renderPath (Path pkg mpath) =
  case mpath of
    Nothing -> (Nothing, pkg)
    Just (ModulePath mname fpath) ->
      case fpath of
        Nothing -> (Just "  ", mname)
        Just (FunctionPath fref refpath) ->
          case refpath of
            Nothing -> (Just "    ", renderRef fref)
            Just (RefPath  refref callpath) ->
              case callpath of
                Nothing ->
                  (Just "      -> ", renderRef refref)
                Just (CallPath srcLoc) ->
                  (Just "           ", renderLoc srcLoc)

renderRef :: FunctionRef -> Text
renderRef (UnresolvedRef ur) = ur
renderRef (ResolvedRef fd) = functionName fd

renderLoc :: SourceLocation -> Text
renderLoc SourceLocation{..} =
  "<Line " <> T.pack (show sourceLine) <>
  ", Column " <> T.pack (show sourceColumn) <> ">"

customAttr :: A.AttrName
customAttr = L.listSelectedAttr `mappend` "custom"

appEvent :: AppState -> T.BrickEvent () e -> T.EventM () (T.Next AppState)
appEvent s@AppState{..} (T.VtyEvent e)  =
  case e of
    Vty.EvKey Vty.KEsc [] -> M.halt s
    Vty.EvKey (Vty.KChar '+') [] ->
      case L.listSelectedElement list of
        Nothing -> M.continue s
        Just (i, p) -> do
          let ex = expand s p
              list' = listData sourceInfo ex i
          M.continue $ s{expansion = ex, list = list'}
    Vty.EvKey (Vty.KChar '-') [] ->
      case L.listSelectedElement list of
        Nothing -> M.continue s
        Just (i, p) -> do
          let ex = collapse s p
              list' = listData sourceInfo ex i
          M.continue $ s{expansion = ex, list = list'}
    _ -> do
      list' <- L.handleListEvent e list
      M.continue s{list=list'}
appEvent s _ = M.continue s
