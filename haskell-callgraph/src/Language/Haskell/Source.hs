{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
module Language.Haskell.Source
  ( PackageName
  , ModuleName
  , ModuleInfo(..)
  , FunctionDefinition(..)
  , FunctionCall(..)
  , SourceLocation(..)
  , FunctionCallGraph
  , BaseSourceInfo(..)
  , RawSourceInfo
  , fromRaw
  , FunctionRef(..)
  , SourceInfo
  , fromKCallGraph
  ) where

import CallGraph
import Control.Monad.State.Strict
import Data.Store
import Data.Graph
import Data.Graph.Sparse as SG
import Data.List.Extra (groupSort)
import Language.Kythe.Schema.Typed
import qualified Proto.Kythe.Proto.Storage as K
import qualified Proto.Kythe.Proto.Storage_Fields as K
import RIO
import RIO.List (find, isSuffixOf, nub, partition)
import qualified RIO.Map as M
import qualified RIO.Set as Set
import qualified RIO.Text as T
import qualified RIO.Vector as V

type PackageName = Text
type ModuleName = Text
type FunctionName = Text

data FunctionDefinition = FunctionDefinition
  { package :: !PackageName
  , module_ :: !ModuleName
  , functionName :: !FunctionName
  } deriving (Eq, Ord, Show, Generic)

instance Store FunctionDefinition

data SourceLocation = SourceLocation Int deriving (Eq, Show, Generic)

instance Store SourceLocation

data FunctionCall = FunctionCall !SourceLocation -- Byte offset or line/column
  deriving (Eq, Show, Generic)

instance Store FunctionCall

type BaseFunctionCallGraph name = SparseGraph name [FunctionCall]

type FunctionCallGraph = BaseFunctionCallGraph FunctionRef

data ModuleInfo name = ModuleInfo
  { modPath :: FilePath
  , modText :: ByteString
  , modFunctions :: Set name
  } deriving (Eq, Show, Generic)

instance (Ord name, Store name) => Store (ModuleInfo name)

data BaseSourceInfo name = BaseSourceInfo
  { calls :: BaseFunctionCallGraph name
  , packages :: Map PackageName (Map ModuleName (ModuleInfo name))
  } deriving (Eq, Show, Generic)

instance (Ord name) => Semigroup (BaseSourceInfo name) where
  (<>) si1 si2 =
    BaseSourceInfo
    { calls = calls si1 `union` calls si2
    , packages = packages si1 <> packages si2
    }

instance (Ord name) => Monoid (BaseSourceInfo name) where
  mempty = BaseSourceInfo mempty mempty
  mappend = (<>)

type RawSourceInfo = BaseSourceInfo K.VName
type SourceInfo = BaseSourceInfo FunctionRef

data FunctionRef
  = ResolvedRef FunctionDefinition
  | UnresolvedRef Text
  deriving (Eq, Ord, Show, Generic)

instance Store FunctionRef

instance (Ord name, Store name) => Store (BaseSourceInfo name)

fromRaw :: RawSourceInfo -> SourceInfo
fromRaw raw = BaseSourceInfo calls' packages'
  where
    (packages', vname2def) = flip runState M.empty $
      flip M.traverseWithKey (packages raw) $
      \pn modules -> flip M.traverseWithKey modules (mapVName pn)
    mapVName :: PackageName -> ModuleName -> ModuleInfo K.VName -> State (Map K.VName FunctionDefinition) (ModuleInfo FunctionRef)
    mapVName pn mn ModuleInfo{modFunctions=fs,..} = do
      fs' <- fmap Set.fromList . forM (Set.toList fs) $ \vName -> do
        let fn = extractFunctionName vName
            definition = FunctionDefinition pn mn fn
        modify $ M.insert vName definition
        return $ ResolvedRef definition
      pure ModuleInfo{modFunctions=fs',..}
    extractFunctionName :: K.VName -> Text
    extractFunctionName vname = last . T.split (== ':') $ vname ^. K.signature
    calls' = SG.mapNodes resolveRef (calls raw)
    resolveRef vn = case M.lookup vn vname2def of
                      Just d -> ResolvedRef d
                      Nothing -> UnresolvedRef (vn ^. K.signature)

type Roots
   = ( Map PackageName (Map ModuleName (ModuleInfo K.VName))
     , Map K.VName  [(K.VName, [FunctionCall])])

fromKCallGraph :: KCallGraph -> BaseSourceInfo K.VName
fromKCallGraph CallGraph{..} = collect $ execState go noInfo
  where
    collect (ps, fcs) = -- traceShow (fcs) $
      let fvnames = M.fromList $ zip (M.keys fcs) [0..] 
          fixCalls cs = flip mapMaybe cs $ \(vn, fCalls) -> do
            fIndex <- M.lookup vn fvnames
            return (fIndex, fCalls)
      in BaseSourceInfo {
        calls = SG.fromList $ map (second fixCalls) $ M.toList fcs
        , packages = ps
        }
    noInfo = (M.empty, M.empty)
    knodes :: Vector (Maybe KNode)
    knodes = V.fromList $ flip map (vertices cgGraph) $ \v ->
      let (node, k, _) = cgNodeFromVertex v
      in parseFacts k node
    go :: State Roots ()
    go =
      forM_ (V.toList $ V.indexed knodes) $ \(v, mKnode) -> forM_ mKnode $ \knode ->do
      if kind knode == PackageNK -- Kythe package == Haskell module
        then parseModule v knode
        else return () -- starting from packages

    parseModule :: Vertex -> KNode -> State Roots ()
    parseModule v pKnode = do
      let (_, _, pBranches) = cgNodeFromVertex v
          (pname, mname) = parsePackageVName (vname pKnode)
          subKnodes = flip mapMaybe pBranches $ \k -> do
            v' <- cgVertexFromKey k
            knode <- join $ knodes V.!? v'
            pure (v', knode)
      case -- traceShow (vname pKnode, length pBranches) $
        partition (\(_, k) -> kind k == FileNK) subKnodes of
        (fKnodes, other) | Just (p, txt) <- getFileInfo
                                            (map snd fKnodes)  -> do
          functions <- forM other $ \(v', vKnode) ->
            parseDefinition vKnode v'
          let modInfo = ModuleInfo
                { modPath = p
                , modText = txt
                , modFunctions = Set.fromList functions
                }
              pmsDelta = M.singleton pname $
                M.singleton mname modInfo
          modify $ first (M.unionWith (<>) pmsDelta)
        (found, _other) ->
          error $ "Expected to have 1 file node for module " ++ show (pname, mname) ++
            " vname " ++ show (vname pKnode) ++ " node " ++ show (facts pKnode) ++ " but found " ++ show (length found) ++
            "\n all children: " ++ show (map (vname . snd) subKnodes)

    getFileInfo ::[KNode] -> Maybe (FilePath, ByteString)
    getFileInfo [kn] = Just $ getFileInfo' kn
    getFileInfo xs =
      let fileInfos = map getFileInfo' xs
      in case partition ((".hs-boot" `isSuffixOf`) . fst) fileInfos of
        (_, [x]) -> Just x
        _ -> Nothing
    getFileInfo' knode =
      let fpath = T.unpack $ knode ^. to vname . K.path
          text = case findFact textFactName (facts knode) of
            Just f -> f ^. K.factValue
            Nothing -> ""
      in (fpath, text)
    parseDefinition :: KNode -> Vertex -> State Roots K.VName
    parseDefinition dKnode v = do
      let (_, _, branches) = cgNodeFromVertex v
          !fCalls = groupSort . flip mapMaybe branches $ \branch -> do
            av <- cgVertexFromKey branch
            aKnode <- join $ knodes V.!? av
            if kind aKnode == AnchorNK
              then extractCall aKnode av
              else error "bad node kind"
      forM_ fCalls $ \(vn, _) -> do
        let maybeInsertEmpty (Just x) = Just x
            maybeInsertEmpty Nothing = Just []
        modify' $ second (M.alter maybeInsertEmpty vn)
      modify' $ second (M.insert (vname dKnode) fCalls)
      return $ vname dKnode

    extractCall :: KNode -> Vertex -> Maybe (K.VName, FunctionCall)
    extractCall aKnode v = do
      locFact <- findFact locStartFactName (facts aKnode)
      start <- readMaybe (T.unpack . decodeUtf8With lenientDecode $ locFact ^. K.factValue)
      let (_, _, branches) = cgNodeFromVertex v
      -- for some reason haskell-indexer seem to create duplicated entries sometimes
      case nub branches of
        [refKey] -> do
          v' <- cgVertexFromKey refKey
          rKnode <- join $ knodes V.!? v'
          return (vname rKnode, FunctionCall (SourceLocation start))
        x -> error $ "Anchor with bad branches " ++ show (x, facts aKnode)

parseFacts :: K.VName -> [K.Entry] -> Maybe KNode
parseFacts vn [] = pure $ KNode VariableNK vn [] -- assuming orphan nodes to be function refs
parseFacts _ facts = do
  kindFact <- find (\f -> f ^. K.factName == kindFactName) facts
  kind <- parseNodeKind (decodeUtf8With lenientDecode $ kindFact ^. K.factValue)
  let vname = kindFact ^. K.source
  return KNode{..}

deriving instance Eq NodeKind

data KNode = KNode
  { kind :: NodeKind
  , vname :: K.VName
  , facts :: [K.Entry]
  }

kindFactName :: Text
kindFactName = "/kythe/node/kind"
textFactName :: Text
textFactName = "/kythe/text"
locStartFactName :: Text
locStartFactName ="/kythe/loc/start"

findFact :: Text -> [K.Entry] -> Maybe K.Entry
findFact name = find (\f -> f ^. K.factName == name)

parseNodeKind :: Text -> Maybe NodeKind
parseNodeKind "anchor" = Just AnchorNK  
parseNodeKind "file" = Just FileNK    
parseNodeKind "package" = Just PackageNK 
parseNodeKind "variable" = Just VariableNK
parseNodeKind _ = Nothing

parsePackageVName :: K.VName -> (PackageName, ModuleName)
parsePackageVName vname =
  let parts = T.split (== '-') (vname ^. K.signature)
  in case reverse parts of
       [noDashes] -> second (T.drop 1) $ T.break (== ':') noDashes
       (hashMod:_version:packageRev) ->
         ( T.intercalate "-" $ reverse packageRev
         , T.drop 23 hashMod -- 22 chars hash + ':'
          )
       _ ->
         error $
         "Could not extract package name and module name from " ++ show parts
