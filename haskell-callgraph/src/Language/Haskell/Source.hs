{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
module Language.Haskell.Source
  ( PackageName
  , ModuleName
  , FunctionDefinition(..)
  , FunctionCall(..)
  , SourceLocation(..)
  , FunctionCallGraph
  , SourceInfo(..)
  , fromKCallGraph
  , printSourceInfo
  ) where

import CallGraph hiding (Node)
import Control.Monad.State.Strict
import Data.Graph
import Data.Graph.Sparse as SG
import qualified Data.IntMap as IntMap
import Data.List.Extra (groupSort)
import qualified Data.Vector.Generic.Mutable as MV
import Language.Kythe.Schema.Typed
import qualified Proto.Kythe.Proto.Storage as K
import qualified Proto.Kythe.Proto.Storage_Fields as K
import RIO
import RIO.List (find, intercalate, nub)
import qualified RIO.Map as M
import qualified RIO.Set as S
import qualified RIO.Text as T
import qualified RIO.Vector as V

type PackageName = Text
type ModuleName = Text

data FunctionDefinition = FunctionDefinition
  { package :: !PackageName
  , module_ :: !ModuleName
  , functionName :: !Text
  } deriving (Eq, Ord, Show)

data FunctionCall = FunctionCall !SourceLocation -- Byte offset or line/column
  deriving (Eq, Show)

data SourceLocation = SourceLocation Int deriving (Eq, Show)

type FunctionCallGraph = SparseGraph FunctionDefinition [FunctionCall]

data SourceInfo = SourceInfo
  { calls :: FunctionCallGraph
  , packages :: Map PackageName (Set ModuleName)
  , files :: Map ModuleName (FilePath, ByteString)
  , modules :: Map ModuleName (Set FunctionDefinition)
  } deriving (Eq, Show)

type Roots
   = ( Map PackageName (Set ModuleName)
     , Map ModuleName (Set FunctionDefinition)
     , Map ModuleName (FilePath, ByteString)
     , Map K.VName  (FunctionDefinition, [(K.VName, [FunctionCall])]))


fromKCallGraph :: KCallGraph -> SourceInfo
fromKCallGraph CallGraph{..} = collect $ execState go noInfo
  where
    collect (ps, ms, fs, fcs) = -- traceShow (fcs) $
      let fvnames = M.fromList $ zip (M.keys fcs) [0..] 
          fixCalls cs = flip mapMaybe cs $ \(vn, fCalls) -> do
            fIndex <- M.lookup vn fvnames
            return (fIndex, fCalls)            
      in SourceInfo {
      calls = SG.fromList $ map (second fixCalls) $ M.elems fcs
      , packages = ps
      , modules = ms
      , files = fs
      }
    noInfo = (M.empty, M.empty, M.empty, M.empty)
    knodes :: Vector KNode
    knodes = V.fromList $ flip map (vertices cgGraph) $ \v ->
      let (node, _, _) = cgNodeFromVertex v
      in case parseFacts node of
           Just knode -> knode
           Nothing -> error $ "Bad node " ++ show node
    fVnames = flip V.mapMaybe knodes $ \KNode{..} ->
      case kind of
        VariableNK -> Just vname
        _ -> Nothing
    fVnameIndices = M.fromList [ (fname, i) | (i, fname) <- V.toList $ V.indexed fVnames]
    colon = ':'
    extractFunctionName :: K.VName -> Text
    extractFunctionName vname = last . T.split (== colon) $ vname ^. K.signature
    go :: State Roots ()
    go =
      forM_ (V.toList $ V.indexed knodes) $ \(v, knode) -> do
      if kind knode == PackageNK
        then parsePackage v knode
        else return () -- starting from packages

    parsePackage :: Vertex -> KNode -> State Roots ()
    parsePackage v pKnode = do
      let (pNode, _, pBranches) = cgNodeFromVertex v
      (ps, ms, fs, fcs) <- {-traceShow ("### visited top", v, facts pKnode) $ -}
        get
      let (pname, mname) = parsePackageVName (vname pKnode)
          pms' = case M.lookup pname ps of
                   Nothing -> S.singleton mname
                   Just pms -> S.insert mname pms
      put ( M.insert pname pms' ps
          , M.insert mname S.empty ms
          , fs
          , fcs)
      forM_ (mapMaybe cgVertexFromKey pBranches) $ \v' ->
        let Just fKnode = knodes V.!? v'
            (_, _, fBranches) = cgNodeFromVertex v'
        in case -- traceShow (">>> visited in pkg", v', facts fKnode) $
                kind fKnode of
          FileNK -> do
            addFileInfo mname (vname fKnode) (facts fKnode)
            return Nothing
          VariableNK -> do
            Just <$> parseDefinition (pname, mname) fKnode v'
          _ -> error $ "Unexpected node in package: " ++ show (pname, mname) ++":" ++ show (facts fKnode)
    addFileInfo :: ModuleName -> K.VName -> [K.Entry] -> State Roots ()
    addFileInfo mname vname node = modify' $ \(ps, ms, fs, fcs) ->
      let fname = T.unpack $ vname ^. K.path
          text = case findFact textFactName node of
            Just fact -> fact ^. K.factValue
            Nothing -> ""
      in (ps, ms, M.insert mname (fname, text) fs, fcs)
    parseDefinition :: (PackageName, ModuleName) -> KNode -> Vertex -> State Roots ()
    parseDefinition (pname, mname) dKnode v = do
      let fname = extractFunctionName $ vname dKnode
          definition = FunctionDefinition pname mname fname
          fIndex = case M.lookup (vname dKnode) fVnameIndices of
            Just i -> i
            Nothing -> error $ "Unknown definition VName " ++
                       show (vname dKnode)
          (_, _, branches) = cgNodeFromVertex v
          fCalls = groupSort . flip mapMaybe branches $ \branch -> do
            av <- cgVertexFromKey branch
            aKnode <- knodes V.!? av
            if kind aKnode == AnchorNK
              then extractCall aKnode av
              else fail "bad node kind"
      modify' $ \(ps, ms, fs, fcs) ->
        ( ps
        , M.insertWith S.union mname (S.singleton definition) ms
        , fs
        , M.insert (vname dKnode) (definition, fCalls) fcs
        )

    extractCall :: KNode -> Vertex -> Maybe (K.VName, FunctionCall)
    extractCall aKnode v = do
      locFact <- findFact locStartFactName (facts aKnode)
      start <- readMaybe (T.unpack . decodeUtf8With lenientDecode $ locFact ^. K.factValue)
      let (_, _, branches) = cgNodeFromVertex v
      -- for some reason haskell-indexer seem to create duplicated entries sometimes
      case nub branches of
        [refKey] -> do
          v <- cgVertexFromKey refKey
          rKnode <- knodes V.!? v
          return (vname rKnode, FunctionCall (SourceLocation start))
        x -> error $ "Anchor with bad branches " ++ show (x, facts aKnode)
      
    parseInFile :: (PackageName, ModuleName) -> Vertex -> State Roots ()
    parseInFile (pname, mname) v =
      let Just knode = knodes V.!? v
          (_, _, branches) = cgNodeFromVertex v
      in case kind of
        x -> error $ "Encountered " ++ show (facts knode)

parseFacts :: [K.Entry] -> Maybe KNode
parseFacts facts = do
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

kindFactName = "/kythe/node/kind"
textFactName = "/kythe/text"
locStartFactName ="/kythe/loc/start"

findFact :: Text -> [K.Entry] -> Maybe K.Entry
findFact name = find (\f -> f ^. K.factName == name)

nodeKind :: [K.Entry] -> Maybe NodeKind
nodeKind facts = do
  kindFact <- findFact kindFactName facts
  parseNodeKind (decodeUtf8With lenientDecode $ kindFact ^. K.factValue)

parseNodeKind :: Text -> Maybe NodeKind
parseNodeKind "anchor" = Just AnchorNK  
parseNodeKind "file" = Just FileNK    
parseNodeKind "package" = Just PackageNK 
parseNodeKind "variable" = Just VariableNK
parseNodeKind _ = Nothing

parsePackageVName :: K.VName -> (PackageName, ModuleName)
parsePackageVName vname =
  let components = T.split (== '-') (vname ^. K.signature)
  in case reverse components of
       [noDashes] -> T.break (==':') noDashes
       (hashMod:_version:packageRev) ->
         ( T.intercalate "-" $ reverse packageRev
         , T.drop 23 hashMod -- 22 chars hash + ':'
          )
       x ->
         error $
         "Could not extract package name and module name from " ++
         show components

-- | basic printer
printSourceInfo :: SourceInfo -> IO ()
printSourceInfo SourceInfo{..} =
  forM_ (M.toList packages) $ \(pname, moduleNames) -> do
    putStrLn $ "Package: " ++ show pname
    forM_ (S.toList moduleNames) $ \mname -> do
      putStrLn $ "-- Module: " ++ show mname
      forM_ (M.lookup mname modules) $ \definitions ->
        forM_ (S.toList definitions) $ \definition -> do
          putStrLn $ "-- -- Function: " ++ show (functionName definition)
          forM_ (lookupNode definition calls) $ \(_, outcalls) -> do
            forM_ (IntMap.toList outcalls) $ \(i, fCalls) -> do
              let target = maybe "<unknown>" (functionName . fst) $
                           assocTable calls V.!? i
              putStrLn $ "-- -- -- Calls " ++ show target ++ " (" ++
                intercalate "," (map show fCalls) ++ ")"

