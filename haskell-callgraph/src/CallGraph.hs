{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module CallGraph
  ( CallGraph(..)
  , KCallGraph
  , fromStream
  , fromEdges
  , pathsBetween
  , children
  , nodeLabel
  ) where

import Conduit
import qualified Data.Attoparsec.ByteString as AB
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.Conduit.Combinators as C
import Data.Graph (graphFromEdges, Graph, Vertex)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.Monoid
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Lens.Family2 ((^.), to)
import qualified Proto.Kythe.Proto.Storage as K
import qualified Proto.Kythe.Proto.Storage_Fields as K

type Node = [K.Entry]
-- using path + signature for now
type Key = (Text, Text)

type KCallGraph = CallGraph Node Key

data CallGraph n k = CallGraph
  { cgGraph :: Graph
  , cgNodeFromVertex :: Vertex -> (n, k, [k])
  , cgVertexFromKey :: k -> Maybe Vertex
  }

children :: Vertex -> CallGraph n k -> [Vertex]
children i CallGraph{..} = mapMaybe cgVertexFromKey children
  where
    (_, _, children) = cgNodeFromVertex i

nodeLabel :: Vertex -> KCallGraph -> Text
nodeLabel v CallGraph {..} = labeledFacts facts
  where
    (facts, _, _) = cgNodeFromVertex v
    labeledFacts = T.intercalate "," . mapMaybe labelFact
    labelFact :: K.Entry -> Maybe Text
    labelFact n = case n ^. K.factName of
      "/kythe/node/kind" ->
        Just $ T.decodeUtf8 (n ^. K.factValue) <> ":" <> n ^. K.source . K.signature
      _ ->
        Nothing

fromStream :: MonadIO m => ConduitT K.Entry Void m KCallGraph
fromStream = fromEdges <$> collectEdges

fromEdges :: Ord k => [(n, k, [k])] -> CallGraph n k
fromEdges edges = CallGraph {..}
  where
    (cgGraph, cgNodeFromVertex, cgVertexFromKey) = graphFromEdges edges

collectEdges :: MonadIO m => ConduitT K.Entry Void m [([K.Entry], Key, [Key])]
collectEdges =
  map (\(nm, (facts, children)) -> (facts, nm, children)) . M.toList <$>
  C.foldl collectEntry M.empty
  where
    collectEntry ::
         M.Map Key ([K.Entry], [Key])
      -> K.Entry
      -> (M.Map Key ([K.Entry], [Key]))
    collectEntry collected e =
      let source = e ^. K.source . to vname2key
          target = e ^. K.target . to vname2key
      in if e ^. K.factName == "/"
           then let (s', t') =
                      case e ^. K.edgeKind of
                        "/kythe/edge/childof" -> (target, source)
                        _ -> (source, target)
                in case M.lookup s' collected of
                     Nothing -> M.insert s' ([], [t']) collected
                     Just (x, ys) -> M.insert s' (x, t' : ys) collected
           else case M.lookup source collected of
                  Nothing -> M.insert source ([e], []) collected
                  Just (facts, xs) -> M.insert source (e : facts, xs) collected

vname2key :: K.VName -> Key
vname2key vn = (vn ^. K.path, vn ^. K.signature)

pathsBetween :: CallGraph n k -> Vertex -> Vertex -> [[Vertex]]
pathsBetween g x y = go x y (Set.singleton x)
  where
    go source target visited
      | source == target = [[source]]
      | otherwise =
        [ source : path
        | child <- children source g
        , Set.notMember child visited
        , path <- go child target (Set.insert child visited)
        ]
