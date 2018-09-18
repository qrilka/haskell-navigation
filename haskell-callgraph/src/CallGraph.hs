{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
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
import qualified Data.Conduit.Combinators as C
import Data.Graph (graphFromEdges, Graph, Vertex)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Monoid
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Lens.Family2 ((^.))
import qualified Proto.Kythe.Proto.Storage as K
import qualified Proto.Kythe.Proto.Storage_Fields as K
import Text.Regex

type Node = [K.Entry]
type Key = K.VName

type KCallGraph = CallGraph Node Key

data CallGraph n k = CallGraph
  { cgGraph :: Graph
  , cgNodeFromVertex :: Vertex -> (n, k, [k])
  , cgVertexFromKey :: k -> Maybe Vertex
  }

children :: Vertex -> CallGraph n k -> [Vertex]
children i CallGraph{..} = mapMaybe cgVertexFromKey branches
  where
    (_, _, branches) = cgNodeFromVertex i

nodeLabel :: Vertex -> KCallGraph -> Text
nodeLabel v CallGraph {..} = labeledFacts facts
  where
    (facts, _, _) = cgNodeFromVertex v
    labeledFacts = T.intercalate "," . mapMaybe labelFact
    labelFact :: K.Entry -> Maybe Text
    labelFact n = case n ^. K.factName of
      "/kythe/node/kind" ->
        case n ^. K.factValue of
          "file" -> Just $ "file:" <> n ^. K.source . K.path
          _ -> Just $ T.decodeUtf8 (n ^. K.factValue) <> ":" <>
               simplify (n ^. K.source . K.signature)
      _ ->
        Nothing
    simplify t = removePackageHash . fromMaybe t $ T.stripPrefix "haskell:" t
    removePackageHash = T.pack . (\s -> subRegex hyphenHashColon s ":") . T.unpack
    hyphenHashColon = mkRegex "-\\w{22}:"

fromStream :: MonadIO m => ConduitT K.Entry Void m KCallGraph
fromStream = fromEdges <$> collectEdges

fromEdges :: Ord k => [(n, k, [k])] -> CallGraph n k
fromEdges edges = CallGraph {..}
  where
    (cgGraph, cgNodeFromVertex, cgVertexFromKey) = graphFromEdges edges

collectEdges :: MonadIO m => ConduitT K.Entry Void m [([K.Entry], Key, [Key])]
collectEdges =
  map (\(nm, (facts, branches)) -> (facts, nm, branches)) . M.toList <$>
  C.foldl collectEntry M.empty
  where
    collectEntry ::
         M.Map Key ([K.Entry], [Key])
      -> K.Entry
      -> (M.Map Key ([K.Entry], [Key]))
    collectEntry collected e =
      let source = e ^. K.source
          target = e ^. K.target
      in if e ^. K.factName == "/"
           then let (s', t') =
                      case e ^. K.edgeKind of
                        "/kythe/edge/childof" -> (target, source)
                        _ -> (source, target)
                in case M.lookup s' collected of
                     Nothing ->
                       insertMissing t' ([],[]) $
                       M.insert s' ([], [t']) collected
                     Just (x, ys) ->
                       insertMissing t' ([],[]) $
                       M.insert s' (x, t' : ys) collected
           else case M.lookup source collected of
                  Nothing -> M.insert source ([e], []) collected
                  Just (facts, xs) -> M.insert source (e : facts, xs) collected
    insertMissing k v = flip M.alter k $ \case
      Just x -> Just x
      Nothing -> Just v

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
