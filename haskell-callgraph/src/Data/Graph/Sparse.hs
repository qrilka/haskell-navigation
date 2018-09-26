{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Data.Graph.Sparse
  ( SparseGraph(..)
  , fromList
  , union
  , mapNodes
  , lookupNode
  , pathsBetween
  , pathsForestBetween
  , PathTree (..)
  , PathForest
  ) where

import Data.Store
import qualified Data.IntMap as IntMap
import RIO as RIO
import qualified RIO.Map as M
import qualified RIO.Set as Set
import qualified RIO.Vector as V

data SparseGraph node edge = SparseGraph
  { assocTable :: Vector (node, IntMap edge)
  , nodeIndices :: Map node Int
  } deriving (Eq, Show, Generic)

instance (Ord node, Semigroup edge) => Semigroup (SparseGraph node edge) where
  (<>) = union

instance (Ord node, Semigroup edge) => Monoid (SparseGraph node edge) where
  mempty = SparseGraph mempty mempty
  mappend = (<>)

instance (Ord node, Store node, Store edge) =>
         Store (SparseGraph node edge)

fromList :: Ord node => [(node, [(Int, edge)])] -> SparseGraph node edge
fromList nodes = SparseGraph{..}
  where
    assocTable = V.fromList $ map (second IntMap.fromList) nodes
    nodeIndices = M.fromList $ zip (map fst nodes) [0..]

-- | has some preconditions
union ::
     (Ord node, Semigroup edge)
  => SparseGraph node edge
  -> SparseGraph node edge
  -> SparseGraph node edge
union (SparseGraph t1 i1) (SparseGraph t2 i2) =
  SparseGraph table indices
  where
    newNodes = flip V.mapMaybe (V.indexed t2) $ \(i, (n, cs)) ->
      if n `M.notMember` i1 then Just (i, (n, cs)) else Nothing

    fromI2 :: Map Int Int
    fromI2 = M.fromList . V.toList $
      V.mapMaybe (\(v2, (n, _)) -> (v2,) <$> M.lookup n i1) (V.indexed t2) <>
      V.map (\(v0, v2)-> (v2, V.length t1 + v0)) (V.indexed $ V.map fst newNodes)

    indices = M.fromList [(n, i) | (i, (n, _)) <- V.toList $ V.indexed table]

    table = t1' <> V.map (second remapCalls) (V.map snd newNodes)

    t1' = flip V.map t1 $ \(n, calls1) ->
      case M.lookup n i2 of
        Just v2 | Just (_, calls2) <- t2 V.!? v2 ->
          let calls2' = remapCalls calls2
          in (n, IntMap.unionWith (<>) calls2' calls1)
        _ -> (n, calls1)

    remapCalls calls =
      let reKey (k, c) = do
            i <- M.lookup k fromI2
            pure (i, c)
      in IntMap.fromList $ mapMaybe reKey $ IntMap.toList calls

mapNodes ::
     (Ord node1, Ord node2)
  => (node1 -> node2)
  -> SparseGraph node1 edge
  -> SparseGraph node2 edge
mapNodes f g = SparseGraph table indices
  where
    table = V.map (first f) (assocTable g)
    indices = M.fromList [(n, i) | (i, (n, _)) <- V.toList $ V.indexed table]

lookupNode :: Ord node => node -> SparseGraph node edge -> Maybe (node, IntMap edge)
lookupNode n SparseGraph{..} = do
  i <- M.lookup n nodeIndices
  assocTable V.!? i

-- | Note: doesn't include source node (we have no incoming edge for it)
pathsBetween :: SparseGraph n e -> Int -> Int -> [[(Int, e)]]
pathsBetween SparseGraph{..} x y = go x y (Set.singleton x)
  where
    go source target visited
      | source == target = [[]]
      | otherwise =
        [ (child, edge) : path
        | (child, edge) <- maybe [] (IntMap.assocs . snd) $ assocTable V.!? source
        , Set.notMember child visited
        , path <- go child target (Set.insert child visited)
        ]

data PathTree e
  = PathEnd
  | PathTree ![(e, PathTree e)]
  deriving (Show)
type PathForest e = [PathTree e]

pathsForestBetween :: SparseGraph n e -> Int -> Int -> PathForest (Int, e)
pathsForestBetween SparseGraph{..} x y = go x y (Set.singleton x)
  where
    go source target visited
      | source == target = [PathEnd]
      | otherwise =
        [ PathTree [((child, edge), joinTrees subforest)]
        | (child, edge) <- maybe [] (IntMap.assocs . snd) $ assocTable V.!? source
        , Set.notMember child visited
        , let subforest = go child target (Set.insert child visited)
        , not $ null subforest
        ]
    joinTrees xs = case concat (map pathBranches xs) of
      [] -> PathEnd
      ys -> PathTree ys
    pathBranches PathEnd = []
    pathBranches (PathTree bs) = bs
