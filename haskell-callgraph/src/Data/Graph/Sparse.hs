{-# LANGUAGE RecordWildCards #-}
module Data.Graph.Sparse
  ( SparseGraph(..)
  , fromList
  , lookupNode
  , pathsBetween
  ) where

import RIO as RIO
import qualified Data.IntMap as IntMap
import qualified RIO.Map as M
import qualified RIO.Set as Set
import qualified RIO.Vector as V

data SparseGraph node edge = SparseGraph
  { assocTable :: Vector (node, IntMap edge)
  , nodeIndices :: Map node Int
  } deriving (Eq, Show)

fromList :: Ord node => [(node, [(Int, edge)])] -> SparseGraph node edge
fromList nodes = SparseGraph{..}
  where
    assocTable = V.fromList $ map (second IntMap.fromList) nodes
    nodeIndices = M.fromList $ zip (map fst nodes) [0..]

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
