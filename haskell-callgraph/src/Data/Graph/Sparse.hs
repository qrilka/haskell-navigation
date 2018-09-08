{-# LANGUAGE RecordWildCards #-}
module Data.Graph.Sparse
  ( SparseGraph(..)
  , fromList
  , lookupNode
  ) where

import RIO as RIO
import qualified Data.IntMap as IntMap
import qualified RIO.Map as M
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
