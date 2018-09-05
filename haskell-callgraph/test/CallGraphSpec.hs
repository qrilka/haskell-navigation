{-# LANGUAGE OverloadedStrings #-}
module CallGraphSpec
  ( spec
  ) where

import CallGraph
import Test.Hspec

spec :: Spec
spec = do
  describe "CallGraph" $ do
    it "proper path from simple linear graph" $ do
      let graph1 = fromEdges [((), 0, [1]),((), 1, [2]),((),2,[])]
      pathsBetween graph1 0 2 `shouldBe` [[0, 1, 2]]
    it "proper multiple paths" $ do
      let graph2 = fromEdges [((), 0, [1, 2]),((), 1, [2]),((),2,[])]
      pathsBetween graph2 0 2 `shouldBe` [[0, 1, 2],[0,2]]
    it "proper paths from cyclic graph" $ do
      let graph2 = fromEdges [((), 0, [1, 2]),((), 1, [2,0]),((),2,[0])]
      pathsBetween graph2 0 2 `shouldBe` [[0, 1, 2],[0,2]]
