module B
  ( bFunction1
  , bFunction2
  ) where

import A (aConst, aFunction)

bFunction1 :: Int -> Int -> Int
bFunction1 x y = bFunction2 x + aFunction y + aConst + bInner
  where
    bInner = aConst + bConst

bFunction2 :: Int -> Int
bFunction2 x = aConst * x

bConst :: Int
bConst = 666
