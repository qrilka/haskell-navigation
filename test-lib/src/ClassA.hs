{-# LANGUAGE FlexibleInstances #-}
module ClassA where

class ClassA a where
  functionA :: a -> [a]

instance ClassA Char where
  functionA = replicate 3

instance ClassA Int where
  functionA x = [x, 1 + x]
