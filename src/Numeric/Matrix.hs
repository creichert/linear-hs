{-# LANGUAGE CPP #-}
{-# OPTIONS -Wall #-}
-----------------------------------------------------------------------------
---- |
---- Module : Numeric.Matrix
---- License : BSD3
---- Maintainer : Christopher Reichert <creichert07@gmail.com>
---- Stability : experimental
----
---- Matrix
----
---- It can be assumed that these operations are undefined for Vectors
---- of unequal length.


module Numeric.Matrix(
        Matrix(..)
      , mkMatrix
      ) where

data Matrix a = Matrix Int Int [[a]]
    deriving (Show)

instance Eq a => Eq (Matrix a) where
    (==) (Matrix r1 c1 xs) (Matrix r2 c2 ys) = r1 == r2 && c1 == c2 && xs == ys

instance (Num a) => Num (Matrix a) where
    (+)         = error "Undefined operation for matrices"
    (-)         = error "Undefined operation for matrices."
    (*)         = error "Undefined operation for matrices."
    abs         = error "Undefined operation for matrices."
    signum      = error "Undefined operation for matrices."
    fromInteger = error "Undefined operation for matrices."

-- | Create a new matrix from a list of lists of Int.
mkMatrix :: (Num a) => [[a]] -> Matrix a
mkMatrix [] = Matrix 0    0    [[0]]
mkMatrix xs = Matrix rows cols xs
    where rows = length xs
          cols = length $ head xs

