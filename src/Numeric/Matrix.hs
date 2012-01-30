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
---- It can be assumed that these operations are undefined for matrices
---- of unequal length.

module Numeric.Matrix(
        Matrix(..)
      , mkMatrix
      , add
      , rows
      , cols
      , (-), subtract
      , negate
      , (*), multiply
      ) where

import Prelude hiding (subtract, negate)
import qualified Prelude as P (negate)

import Data.List

data Matrix a = Matrix Int Int [[a]]
    deriving (Show)

instance Eq a => Eq (Matrix a) where
    (==) (Matrix r1 c1 xs) (Matrix r2 c2 ys) = r1 == r2 && c1 == c2 && xs == ys

instance (Num a) => Num (Matrix a) where
    (+)         = add
    (-)         = subtract
    (*)         = multiply
    abs         = error "Undefined operation for matrices."
    signum      = error "Undefined operation for matrices."
    fromInteger = error "Undefined operation for matrices."

-- | Create a new matrix from a list of lists of Int.
mkMatrix :: (Num a) => [[a]] -> Matrix a
mkMatrix [] = Matrix 0    0    [[0]]
mkMatrix xs =
    if consistent xs
    then Matrix rows' cols' xs
    else error "Inconsistent matrix data. Check your row/column sizes match."
    where rows' = length xs
          cols' = length $ head xs
          consistent ys = length (nub $ map (length) ys) == 1

-- | Matrix addition.
add :: (Num a) => Matrix a -> Matrix a -> Matrix a
add a b
    | rows a == rows b && cols a == cols b = add' a b
    | otherwise        = error "Unequal number of rows and/or columns."
    where add' (Matrix r c xs) (Matrix _ _ ys) = Matrix r c (added xs ys)
          added (z:zs) (w:ws) = zipWith (+) z w : added zs ws
          added _      _      = []

-- | Return the number of rows in the matrix.
rows :: Matrix a -> Int
rows (Matrix n _ _) = n

-- | Return the number of columns in the matrix.
cols :: Matrix a -> Int
cols (Matrix _ m _) = m

-- | Matrix subtraction.
subtract :: (Num a) => Matrix a -> Matrix a -> Matrix a
subtract a b = a + (negate b)

-- | Negate all elements in a matrix.
negate :: (Num a) => Matrix a -> Matrix a
negate (Matrix n m xs) = Matrix n m (neg xs)
    where neg (y:ys) = map P.negate y : neg ys 
          neg _      = []

-- | Multiply two matrices.
multiply :: (Num a) => Matrix a -> Matrix a -> Matrix a
multiply a b
    | cols a == rows b = multiply' a b
    | otherwise        = error "column/row mismatch when multiplying matrices."
    where multiply' (Matrix r c xs) (Matrix _ _ ys) = Matrix r c (mult xs ys)
          mult (x:xs) (y:ys) = zipWith (*) x y : mult xs ys
          mult _      _      = []

