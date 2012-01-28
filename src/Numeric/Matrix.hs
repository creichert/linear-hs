{-# LANGUAGE CPP #-}
{-# OPTIONS -Wall #-}
-----------------------------------------------------------------------------
---- |
---- Module : Numeric.Matrix
---- License : BSD3
---- Maintainer : Christopher Reichert <creichert07@gmail.com>
---- Stability : experimental
----
---- Vector
----
---- It can be assumed that these operations are undefined for Vectors
---- of unequal length.


module Numeric.Matrix(
        Matrix(..)
      ) where

data Matrix a = Matrix Int Int [[a]]

