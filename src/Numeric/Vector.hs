{-# LANGUAGE CPP #-}
{-# OPTIONS -Wall #-}
-----------------------------------------------------------------------------
---- |
---- Module : Numeric.Vector
---- License : BSD3
---- Maintainer : Christopher Reichert <creichert07@gmail.com>
---- Stability : experimental
----
---- Vectors.
----
---- It can be assumed that these operations are undefined for Vectors
---- of unequal length.

module Numeric.Vector(
      Vector(..)
    , (+), add
    , (-), sub
    , mkVector
    , multScalar
    , magnitude
    , dot
    , norm
    , angle
    , perpindicular
    , normalize
    , cross
    , (//), parallel
    ) where

infix 3 //

-- | Basic vector data type.
--   contains the number of dimensions and the list containing the data.
data Vector a = Vector Int [a]
    deriving (Show)

instance Eq a => Eq (Vector a) where
    (==) (Vector n xs) (Vector m ys) = n == m && xs == ys

instance (Num a) => Num (Vector a) where
    (+)         = add
    (-)         = sub
    (*)         = error "Undefined operation for vectors."
    abs         = error "Undefined operation for vectors."
    signum      = error "Undefined operation for vectors."
    fromInteger = error "Undefined operation for vectors."


-- | @'add'@ performs vector addition.
add :: (Num a) => Vector a -> Vector a -> Vector a
add (Vector n xs) (Vector m ys) = Vector (max n m) (arrayadd xs ys)
    where arrayadd [] w         = w
          arrayadd z []         = z
          arrayadd (z:zs) (w:ws) = (z + w) : arrayadd zs ws

-- | @'sub'@ performs vector subtraction.
sub :: (Num a) => Vector a -> Vector a -> Vector a
sub v (Vector n xs) = add v (Vector n (map (\x -> -x) xs))

-- | Create a vector from a list.
mkVector :: (Num a) => [a] -> Vector a
mkVector xs = Vector (length xs) xs

-- | Scalar multiplication with a vector.
multScalar :: (Num a) => a -> Vector a -> Vector a
multScalar n (Vector d xs) = Vector d (map (n*) xs)

-- | Calculates the magnitude of a vector.
magnitude :: (Num a, Real a, Floating b) => Vector a -> b
magnitude v = sqrt $ realToFrac $ v `dot` v

-- | Calculates the magnitude of a vector.
-- defined in terms of 'magnitude'
norm :: (Num a, Real a, Floating b) => Vector a -> b
norm = magnitude

-- | Calculate the dot product of two vectors.
dot :: (Num a) => Vector a -> Vector a -> a
dot (Vector _ xs) (Vector _ ys) = sum $ multComp xs ys
    where multComp = zipWith (*)

-- | angle between two vectors.
angle :: (Num a, Real a, Floating b) => Vector a -> Vector a -> b
angle v w = acos $ realToFrac  (v `dot` w) / magnitudes
    where magnitudes = magnitude v * magnitude w

-- | Determine whether two vectors are perpindicular.
perpindicular :: (Eq a, Num a) => Vector a -> Vector a -> Bool
perpindicular v w = (v `dot` w) == 0

-- | Normalize a vector to yield a unit vector.
normalize :: (Real a, Floating a) => Vector a -> Vector a
normalize v = multScalar (recip $ magnitude v) v

-- | Take the cross product of a 2 or 2 dimensional 'Vector'
cross :: (Num a, Fractional a) => Vector a -> Vector a -> Vector a
cross (Vector n xs) (Vector m ys)
        | n == 3 && m == 3 = cross3D' (Vector n xs) (Vector m ys)
        | n == 2 && m == 2 = cross2D' (Vector n xs) (Vector m ys)
        | otherwise = error "Undefined operation for cross product."

-- internal cross product of 3D vector.
cross3D' :: (Num a) => Vector a -> Vector a -> Vector a
cross3D' (Vector _ xs) (Vector _ ys) = mkVector [ xs!!1 * ys!!2 - xs!!2 * ys!!1,
                                           -(head xs * ys!!2 - xs!!2 * head ys),
                                            head xs * ys!!1 - xs!!1 * head ys]

-- internal cross product of 2D vector.
cross2D' :: (Num a, Fractional a) => Vector a -> Vector a -> Vector a
cross2D' (Vector _ xs) (Vector _ ys) = mkVector [0,0, crossLists]
    where crossLists = head xs * ys!!1 - xs!!1 * head ys

-- | Determine whether two vectors are parallel.
(//) :: (Num a, Fractional a) => Vector a -> Vector a -> Bool
v // w = parallel v w

-- | Determine whether two vectors are parallel. See '(//)'.
parallel :: (Num a, Fractional a) => Vector a -> Vector a -> Bool
parallel v w = v `cross` w == Vector dim [0,0,0]
    where dim = max (dimensions v) (dimensions w)

dimensions :: Vector a -> Int
dimensions (Vector n _) = n

