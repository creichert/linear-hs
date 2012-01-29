-----------------------------------------------------------------------------
---- |
---- Module : Test.Vector
---- License : BSD3
---- Maintainer : Christopher Reichert <creichert07@gmail.com>
---- Stability : experimental
----
---- HUnit tests == test_*
---- QuickCheck tests == prop_*
----


module Test.Vector where

import Data.Char
import Test.HUnit
import Test.QuickCheck

import Numeric.Vector ( Vector(..)
                      , mkVector
                      , multScalar
                      , dot
                      , magnitude
                      , norm
                      , angle
                      , cross
                      , perpindicular
                      , (//), parallel )


--------------------------------- HUnit Tests --------------------------------
-- angle unit tests
test_angle1 = "angle 1"  ~: 0.2257261285527342 ~=? angle v w
    where v = mkVector [1,2,3]
          w = mkVector [4,5,6]
test_angle2 = "angle 2" ~: 1.5495015615255643 ~=? angle v w
    where v = mkVector [12.4,45.2,0]
          w = mkVector [3,4.345,234]
test_angle3 = "angle 3" ~: 1.1625141016326912 ~=? angle v w
    where v = mkVector [2,3,4]
          w = mkVector [1,-2,3]

-- perindicular test
test_perpindicular1 = "perpindicular 1" ~: True ~=? perpindicular v w
    where v = mkVector [1,0,0]
          w = mkVector [0,1,0]
test_perpindicular2 = "perpindicular 2" ~: False ~=? perpindicular v w
    where v = mkVector [12.4,45.2,0]
          w = mkVector [3,4.345,234]

-- cross product
test_cross3D = "cross 1" ~: Vector 3 [-15, -2, 39] ~=? v `cross` w
    where v = mkVector [3,-3,1]
          w = mkVector [4,9,2]
test_cross2D = "cross 2" ~: Vector 3 [0, 0, 41] ~=? v `cross` w
    where v = mkVector [5,2]
          w = mkVector [-3,7]

-- parallel
test_parallel1 = "parallel 1" ~: True ~=? parallel v w
    where v = mkVector [2,2,2]
          w = mkVector [1,1,1]
test_parallel2 = "parallel 2" ~: True ~=? parallel v w
    where v = mkVector [11,11,11]
          w = mkVector [22,22,22]
test_parallel3 = "parallel 3" ~: False ~=? parallel v w
    where v = mkVector [1,15,11]
          w = mkVector [3,3.22,8]
test_parallel4 = "parallel 4" ~: True ~=? parallel v w
    where v = mkVector [2.7,2.7,2.7]
          w = mkVector [2.14,2.14,2.14]
test_parallel5 = "parallel 5" ~: True ~=? parallel v w
    where v = mkVector [2,-1,0]
          w = mkVector [-(1/2), 1/4, 0]
test_parallel6 = "parallel 6" ~: False ~=? parallel v w
    where v = mkVector [232342,2342341,0]
          w = mkVector [-(1/2), 1/4, 12312]

utests_Vector = TestList [ TestLabel "test angle1" test_angle1
                         , TestLabel "test angle2" test_angle2
                         , TestLabel "test angle3" test_angle3
                         , TestLabel "test perpindicular1" test_perpindicular1
                         , TestLabel "test perpindicular2" test_perpindicular2
                         , TestLabel "test cross product1" test_cross3D
                         , TestLabel "test cross product2D" test_cross2D
                         , TestLabel "test parallel1" test_parallel1
                         , TestLabel "test parallel2" test_parallel2
                         , TestLabel "test parallel3" test_parallel3
                         , TestLabel "test parallel4" test_parallel4
                         , TestLabel "test parallel5" test_parallel5
                         , TestLabel "test parallel6" test_parallel6
                         ]


----------------------------------  Quick Tests -----------------------------

-- * test basic vector addition. Because of the nature of the test data
--   we also need to feed equal length lists to tests agains zipWith.
prop_vectorAddition xs ys = (v + w) == mkVector (added xs ys) &&
                            mkVector rs + mkVector fs ==
                            mkVector (zipWith (+) rs fs)
    where _ = xs :: [Double]
          _ = ys :: [Double]
          rs = [1..1000]
          fs = [5001..6000]
          v = mkVector xs
          w = mkVector ys
          added []     ws     = ws
          added zs     []     = zs
          added (z:zs) (w:ws) = z + w : added zs ws

-- * test vector subtraction
prop_vectorSubtraction xs ys = -- (v - w) == mkVector (subtd xs ys) &&
                            mkVector rs - mkVector fs ==
                            mkVector (zipWith (-) rs fs)
    where _ = xs :: [Double]
          _ = ys :: [Double]
          rs = [1..1000]
          fs = [5001..6000]
          v = mkVector xs
          w = mkVector ys
          subtd []     ws     = ws
          subtd zs     []     = zs
          subtd (z:zs) (w:ws) = z - w : subtd zs ws

-- * test making Vector creation from a list of Int's
prop_mkVectorInt xs = mkVector xs == Vector (length xs) xs
    where _ = xs :: [Int]

-- * test making Vector creation from a list of Doubles
prop_mkVectorDouble xs =  mkVector xs == Vector (length xs) xs
    where _ = xs :: [Double]

-- * test scalar multiplication routine.
prop_scalarMult x xs = multScalar x (mkVector xs) ==
                                 mkVector (sm xs)
    where _  = x :: Int
          _  = xs :: [Int]
          sm = map (x*)

-- * test magnitude
-- TODO: We have to round these numberse because for some
--       reason the floating point precision get's slight off
prop_testMagnitude xs = round (magnitude (mkVector xs)) ==
                        round (sqrt $ realToFrac $ sum (map(^2) xs))
    where _ = xs :: [Int]

-- * test dot product and it's reversal
prop_dotProduct xs = v `dot` w == w `dot` v && v `dot` w == testDot v w
    where _ = xs :: [Int]
          v = mkVector xs
          w = mkVector xs
          testDot (Vector _ xs) (Vector _ ys) = sum $ zipWith (*) xs ys

prop_testAngle = undefined
prop_testPerpindicular = undefined

-- * test norm
-- TODO: We have to round these numberse because for some
--       reason the floating point precision get's slight off
prop_testNorm xs = round (norm (mkVector xs)) ==
                   round (sqrt $ realToFrac $ sum (map(^2) xs))
    where _ = xs :: [Int]

qtests_Vector = [
         ("vector addition...", quickCheck prop_vectorAddition)
       , ("vector subtraction...", quickCheck prop_vectorSubtraction)
       , ("make vector of integers...", quickCheck prop_mkVectorInt)
       , ("make vector of doubles...", quickCheck prop_mkVectorDouble)
       , ("test magnitude...", quickCheck prop_testMagnitude)
       , ("scalar multiplication of integers...", quickCheck prop_scalarMult)
       , ("dot product ...", quickCheck prop_dotProduct)
--     , ("test angle calculation...", quickCheck prop_testAngle
--     , ("test perpindicular...", quickCheck prop_testPerpindicular
       , ("test norm...", quickCheck prop_testNorm)
       ]

