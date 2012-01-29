-----------------------------------------------------------------------------
---- |
---- Module : Test.Matrix
---- License : BSD3
---- Maintainer : Christopher Reichert <creichert07@gmail.com>
---- Stability : experimental
----
---- HUnit tests == test_*
---- QuickCheck tests == prop_*
----

module Test.Matrix where

import Test.HUnit

import Numeric.Matrix (Matrix(..), mkMatrix, add, rows, cols)

------------------------ HUnit Tests ---------------------------------------
test_make1 = "matrix make 1" ~: mkMatrix xs ~=? Matrix 3 3 xs
    where xs = [[2,4,6],[8,10,12],[1,2,3]]
test_make2 = "matrix make 2" ~: mkMatrix xs ~=? Matrix 2 2 xs
    where xs = [[2,4],[8,10]]

test_add1 = "matrix add 1"  ~: c ~=? add a b
    where a = mkMatrix [[1,2,3], [4,5,6], [7,8,9]]
          b = mkMatrix [[1,2,3], [4,5,6], [7,8,9]]
          c = Matrix 3 3 [[2,4,6], [8,10,12], [14,16,18]]

test_add2 = "matrix add 2"  ~: c ~=? add a b
    where a = mkMatrix [[2.14,3.45,78.292], [23.123,1232.1,1232.12], [1.1,8,9]]
          b = mkMatrix [[5.54,12.45,8.298], [1546.123,12.1,12], [3.4,8,22]]
          c = Matrix 3 3 [[7.68,15.899999999999999,86.59],
                          [1569.246,1244.1999999999998,1244.12],
                          [4.5,16.0,31.0]]

test_rows1 = "test rows 1" ~: 3 ~=? rows a
    where a = mkMatrix [[1,2,3],[1,2,3],[1,2,3]]
test_cols1 = "test cols 1" ~: 3 ~=? cols a
    where a = mkMatrix [[1,2,3],[1,2,3],[1,2,3]]
test_rows2 = "test rows 1" ~: 2 ~=? rows a
    where a = mkMatrix [[1,2],[1,2]]
test_cols2 = "test cols 1" ~: 2 ~=? cols a
    where a = mkMatrix [[1,2],[1,2]]

utests_Matrix = TestList [ TestLabel "matrix make 1" test_make1
                         , TestLabel "matrix make 2" test_make2
                         , TestLabel "matrix add 2" test_add1
                         , TestLabel "matrix add 2" test_add2
                         , TestLabel "test rows 1" test_rows1
                         , TestLabel "test cols 1" test_cols1
                         , TestLabel "test rows 2" test_rows2
                         , TestLabel "test cols 2" test_cols2
                         ]

