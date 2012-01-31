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

import Numeric.Matrix (Matrix(..), mkMatrix, add, rows, cols,
                       (-), (*))

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

test_subtract1 = "test subtract 1" ~: c ~=? a - b
    where a = mkMatrix [[1,2,3],[1,2,3],[1,2,3]]
          b = mkMatrix [[1,1,1],[1,1,1],[1,1,1]]
          c = Matrix 3 3 [[0,1,2],[0,1,2],[0,1,2]]

test_subtract2 = "test subtract 2" ~: c ~=? a - b
    where a = mkMatrix [[1.2,2.5,3.8],[1.45,2.14,3.34],[1.87,2.99,3.123]]
          b = mkMatrix [[1.1,1.4,1.38],[1,3,5.435],[-30,-20.123,0]]
          c = Matrix 3 3 [[9.999999999999987e-2,1.1,2.42],
                          [0.44999999999999996,
                          -0.8599999999999999,
                          -2.0949999999999998],
                          [31.87,23.113,3.123]]

test_subtract3 = "test subtract 3" ~: c ~=? a - b
    where a = mkMatrix [[1,2],[1,2]]
          b = mkMatrix [[1,1],[1,5]]
          c = Matrix 2 2 [[0,1],[0,-3]]

test_multiply1 = "test multiply 1" ~: c ~=? a * b
    where a = mkMatrix [[1,2], [3,4]]
          b = mkMatrix [[5,0], [0,2]]
          c = mkMatrix [[5,4],[15,8]]


utests_Matrix = TestList [ TestLabel "matrix make 1" test_make1
                         , TestLabel "matrix make 2" test_make2
                         , TestLabel "matrix add 2" test_add1
                         , TestLabel "matrix add 2" test_add2
                         , TestLabel "test rows 1" test_rows1
                         , TestLabel "test cols 1" test_cols1
                         , TestLabel "test rows 2" test_rows2
                         , TestLabel "test cols 2" test_cols2
                         , TestLabel "test subtract 1" test_subtract1
                         , TestLabel "test subtract 2" test_subtract2
                         , TestLabel "test subtract 3" test_subtract3
                         , TestLabel "test mult 1" test_multiply1
                         ]

