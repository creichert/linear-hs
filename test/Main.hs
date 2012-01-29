-----------------------------------------------------------------------------
---- |
---- Module : Main
---- License : BSD3
---- Maintainer : Christopher Reichert <creichert07@gmail.com>
---- Stability : experimental
----
---- linear-hs tests entry point.
----

import Text.Printf

import Test.HUnit
import Test.QuickCheck

import Test.Vector
import Test.Matrix

main = do putStrLn "Running vector quick tests."
          mapM_ (\(s,a) -> printf "%-25s: " s >> a) qtests_Vector
          putStrLn "Running vector unit tests."
          runTestTT utests_Vector

          putStrLn "Running vector unit tests."
          runTestTT utests_Matrix

