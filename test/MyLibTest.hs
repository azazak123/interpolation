module Main (main) where

import Data.List
import Data.Ord
import Lib
import Point
import Polynomial
import Test.Tasty
import Test.Tasty.QuickCheck

eps = 0.000001

main = defaultMain tests

tests =
  testGroup
    "Tests"
    [test]

test =
  testGroup
    "(checked by QuickCheck)"
    [ testProperty "Square check" square,
      testProperty "Cubic check" cubic,
      testProperty "Line check" line,
      testProperty "Hard function check" hardFunc
    ]

-- | f(x) = x^2
square :: Double -> Bool
square x = (< eps) . abs $ x * x - result
  where
    result = toFunc (interpolateLagrange $ Points [Point (0, 0), Point (2, 4), Point (-2, 4)]) x

-- | f(x) = x^3 - 1
cubic :: Double -> Bool
cubic x = (< eps) . abs $ (x * x * x - 1) - result
  where
    result = toFunc (interpolateLagrange $ Points [Point (0, -1), Point (2, 7), Point (-2, -9), Point (1, 0)]) x

-- | f(x)= -2x
line :: Double -> Bool
line x = (< eps) . abs $ (-2 * x) - result
  where
    result = toFunc (interpolateLagrange $ Points [Point (0, 0), Point (2, -4), Point (-2, 4)]) x

-- | f(x) = x^4 + 6x^3 - 10x
hardFunc :: Double -> Bool
hardFunc x = (< eps) . abs $ (x ^^ 4 - 6 * x ^^ 3 - 10 * x) - result
  where
    result =
      toFunc
        ( interpolateLagrange $
            Points
              [ Point (0, 0),
                Point (1, -15),
                Point (2, -52),
                Point (3, -111),
                Point (4, -168),
                Point (5, -175)
              ]
        )
        x
