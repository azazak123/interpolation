module Main (main) where

import Data.List
import Data.Ord
import Math.Interpolation
import Math.Interpolation.Point
import Math.Interpolation.Polynomial
import Test.Tasty
import Test.Tasty.QuickCheck

eps = 0.00001

main = defaultMain tests

tests =
  testGroup
    "Tests"
    [lagrangeTest, newtonTest]

lagrangeTest =
  testGroup
    "Lagrange"
    [ testProperty "Square check" $ square interpolateLagrange,
      testProperty "Cubic check" $ cubic interpolateLagrange,
      testProperty "Line check" $ line interpolateLagrange,
      testProperty "Hard function check" $ hardFunc interpolateLagrange
    ]

newtonTest =
  testGroup
    "Newton"
    [ testProperty "Square check" $ square interpolateNewton,
      testProperty "Cubic check" $ cubic interpolateNewton,
      testProperty "Line check" $ line interpolateNewton,
      testProperty "Hard function check" $ hardFunc interpolateNewton
    ]

-- | f(x) = x^2
square :: (Points Double -> Polynomial Double) -> Double -> Bool
square f x = (< eps) . abs $ x * x - result
  where
    result = toFunc (f $ Points [Point (0, 0), Point (2, 4), Point (-2, 4)]) x

-- | f(x) = x^3 - 1
cubic :: (Points Double -> Polynomial Double) -> Double -> Bool
cubic f x = (< eps) . abs $ (x * x * x - 1) - result
  where
    result = toFunc (f $ Points [Point (0, -1), Point (2, 7), Point (-2, -9), Point (1, 0)]) x

-- | f(x)= -2x
line :: (Points Double -> Polynomial Double) -> Double -> Bool
line f x = (< eps) . abs $ (-2 * x) - result
  where
    result = toFunc (f $ Points [Point (0, 0), Point (2, -4), Point (-2, 4)]) x

-- | f(x) = x^4 + 6x^3 - 10x
hardFunc :: (Points Double -> Polynomial Double) -> Double -> Bool
hardFunc f x = (< eps) . abs $ (x ^^ 4 - 6 * x ^^ 3 - 10 * x) - result
  where
    result =
      toFunc
        ( f $
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
