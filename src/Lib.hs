module Lib (interpolateLagrange) where

import Point
import Polynomial

interpolateLagrange (Points p) = sum $ fmap addend p
  where
    addend (Point (x, y)) = (*) (Polynomial [y]) $ product $ fmap factor p
      where
        factor (Point (x1, y1))
          | x1 /= x = Polynomial [-x1 / (x - x1), 1 / (x - x1)]
          | otherwise = Polynomial [1]
