module Math.Interpolation (interpolateLagrange, interpolateNewton) where

import Control.Arrow (first)
import Math.Interpolation.Point as Point
import Math.Interpolation.Polynomial

-- | Calculate Lagrange polynomial for provided points
interpolateLagrange (Points p) = sum $ fmap addend p
  where
    addend (Point (x, y)) = (*) (Polynomial [y]) $ product $ fmap factor p
      where
        factor (Point (x1, y1))
          | x1 /= x = Polynomial [-x1 / (x - x1), 1 / (x - x1)]
          | otherwise = Polynomial [1]

-- | Calculate polynomial for provided points using Newtonian Interpolating algorithm
interpolateNewton (Points p) = sum . fmap addend . zip [1 ..] $ p
  where
    addend (index, Point (x, y))
      | index > 1 = (*) coef $ product $ factor <$> init usedPoints
      | otherwise = Polynomial [y]
      where
        usedPoints = take index p
        factor (Point (x, y)) = Polynomial [-x, 1]
        coef = Polynomial [sum $ fraction <$> usedPoints]
        fraction (Point (x, y)) = (/) y . product . fmap (x -) . filter (/= x) $ Point.fst <$> usedPoints

interpolateSquares m (Points p) =
  fmap (first d)
    . zip [0 ..]
    . fmap (fmap c . take m . iterate (+ 1))
    . take m
    $ [0 ..]
  where
    c j = sum $ (^^ j) . Point.fst <$> p
    d k = sum $ fmap addend p
      where
        addend (Point (x, y)) = y * x ^^ k
