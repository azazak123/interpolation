module Math.Interpolation (interpolateLagrange, interpolateNewton, interpolateSquares) where

import Math.Interpolation.Equations
import Math.Interpolation.Point as Point
import Math.Interpolation.Polynomial

-- | Calculate Lagrange polynomial for provided points
interpolateLagrange :: (Eq a, Fractional a) => Points a -> Polynomial a
interpolateLagrange (Points p) = sum $ fmap addend p
  where
    addend (Point (x, y)) = (*) (Polynomial [y]) $ product $ fmap factor p
      where
        factor (Point (x1, _))
          | x1 /= x = Polynomial [-x1 / (x - x1), 1 / (x - x1)]
          | otherwise = Polynomial [1]

-- | Calculate polynomial for provided points using Newtonian Interpolating algorithm
interpolateNewton :: (Eq a, Fractional a) => Points a -> Polynomial a
interpolateNewton (Points p) = sum . fmap addend . zip [1 ..] $ p
  where
    addend (index, Point (_, y))
      | index > 1 = (*) coef $ product $ factor <$> init usedPoints
      | otherwise = Polynomial [y]
      where
        usedPoints = take index p
        factor (Point (x1, _)) = Polynomial [-x1, 1]
        coef = Polynomial [sum $ fraction <$> usedPoints]
        fraction (Point (x1, y1)) = (/) y1 . product . fmap (x1 -) . filter (/= x1) $ Point.fst <$> usedPoints

-- | Calculate Polynomial for provided points using square algorithm
interpolateSquares :: (Eq a, Fractional a) => Int -> Points a -> Polynomial a
interpolateSquares m (Points p) =
  Polynomial
    $ (`solveGauss` consts)
      . fmap (fmap c . take m . iterate (+ (1 :: Integer)))
      . take m
    $ [0 ..]
  where
    consts = fmap d [0 :: Integer ..]
    c j = sum $ (^^ j) . Point.fst <$> p
    d k = sum $ fmap addend p
      where
        addend (Point (x, y)) = y * x ^^ k
