module Math.Interpolation.Equations where

import Control.Arrow (first)
import Data.Bifunctor (bimap)
import Math.Interpolation.Utils

type Vector a = [a]

type Matrix a = [Vector a]

-- | Solve System of linear algebraic equations for provided coefficient matrix and constants vector
solveGauss :: (Eq a, Fractional a) => Matrix a -> Vector a -> Vector a
solveGauss m v =
  foldl getCurrentRoot []
    . zip [0 ..]
    . reverse
    . fmap (bimap head tail . dup)
    . foldl iteration (zipWith (:) v $ swapRow m 0)
    $ take (length m) [1 ..]
  where
    getCurrentRoot roots (index, (c, equation)) =
      (-) c (sum $ zipWith (*) revRoots revEq) / (revEq !! index) : roots
      where
        revEq = reverse equation
        revRoots = reverse roots
    iteration nextM index =
      (++)
        (take size nextM)
        . (++)
          (filter ((== 0) . (!! index)) $ drop size nextM)
        . fmap (zipWith (-) current . uncurry fmap . first ((*) . coef) . dup)
        . filter ((/= 0) . (!! index))
        . drop size
        $ nextM
      where
        current = nextM !! (index - 1)
        size = index
        coef divisor = current !! index / divisor !! index

-- | Swap matrix rows which has zero coefficient on diagonal
swapRow :: (Eq a, Num a) => Matrix a -> Int -> Matrix a
swapRow matrix index
  | index == length matrix = matrix
  | matrix !! index !! index == 0 =
      swapRow
        (take index matrix ++ drop (index + 1) matrix ++ [matrix !! index])
        index
  | otherwise = swapRow matrix $ index + 1
