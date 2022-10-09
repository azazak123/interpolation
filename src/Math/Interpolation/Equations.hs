module Math.Interpolation.Equations where

import Control.Arrow (first)
import Data.Bifunctor (bimap)
import Math.Interpolation.Utils

type Vector a = [a]

type Matrix a = [Vector a]

solveGauss m v =
  foldl getCurrentRoot []
    . zip [0 ..]
    . reverse
    . fmap (bimap head tail . dup)
    . foldl iteration (zipWith (:) v m)
    $ take (length m) [1 ..]
  where
    getCurrentRoot roots (index, (c, equation)) =
      (-) c (sum $ zipWith (*) revRoots revEq) / (revEq !! index) : roots
      where
        revEq = reverse equation
        revRoots = reverse roots
    iteration nextM index =
      (++) (take size nextM)
        . fmap (zipWith (-) current . uncurry fmap . first ((*) . coef) . dup)
        . drop size
        $ nextM
      where
        current = nextM !! (index - 1)
        size = index
        coef divisor = current !! index / divisor !! index
