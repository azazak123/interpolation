module Polynomial (Polynomial (..)) where

import Data.Coerce (coerce)

newtype Polynomial a = Polynomial [a]

instance Show a => Show (Polynomial a) where
  show (Polynomial p) = tail . concat . zipWith concatCoefLevel [0 :: Integer ..] $ p
    where
      concatCoefLevel level coefficient = ' ' : show coefficient ++ "x^" ++ show level

instance (Num a) => Num (Polynomial a) where
  (+) = polynomialZipWithDefault (+)

  (-) = polynomialZipWithDefault (-)

  abs = Polynomial . fmap abs . coerce

  signum = Polynomial . fmap signum . coerce

  fromInteger n = Polynomial [fromInteger n]

  (*) (Polynomial p1) (Polynomial p2) = sum . fmap mul . zip [0 ..] $ p1
    where
      mul (index, coef) = Polynomial $ replicate index 0 ++ fmap (* coef) p2

polynomialIncreaseSize additionalSize (Polynomial p) = (++) p $ replicate additionalSize 0

polynomialZipWithDefault f (Polynomial p1) (Polynomial p2)
  | length p1 > length p2 = Polynomial $ zipWith f p1 $ increaseSize p2
  | otherwise = Polynomial $ zipWith f (increaseSize p1) p2
  where
    increaseSize = polynomialIncreaseSize (abs $ length p1 - length p2) . coerce
