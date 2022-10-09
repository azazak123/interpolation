module Math.Interpolation.Polynomial (Polynomial (..), toFunc, toList) where

import Data.Coerce (coerce)

-- | Polynomial type
newtype Polynomial a = Polynomial [a]

-- | Implement Show for Polynomial
instance Show a => Show (Polynomial a) where
  show (Polynomial p) = tail . concat . zipWith concatCoefLevel [0 :: Integer ..] $ p
    where
      concatCoefLevel level coefficient = ' ' : show coefficient ++ "x^" ++ show level

-- | Implement Num for Polynomial
instance (Num a) => Num (Polynomial a) where
  (+) = polynomialZipWithDefault (+)

  (-) = polynomialZipWithDefault (-)

  abs = Polynomial . fmap abs . coerce

  signum = Polynomial . fmap signum . coerce

  fromInteger n = Polynomial [fromInteger n]

  (*) (Polynomial p1) (Polynomial p2) = sum . fmap mul . zip [0 ..] $ p1
    where
      mul (index, coef) = Polynomial $ replicate index 0 ++ fmap (* coef) p2

-- | Calculate function using Lagrange Polynomial and provided argument
toFunc :: Fractional c => Polynomial c -> c -> c
toFunc (Polynomial p) x = foldl (\acc (level, coef) -> acc + coef * x ^^ level) 0 . zip [0 :: Integer ..] $ p

-- | Convert Polynomial to list
toList :: Polynomial a -> [a]
toList = coerce

-- | Convert list to Polynomial
fromList :: [a] -> Polynomial a
fromList = coerce

-- | Adds zeros to the end of provided Polynomial
polynomialIncreaseSize :: Num a => Int -> Polynomial a -> Polynomial a
polynomialIncreaseSize additionalSize (Polynomial p) = fromList $ (++) p $ replicate additionalSize 0

-- | Zip Polynomial coefficients with provided functionName
-- Adds zeros to shorter Polynomial
polynomialZipWithDefault :: (Num a, Num b) => (a -> b -> c) -> Polynomial a -> Polynomial b -> Polynomial c
polynomialZipWithDefault f (Polynomial p1) (Polynomial p2)
  | length p1 > length p2 = Polynomial $ zipWith f p1 $ increaseSize p2
  | otherwise = Polynomial $ zipWith f (increaseSize p1) p2
  where
    increaseSize :: (Num a) => [a] -> [a]
    increaseSize = toList . polynomialIncreaseSize (abs $ length p1 - length p2) . fromList
