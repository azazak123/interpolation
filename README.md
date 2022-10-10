# Interpolation

Haskell library which provides some techniques to create interpolation polynomial.

## General description

Library provides two techniques of calculating interpolation:

- Lagrange
- Newton
- Square

## Some examples

``` haskell
-- imports
import Math.Interpolation
import Math.Interpolation.Point
import Math.Interpolation.Polynomial

-- calculate Lagrange Polynomial for these points (f(x) = x^2)
squarePolynomial = interpolateLagrange $ Points [Point (0, 0), Point (2, 4), Point (-2, 4)]

-- or we can use Newton algorithm
squarePolynomial = interpolateNewton $ Points [Point (0, 0), Point (2, 4), Point (-2, 4)]

-- or we can use square algorithm
squarePolynomial = interpolateSquares 3 $ Points [Point (0, 0), Point (2, 4), Point (-2, 4)]

-- get function from received Polynomial
square = toFunc $ squarePolynomial

-- call function
square 10 -- 100.0
```

## Documentation 

To generate documentation run

``` bash
cabal haddock
```

## Test

To run test run

```bash
cabal test
```
