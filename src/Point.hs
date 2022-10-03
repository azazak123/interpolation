module Point (Point (..), Points (..)) where

newtype Point a = Point (a, a)

newtype Points a = Points [Point a]
