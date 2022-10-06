module Math.Interpolation.Point where

-- | One point
newtype Point a = Point (a, a)

-- | List of points
newtype Points a = Points [Point a]

fst (Point (x, y)) = x

snd (Point (x, y)) = y
