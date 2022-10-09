module Math.Interpolation.Point where

-- | One point
newtype Point a = Point (a, a)

-- | List of points
newtype Points a = Points [Point a]

fst :: Point a -> a
fst (Point (x, _)) = x

snd :: Point a -> a
snd (Point (_, y)) = y
