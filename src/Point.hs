module Point where

-- | One point
newtype Point a = Point (a, a)

-- | List of points
newtype Points a = Points [Point a]
