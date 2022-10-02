module Lib () where

import Polynomial (Polynomial (..))

newtype Point a = Point (a, a)

newtype Points a = Points [Point a]
