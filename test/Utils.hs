module Utils where

import Data.Fixed
import Test.QuickCheck

minmax :: Ord a => a -> a -> (a, a)
minmax x y = (min x y, max x y)

range :: Real a => a -> a -> a
range m x = mod' (x + m) (2 * m) - m

(~~) :: (Ord a, Fractional a) => a -> a -> Property
x ~~ y = (abs (x - y) < 1.0e-12) === True
