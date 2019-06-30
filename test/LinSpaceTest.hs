module LinSpaceTest where

import Test.QuickCheck

import Poisson
import Utils



prop_linspace_length :: (Double, Double) -> NonNegative (Small Int) -> Property
prop_linspace_length (x', y') (NonNegative (Small n')) =
  let n = n' + 2
      (x, y) = minmax (range 5 x') (range 5 y')
  in length (linspace (x, y) n) === n

prop_linspace_bounds :: (Double, Double) -> NonNegative (Small Int) -> Property
prop_linspace_bounds (x', y') (NonNegative (Small n')) =
  let n = n' + 2
      (x, y) = minmax (range 5 x') (range 5 y')
      xs =linspace (x, y) n
  in (head xs, last xs) === (x, y)
