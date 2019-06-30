module PolyTasty where

import Test.QuickCheck

import Poisson
import Utils



simplePoly :: Num a => [a] -> a -> a
simplePoly [] _ = 0
simplePoly (c:cs) x = c + x * simplePoly cs x



prop_approxPoly :: [Double] -> Double -> Property
prop_approxPoly cs' x' =
  let cs = map (range 1) $ take 10 cs'
      x = range 1 x'
      n = length cs
      p = FunPoly $ approxPoly n 1e-12 (simplePoly cs)
  in eval p x ~~ simplePoly cs x
