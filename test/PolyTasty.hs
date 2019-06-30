module PolyTasty where

import Data.List
import Math.Polynomial
import Test.QuickCheck

import Poisson
import Utils



prop_approxPoly :: [Double] -> Double -> Property
prop_approxPoly cs' x' =
  let cs = map (range 1) $ take 10 cs'
      x = range 1 x'
      n = length cs
      p = poly LE cs
      q = FunPoly $ approxPoly n 1e-12 (evalPoly p)
  in eval q x ~~ evalPoly p x

prop_norm :: [Double] -> Property
prop_norm cs' =
  let cs = map (range 1) $ take 10 cs'
      n = length cs
      p = poly LE cs
      q = FunPoly p
      p2 = polyIntegral (multPoly p p)
  in norm n 1e-12 q ~~ sqrt ((evalPoly p2 1 - evalPoly p2 (-1)) / 2)
