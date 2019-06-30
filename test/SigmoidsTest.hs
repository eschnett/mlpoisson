module SigmoidsTest where

import Test.QuickCheck

import Poisson
import Utils



prop_logistic_inverse :: Double -> Property
prop_logistic_inverse x' =
  let x = range 5 x'
  in unlogistic (logistic x) ~~ x

prop_logistic_range :: Double -> Property
prop_logistic_range x = (abs (logistic x) <= 1) === True

prop_logistic_monotonic :: Double -> Double -> Property
prop_logistic_monotonic x' y' =
  let x = range 5 x'
      y = range 5 y'
  in compare (logistic x) (logistic y) === compare x y

prop_clamp_range :: Double -> Property
prop_clamp_range x = (abs (clamp x) <= 1) === True

prop_clamp_monotonic :: Double -> Double -> Property
prop_clamp_monotonic x' y' =
  let x = range 1 x'
      y = range 1 y'
  in compare (clamp x) (clamp y) === compare x y

prop_smoothstep_range :: Double -> Property
prop_smoothstep_range x = (abs (smoothstep x) <= 1) === True

prop_smoothstep_monotonic :: Double -> Double -> Property
prop_smoothstep_monotonic x' y' =
  let x = range 1 x'
      y = range 1 y'
  in compare (smoothstep x) (smoothstep y) === compare x y

prop_tanh_inverse :: Double -> Property
prop_tanh_inverse x' =
  let x = range 5 x'
  in atanh (tanh x) ~~ x

prop_tanh_range :: Double -> Property
prop_tanh_range x = (abs (tanh x) <= 1) === True

prop_tanh_monotonic :: Double -> Double -> Property
prop_tanh_monotonic x' y' =
  let x = range 5 x'
      y = range 5 y'
  in compare (tanh x) (tanh y) === compare x y
