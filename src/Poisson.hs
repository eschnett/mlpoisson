{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-type-defaults #-}

module Poisson where

import Control.Exception (assert)
import Data.List
import Math.Polynomial hiding (x)
import Math.Polynomial.Interpolation
import Math.Polynomial.Legendre
import qualified Math.Polynomial

default (Int)



class (Eq a, Num a) => FunOk a
instance (Eq a, Num a) => FunOk a

-- | Fun is a category
data Fun a where
  FunHask :: (a -> a) -> Fun a
  FunHaskD :: (a -> a) -> (a -> a) -> Fun a
  FunPoly :: Poly a -> Fun a
  FunId :: Fun a
  FunComp :: Fun a -> Fun a -> Fun a
  FunZero :: Fun a
  FunScale :: a -> Fun a -> Fun a
  FunProd :: Fun a -> Fun a -> Fun a
  FunSum :: Fun a -> Fun a -> Fun a

infixr 1 <<<
(<<<) :: Fun a -> Fun a -> Fun a
(<<<) = FunComp

infixr 3 .*
(.*) :: a -> Fun a -> Fun a
(.*) = FunScale

infixr 3 ***
(***) :: Fun a -> Fun a -> Fun a
(***) = FunProd

infixr 2 +++
(+++) :: Fun a -> Fun a -> Fun a
(+++) = FunSum

eval :: FunOk a => Fun a -> a -> a
eval (FunHask f) = f
eval (FunHaskD f _df) = f
eval (FunPoly p) = evalPoly p
eval FunId = id
eval (FunComp g f) = eval g . eval f
eval FunZero = const 0
eval (FunScale a f) = \x -> a * eval f x
eval (FunProd f g) = \x -> eval f x * eval g x
eval (FunSum f g) = \x -> eval f x + eval g x

deriv :: FunOk a => Fun a -> Fun a
deriv (FunHask f) = undefined
deriv (FunHaskD _f df) = FunHaskD df undefined
deriv (FunPoly p) = FunPoly (polyDeriv p)
deriv FunId = FunZero
deriv (FunComp g f) = (deriv g <<< f) *** deriv f
deriv FunZero = FunZero
deriv (FunScale a f) = a .* deriv f
deriv (FunProd f g) = deriv f *** g +++ f *** deriv g
deriv (FunSum f g) = deriv f +++ deriv g

optimize :: FunOk a => Fun a -> Fun a

optimize (FunComp FunId f) = f
optimize (FunComp f FunId) = f
optimize (FunComp FunZero f) = FunZero

optimize (FunScale a FunZero) = FunZero
optimize (FunScale a f) = if | a == 0 -> FunZero
                             | a == 1 -> f
                             | otherwise -> FunScale a f

optimize (FunProd FunZero g) = FunZero
optimize (FunProd f FunZero) = FunZero
optimize (FunProd (FunScale a f) (FunScale b g)) =
  FunScale (a * b) (FunProd f g)
optimize (FunProd f g) = FunProd f g

optimize (FunSum FunZero g) = g
optimize (FunSum f FunZero) = f

optimize f = f



-- | Logistic function
-- > 1/2 + 1/2 tanh (x / 2)
logistic :: Floating a => a -> a
-- logistic x = 1 / (1 + exp (-x))
logistic x = 1/2 + 1/2 * tanh (x / 2)

-- | Inverse of logistic function
-- > y = 1/2 + 1/2 tanh (x / 2)
-- > 2 y - 1 = tanh (x / 2)
-- > 2 atanh (2 y - 1) = x
unlogistic :: Floating a => a -> a
unlogistic y = 2 * atanh (2 * y - 1)

clamp :: (Num a, Ord a) => a -> a
clamp x = if | x < -1 -> -1
             | x > 1 -> 1
             | otherwise -> x

dclamp :: (Num a, Ord a) => a -> a
dclamp x = if | x < -1 -> 0
              | x > 1 -> 0
              | otherwise -> 1

smoothstep :: (Fractional a, Ord a) => a -> a
smoothstep x = if | x < -1 -> -1
                  | x > 1 -> 1
                  | otherwise -> 3/2 * x - 1/2 * x^3

dsmoothstep :: (Fractional a, Ord a) => a -> a
dsmoothstep x = if | x < -1 -> 0
                   | x > 1 -> 0
                   | otherwise -> 3/2 - 3/2 * x^2

dtanh :: Floating a => a -> a
dtanh x = 1 / (cosh x)^2

datanh :: Fractional a => a -> a
datanh x = 1 / (1 - x^2)

-- | Gaussian
gaussian :: Floating a => a -> a -> a
gaussian w x = exp (-1/2 * (x / w)^2)

dgaussian :: Floating a => a -> a -> a
dgaussian w x = - x / w^2 * gaussian w x



linspace :: Fractional a => (a, a) -> Int -> [a]
linspace (xmin, xmax) n =
  assert (n >= 2) $
  [fi (i - imax) / fi (imin - imax) * xmin +
   fi (i - imin) / fi (imax - imin) * xmax | i <- [imin .. imax]]
  where fi = fromIntegral
        imin = 0
        imax = n - 1

xpoly :: (Eq a, Num a) => Poly a
xpoly = Math.Polynomial.x

approxPoly :: (Fractional a, Ord a) => Int -> a -> (a -> a) -> Poly a
approxPoly n eps f =
  let -- xs = [-1] ++ legendreRoots (n - 2) eps ++ [1]
      xs = legendreRoots n eps
      ys = map f xs
  in lagrangePolyFit (zip xs ys)



-- | Sigmoid function
sigmoid :: Floating a => Fun a
sigmoid = FunHaskD tanh dtanh

-- | Inverse of sigmoid function
unsigmoid :: Floating a => Fun a
unsigmoid = FunHaskD atanh datanh



diff :: Floating a => Fun a -> Fun a -> Fun a
diff f g = f +++ (-1) .* g

norm :: (Floating a, Ord a) => Int -> a -> Fun a -> a
norm n eps f = let p = approxPoly n eps \x -> (eval f x)^2
                   p0 = polyIntegral p
               in sqrt ((evalPoly p0 1 - evalPoly p0 (-1)) / 2)



poisson :: IO ()
poisson =
  let n = 10
      eps = 1.0e-12

      sol = FunPoly $ approxPoly n eps $ gaussian 0.25
      dsol = FunPoly $ approxPoly n eps $ dgaussian 0.25

      f0 = unsigmoid
      f1 = FunPoly xpoly
      f2 = sigmoid
      f3 = sol
      f = f3 <<< f2 <<< f1 <<< f0
      df = deriv f

      xs = linspace (-1, 1) 21 :: [Double]
      ys = map (eval f) xs
      rs = map (eval (diff f sol)) xs
      r2 = norm n eps (diff f sol)
      dys = map (eval df) xs
      drs = map (eval (diff df dsol)) xs
      dr2 = norm n eps (diff f sol)
  in do putStr $ unlines $ show <$> zip4 [0..] xs ys rs
        putStrLn $ show r2
        putStrLn ""
        putStr $ unlines $ show <$> zip4 [0..] xs dys drs
        putStrLn $ show dr2
