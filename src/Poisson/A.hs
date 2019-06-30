{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-type-defaults #-}

module Poisson.A where

import Prelude hiding (id, (.))
import qualified Prelude
import Data.Kind
import Data.List
import Math.Polynomial hiding (x)
import Math.Polynomial.Interpolation
import Math.Polynomial.Legendre
import qualified Math.Polynomial

default (Int)



-- | A constrained category, so that we can compose functions
class Category k where
  type Ok k :: Type -> Constraint
  id :: Ok k a => k a a
  (.) :: (Ok k a, Ok k b, Ok k c) => k b c -> k a b -> k a c
  eval :: (Ok k a, Ok k b) => k a b -> a -> b
  deriv :: (Ok k a, Ok k b) => k a b -> k a b

class Unconstrained a
instance Unconstrained a

-- | Hask is a category
instance Category (->) where
  type Ok (->) = Unconstrained
  id = Prelude.id
  (.) = (Prelude..)
  eval = id
  deriv = undefined

data Fun a b where
  FunHask :: (a -> b) -> Fun a b
  FunPoly :: Poly a -> Fun a a
  FunId :: Fun a a
  FunComp :: FunOk b => Fun b c -> Fun a b -> Fun a c
  FunZero :: Fun a b
  FunProd :: Fun a b -> Fun a b -> Fun a b

class (Eq a, Num a) => FunOk a
instance (Eq a, Num a) => FunOk a

evalFun :: (FunOk a, FunOk b) => Fun a b -> a -> b
evalFun (FunHask f) = f
evalFun (FunPoly p) = evalPoly p
evalFun FunId = id
evalFun (FunComp g f) = evalFun g . evalFun f
evalFun FunZero = const 0
evalFun (FunProd f g) = \x -> evalFun f x * evalFun g x

derivFun :: (FunOk a, FunOk b) => Fun a b -> Fun a b
derivFun (FunHask f) = undefined
derivFun (FunPoly p) = FunPoly (polyDeriv p)
derivFun FunZero = FunZero
derivFun FunId = FunZero
-- > D g (f x) = g' (f x) * f' x
-- derivFun (FunComp g f) = FunProd (FunComp (derivFun g) f) (derivFun f)

-- | Fun is a category
instance Category Fun where
  type Ok Fun = FunOk
  id = FunId
  (.) = FunComp
  eval = evalFun
  deriv = derivFun



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

-- | Gaussian
gaussian :: Floating a => a -> a -> a
gaussian w x = exp (-1/2 * (x / w)^2)

-- | Sigmoid function
sigmoid :: Floating a => a -> a
sigmoid = tanh

-- | Inverst of sigmoid function
unsigmoid :: Floating a => a -> a
unsigmoid = atanh



linspace :: Fractional a => (a, a) -> Int -> [a]
linspace (xmin, xmax) n =
  [ sumx / fi (2 * (n-1)) + lenx * fi (2*i - (n-1)) / fi (2 * (n-1))
  | i <- [0 .. n-1] ]
  where fi = fromIntegral
        sumx = xmax + xmin
        lenx = xmax - xmin

xpoly :: (Eq a, Num a) => Poly a
xpoly = Math.Polynomial.x

approxPoly :: (Fractional a, Ord a) => Int -> a -> (a -> a) -> Poly a
approxPoly n eps f =
  let -- xs = [-1] ++ legendreRoots (n - 2) eps ++ [1]
      xs = legendreRoots n eps
      ys = map f xs
  in lagrangePolyFit (zip xs ys)



sol :: (Floating a, Ord a) => Poly a
sol = approxPoly 10 1.0e-12 (gaussian 0.25)

res :: (Floating a, Ord a) => (a -> a) -> a -> a
res f x = f x - evalPoly sol x



main :: IO ()
main = let f0 = FunHask unsigmoid
           f1 = FunPoly xpoly -- sol
           f2 = FunHask sigmoid
           f3 = FunPoly xpoly
           f = f3 . f2 . f1 . f0
           xs = linspace (-1, 1) 21 :: [Double]
           ys = map (eval f) xs
           rs = map (res (eval f)) xs
       in putStrLn $ unlines $ show <$> zip4 [0..] xs ys rs
