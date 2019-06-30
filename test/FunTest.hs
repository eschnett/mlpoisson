{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module FunTest where

import Test.QuickCheck
import qualified Test.QuickCheck.Function as Q

import Poisson



prop_Fun_idl :: Q.Fun Rational Rational -> Rational -> Property
prop_Fun_idl (Fn f) x = eval (FunId <<< FunHask f) x === f x

prop_Fun_idr :: Q.Fun Rational Rational -> Rational -> Property
prop_Fun_idr (Fn f) x = eval (FunHask f <<< FunId) x === f x

prop_Fun_assoc :: Q.Fun Rational Rational
               -> Q.Fun Rational Rational
               -> Q.Fun Rational Rational
               -> Rational
               -> Property
prop_Fun_assoc (Fn h) (Fn g) (Fn f) x =
  eval ((FunHask h <<< FunHask g) <<< FunHask f) x ===
  eval (FunHask h <<< (FunHask g <<< FunHask f)) x
