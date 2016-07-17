{-# LANGUAGE PatternSynonyms #-}
-- | Example of Haskell to hardware via reification and CCC

{-# LANGUAGE CPP, LambdaCase, GADTs, TypeOperators, DataKinds, TypeApplications, TypeFamilies #-}

{-# OPTIONS_GHC -Wall -fno-warn-unticked-promoted-constructors -fno-warn-missing-signatures #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-} -- TEMP
{-# OPTIONS_GHC -fno-warn-unused-binds   #-} -- TEMP

module Main where

import Data.Tuple (swap)
import GHC.Generics hiding (S)

import ReificationRules.Run (go,goSep)
import ReificationRules.Misc (transpose)

import ShapedTypes.Nat
import ShapedTypes.Pair
import ShapedTypes.Vec
import ShapedTypes.LPow (LPow)
import ShapedTypes.RPow (RPow)
import qualified ShapedTypes.LPow as L
import qualified ShapedTypes.RPow as R
import qualified ShapedTypes.Fams as F
import ShapedTypes.Sized
import ShapedTypes.Linear
import ShapedTypes.Scan
import ShapedTypes.FFT

-- GADT versions
type RBin = RPow Pair
type LBin = LPow Pair

-- Type family versions
type RBin' n = F.RPow Pair n
type LBin' n = F.LPow Pair n

main :: IO ()
main = do
  goSep "swap" 0.5 (swap @Int @Bool)
  goSep "sum-p" 0.5 (sum @Pair @Int)
  goSep "sum-rb5" 1 (sum @(RBin N5) @Int)
  goSep "transpose-rb4" 4 (transpose @Pair @(RBin N4) @Bool)
  goSep "dot-lb3" 1 ((<.>) @(LBin N3) @Int)
  goSep "linapp-v3-2" 1.5 (($@) @(Vec N3) @(Vec N2) @Int)
  goSep "linapp-r3-2" 5.0 (($@) @(RBin N3) @(RBin N2) @Int)
  goSep "linapp-l3-2" 5.0 (($@) @(LBin N3) @(LBin N2) @Int)
  goSep "lincomp-r342" 10 ((.@) @(RBin N3) @(RBin N4) @(RBin N2) @Int)
  goSep "lsums-rb4" 1 (lsums @(RBin N4) @Int)
  goSep "lsums-lb4" 1 (lsums @(LBin N4) @Int)
  goSep "powers-rb4" 1 (powers @(RBin N4) @Int)
  goSep "evalPoly-rb4" 1 (evalPoly @(RBin N4) @Int)
  goSep "fft-rb5" 8 (fft @(RBin N5) @Double)
  goSep "fft-lb5" 8 (fft @(LBin N5) @Double)

{--------------------------------------------------------------------
    Example helpers
--------------------------------------------------------------------}

-- Generalized matrices

type Matrix  m n a = Vec   n (Vec   m a)
type MatrixR m n a = RBin  n (RBin  m a)
type MatrixL m n a = LBin  n (LBin  m a)

type And1 f = f :*: Par1

pattern And1 :: f a -> a -> And1 f a
pattern And1 fa a = fa :*: Par1 a

andTot :: (f a, a) -> And1 f a
andTot = uncurry And1

powers :: (LScan f, Applicative f, Num a) => a -> And1 f a
powers = andTot . lproducts . pure

evalPoly :: (LScan f, Applicative f, Foldable f, Num a) => And1 f a -> a -> a
evalPoly coeffs x = coeffs <.> powers x
