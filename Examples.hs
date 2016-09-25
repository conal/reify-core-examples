-- -*- flycheck-disabled-checkers: '(haskell-ghc haskell-stack-ghc); -*-
{-# LANGUAGE PatternSynonyms, ConstraintKinds, ScopedTypeVariables, AllowAmbiguousTypes #-}
{-# LANGUAGE CPP, LambdaCase, GADTs, TypeOperators, DataKinds, TypeApplications, TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Example of Haskell to hardware via reification and CCC

{-# OPTIONS_GHC -Wall -fno-warn-unticked-promoted-constructors -fno-warn-missing-signatures #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-} -- TEMP
{-# OPTIONS_GHC -fno-warn-unused-binds   #-} -- TEMP

module Main where

import Control.Arrow ((***))
import Data.Tuple (swap)
import GHC.Generics hiding (S,C)
import Data.Key

import Circat.Complex

import ReificationRules.Run (go,goSep)
import ReificationRules.Misc ((:*),Unop,transpose,unknown)

import ShapedTypes.Nat
import ShapedTypes.Pair
import ShapedTypes.Vec
import ShapedTypes.LPow (LPow)
import ShapedTypes.RPow (RPow)
import qualified ShapedTypes.LPow as L
import qualified ShapedTypes.RPow as R
import qualified ShapedTypes.Fams as F
import ShapedTypes.Fams (LVec,RVec)
import ShapedTypes.Sized
import ShapedTypes.Linear
import ShapedTypes.Scan
import ShapedTypes.FFT
import ShapedTypes.Shift
import ShapedTypes.Orphans ()

-- GADT versions
type RBin = RPow Pair
type LBin = LPow Pair

-- Type family versions
type RBin' n = F.RPow Pair n
type LBin' n = F.LPow Pair n

main :: IO ()
main = do
  return ()
--   goSep "foo-2" 0.5 (\ (a,b) -> (-1) * a + b :: Double)
--   goSep "swap" 0.5 (swap @Int @Bool)
--   goSep "sum-p" 0.5 (sum @Pair @Int)
--   goSep "sum-rb5" 1 (sum @(RBin N5) @Int)
--   goSep "transpose-rb4" 4 (transpose @Pair @(RBin N4) @Bool)
--   goSep "dot-lb3" 1 ((<.>) @(LBin N3) @Int)
--   goSep "linapp-v3-2" 1.5 (($@) @(Vec N3) @(Vec N2) @Int)
--   goSep "linapp-r3-2" 5.0 (($@) @(RBin N3) @(RBin N2) @Int)
--   goSep "linapp-l3-2" 5.0 (($@) @(LBin N3) @(LBin N2) @Int)
--   goSep "lincomp-r342" 10 ((.@) @(RBin N3) @(RBin N4) @(RBin N2) @Int)
--   goSep "lsums-rb8" 4 (lsums @(RBin N8) @Int)
--   goSep "lsums-lb8" 8 (lsums @(LBin N8) @Int)
--   goSep "powers-rb4" 1 (powers @(RBin N4) @Int)
--   goSep "evalPoly-rb4" 1 (evalPoly @(RBin N4) @Int)
--   goSep "dot-r3-d" 1 ((<.>) @(RBin N3) @Double)
--   goSep "dot-r3-c" 2 ((<.>) @(RBin N3) @C)

-- --   -- For revised parallel scan talk (2016-10-05)
--   goSep "lsums-p"   0.5 (lsums @Pair @Int)
--   goSep "lsums-lv8" 0.5 (lsums @(LVec N8) @Int)
--   goSep "lsums-rv8" 3.0 (lsums @(RVec N8) @Int) -- yipes!
--   goSep "lsums-lv16" 0.5 (lsums @(LVec N16) @Int)
--   goSep "lsums-lv5xlv11" 1 (lsums @(LVec N5 :*: LVec N11) @Int)

--   goSep "lsumsp-u" 0.75 (lsums' @U1 @Int)
--   goSep "lsumsp-p" 0.75 (lsums' @Par1 @Int)

--   goSep "lsums-u"   0.75 (lsums @U1 @Int)
--   goSep "lsums-i"   0.75 (lsums @Par1 @Int)
--   goSep "lsums-uxi" 0.75 (lsums @(U1 :*: Par1) @Int)
--   goSep "lsums-1-0" 0.75 (lsums @(Par1 :*: U1) @Int)
--   goSep "lsums-1-1-0" 0.75 (lsums @(Par1 :*: (Par1 :*: U1)) @Int)

--   goSep "lsums-lpow-4-2" 1 (lsums @(LPow (LVec N4) N2) @Int)
--   goSep "lsums-rpow-4-2" 1 (lsums @(RPow (LVec N4) N2) @Int)

--   goSep "unknown-lv4-lv6" 1 (unknown :: LVec N4 Int -> LVec N6 Bool)

--   goSep "lsums-0-1-1-1-1-l" 1 (lsums @((((U1 :*: Par1) :*: Par1) :*: Par1) :*: Par1) @Int)
--   goSep "lsums-1-1-1-1-0-r" 1 (lsums @(Par1 :*: (Par1 :*: (Par1 :*: (Par1 :*: U1)))) @Int)

--   goSep "lsums-lv5-5-6-l" 1 (lsums @((LVec N5 :*: LVec N5) :*: LVec N6) @Int)
--   goSep "lsums-lv5-5-6-r" 1 (lsums @(LVec N5 :*: (LVec N5 :*: LVec N6)) @Int)

--   goSep "lsums-lv1-7" 0.5 (lsums @(LVec N1 :*: LVec N7) @Int)
--   goSep "lsums-lv1-15" 0.75 (lsums @(LVec N1 :*: LVec N15) @Int)

--   goSep "lsumsp-lv8-and-lv8" 1 (lsums' @(LVec N8) @Int *** lsums' @(LVec N8) @Int)

--   goSep "lsumsp-lv8-lv8-unknown" 1
--     (unknown . (lsums' *** lsums') :: LVec N8 Int :* LVec N8 Int -> LVec N16 Int)

--   goSep "lsums-lv5"  0.5 (lsums @(LVec N5 ) @Int)
--   goSep "lsums-lv11" 0.5 (lsums @(LVec N11) @Int)

--   goSep "lsums-rv16" 1 (lsums @(RVec N16) @Int)

--   goSep "lsums-lv8-lv8-unknown" 1
--     (unknown . (lsums *** lsums) :: LVec N8 Int :* LVec N8 Int -> And1 (LVec N16) Int)

  goSep "lsums-lv4olv4-unknown" 1
    (unknown . fmap lsums :: LVec N4 (LVec N4 Int) -> And1 (LVec N4 :.: LVec N4) Int)

--   goSep "lsums-lv5-lv11-unknown" 1.0
--     (unknown . (lsums *** lsums) :: LVec N5 Int :* LVec N11 Int -> And1 (LVec N16) Int)

--   goSep "lsumsp-lv4-5-7-l" 1 (lsums' @((LVec N4 :*: LVec N5) :*: LVec N7) @Int)
--   goSep "lsumsp-lv4-5-7-r" 1 (lsums' @(LVec N4 :*: (LVec N5 :*: LVec N7)) @Int)

--   goSep "lsumsp-lv5-5-6-l" 1 (lsums' @((LVec N5 :*: LVec N5) :*: LVec N6) @Int)
--   goSep "lsumsp-lv5-5-6-r" 1 (lsums' @(LVec N5 :*: (LVec N5 :*: LVec N6)) @Int)

--   goSep "lsumsp-lv5xlv11" 1 (lsums' @(LVec N5 :*: LVec N11) @Int)
  -- The next are logically identical, but the commutation optimization in
  -- Circat can make them appear to differ slightly. For comparison, I turn on
  -- NoCommute in Circat.hs.
--   goSep "lsumsp-lv8xlv8" 1 (lsums' @(LVec N8 :*: LVec N8) @Int)
--   goSep "lsumsp-p-lv8" 1 (lsums' @(Pair :.: LVec N8) @Int)
--   goSep "lsumsp-lv8-p" 1 (lsums' @(LVec N8 :.: Pair) @Int)
--   goSep "lsumsp-lv16" 0.5 (lsums' @(LVec N16) @Int)
--   goSep "lsumsp-lv4olv4" 1 (lsums' @(LVec N4 :.: LVec N4) @Int)
--   goSep "lsumsp-lb4" 1 (lsums' @(LBin N4) @Int)
--   goSep "lsumsp-rb4" 1 (lsums' @(RBin N4) @Int)
--   goSep "lsumsp-bush2" 1 (lsums' @(F.Bush N2) @Int)

--   goSep "lsums-rb3" 1.5 (lsums @(RBin N3) @Int)

   -- 0:0.5; 1:0.75; 2:1.5; 3:8
--    goSep "sum-bush0" 0.5  (sum @(F.Bush N0) @Int)
--    goSep "sum-bush1" 0.75 (sum @(F.Bush N1) @Int)
--    goSep "sum-bush2" 1.5  (sum @(F.Bush N2) @Int)
--    goSep "sum-bush3" 8    (sum @(F.Bush N3) @Int)

--   goSep "power-p"   1 (power @Pair      @Int)
--   goSep "power-rb4" 1 (power @(RBin N4) @Int)
--   goSep "power-lb4" 1 (power @(LBin N4) @Int)

   -- 0:0.75; 1:1; 2:2; 3:16
--    goSep "lsums-bush0" 0.75 (lsums @(F.Bush N0) @Int)
--    goSep "lsums-bush1" 1    (lsums @(F.Bush N1) @Int)
--    goSep "lsums-bush2" 2    (lsums @(F.Bush N2) @Int)
--    goSep "lsums-bush3" 8    (lsums @(F.Bush N3) @Int)

   -- 1:0.5; 2:0.75; 3:1; 4:4, 5:8
--    goSep "lsums-bushp1" 0.5  (lsums @(F.Bush N1) @Int)
--    goSep "lsums-bushp2" 0.75 (lsums @(F.Bush N2) @Int)
--    goSep "lsums-bushp3" 1    (lsums @(F.Bush N3) @Int)
--    goSep "lsums-bushp4" 4    (lsums @(F.Bush N4) @Int)
--    goSep "lsums-bushp5" 16   (lsums @(F.Bush N5) @Int)

--   goSep "cis-pi-c" 1 (cis pi :: C)
--   goSep "cis-pi-2-c" 1 (cis (pi/2) :: C)
--   goSep "sin-pi" 1 (sin pi :: Double)
--   goSep "cis-mpi-e" 1 (cis (-pi) :: C)
--   goSep "cis-mpi-2-c" 1 (cis (-pi/2) :: C)

--   goSep "dft-p" 1.5 (dft @Pair @Double)

--   goSep "dft-v4"  8 (dft @(Vec N4) @Double)
--   goSep "dft-v8" 40 (dft @(Vec N8) @Double)

--   goSep "dft-rb2"   8 (dft @(RBin N2) @Double)
--   goSep "dft-rb3"  15 (dft @(RBin N3) @Double)
--   goSep "dft-rb4"  65 (dft @(RBin N4) @Double)
--   goSep "dft-rb5" 200 (dft @(RBin N5) @Double)

--   goSep "foo" 1 o8sq
--   goSep "foo" 2 (twiddles :: Pair (RPow Pair N2 (Complex Double)))

--   goSep "fft-p" 1 (fft @Pair @Double)

--   goSep "fft-v2ov4" 3.0 (fft @(Vec N2 :.: Vec N4) @Double)
--   goSep "fft-v4ov2" 3.0 (fft @(Vec N4 :.: Vec N2) @Double)

--   goSep "fft-rb0"  0.7 (fft @(RBin N0) @Double)
--   goSep "fft-lb0"  0.7 (fft @(LBin N0) @Double)
--   goSep "fft-rb1"  1.5 (fft @(RBin N1) @Double)
--   goSep "fft-lb1"  1.5 (fft @(LBin N1) @Double)
--   goSep "fft-rb2"  2.2 (fft @(RBin N2) @Double)
--   goSep "fft-lb2"  2.2 (fft @(LBin N2) @Double)
--   goSep "fft-rb3"  3.3 (fft @(RBin N3) @Double)
--   goSep "fft-lb3"  3.3 (fft @(LBin N3) @Double)
--   goSep "fft-rb4"  5.4 (fft @(RBin N4) @Double)
--   goSep "fft-lb4"  5.4 (fft @(LBin N4) @Double)
--   goSep "fft-rb5" 11.0 (fft @(RBin N5) @Double)
--   goSep "fft-lb5" 11.0 (fft @(LBin N5) @Double)
--   goSep "fft-rb6" 16.5 (fft @(RBin N6) @Double)
--   goSep "fft-lb6" 16.5 (fft @(LBin N6) @Double)

--   goSep "fft-rb7" 30 (fft @(RBin N7) @Double)
--   goSep "fft-lb7" 30 (fft @(LBin N7) @Double)
--   goSep "fft-rb8" 100 (fft @(RBin N8) @Double)
--   goSep "fft-lb8" 100 (fft @(LBin N8) @Double)

--   goSep "fft-rpow-2-4" 5 (fft @(RPow (RBin N2) N4) @Double)

--   goSep "fft-bush0" 1.5 (fft @(F.Bush N0) @Double)
--   goSep "fft-bush1" 2.2 (fft @(F.Bush N1) @Double)
--   goSep "fft-bush2" 5.4 (fft @(F.Bush N2) @Double)
--   goSep "fft-bush3" 30 (fft @(F.Bush N3) @Double)

--   goSep "fft-v3" 2 (fft @(Vec  N3) @Double)

--   goSep "fft-a" 2 (fft @(Vec N3 :.: Vec N2) @Double)
--   goSep "fft-b" 2 (fft @(Vec N2 :.: Vec N3) @Double)

--   goSep "fft-c" 2 (fft @((Vec N2 :.: Vec N3) :.: Vec N4) @Float)
--   goSep "fft-d" 2 (fft @(Vec N2 :.: (Vec N3 :.: Vec N4)) @Float)

--   goSep "fft-a" 2 (fft @((Pair :.: Pair) :.: Pair) @Float)
--   goSep "fft-b" 2 (fft @(Pair :.: (Pair :.: Pair)) @Float)

--   goSep "powersp-rb4" 1 (powers' @(RBin N4) @Int)
--   goSep "powersp-rb3-c" 2 (powers' @(RBin N3) @C)
--   goSep "powersp-rb3-rb3-c" 5 (powers'2 @(RBin N3) @(RBin N3) @C)
--   goSep "twiddles-rb2-rb3" 5 (twiddles @(RBin N2) @(RBin N3) @Double)

--   goSep "lowerDiag-rb2" 1 (lowerDiag @(Vec N2) @Int)

{--------------------------------------------------------------------
    Example helpers
--------------------------------------------------------------------}

type C = Complex Double

-- Generalized matrices

type Matrix  m n a = Vec   n (Vec   m a)
type MatrixR m n a = RBin  n (RBin  m a)
type MatrixL m n a = LBin  n (LBin  m a)

-- shift leftward, dropping the initial zero
lsums' :: (LFScan f, Traversable f, Num b) => Unop (f b)
lsums' = snd . shiftR . lsums

power :: forall f a. (Foldable f, Applicative f, Num a) => a -> a
power = (product :: f a -> a) . pure

-- power a = product (pure a :: f a)
-- -- power = product . (pure :: a -> f a)

type And1 f = f :*: Par1

pattern And1 :: f a -> a -> And1 f a
pattern And1 fa a = fa :*: Par1 a

andTot :: (f a, a) -> And1 f a
andTot = uncurry And1

type SA f = (LScan f, Applicative f)

powers :: (SA f, Num a) => a -> And1 f a
powers = andTot . lproducts . pure

evalPoly :: (SA f, Foldable f, Num a) => And1 f a -> a -> a
evalPoly coeffs x = coeffs <.> powers x

powers' :: (SA f, Num a) => a -> f a
powers' = fst . lproducts . pure

powers'2 :: (SA f, SA g, Num a) => a -> g (f a)
powers'2 = fmap powers' <$> powers'

lowerDiag :: forall f a. (Keyed f, Ord (Key f), Num a) => Unop (f (f a))
lowerDiag = 
  mapWithKey (\ i as ->
    mapWithKey (\ j a -> if j <= i then a else 0) as)

-- infixr 3 ***
-- (***) :: (a -> c) -> (b -> d) -> (a,b) -> (c,d)
-- (f *** g) (a,b) = (f a, g b)
-- {-# INLINE (***) #-}
