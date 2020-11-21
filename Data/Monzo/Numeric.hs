{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-|
Module      : Data.Monzo.Numeric
Copyright   : 2020 Matthew Yacavone
Maintainer  : matthew@yacavone.net

See Data.Monzo for more detailed documentation.
-}
module Data.Monzo.Numeric where

import GHC.TypeNats
import Control.Exception
import Numeric.Natural
import Data.Ratio
import Data.List (foldl', group)
import Data.Numbers.Primes

import Data.Algebraic
import Data.Monzo.Type

instance (Num a, Eq a, KnownNat n) => Num (MonzoFrom n a) where
  (*) = mapMonzo2 (+)
  fromInteger z | z <= 0 = throw Underflow
                | z == 1 = fromList []
                | otherwise = fromPrimePowers (map (,1) $ primeFactors z)
  -- the remaining operations are either undefined or trivial
  (+) = error "addition is not defined for monzos"
  (-) = error "subtraction is not defined for monzos"
  negate = throw Underflow
  abs = id
  signum = const (fromInteger 1)

instance (Num a, Eq a, KnownNat n) => Fractional (MonzoFrom n a) where
  (/) = mapMonzo2 (-)
  fromRational r = fromInteger (numerator r) / fromInteger (denominator r)

instance (Fractional a, Eq a, KnownNat n) => Algebraic (MonzoFrom n a) where
  pow a k = mapMonzo (* realToFrac k) a

instance (Integral a, KnownNat n) => Real (MonzoFrom n a) where
  toRational (toPrimePowers -> prs) = product (fmap (\(p,e) -> (p%1) ^^ e) prs)

toNthRoot :: (Real a, Eq a, KnownNat n) => MonzoFrom n a -> (Natural, Rational)
toNthRoot a = (fromInteger n, toRational a')
  where n = foldl' lcm 1 (fmap (denominator . toRational) (toList a))
        a' = mapMonzo (\e -> numerator (toRational e * fromInteger n)) a

instance (Real a, Eq a, KnownNat n) => Ord (MonzoFrom n a) where
  compare a b = let (_,r) = toNthRoot (a/b) in compare r 1

toFloat :: (Real a, Floating b, KnownNat n) => MonzoFrom n a -> b
toFloat (toPrimePowers -> prs) =
  product (fmap (\(p,e) -> fromInteger p ** realToFrac e) prs)


red :: (Real a, KnownNat n) => MonzoFrom n a -> MonzoFrom n a -> MonzoFrom n a
red a b = b / (a ^^ k)
  where k = floor (log (toFloat b) / log (toFloat a)) :: Integer

balRed :: (Real a, KnownNat n) => MonzoFrom n a -> MonzoFrom n a -> MonzoFrom n a
balRed a b = b / (a ^^ k)
  where k = round (log (toFloat b) / log (toFloat a)) :: Integer
