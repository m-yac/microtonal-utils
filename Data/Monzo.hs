{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
module Data.Monzo
  ( Monzo
  , toExps
  , fromExps
  , toList
  , fromList
  , idxMonzo
  , toNthRoot
  , toFloat
  , red
  , balRed
  , showRational
  ) where

import Prelude hiding (sqrt)
import Data.Ratio
import Data.List (foldl', genericLength)
import Data.Numbers.Primes
import Numeric.Natural
import Control.Exception

import qualified Data.ExpList as EL
import Data.Algebraic

default (Integer, Rational, Double)

showsRationalPrec :: Int -> Rational -> ShowS
showsRationalPrec p r
  | denominator r == 1, numerator r >= 0 = shows (numerator r)
  | denominator r == 1, otherwise        = showParen (p > 6) $ shows (numerator r)
  | otherwise = showParen (p > 7) $ shows (numerator r) . showString "/" . shows (denominator r)

showRational :: Rational -> String
showRational r = showsRationalPrec 7 r []

instance {-# OVERLAPPING #-} Show Rational where
  showsPrec = showsRationalPrec


newtype Monzo a = Monzo { toExps :: EL.ExpList a } deriving Eq

fromExps :: EL.ExpList a -> Monzo a
fromExps = Monzo

toList :: Monzo a -> [a]
toList (Monzo es) = EL.toList es

fromList :: (Eq a, Num a) => [a] -> Monzo a
fromList es = Monzo (EL.fromList es)

idxMonzo :: (Eq a, Num a) => Monzo a -> Int -> a
idxMonzo (Monzo es) = EL.idxExpList es

mapMonzo :: (Eq b, Num b) => (a -> b) -> Monzo a -> Monzo b
mapMonzo f = fromExps . EL.mapExpList f . toExps

mapMonzo2 :: (Eq c, Num a, Num b, Num c)
          => (a -> b -> c) -> Monzo a -> Monzo b -> Monzo c
mapMonzo2 f a b = fromExps (EL.mapExpList2 f (toExps a) (toExps b))

instance Show a => Show (Monzo a) where
  showsPrec p xs = showParen (p > 10) $
    showString "fromList " . shows (toList xs)


instance (Eq a, Num a) => Num (Monzo a) where
  (*) = mapMonzo2 (+)
  fromInteger m | m <= 0 = throw Underflow
                | m == 1 = fromList []
                | otherwise = fromList (go (primeFactors m) primes)
    where go (f:fs) (p:ps) | f >  p = fromInteger 0 : go (f:fs) ps
                           | f == p = let (eqs, fs') = span (== p) fs
                                       in (1 + genericLength eqs) : go fs' ps
          go _ _ = []
  -- the remaining operations are either undefined or trivial
  (+) = error "addition is not defined for monzos"
  (-) = error "subtraction is not defined for monzos"
  negate = throw Underflow
  abs = id
  signum = const (fromInteger 1)

instance (Eq a, Num a) => Fractional (Monzo a) where
  (/) = mapMonzo2 (-)
  fromRational r = fromInteger (numerator r) / fromInteger (denominator r)

instance (Eq a, Fractional a) => Algebraic (Monzo a) where
  pow a k = mapMonzo (* realToFrac k) a

instance Integral a => Real (Monzo a) where
  toRational (toList -> es) = product fs
    where fs = (\(p,e) -> (p%1) ^^ e) <$> zip primes es

toNthRoot :: (Eq a, Real a) => Monzo a -> (Natural, Rational)
toNthRoot a = (fromInteger n, toRational a')
  where n = foldl' lcm 1 (fmap (denominator . toRational) (toList a))
        a' = mapMonzo (\e -> numerator (toRational e * fromInteger n)) a

instance (Eq a, Real a) => Ord (Monzo a) where
  compare a b = let (_,r) = toNthRoot (a/b) in compare r 1

toFloat :: (Real a, Floating b) => Monzo a -> b
toFloat (toList -> es) = product fs
  where fs = (\(p,e) -> fromInteger p ** realToFrac e) <$> zip primes es


red :: (Real a) => Monzo a -> Monzo a -> Monzo a
red a b = b / (a ^^ k)
  where k = floor (log (toFloat b) / log (toFloat a)) :: Integer

balRed :: (Real a) => Monzo a -> Monzo a -> Monzo a
balRed a b = b / (a ^^ k)
  where k = round (log (toFloat b) / log (toFloat a)) :: Integer
