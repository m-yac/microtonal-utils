{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-|
Module      : Data.Monzo.Type
Copyright   : 2020 Matthew Yacavone
Maintainer  : matthew@yacavone.net

See Data.Monzo for more detailed documentation.
-}
module Data.Monzo.Type
  ( MonzoFrom
  , Monzo
  , fromList
  , toList
  , idxMonzo
  , toPrimePowers
  , fromPrimePowers
  , takeMonzo
  , dropMonzo
  , padMonzo
  , mapMonzo
  , mapMonzo2
  ) where

import Data.Function (on)
import Data.List (dropWhileEnd, sortBy)
import Data.Numbers.Primes
import Data.Proxy
import GHC.TypeNats
import Control.Exception

-- | A list of exponents of type @a@ for all primes after the @(n+1)@th. For
-- example, @'MonzoFrom' 2 'Integer'@ has whole number exponents for 5, 7, 11,
-- etc. but not 2 or 3.
--
-- This type has numeric instances that automatically convert it from rational
-- number literals (for example, @(5/4 :: Monzo Integer)@ is a valid
-- haskell expression) and which define multiplcation, division, @'pow'@, etc.
-- in the correct way.
newtype MonzoFrom (n :: Nat) a = Monzo [a] deriving Eq

-- | A list of prime exponents of type @a@.
type Monzo a = MonzoFrom 0 a

-- | Convert a list of exponents to a monzo, ignoring trailing zeros. For
-- example, @fromList [-2,0,1] == 5/4 :: Monzo Integer@.
--
-- This can be used with the DataKinds and TypeApplcations extensions to
-- indicate whether the monzo should start from a particular prime. For example,
-- @fromList \@0 [-1,1] == 3/2@ but @fromList \@2 [-1,1] == 11/5@.
fromList :: forall n a. (Num a, Eq a) => [a] -> MonzoFrom n a
fromList es = Monzo (dropWhileEnd (== fromInteger 0) es)

-- | The inverse of @'fromList'@.
toList :: forall n a. MonzoFrom n a -> [a]
toList (Monzo es) = es

-- | Get the exponent of the @(i+1)@th prime in this monzo. For example,
-- @fromList \@0 [-2,0,1] \`idxMonzo` 2 == 1@.
--
-- This can be used with the DataKinds and TypeApplcations extensions to
-- indicate whether the monzo should start from a particular prime. For example,
-- @idxMonzo \@2 (fromList [0,1,2,3,4]) 3 == 1@.
--
-- If @i < n@, this function throws an error.
idxMonzo :: forall n a. (Num a, KnownNat n) => MonzoFrom n a -> Int -> a
idxMonzo (toList -> es) i =
  if i >= n' then (es ++ repeat 0) !! (i-n')
             else throw (IndexOutOfBounds "idxMonzo")
  where n' = fromIntegral $ natVal (Proxy :: Proxy n)

-- | Converts a list of primes and their exponents to a monzo. For example,
-- @fromPrimePowers [(2,-2),(5,1)] == fromList [-2,0,1] :: Monzo Integer@.
--
-- If the list contains primes which are not allowed in the given 'MonzoFrom',
-- e.g. @fromPrimePowers [(2,x)] :: MonzoFrom 1 Integer@, this function throws
-- an error.
fromPrimePowers :: forall n a p. (Num a, Eq a, KnownNat n, Integral p)
                => [(p,a)] -> MonzoFrom n a
fromPrimePowers pes
  | toList (takeMonzo n' m0) == [] = dropMonzo m0
  | otherwise = error "prime not present in MonzoFrom"
  where go :: [(p,a)] -> [p] -> [a]
        go ((fp,fe):fs) (p:ps)
          | fp >  p = fromInteger 0 : go ((fp,fe):fs) ps
          | fp == p = let (eqs, fs') = span ((== p) . fst) fs
                       in (fe + sum (map snd eqs)) : go fs' ps
        go _ _ = []
        n' = fromIntegral $ natVal (Proxy :: Proxy n)
        m0 = fromList @0 $ go (sortBy (compare `on` fst) pes)
                              (map fromIntegral primes)

-- | The inverse of 'fromPrimePowers', though this function always returns the
-- list of prime powers in ascending order of the primes.
toPrimePowers :: forall n a p. (Num a, Eq a, KnownNat n, Num p)
              => MonzoFrom n a -> [(p,a)]
toPrimePowers (toList -> es) =
  filter (\(_,e) -> e /= 0) (zip (fromIntegral <$> drop n' primes) es)
  where n' = fromIntegral $ natVal (Proxy :: Proxy n)

dropMonzo :: forall n m a. KnownNat n
          => MonzoFrom m a -> MonzoFrom (n+m) a
dropMonzo (toList -> es) = Monzo (drop n' es)
  where n' = fromIntegral $ natVal (Proxy :: Proxy n)

padMonzo :: forall n m a. (Num a, Eq a, KnownNat n)
         => MonzoFrom (n+m) a -> MonzoFrom m a
padMonzo (toList -> es) = fromList (replicate n' 0 ++ es)
  where n' = fromIntegral $ natVal (Proxy :: Proxy n)

takeMonzo :: forall n a. (Num a, Eq a)
          => Int -> MonzoFrom n a -> MonzoFrom n a
takeMonzo n (toList -> es) = fromList (take n es)

instance (Show a, KnownNat n) => Show (MonzoFrom n a) where
  showsPrec p xs = showParen (p > 10) $
    showString "fromList " . nStr . shows (toList xs)
    where n' = fromIntegral $ natVal (Proxy :: Proxy n)
          nStr = if n' == 0 then showString ""
                 else showString "@" . shows n' . showString " "

zipMonzo :: forall n a b. (Num a, Num b)
         => MonzoFrom n a -> MonzoFrom n b -> MonzoFrom n (a,b)
zipMonzo (toList -> es) (toList -> es') = Monzo (go es es')
  where go [] [] = []
        go [] (y:ys) = (fromInteger 0, y) : go [] ys
        go (x:xs) [] = (x, fromInteger 0) : go xs []
        go (x:xs) (y:ys) = (x, y) : go xs ys

mapMonzo :: forall n a b. (Num b, Eq b)
         => (a -> b) -> MonzoFrom n a -> MonzoFrom n b
mapMonzo f = fromList . fmap f . toList

mapMonzo2 :: forall n a b c. (Num a, Num b, Num c, Eq c)
          => (a -> b -> c) -> MonzoFrom n a -> MonzoFrom n b -> MonzoFrom n c
mapMonzo2 f a b = mapMonzo (uncurry f) (zipMonzo a b)
