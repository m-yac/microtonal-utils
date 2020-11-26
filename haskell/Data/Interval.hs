{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
module Data.Interval where

import Prelude hiding (sqrt)

import Data.Ratio
import Data.Monzo
import Data.Algebraic
import Data.Maybe (fromJust, mapMaybe)
import Data.List (foldl', elemIndex)
import Data.Numbers.Primes

import Data.Interval.Type
import Data.Interval.Pythagorean
import Data.Interval.FJS

default (Integer, Rational, Double)

-- nfjsSeq :: [Rational]
-- nfjsSeq = [1, -1, 2, -2, 3, -3, 4, -4, 5, -5, 6, -6
--           , 1/2, -1/2, 3/2, -3/2, 5/2, -5/2, 7/2, -7/2, 11/2, -11/2]
--
-- nfjsRoT :: Interval
-- nfjsRoT = octaveReduce ((3/2) `pow` (-17/2))
--
-- comma :: Integer -> (Rational, Interval)
-- comma p = head $ mapMaybe go nfjsSeq
--   where go k = let c = balOctaveReduce (fromInteger p / pow (3/2) k)
--                 in if recip nfjsRoT < c && c < nfjsRoT then Just (k,c) else Nothing
--
-- rev :: Interval -> Interval
-- rev a@(toList -> (e1:e2:es)) | denominator e1 == denominator e2
--                               , denominator e2 `elem` [1,2] =
--   foldl' (\a' (e,p) -> a' / pow (snd (comma p)) e) a (zip es (drop 2 primes))
-- rev a@(toList -> [e1]) | denominator e1 == 1 = a
-- rev a@(toList -> []) = a
-- rev _ = error "Interval is invalid?"
--
-- gen :: Interval -> Integer
-- gen (toList -> []) = 0
-- gen (toList -> [e1]) | denominator e1 == 1 = 0
-- gen (toList -> [e1,e2]) | denominator e1 == denominator e2
--                           , denominator e2 `elem` [1,2]
--                           = numerator e2 * (3 - denominator e2)
-- gen _ = error "Interval is not an octave-reduced power of sqrt (3/2)"
--
-- pySymbOld :: Interval -> String
-- pySymbOld a =
--   (     if d `elem` [1,5] then go ["P","sA","A","sqA","AA"]
--                                       ["sd","d","sqd","dd"] o
--    else if d `elem` [4]   then go     ["sA","A","sqA","AA"]
--                                   ["P","sd","d","sqd","dd"] o
--    else if d `elem` [2,6] then go     ["M","aA","A","sqA","AA"]
--                                   ["n","m","sd","d","sqd","dd"] o
--                           else go ["n","M","aA","A","sqA","AA"]
--                                       ["m","sd","d","sqd","dd"] o)
--   ++ show d
--   where g = fromIntegral (gen a)
--         d = 1 + fromJust (elemIndex (g `mod` 7) [0, 4, 1, 5, 2, 6, 3])
--         o = g `div` 7
--         go xs ys i | i >= 0 = xs !! i
--                    | otherwise = ys !! (-i-1)
--
-- symb :: Interval -> String
-- symb a = pySymbOld (rev a) ++ concatMap go (drop 2 (zip primes (toList a)))
--   where go (p,e) | denominator e /= 1 = error "Interval has non-integer prime exponents"
--                  | e > 0 = "^" ++ show (p ^ numerator e)
--                  | e < 0 = "_" ++ show (p ^ numerator (-e))
--                  | otherwise = []
