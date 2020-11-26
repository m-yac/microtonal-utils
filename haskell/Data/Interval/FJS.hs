{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
module Data.Interval.FJS where

import Prelude hiding (sqrt)

import Data.List (foldl', intercalate)
import Data.Maybe (fromJust, mapMaybe)
import Data.Either (partitionEithers)
import Data.Ratio

import Data.Monzo
import Data.Interval.Type
import Data.Interval.Pythagorean
import Data.Algebraic

type CommaFunction = Interval -> PyInterval

data GenFJSInterval = GenFJSInterval PyInterval Rational (MonzoFrom 2 Rational)
                      deriving (Eq, Show)

isValidGenFJSInterval :: GenFJSInterval -> Bool
isValidGenFJSInterval (GenFJSInterval pr e2 _) =
  fst (properFraction e2) == 0 && signum e2 * signum (pyOctaves pr % 1) >= 0

genFJSSymb :: GenFJSInterval -> String
genFJSSymb rfjs | not (isValidGenFJSInterval rfjs) = error "invalid interval"
genFJSSymb (GenFJSInterval pr e2 (toPrimePowers -> prs)) =
  pySymb pr ++ showExps "^" otos ++ showExps "_" utos
  where showFracExp :: Integer -> Rational -> String
        showFracExp p e | e == 1/2 = "sqrt(" ++ show p ++ ")"
                        | numerator e == 1 = "root" ++ show (denominator e)
                                                    ++ "(" ++ show p ++ ")"
                        | otherwise = "(" ++ show p ++ "^" ++ show e ++ ")"
        go (p,e) | e >=  1 = Left (show p) : go (p,e-1)
                 | e <= -1 = Right (show p) : go (p,e+1)
                 | e > 0 = [Left (showFracExp p e)]
                 | e < 0 = [Right (showFracExp p (-e))]
                 | otherwise = []
        (otos, utos) = partitionEithers (concatMap go ((2,e2):prs))
        showExps sep ss | null ss = ""
                        | otherwise = sep ++ intercalate "," ss

fromGenFJSInterval :: CommaFunction -> GenFJSInterval -> Interval
fromGenFJSInterval _ rfjs | not (isValidGenFJSInterval rfjs) = error "invalid interval"
fromGenFJSInterval cf (GenFJSInterval pr e2 (toPrimePowers -> prs)) =
  pyToIntv pr * product (go <$> (2,e2):prs)
  where go (p,e) = (p `pow` e) * (pow (pyToIntv (cf p)) e)

toGenFJSInterval :: CommaFunction -> Interval -> Maybe GenFJSInterval
toGenFJSInterval cf r = do
  -- handle all the primes > 3
  let (es23, esGr3) = (takeMonzo 2 r, dropMonzo @2 r)
      i = foldl' (\i' (p,e) -> i' / pow (pyToIntv (cf p)) e)
                 es23 (toPrimePowers esGr3)
  -- handle the specical case of 2
  let e2 = snd (properFraction (i `idxMonzo` 0 + i `idxMonzo` 1))
      i' = i / pow (2 * pyToIntv (cf 2)) e2
  pr <- intvToPy i'
  pure $ GenFJSInterval pr e2 esGr3


data FJSLike = FJSLike { fifthsSeq :: [Rational]
                       , radOfTol  :: Interval }
                       deriving Show

cfFJSLike :: FJSLike -> CommaFunction
cfFJSLike FJSLike{..} i = head $ mapMaybe go fifthsSeq
  where go k = let c = balOctaveReduce (i / pow (3/2) k)
                in if recip radOfTol < c && c < radOfTol
                   then Just (fromJust . intvToPy $ c / i)
                   else Nothing

theFJS :: FJSLike
theFJS = FJSLike { fifthsSeq = concatMap (\i -> [i,-i]) [1..]
                 , radOfTol  = 65/63 }

commaFJS :: CommaFunction
commaFJS = cfFJSLike theFJS

toFJSInterval :: Interval -> Maybe GenFJSInterval
toFJSInterval = toGenFJSInterval commaFJS

fromFJSInterval :: GenFJSInterval -> Interval
fromFJSInterval = fromGenFJSInterval commaFJS

theNFJS :: FJSLike
theNFJS = FJSLike { fifthsSeq = concatMap (\j -> concatMap (\i -> [i,-i]) [(j+1)..(j+6)] ++
                                                 concatMap (\i -> [i/2,-i/2]) [(2*j+1),(2*j+3)..(2*j+11)])
                                          [0,6..]
                  , radOfTol  = pyToIntv (py SemiDiminished 2) }

commaNFJS :: CommaFunction
commaNFJS = cfFJSLike theNFJS

toNFJSInterval :: Interval -> Maybe GenFJSInterval
toNFJSInterval = toGenFJSInterval commaNFJS

fromNFJSInterval :: GenFJSInterval -> Interval
fromNFJSInterval = fromGenFJSInterval commaNFJS
