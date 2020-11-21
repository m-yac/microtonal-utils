{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module Data.Interval.Pythagorean where

import Prelude hiding (sqrt)
import Control.Exception
import Data.Maybe (fromJust)
import Data.Algebraic

import Data.Interval.Type
import Data.Ratio
import Data.Monzo

signum' :: Integer -> Integer
signum' 0 = 1
signum' x = signum x


data PyInterval = PyInterval Integer Integer deriving Eq

pyFromGen :: Rational -> Integer -> PyInterval
pyFromGen g v | denominator (g * 4) == 1 = PyInterval (numerator (g * 4)) v
              | otherwise = error "generator does not produce a valid Pythagorean interval"

instance Show PyInterval where
  showsPrec p (PyInterval g v) = showParen (p > 10) $
    showString "pyFromGen " . showsPrec 10 (g % 4)
                            . showString " "
                            . showsPrec 10 v

pyGen :: PyInterval -> Rational
pyGen (PyInterval g _) = g%4

pyOctaves :: PyInterval -> Integer
pyOctaves (PyInterval _ v) = v

pyGenOctaves :: PyInterval -> (Rational, Integer)
pyGenOctaves pr = (pyGen pr, pyOctaves pr)

zeroDeg :: Integer -> Integer
zeroDeg 0 = error "invalid degree (0)"
zeroDeg d = d - signum d

unzeroDeg :: Integer -> Integer
unzeroDeg 0 = 1
unzeroDeg d = d + signum d

-- | ...
pyFromDeg :: Integer -> Rational -> PyInterval
pyFromDeg d' o | denominator (o * 4) == 1
               = let d = zeroDeg d'
                     neutGen = ((d * 4 + 3) `mod` 7) - 3
                     g = neutGen * 2 + (numerator (o * (signum d'%1) * 4)) * 7
                  in PyInterval g ((d - g) `div` 7)
               | otherwise = error "offset does not produce a valid Pythagorean interval"

pyDeg :: PyInterval -> Integer
pyDeg (PyInterval g v) = unzeroDeg (g + v * 7)

pyOffset :: PyInterval -> Rational
pyOffset pr@(PyInterval g _) =
  signum (pyDeg pr) * (2 * ((4 * g + 3) `div` 7) - g)%4

pyDegOffset :: PyInterval -> (Integer, Rational)
pyDegOffset pr = (pyDeg pr, pyOffset pr)


pyToIntv :: PyInterval -> Interval
pyToIntv pr = ((3/2) `pow` pyGen pr) * (2 `pow` fromInteger (pyOctaves pr))

intvToPy :: Interval -> Maybe PyInterval
intvToPy r
  | length (toList r) <= 2, e2 <- r `idxMonzo` 0, e3 <- r `idxMonzo` 1
  , denominator (e3 * 4) == 1, denominator (e2 + e3) == 1
  = Just (PyInterval (numerator $ e3 * 4) (numerator $ e2 + e3))
  | otherwise = Nothing


instance Num PyInterval where
  (PyInterval g1 v1) * (PyInterval g2 v2) = PyInterval (g1 + g2) (v1 + v2)
  fromInteger m = maybe (error "integer is not a Pythagorean interval") id
                        (intvToPy (fromInteger m))
  -- the remaining operations are either undefined or trivial
  (+) = error "addition is not defined for Pythagorean intervals"
  (-) = error "subtraction is not defined for Pythagorean intervals"
  negate = throw Underflow
  abs = id
  signum = const (fromInteger 1)

instance Fractional PyInterval where
  (PyInterval g1 v1) / (PyInterval g2 v2) = PyInterval (g1 - g2) (v1 - v2)
  fromRational r = fromInteger (numerator r) / fromInteger (denominator r)

instance Algebraic PyInterval where
  pow (PyInterval g v) k
    | g' <- toRational (fromInteger g * k), v' <- toRational (fromInteger v * k)
    , denominator g' == 1, denominator v' == 1
    = PyInterval (numerator g') (numerator v')
    | otherwise = error "power is not a Pythagorean interval"

instance Ord PyInterval where
  compare pr1 pr2 = compare (pyToIntv pr1) (pyToIntv pr2)


data PyQuality = Perfect | Neutral
               | SemiMajor | SemiMinor | Major | Minor
               | NAugmented Integer | NDiminished Integer
               deriving (Eq, Show)

pattern SemiAugmented, SemiDiminished, Augmented, Diminished,
        DoublyAugmented, DoublyDiminished :: PyQuality
pattern SemiAugmented = NAugmented 2
pattern SemiDiminished = NDiminished 2
pattern Augmented = NAugmented 4
pattern Diminished = NDiminished 4
pattern DoublyAugmented = NAugmented 8
pattern DoublyDiminished = NDiminished 8

isPerfectDeg :: Integer -> Bool
isPerfectDeg d = unzeroDeg (abs (zeroDeg d) `mod` 7) `elem` [1,4,5]

pyQuality :: PyInterval -> PyQuality
pyQuality (pyDegOffset -> (d,o)) = let o' = numerator (o * 4)
                                  in case isPerfectDeg d of
  True  | o' == 0   -> Perfect
        | o' >  0   -> NAugmented o'
        | otherwise -> NDiminished (-o')
  False | o' == 0   -> Neutral
        | o' == 1   -> SemiMajor
        | o' == 2   -> Major
        | o' >  2   -> NAugmented (o'-2)
        | o' == -1  -> SemiMinor
        | o' == -2  -> Minor
        | otherwise -> NDiminished (-2-o')

pyQualityDeg :: PyInterval -> (PyQuality, Integer)
pyQualityDeg pr = (pyQuality pr, pyDeg pr)

mbPy :: PyQuality -> Integer -> Maybe PyInterval
mbPy _ 0 = Nothing
mbPy q d = case (isPerfectDeg d, q) of
  (True , Perfect      ) -> Just $ pyFromDeg d 0
  (True , NAugmented o ) -> Just $ pyFromDeg d (o % 4)
  (True , NDiminished o) -> Just $ pyFromDeg d (-o % 4)
  (False, Neutral      ) -> Just $ pyFromDeg d 0
  (False, SemiMajor    ) -> Just $ pyFromDeg d (1/4)
  (False, Major        ) -> Just $ pyFromDeg d (1/2)
  (False, NAugmented o ) -> Just $ pyFromDeg d ((o+2) % 4)
  (False, SemiMinor    ) -> Just $ pyFromDeg d (-1/4)
  (False, Minor        ) -> Just $ pyFromDeg d (-1/2)
  (False, NDiminished o) -> Just $ pyFromDeg d ((-o-2) % 4)
  _ -> Nothing

py :: PyQuality -> Integer -> PyInterval
py q d = fromJust (mbPy q d)

pySymb :: PyInterval -> String
pySymb pr@(pyDegOffset -> (d,_))
  = qstr ++ show (abs d) ++ (if d < 0 then " down" else "")
  where qstr = case pyQuality pr of
          Perfect -> "P"
          Neutral -> "n"
          SemiMajor -> "sM"
          SemiMinor -> "sm"
          Major -> "M"
          Minor -> "m"
          SemiAugmented -> "sA"
          Augmented -> "A"
          NAugmented k -> showRational (fromIntegral k / 4) ++ "A"
          SemiDiminished -> "sd"
          Diminished -> "d"
          NDiminished k -> showRational (fromIntegral k / 4) ++ "d"

readsPyQuality :: ReadS PyQuality
readsPyQuality s0 = do
  (l1,s1) <- lex s0
  case l1 of
    'P':s     -> pure (Perfect, s ++ s1)
    'n':s     -> pure (Neutral, s ++ s1)
    's':'M':s -> pure (SemiMajor, s ++ s1)
    's':'m':s -> pure (SemiMinor, s ++ s1)
    'M':s     -> pure (Major, s ++ s1)
    'm':s     -> pure (Minor, s ++ s1)
    's':'A':s -> pure (SemiAugmented, s ++ s1)
    'A':s     -> pure (Augmented, s ++ s1)
    's':'d':s -> pure (SemiDiminished, s ++ s1)
    'd':s     -> pure (Diminished, s ++ s1)
    _    -> do
      (n,s2) <- reads s0
      (l2,s3) <- lex s2
      case l2 of
        'A':s | n >= 0 -> pure (NAugmented (n*4), s ++ s3)
        'd':s | n >= 0 -> pure (NDiminished (n*4), s ++ s3)
        "/" -> do
          (d,s4) <- reads s3
          (l3,s5) <- lex s4
          case l3 of
            'A':s | n >= 0, d `elem` [1,2,4] -> pure (NAugmented (n * (4`div`d)), s ++ s5)
            'd':s | n >= 0, d `elem` [1,2,4] -> pure (NDiminished (n * (4`div`d)), s ++ s5)
            _ -> []
        _ -> []

readsPySymb :: ReadS PyInterval
readsPySymb s0 = do
  (q,s1) <- readsPyQuality s0
  (d,s2) <- reads s1
  (l1,s3) <- lex s2
  case l1 of
    "down" -> pure (py q (-d), s3)
    _      -> pure (py q d, s2)

mbReadPySymb :: String -> Maybe PyInterval
mbReadPySymb s | [(r,_)] <- readsPySymb s = Just r
               | otherwise = Nothing

readPySymb :: String -> PyInterval
readPySymb = maybe (error "readPySymb: no parse") id . mbReadPySymb


pyName :: PyInterval -> String
pyName pr@(pyDegOffset -> (d,_))
  = qstr ++ " " ++ dstr ++ ordstr ++ (if d < 0 then " down" else "")
  where qstr = case pyQuality pr of
          Perfect -> "Perfect"
          Neutral -> "neutral"
          SemiMajor -> "semi-Major"
          SemiMinor -> "semi-minor"
          Major -> "Major"
          Minor -> "minor"
          SemiAugmented -> "semi-Augmented"
          Augmented -> "Augmented"
          DoublyAugmented -> "Doubly-Augmented"
          NAugmented k -> showRational (fromIntegral k / 4) ++ "-Augmented"
          SemiDiminished -> "semi-diminished"
          Diminished -> "diminished"
          DoublyDiminished -> "doubly-diminished"
          NDiminished k -> showRational (fromIntegral k / 4) ++ "-diminished"
        dstr = show (abs d)
        ordstr = if last dstr == '1' then "st"
                 else if last dstr == '2' then "nd"
                 else if last dstr == '3' then "rd" else "th"
