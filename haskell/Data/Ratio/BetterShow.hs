{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Ratio.BetterShow where

import Data.Ratio

showsRationalPrec :: Int -> Rational -> ShowS
showsRationalPrec p r
  | denominator r == 1, numerator r >= 0 = shows (numerator r)
  | denominator r == 1, otherwise        = showParen (p > 6) $ shows (numerator r)
  | otherwise = showParen (p > 7) $ shows (numerator r) . showString "/" . shows (denominator r)

showRational :: Rational -> String
showRational r = showsRationalPrec 7 r []

instance {-# OVERLAPPING #-} Show Rational where
  showsPrec = showsRationalPrec
