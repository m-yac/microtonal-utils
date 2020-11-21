{-|
Module      : Data.Monzo
Copyright   : 2020 Matthew Yacavone
Maintainer  : matthew@yacavone.net

A @'Monzo'@ is way of representing a positive number (or in musical terms,
an interval) as a list of prime exponents. In particular, monzos always satisfy
the following equality:

@
'fromList' [e0,e1,...,en] == 2^e0 * 3^e1 * 5^e2 * ... * pn^en
@

where @pn@ is the @(n+1)@th prime number. We call the expression on the right a
/prime factorization/. [1]

For music theory, monzos have two key properties:

1. If the exponents of a monzo are whole numbers, then the result is always a
   rational number - in musical terms,
   <https://en.wikipedia.org/wiki/Just_intonation a just interval>.

2. Every rational number has a unique prime factorization, i.e. a unique monzo
   to which it corresponds.

This means that studying monzos with whole number exponents is the same thing
as studying positive rational numbers (i.e. just intervals). For music, this is
useful because a monzos are often easier to work with, and generally give more
information about an interval than its expression as a rational number.

Some examples:

- The just perfect fifth @3/2@ can be written as @2^(-1) * 3^1@, so corresponds
    to the monzo @'fromList' [-1,1]@. This is an example where a monzo is just
    as useful as its rational number counterpart. Another such example is the
    just major third:

    @
    5\/4 == 'fromList' [-2,0,1]   since 5\/4 == 2^(-2) * 5^1
    @

- To stack intervals represented by rational numbers one has to multiply their
    numerators and denominators. To stack intervals represented by monzos one
    has to add corresponding exponents. The latter is often easier beacuase
    addition is generally easier to do then multiplication and because the
    exponents of a monzo always end up smaller than the numerator and
    denominator of their corresponding rational numbers.

    For example, consider stacking a just perfect fourth (@4/3@) and a just
    minor second (@16/15@) to get a just diminished fifth. Using rational
    numbers one needs to compute @(4*16)/(3*15)@, which I can't easily can't do
    in my head (the answer is @64/45@). Using monzos, the calculation is much
    easier: @4\/3 == 'fromList' [2,-1]@ and @16/15 = 'fromList' [4,-1,-1]@ so
    stacking the two gives @'fromList' [2+4,-1-1,-1] == 'fromList' [6,-2,-1]@.

- Consider the just interval @243/128@. Unless you recognize it, or have
    fantastic mental arithmetic, this gives you no information about what the
    interval represents. It's corresponding monzo is @'fromList' [-7,5]@, which
    tells us much more.

    * The fact that there are only two terms tells us that the interval is
      Pythagorean, or 3-limit (meaning it is composed of only powers of 2 and
      3).

    * Since we know a perfect fifth is @'fromList' [-1,1]@ we can go further
      and conclude out that this interval must be the same as stacking 5 perfect
      fifths going down two octaves, since
      @'fromList' [-7,5] == ('fromList' [-1,1])^5 / ('fromList' [1])^2@.
      Remembering our interval arithmetic, this makes this interval a
      Pythagorean major seventh.

These examples are deliberately contrived, most 5-limit just intervals are
easy enough to work with using their rational number representations. However,
as one deals with more and more complex intervals, monzos make things much
easier and give important perspective.

[1\] If the exponents used are rational, such a factorization is always unique
-}
module Data.Monzo
  ( Monzo
  , MonzoFrom
  -- * Construction
  , fromList
  , fromPrimePowers
  -- * Destruction
  , toList
  , toPrimePowers
  , idxMonzo
  -- * Numeric functions
  -- | See also @'fromInteger'@, @'(*)'@, @'(/)'@, @'fromRational'@, @'pow'@,
  -- and @'toRational'@.
  , toNthRoot
  , toFloat
  , red
  , balRed
  -- * List-like functions
  , takeMonzo
  , dropMonzo
  , padMonzo
  , mapMonzo
  , mapMonzo2
  ) where

import Data.Monzo.Type
import Data.Monzo.Numeric
