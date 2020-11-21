module Data.Interval.Type where

import Data.Monzo

type Interval = Monzo Rational

octaveReduce :: Interval -> Interval
octaveReduce = red 2

balOctaveReduce :: Interval -> Interval
balOctaveReduce = balRed 2
