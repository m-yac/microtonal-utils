module Data.Algebraic where

import Prelude hiding (sqrt)
import Data.Ratio
import Data.Complex
import Data.Functor.Identity
import Data.Functor.Contravariant
import Data.Functor.Const

class Fractional a => Algebraic a where
  pow :: (Real b, Fractional b) => a -> b -> a

root :: (Algebraic a, Integral b) => b -> a -> a
root n x = x `pow` (1 % n)

sqrt :: Algebraic a => a -> a
sqrt = root (2 :: Integer)

-- Some instances for algebraic Prelude types

instance Algebraic Double where
  pow a k = a ** realToFrac k

instance Algebraic Float where
  pow a k = a ** realToFrac k

instance RealFloat a => Algebraic (Complex a) where
  pow a k = a ** realToFrac k

instance Algebraic a => Algebraic (Identity a) where
  pow (Identity a) k = Identity (pow a k)

instance Algebraic a => Algebraic (Op a b) where
  pow (Op f) k = Op $ \a -> pow (f a) k

instance Algebraic a => Algebraic (Const a b) where
  pow (Const a) k = Const (pow a k)
