{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Prelude hiding (sqrt)
import Test.Hspec
import Test.QuickCheck

import Data.Ratio
import qualified Data.ExpList as EL
import Data.Monzo
import Data.Interval.Pythagorean
import Data.Interval.FJS

instance {-# OVERLAPPING #-} Arbitrary Rational where
  arbitrary = do
    d <- arbitrary `suchThat` \d -> d /= 0
    n <- scale (* fromIntegral (abs d)) $ arbitrary
    pure $ n % d
  shrink r =
       [ n' % (denominator r) | n' <- shrink (numerator r) ]
    ++ [ (numerator r) % d'   | d' <- shrink (denominator r) ]

arbitraryPositiveRational :: Gen Rational
arbitraryPositiveRational = do
  d <- resize 99 $ arbitrarySizedNatural `suchThat` \d -> d > 0
  n <- scale (* fromIntegral d) $ arbitrarySizedNatural `suchThat` \n -> n > 0
  pure $ n % d

instance (Eq a, Num a, Arbitrary a) => Arbitrary (EL.ExpList a) where
  arbitrary = EL.fromList <$> arbitrary
  shrink = fmap EL.fromList . shrink . EL.toList

instance (Eq a, Num a, Arbitrary a) => Arbitrary (Monzo a) where
  arbitrary = fromList <$> arbitrary
  shrink = fmap fromList . shrink . toList

instance Arbitrary PyInterval where
  arbitrary = PyInterval <$> scale (*4) arbitrary <*> arbitrary
  shrink (PyInterval g v) =
       [ PyInterval g' v | g' <- shrink g ]
    ++ [ PyInterval g v' | v' <- shrink v ]

instance Arbitrary GenFJSInterval where
  arbitrary = do
    pr <- arbitrary
    e2 <- arbitrary `suchThat` \e2 ->
            isValidGenFJSInterval (GenFJSInterval pr e2 (EL.fromList []))
    GenFJSInterval pr e2 <$> arbitrary
  shrink (GenFJSInterval pr e2 es) =
       [ GenFJSInterval pr' e2 es | pr' <- shrink pr ]
    ++ [ GenFJSInterval pr e2 es' | es' <- shrink es ]

arbitraryFJS :: Gen GenFJSInterval
arbitraryFJS = do
  pg <- arbitrary
  pv <- arbitrary
  es <- arbitrary
  pure $ GenFJSInterval (pyFromGen (pg%1) pv) 0 (EL.mapExpList (%1) es)

arbitraryNFJS :: Gen GenFJSInterval
arbitraryNFJS = do
  pr <- arbitrary
  e2 <- arbitrary `suchThat` \e2 ->
          isValidGenFJSInterval (GenFJSInterval pr (e2%6) (EL.fromList []))
  es <- arbitrary
  pure $ GenFJSInterval pr (e2%6) (EL.mapExpList (%1) es)

spec :: Spec
spec = do

  -- * Data.Monzo

  describe "integer monzos" $ do

    it "satisfy `fromRational . toRational = id`" $ property $
      forAll (arbitrary :: Gen (Monzo Integer)) $ \r ->
        r === fromRational (toRational r)
    it "satisfy `toRational . fromRational = id`" $ property $
      forAll arbitraryPositiveRational $ \r ->
        r === toRational (fromRational r :: Monzo Integer)

    it "have correct multiplication" $ property $
      forAll (arbitrary :: Gen (Monzo Integer)) $ \r1 ->
      forAll (arbitrary :: Gen (Monzo Integer)) $ \r2 ->
        r1 * r2 === fromRational (toRational r1 * toRational r2)
    it "have correct division" $ property $
      forAll (arbitrary :: Gen (Monzo Integer)) $ \r1 ->
      forAll (arbitrary :: Gen (Monzo Integer)) $ \r2 ->
        r1 / r2 === fromRational (toRational r1 / toRational r2)
    it "have correct order" $ property $
      forAll (arbitrary :: Gen (Monzo Integer)) $ \r1 ->
      forAll (arbitrary :: Gen (Monzo Integer)) $ \r2 ->
        compare r1 r2 === compare (toRational r1) (toRational r2)

  -- * Data.Interval.Pythagorean

  describe "zeroDeg/unzeroDeg" $ do

    it "satisfy `zeroDeg . unzeroDeg = id`" $ property $
      forAll arbitrary $ \z ->
        z === zeroDeg (unzeroDeg z)
    it "satisfy `unzeroDeg . zeroDeg = id` for `deg /= 0,-1` " $ property $
      forAll (arbitrary `suchThat` \d -> d /= 0 && d /= -1) $ \d ->
        d === unzeroDeg (zeroDeg d)

  describe "PyIntervals" $ do

    it "satisfy `pyFromGen . pyGenOctaves = id`" $ property $
      forAll arbitrary $ \pr ->
        pr === pyFromGen (pyGen pr) (pyOctaves pr)
    it "satisfy `pyGenOctaves . pyFromGen = id`" $ property $
      forAll (scale (*4) arbitrary) $ \g ->
      forAll arbitrary $ \v ->
        (g%4,v) === pyGenOctaves (pyFromGen (g%4) v)

    it "satisfy `pyFromDeg . pyDegOffset = id`" $ property $
      forAll arbitrary $ \pr ->
        pr === pyFromDeg (pyDeg pr) (pyOffset pr)
    it "satisfy `pyDegOffset . pyFromDeg = id` for `deg /= 0`" $ property $
      forAll (arbitrary `suchThat` \d -> d /= 0) $ \d ->
      forAll (scale (*4) arbitrary) $ \o ->
        let (d',o') = pyDegOffset (pyFromDeg d (o%4))
         in (d,o%4) === (d',o') .||. d === -1 .&&. d === -d' .&&. o%4 === -o'

    it "satisfy `intvToPy . pyToIntv = id`" $ property $
      forAll arbitrary $ \pr ->
        Just pr === intvToPy (pyToIntv pr)
    it "satisfy `pyToIntv . intvToPy = id`" $ property $
      forAll arbitrary $ \e2 -> forAll arbitrary $ \e3 ->
        Just (fromList [e2%1,e3%1]) === fmap pyToIntv (intvToPy (fromList [e2%1,e3%1]))

    it "satisfy `py . pyDegQuality = id`" $ property $
      forAll arbitrary $ \pr ->
        pr == py (pyQuality pr) (pyDeg pr)

    it "satisfy `pyDegQuality . py = id` for `q = NAugmented`" $ property $
      forAll (arbitrary `suchThat` \d -> d /= 0) $ \d ->
      forAll (scale (*4) $ arbitrarySizedNatural `suchThat` \o -> o > 0) $ \o ->
        let (q',d') = pyQualityDeg (py (NAugmented o) d)
         in (NAugmented o, d) === (q',d')
            .||. d === -1 .&&. NDiminished o === q' .&&. d === -d'

    it "satisfy `pyDegQuality . py = id` for `q = NDiminished`" $ property $
      forAll (arbitrary `suchThat` \d -> d /= 0) $ \d ->
      forAll (scale (*4) $ arbitrarySizedNatural `suchThat` \o -> o > 0) $ \o ->
        let (q',d') = pyQualityDeg (py (NDiminished o) d)
         in (NDiminished o, d) === (q',d')
            .||. d === -1 .&&. NAugmented o === q' .&&. d === -d'

    it "satisfy `pyDegQuality . py = id` for `q = Perfect`" $ property $
      forAll (arbitrary `suchThat` \d -> d /= 0 && isPerfectDeg d) $ \d ->
        let (q',d') = pyQualityDeg (py Perfect d)
         in (Perfect, d) === (q',d')
            .||. d === -1 .&&. Perfect === q' .&&. d === -d'

    it "satisfy `pyDegQuality . py = id` for `q = Major/Minor`" $ property $
      forAll (arbitrary `suchThat` \d -> d /= 0 && not (isPerfectDeg d)) $ \d ->
      forAll (elements [Major, SemiMajor, Minor, SemiMinor]) $ \q ->
         (q,d) === pyQualityDeg (py q d)

    it "satisfy `readPySymb . pySymb = id`" $ property $
      withMaxSuccess 200 $ forAll arbitrary $ \pr ->
        pr === readPySymb (pySymb pr)

  -- * Data.Interval.FJS

  describe "FJS Intervals" $ do

    it "satisfy fromFJSInterval . toFJSInterval = id" $ property $
      withMaxSuccess 200 $ forAll (arbitrary :: Gen (Monzo Integer)) $ (\r ->
        Just r === (fromFJSInterval <$> toFJSInterval r)) . realToFrac

    it "satisfy toFJSInterval . fromFJSInterval = id" $ property $
      withMaxSuccess 200 $ forAll arbitraryFJS $ \rfjs ->
        Just rfjs === toFJSInterval (fromFJSInterval rfjs)

  describe "NFJS Intervals" $ do

    it "satisfy fromNFJSInterval . toNFJSInterval = id" $ property $
      withMaxSuccess 200 $ forAll (arbitrary :: Gen (Monzo Integer)) $ (\r ->
        Just r === (fromNFJSInterval <$> toNFJSInterval r)) . realToFrac

    it "satisfy toNFJSInterval . fromNFJSInterval = id" $ property $
      withMaxSuccess 200 $ forAll arbitraryNFJS $ \rfjs ->
        Just rfjs === toNFJSInterval (fromNFJSInterval rfjs)


main :: IO ()
main = do
  hspec spec
