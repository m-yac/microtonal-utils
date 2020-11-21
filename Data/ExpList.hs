{-# LANGUAGE ViewPatterns #-}
module Data.ExpList
  ( ExpList
  , toList
  , fromList
  , idxExpList
  , takeExpList
  , dropExpList
  , mapExpList
  , mapExpList2
  ) where

import Data.List (dropWhileEnd)


newtype ExpList a = ExpList [a] deriving Eq

toList :: ExpList a -> [a]
toList (ExpList es) = es

fromList :: (Eq a, Num a) => [a] -> ExpList a
fromList es = ExpList (dropWhileEnd (== fromInteger 0) es)

zipExpList :: (Num a, Num b) => ExpList a -> ExpList b -> ExpList (a,b)
zipExpList (toList -> es) (toList -> es') = ExpList (go es es')
  where go [] [] = []
        go [] (y:ys) = (fromInteger 0, y) : go [] ys
        go (x:xs) [] = (x, fromInteger 0) : go xs []
        go (x:xs) (y:ys) = (x, y) : go xs ys

dropExpList :: Int -> ExpList a -> ExpList a
dropExpList n (toList -> es) = ExpList (drop n es)

takeExpList :: Int -> ExpList a -> ExpList a
takeExpList n (toList -> es) = ExpList (take n es)

idxExpList :: (Num a) => ExpList a -> Int -> a
idxExpList (toList -> es) i = (es ++ repeat 0) !! i

mapExpList :: (Eq b, Num b) => (a -> b) -> ExpList a -> ExpList b
mapExpList f = fromList . fmap f . toList

mapExpList2 :: (Eq c, Num a, Num b, Num c)
          => (a -> b -> c) -> ExpList a -> ExpList b -> ExpList c
mapExpList2 f a b = mapExpList (uncurry f) (zipExpList a b)

instance Show a => Show (ExpList a) where
  showsPrec p xs = showParen (p > 10) $
    showString "fromList " . shows (toList xs)
