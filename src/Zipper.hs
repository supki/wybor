-- | Non-empty list zipper
{-# LANGUAGE DeriveFunctor #-}
module Zipper
  ( Zipper
  , fromNonEmpty
  , fromList
  , zipperN
  , zipper
  , before
  , focus
  , after
  , left
  , right
  , prepend
  , append
  ) where

import           Control.Lens
import           Data.Foldable (Foldable(foldMap), toList)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Monoid ((<>))

data Zipper a = Zipper [a] a [a] deriving (Show, Eq, Functor)

instance Foldable Zipper where
  foldMap f (Zipper xs y zs) = foldMap f (reverse xs) <> f y <> foldMap f zs

-- | @zipperN n f z@ applies the function @f@ to the 'Zipper' @z@
-- at *at most @n@* points around its 'focus'
zipperN :: Int -> ([a] -> a -> [a] -> b) -> Zipper a -> b
zipperN n f (Zipper xs y zs) = zipper f $
  case (drop q xs, drop q zs) of
    ([], _) -> Zipper xs y (take (n - length xs - 1) zs)
    (_, []) -> Zipper (take (n - length zs - 1) xs) y zs
    _       -> Zipper (take q xs) y (take (q + r) zs)
 where
  (q, r) = (n - 1) `quotRem` 2
{-# ANN zipperN "HLint: ignore Redundant lambda" #-}

zipper :: ([a] -> a -> [a] -> b) -> Zipper a -> b
zipper f (Zipper xs y zs) = f (reverse xs) y zs

fromList :: [a] -> Maybe (Zipper a)
fromList = fmap fromNonEmpty . NonEmpty.nonEmpty

fromNonEmpty :: NonEmpty a -> Zipper a
fromNonEmpty (x :| xs) = Zipper [] x xs

focus :: Lens' (Zipper a) a
focus f (Zipper xs y zs) = f y <&> \y' -> Zipper xs y' zs

before, after :: Lens' (Zipper a) [a]
before f (Zipper xs y zs) = f xs <&> \xs' -> Zipper xs' y zs
after  f (Zipper xs y zs) = f zs <&> \zs' -> Zipper xs  y zs'

left :: Zipper a -> Zipper a
left z@(Zipper xs y ys) =
  case xs of
    []     -> z
    u : us -> Zipper us u (y : ys)

right :: Zipper a -> Zipper a
right z@(Zipper xs x ys) =
  case ys of
    []     -> z
    u : us -> Zipper (x : xs) u us

prepend :: Foldable f => f a -> Zipper a -> Zipper a
prepend xs = over before (reverse (toList xs) ++)

append :: Foldable f => f a -> Zipper a -> Zipper a
append xs = over after (++ toList xs)
