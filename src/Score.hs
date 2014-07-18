{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
module Score
  ( Score
  , Input(..)
  , Choice(..)
  , score
#ifdef TEST
  , minimum
#endif
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Maybe (mapMaybe)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Prelude hiding (minimum)


newtype Score = Score Double deriving (Show, Eq, Ord)

instance Bounded Score where
  minBound = Score 0.0
  maxBound = Score 1.0

newtype Input = Input Text deriving (Show, Eq)

newtype Choice = Choice Text deriving (Show, Eq)

score :: Input -> Choice -> Score
score (Input q) (Choice c)
  | Nothing      <- Text.uncons (Text.toLower q) = maxBound
  | Just (x, xs) <- Text.uncons (Text.toLower q)
  , Just ml      <- sub x xs (Text.toLower c)    = Score ((fi ql / fi ml) / fi cl)
  | otherwise = minBound
 where
  fi = fromIntegral
  ql = Text.length q
  cl = Text.length c

-- | The shortest substring match length
sub :: Char -> Text -> Text -> Maybe Int
sub q qs = minimum . mapMaybe (rec qs <=< Text.stripPrefix (Text.singleton q)) . Text.tails

rec :: Text -> Text -> Maybe Int
rec = go 0
 where
  go !len xs bs = case Text.uncons xs of
    Nothing      -> Just len
    Just (a, as) -> case Text.findIndex (== a) bs of
      Nothing -> Nothing
      Just i  -> go (len + i) as (Text.drop (i + 1) bs)

minimum :: Ord a => [a] -> Maybe a
minimum = foldr (\x a -> liftA2 min (Just x) a <|> Just x) Nothing
