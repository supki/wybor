{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Console line fuzzy search as a library.
--
-- /Note:/ It's probably better to toy with the library through
-- the @wybor@ executable, but it's still possible to use it from ghci,
-- albeit it would not be pretty
--
-- First, we need a bit of setup. Both extensions aren't strictly necessary
-- but they make our life considerably easier. Imports, on the other hand, are
-- essential.
--
-- @
-- >>> :set -XOverloadedStrings
-- >>> :set -XOverloadedList
-- >>> import Control.Lens
-- >>> import Wybor
-- @
--
-- So, this starts a sligtly customized selection process (more settings are avaliable,
-- please see 'HasWybor' class):
--
-- @
-- >>> select (fromTexts ["foo", "bar", "baz"] & prefix .~ "λ> ")
-- λ>
-- __foo__
-- bar
-- baz
-- @
--
-- This new prompt (@λ>@) expects us to enter symbols to narrow the list of outcomes, e.g.:
--
-- @
-- λ> b
-- __bar__
-- baz
-- @
--
-- @
-- λ> bz
-- __baz__
-- @
--
-- At this point there is only one acceptable outcome, so we press @Enter@ and end up with:
--
-- @
-- Right (Just "baz")
-- @
--
-- Of course, we can press @Enter@ at any point of the process, selecting
-- the focused alternative (marked bold here). Other hotkeys:
--
--   - @C-j@ selects the focused alternative (like @Enter@)
--   - @C-u@ clears the line
--   - @C-w@ deletes last word
--   - @C-h@ deletes last character (as does @Backspace@)
--   - @C-n@ focuses the next alternative
--   - @C-p@ focuses the previous alternative
--   - @C-d@ aborts the selection
--
module Wybor
  ( Wybor
  , HasWybor(..)
  , fromAssoc
  , fromTexts
  , select
  , selections
  , TTYException(..)
#ifdef TEST
  , pipeline
  , keyEnter
  , keyBksp
  , keyCtrl
#endif
  ) where

import           Control.Exception (try)
import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans.Resource (MonadResource, runResourceT)
import           Control.Monad.IO.Class (MonadIO(..))
import           Data.Conduit (Source, Conduit, (=$=), ($$))
import qualified Data.Conduit as C
import           Data.Char (isPrint, isSpace, chr, ord)
import           Data.Foldable (toList)
import           Data.Function (fix,on)
import           Data.List (sortBy)
import           Data.List.NonEmpty (NonEmpty)
import           Data.Monoid (Monoid(..), (<>))
import           Data.Text (Text)
import qualified Data.Text as Text
import           Prelude hiding (unlines)
import           System.Console.ANSI as Ansi

import           Score (score, Input(..), Choice(..))
import           TTY (TTY, TTYException)
import qualified TTY
import           Zipper (Zipper, focus, zipperN)
import qualified Zipper


-- | The description of the alternative choices, see 'HasWybor'
data Wybor a = Wybor
  { _alternatives :: NonEmpty (Text, a)
  , _visible      :: Int
  , _prefix       :: Text
  , _initial      :: Text
  } deriving (Show, Eq, Functor)

-- | A bunch of lenses to pick and configure 'Wybor'
class HasWybor t a | t -> a where
  wybor :: Lens' t (Wybor a)

  -- | Alternative choices
  alternatives :: Lens' t (NonEmpty (Text, a))
  alternatives = wybor . \f x -> f (_alternatives x) <&> \y -> x { _alternatives = y }
  {-# INLINE alternatives #-}

  -- | How many alternative choices to show at once?
  visible :: Lens' t Int
  visible = wybor . \f x -> f (_visible x) <&> \y -> x { _visible = y }
  {-# INLINE visible #-}

  -- | Prompt prefix
  prefix :: Lens' t Text
  prefix = wybor . \f x -> f (_prefix x) <&> \y -> x { _prefix = y }
  {-# INLINE prefix #-}

  -- | Initial search string
  initial :: Lens' t Text
  initial = wybor . \f x -> f (_initial x) <&> \y -> x { _initial = y }
  {-# INLINE initial #-}

instance HasWybor (Wybor a) a where
  wybor = id
  {-# INLINE wybor #-}


-- | Select an item from 'Wybor' once.
--
-- The user can interrupt the process with @C-d@ and then you get 'Nothing'.
-- Exceptions result in @'Left' _@
select :: HasWybor t a => t -> IO (Either TTYException (Maybe a))
select c = try . runResourceT $ selections c $$ C.await

-- | Continuously select items from 'Wybor'. Also see 'select'
selections :: (HasWybor t a, MonadResource m) => t -> Source m a
selections = TTY.withTTY . pipeline

-- | Construct 'Wybor' from the nonempty list of key-value pairs
fromAssoc :: NonEmpty (Text, a) -> Wybor a
fromAssoc xs = Wybor { _alternatives = xs, _prefix = ">>> ", _visible = 10, _initial = "" }

-- | Construct 'Wybor' from the nonempty list of strings.
--
-- The strings are used both as keys and values
fromTexts :: NonEmpty Text -> Wybor Text
fromTexts = fromAssoc . fmap (\x -> (x, x))

pipeline :: (HasWybor t a, MonadIO m) => t -> TTY -> Source m a
pipeline c tty = sourceInput tty =$= parseInput =$= renderUI tty (view wybor c)

sourceInput :: MonadIO m => TTY -> Source m Char
sourceInput tty = fix $ \loop -> TTY.getChar tty >>= C.yield >> loop

parseInput :: Monad m => Conduit Char m Event
parseInput = C.await >>= \mk -> case mk of
  Nothing ->         return ()
  Just k  -> case handleKey k of
    Nothing       -> return ()
    Just Nothing  ->              parseInput
    Just (Just e) -> C.yield e >> parseInput


data Event =
    Done
  | Down
  | Up
  | Clear
  | DeleteWord
  | DeleteChar
  | Append Char
    deriving (Show, Eq)

handleKey :: Char -> Maybe (Maybe Event)
handleKey c
  | c == keyEnter    = Just (Just Done)       -- Enter, C-j
  | c == keyCtrl 'D' = Nothing                -- C-d
  | c == keyCtrl 'N' = Just (Just Down)       -- C-n
  | c == keyCtrl 'P' = Just (Just Up)         -- C-p
  | c == keyCtrl 'U' = Just (Just Clear)      -- C-u
  | c == keyCtrl 'W' = Just (Just DeleteWord) -- C-w
  | c == keyBksp     = Just (Just DeleteChar) -- Backspace
  | c == keyCtrl 'H' = Just (Just DeleteChar) -- C-h
  | isPrint c        = Just (Just (Append c))
  | otherwise        = Just Nothing

keyEnter, keyBksp :: Char
keyEnter = '\n'
keyBksp  = '\DEL'

-- | @keyCtrl c@ converts @c@ to Ctrl-@c@ character. Only expected to work
-- with upper case ASCII characters from @A-Z@ range.
keyCtrl :: Char -> Char
keyCtrl a = chr (ord a - 64)


data Query a b = Query
  { _matches :: Maybe (Zipper (a, b))
  , _input   :: a
  } deriving (Show, Eq)

matches :: Lens' (Query a b) (Maybe (Zipper (a, b)))
matches f x = f (_matches x) <&> \y -> x { _matches = y }

input :: Lens' (Query a b) a
input f x = f (_input x) <&> \y -> x { _input = y }

handleEvent :: Wybor a -> Query Text a -> Event -> Either a (Query Text a)
handleEvent c s = \case
  Done       -> maybe (Right s) Left (done s)
  Down       -> Right (down s)
  Up         -> Right (up s)
  Clear      -> Right (clear c)
  DeleteChar -> Right (deleteChar c s)
  DeleteWord -> Right (deleteWord c s)
  Append x   -> Right (append c s x)

done :: Query a b -> Maybe b
done = preview (matches.traverse.focus._2)

down :: Query a b -> Query a b
down = over (matches.traverse) Zipper.right

up :: Query a b -> Query a b
up = over (matches.traverse) Zipper.left

clear :: Wybor a -> Query Text a
clear c = fromInput c mempty

append :: Wybor a -> Query Text a -> Char -> Query Text a
append c s x = let q' = view input s |> x in fromInput c q'

deleteChar :: Wybor a -> Query Text a -> Query Text a
deleteChar c = maybe (clear c) (fromInput c . fst) . unsnoc . view input

deleteWord :: Wybor a -> Query Text a -> Query Text a
deleteWord c = fromInput c . view (input.to (Text.dropWhileEnd (not . isSpace) . Text.stripEnd))

fromInput :: Wybor a -> Text -> Query Text a
fromInput c q = Query { _matches = view (alternatives.to (computeMatches q)) c, _input = q }

fromSelect :: Wybor a -> Query Text a
fromSelect c = fromInput c (view initial c)

computeMatches :: Text -> NonEmpty (Text, a) -> Maybe (Zipper (Text, a))
computeMatches q = Zipper.fromList . sortOnByOf choiceScore cmp positive . toList
 where
  choiceScore = score (Input q) . Choice . fst
  positive s = s > minBound
  cmp = flip compare

sortOnByOf :: Ord b => (a -> b) -> (b -> b -> Ordering) -> (b -> Bool) -> [a] -> [a]
sortOnByOf f c p = map fst . sortBy (c `on` snd) . filter (p . snd) . map (\x -> (x, f x))


renderUI :: MonadIO m => TTY -> Wybor a -> Conduit Event m a
renderUI tty c = do
  row <- TTY.getCursorRow tty
  let offset = max 0 (linesTaken c - (TTY.winHeight tty - row))
  replicateM_ offset (TTY.putLine tty)
  flip fix (fromSelect c) $ \loop s -> do
    render tty c (row - offset) s
    me <- C.await
    case me of
      Nothing -> cleanUp
      Just e  -> either (\r -> cleanUp >> C.yield r >> loop s) loop (handleEvent c s e)
 where
  cleanUp = TTY.clearScreenBottom tty

render :: MonadIO m => TTY -> Wybor a -> Int -> Query Text a -> m ()
render tty c top s = liftIO . TTY.withHiddenCursor tty $ do
  let column  = lengthOf (prefix.each) c + lengthOf (input.each) s
      content = unlines (map cleanly (take (linesTaken c) (choicesLines tty c s ++ repeat "")))
  TTY.moveCursor tty top 0
  TTY.putText tty content
  TTY.moveCursor tty top column
 where
  unlines = Text.intercalate "\n"
  cleanly l = l <> Text.pack Ansi.clearFromCursorToLineEndCode

choicesLines :: TTY -> Wybor a -> Query Text b -> [Text]
choicesLines tty c s =
  let
    mz = preview (matches.traverse) s
    n  = view visible c
    w  = TTY.winWidth tty
  in
    Text.take w (view prefix c <> view input s) :
    maybe [] (zipperN n combine . fmap (Text.take w . fst)) mz
 where
  combine xs y zs = xs ++ [swap <> y <> unswap] ++ zs
  swap   = Text.pack (Ansi.setSGRCode [Ansi.SetSwapForegroundBackground True])
  unswap = Text.pack (Ansi.setSGRCode [Ansi.SetSwapForegroundBackground False])

linesTaken :: Wybor a -> Int
linesTaken c = min (lengthOf (alternatives.folded) c) (view visible c) + 1
