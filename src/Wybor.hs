{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Console line fuzzy search as a library.
--
-- It's probably easier to toy with the library through
-- the @wybor@ executable, although this example uses @ghci@, which is
-- also a pretty capable environment for toying
--
-- First, we need a bit of setup. Both extensions aren't strictly necessary
-- but they make our life considerably easier. Imports, on the other hand, are
-- essential.
--
-- /Note:/ @-XOverloadedLists@ requires GHC >= 7.8
--
-- @
-- >>> :set -XOverloadedStrings
-- >>> :set -XOverloadedLists
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
  ( select
  , selections
  , Wybor
  , fromAssoc
  , fromTexts
  , fromIO
  , HasWybor(..)
  , TTYException(..)
#ifdef TEST
  , pipeline
  , keyEnter
  , keyBksp
  , keyCtrl
#endif
  ) where

import           Control.Exception (try)
import           Control.Lens hiding (lined)
import           Control.Monad
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Trans.Resource (MonadResource, runResourceT)
import           Data.Conduit (Source, Conduit, (=$=), ($$))
import qualified Data.Conduit as C
import           Data.Char (isPrint, isSpace, chr, ord)
import           Data.Data (Typeable, Data)
import           Data.Foldable (Foldable, toList)
import           Data.Function (on)
import           Data.List (sortBy)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Monoid (Monoid(..), (<>))
import           Data.Sequence (Seq, (><))
import qualified Data.Sequence as Seq
import           Data.Sequence.Lens (seqOf)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Prelude hiding (unlines)
import           System.Console.ANSI as Ansi

import           Score (score, Input(..), Choice(..))
import qualified Score
import           TTY (TTY, TTYException)
import qualified TTY
import           Zipper (Zipper, focus, zipperN)
import qualified Zipper


-- | Select an item from 'Wybor' once
--
-- The user can interrupt the process with @C-d@ and then you get 'Nothing'.
-- Exceptions result in @'Left' _@
select :: Wybor a -> IO (Either TTYException (Maybe a))
select c = try . runResourceT $ selections c $$ C.await

-- | Continuously select items from 'Wybor'
--
-- Exceptions (see 'TTYException') aren't caught
selections :: MonadResource m => Wybor a -> Source m a
selections = TTY.withTTY . pipeline

-- | The description of the alternative choices, see 'HasWybor'
data Wybor a = Wybor
  { _alts :: Alternatives (NonEmpty (Text, a))
  , _conf :: Conf Text
  } deriving (
    Functor
#if __GLASGOW_HASKELL__ >= 708
  , Typeable
#endif
  )

alts :: Lens (Wybor a) (Wybor b) (Alternatives (NonEmpty (Text, a))) (Alternatives (NonEmpty (Text, b)))
alts f x = f (_alts x) <&> \y -> x { _alts = y }
{-# INLINE alts #-}

conf :: Lens' (Wybor a) (Conf Text)
conf f x = f (_conf x) <&> \y -> x { _conf = y }
{-# INLINE conf #-}

data Alternatives a
  = Static a
  | Dynamic (IO (Maybe (Maybe a)))

instance Functor Alternatives where
  fmap f (Static a) = Static (f a)
  fmap f (Dynamic k) = Dynamic (fmap (fmap (fmap f)) k)

data Conf a = Conf
  { _visible, _height :: Int
  , _initial, _prefix :: a
  } deriving (Show, Eq, Typeable, Data, Functor)

-- | A bunch of lenses to pick and configure Wybor
class HasWybor t a | t -> a where
  wybor :: Lens' t (Wybor a)

  -- | How many alternative choices to show at once?
  visible :: Lens' t Int
  visible = wybor.conf. \f x -> f (_visible x) <&> \y -> x { _visible = y }
  {-# INLINE visible #-}

  -- | How many lines every alternative takes on the screen?
  height :: Lens' t Int
  height = wybor.conf. \f x -> f (_height x) <&> \y -> x { _height = y }
  {-# INLINE height #-}

  -- | Initial search string
  initial :: Lens' t Text
  initial = wybor.conf. \f x -> f (_initial x) <&> \y -> x { _initial = y }
  {-# INLINE initial #-}

  -- | Prompt prefix
  prefix :: Lens' t Text
  prefix = wybor.conf. \f x -> f (_prefix x) <&> \y -> x { _prefix = y }
  {-# INLINE prefix #-}

instance HasWybor (Wybor a) a where
  wybor = id
  {-# INLINE wybor #-}

-- | Construct 'Wybor' from the nonempty list of strings
--
-- The strings are used both as keys and values
fromTexts :: NonEmpty Text -> Wybor Text
fromTexts = fromAssoc . fmap (\x -> (x, x))

-- | Construct 'Wybor' from the nonempty list of key-value pairs
fromAssoc :: NonEmpty (Text, a) -> Wybor a
fromAssoc = fromAlternatives . Static

-- | Construct 'Wybor' from the 'IO' action that streams choices
--
-- It's useful when the list of alternatives is populated over
-- time from multiple sources (for instance, from HTTP responses)
--
-- The interface is tailored for the use with closeable queues from the "stm-chans" package:
--
-- >>> q <- newTMQueueIO
-- >>> ... {- a bunch of threads populating and eventually closing the queue -}
-- >>> c <- 'select' ('fromIO' (atomically (tryReadTMQueue q)))
-- >>> print c
--
-- That is, if the 'IO' action returns @'Nothing'@ the queue will never be read from again
-- and it can return @'Just' 'Nothing'@ when there's nothing to add to the choices __yet__
--
-- It's still possible to use non-fancy queues:
--
-- >>> q <- newTQueueIO
-- >>> ... {- a bunch of threads populating the queue -}
-- >>> c <- 'select' ('fromIO' (fmap Just (atomically (tryReadTQueue q))))
-- >>> print c
--
-- If choices are static, you will be served better by 'fromAssoc' and 'fromTexts'
fromIO :: IO (Maybe (Maybe (NonEmpty (Text, a)))) -> Wybor a
fromIO = fromAlternatives . Dynamic

fromAlternatives :: Alternatives (NonEmpty (Text, a)) -> Wybor a
fromAlternatives xs = Wybor
  { _alts = xs
  , _conf = defaultConf
  }

defaultConf :: Conf Text
defaultConf = Conf
  { _visible = 10
  , _height  = 1
  , _prefix  = ">>> "
  , _initial = ""
  }

pipeline :: MonadIO m => Wybor a -> TTY -> Source m a
pipeline w tty = do
  pos <- prerenderUI w tty
  sourceInput (view alts w) tty =$= renderUI tty pos w

sourceInput :: MonadIO m => Alternatives (NonEmpty (Text, a)) -> TTY -> Source m (Event a)
sourceInput (Static p)   tty = do yieldChoices p; loop where loop = keyEvent tty loop
sourceInput (Dynamic io) tty = interleaving
 where
  interleaving = liftIO io >>= \case
    Nothing       -> let loop = keyEvent tty loop in loop
    Just Nothing  -> keyEvent tty interleaving
    Just (Just p) -> do yieldChoices p; keyEvent tty interleaving

keyEvent :: MonadIO m => TTY -> Source m (Event a) -> Source m (Event a)
keyEvent tty c = TTY.getCharNonBlocking tty >>= \case
  Nothing -> c
  Just k  -> case parseChar k of
    Nothing       -> return ()
    Just Nothing  ->                     c
    Just (Just e) -> do yieldKeyEvent e; c

yieldChoices :: MonadIO m => NonEmpty (Text, a) -> Source m (Event a)
yieldChoices = C.yield . Left . AppendChoices

yieldKeyEvent :: MonadIO m => KeyEvent -> Source m (Event a)
yieldKeyEvent = C.yield . Right

type Event a = Either (GenEvent a) KeyEvent

newtype GenEvent a = AppendChoices (NonEmpty (Text, a)) deriving (Show, Eq, Functor)

data KeyEvent =
    Done
  | Abort
  | Down
  | Up
  | Clear
  | DeleteWord
  | DeleteChar
  | AppendChar Char
    deriving (Show, Eq)

parseChar :: Char -> Maybe (Maybe KeyEvent)
parseChar c
  | c == keyEnter    = Just (Just Done)
  | c == keyCtrl 'D' = Nothing
  | c == keyCtrl 'N' = Just (Just Down)
  | c == keyCtrl 'P' = Just (Just Up)
  | c == keyCtrl 'U' = Just (Just Clear)
  | c == keyCtrl 'W' = Just (Just DeleteWord)
  | c == keyBksp     = Just (Just DeleteChar)
  | c == keyCtrl 'H' = Just (Just DeleteChar)
  | isPrint c        = Just (Just (AppendChar c))
  | otherwise        = Just Nothing

keyEnter, keyBksp :: Char
keyEnter = '\n'
keyBksp  = '\DEL'

-- | @keyCtrl c@ converts @c@ to Ctrl-@c@ character. Only expected to work
-- with upper case ASCII characters from @A-Z@ range.
keyCtrl :: Char -> Char
keyCtrl a = chr (ord a - 64)

newtype Choices a = Choices
  { unChoices :: Seq (Text, a)
  }

choices :: Iso (Choices a) (Choices b) (Seq (Text, a)) (Seq (Text, b))
choices = iso unChoices Choices

data Query a b = Query
  { _matches :: Maybe (Zipper (a, b))
  , _input   :: a
  } deriving (Show, Eq)

matches :: Lens' (Query a b) (Maybe (Zipper (a, b)))
matches f x = f (_matches x) <&> \y -> x { _matches = y }

input :: Lens' (Query a b) a
input f x = f (_input x) <&> \y -> x { _input = y }

handleEvent :: Choices a -> Query Text a -> KeyEvent -> Maybe (Either a (Query Text a))
handleEvent c s = \case
  Done         -> Just (maybe (Right s) Left (done s))
  Abort        -> Nothing
  Down         -> Just (Right (down s))
  Up           -> Just (Right (up s))
  Clear        -> Just (Right (clear c))
  DeleteChar   -> Just (Right (deleteChar c s))
  DeleteWord   -> Just (Right (deleteWord c s))
  AppendChar x -> Just (Right (append s x))

done :: Query a b -> Maybe b
done = preview (matches.traverse.focus._2)

down :: Query a b -> Query a b
down = over (matches.traverse) Zipper.right

up :: Query a b -> Query a b
up = over (matches.traverse) Zipper.left

clear :: Choices a -> Query Text a
clear c = fromInput c mempty

append :: Query Text a -> Char -> Query Text a
append s x = fromInput (Choices (seqOf (matches.folded.folded) s)) (view input s |> x)

deleteChar :: Choices a -> Query Text a -> Query Text a
deleteChar c = maybe (clear c) (fromInput c . fst) . unsnoc . view input

deleteWord :: Choices a -> Query Text a -> Query Text a
deleteWord c = fromInput c . view (input.to (Text.dropWhileEnd (not . isSpace) . Text.stripEnd))

fromInput :: Choices a -> Text -> Query Text a
fromInput c q = Query { _matches = view (choices.to (computeMatches q)) c, _input = q }

fromNothing :: Wybor a -> Query Text b
fromNothing c = Query { _matches = Nothing, _input = view initial c }

computeMatches :: Foldable f => Text -> f (Text, a) -> Maybe (Zipper (Text, a))
computeMatches "" = Zipper.fromList . toList
computeMatches q  = Zipper.fromList . sortOnByOf choiceScore (flip compare) Score.positive . toList
 where
  choiceScore = score (Input q) . Choice . fst

sortOnByOf :: (Foldable f, Ord b) => (a -> b) -> (b -> b -> Ordering) -> (b -> Bool) -> f a -> [a]
sortOnByOf f c p = map fst . sortBy (c `on` snd) . filter (p . snd) . map (\x -> (x, f x)) . toList


prerenderUI :: MonadIO m => Wybor a -> TTY -> m Int
prerenderUI c tty = do
  row <- TTY.getCursorRow tty
  let offset = max 0 (linesTaken c - (TTY.winHeight tty - row))
  replicateM_ offset (TTY.putLine tty)
  return (row - offset)

renderUI :: MonadIO m => TTY -> Int -> Wybor a -> Conduit (Event b) m b
renderUI tty p c = rendering (Choices Seq.empty) (fromNothing c)
 where
  rendering cs s = renderQuery tty c p s >> C.await >>= \case
    Nothing ->
      cleanUp
    Just (Left (AppendChoices xs)) ->
      let
        f   = NonEmpty.filter (\x -> Score.positive (score (Input (view input s)) (Choice (fst x))))
        cs' = over choices (>< Seq.fromList (toList xs)) cs
        s'  = over matches (maybe (Zipper.fromList (f xs)) (Just . Zipper.append (f xs))) s
      in
        rendering cs' s'
    Just (Right e) -> case handleEvent cs s e of
      Nothing         -> cleanUp
      Just (Left x)   -> do cleanUp; C.yield x; rendering cs s
      Just (Right s') -> rendering cs s'

  cleanUp = TTY.clearScreenBottom tty

renderQuery :: MonadIO m => TTY -> Wybor a -> Int -> Query Text b -> m ()
renderQuery tty c top s =
  liftIO . TTY.withHiddenCursor tty $ renderContent tty top (columnsTaken c s) (content tty c s)

renderContent :: TTY -> Int -> Int -> Text -> IO ()
renderContent tty x y t = do
  TTY.moveCursor tty x 0
  TTY.putText tty t
  TTY.moveCursor tty x y

content :: TTY -> Wybor a -> Query Text b -> Text
content tty c = review lined . map (text . unline . clean . highlight tty . expand tty c . line) . items c

data Item s = Plain s | Chosen s | Prefix s deriving (Functor)

item :: (s -> a) -> (s -> a) -> (s -> a) -> Item s -> a
item f _ _ (Plain s)  = f s
item _ g _ (Chosen s) = g s
item _ _ h (Prefix s) = h s

text :: Item s -> s
text = item id id id

lined :: Iso' Text [Text]
lined = iso Text.lines (Text.intercalate "\n")

line :: Item Text -> Item [Text]
line = fmap (view lined)

expand :: TTY -> Wybor a -> Item [Text] -> Item [Text]
expand tty c = \case
  Plain  xs -> Plain  (compose h w xs)
  Chosen xs -> Chosen (compose h w xs)
  Prefix xs -> Prefix (compose 1 w xs)
 where
  compose x y xs = (take x (map (Text.take y) xs ++ repeat ""))
  w = TTY.winWidth tty
  h = view height c

clean :: Item [Text] -> Item [Text]
clean = fmap (map (\l -> l <> Text.pack Ansi.clearFromCursorToLineEndCode))

highlight :: TTY -> Item [Text] -> Item [Text]
highlight _   xs@(Plain _) = xs
highlight tty (Chosen xs) =
  Chosen (map (\x -> swap True <> Text.justifyLeft (TTY.winWidth tty) ' ' x <> swap False) xs)
highlight _   xs@(Prefix _) = xs

swap :: Bool -> Text
swap b = Text.pack (Ansi.setSGRCode [Ansi.SetSwapForegroundBackground b])

unline :: Item [Text] -> Item Text
unline = fmap (review lined)

items :: Wybor a -> Query Text b -> [Item Text]
items c s = take (view visible c + 1) $
     Prefix (view prefix c <> view input s)
  :  maybe [] (zipperN (view visible c) combine . fmap fst) (preview (matches.traverse) s)
  ++ repeat (Plain "")
 where
  combine xs y zs = map Plain xs ++ [Chosen y] ++ map Plain zs

linesTaken :: Wybor a -> Int
linesTaken c = view visible c * view height c + 1

columnsTaken :: Wybor a -> Query Text b -> Int
columnsTaken c s = lengthOf (beside (prefix.each) (input.each)) (c, s)
