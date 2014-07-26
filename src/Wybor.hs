{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
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
  , fromSource
  , fromAssoc
  , fromTexts
  , HasWybor(..)
  , TTYException(..)
#ifdef TEST
  , pipeline
  , keyEnter
  , keyBksp
  , keyCtrl
#endif
  ) where

import           Control.Concurrent.Async (Async, async, cancel)
import           Control.Concurrent.STM.TQueue (TQueue, newTQueueIO, tryReadTQueue)
import           Control.Exception (try)
import           Control.Lens hiding (lined)
import           Control.Monad
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.STM (atomically)
import           Control.Monad.Trans.Control (MonadBaseControl(..), liftBaseWith)
import           Control.Monad.Trans.Resource (MonadResource, ResourceT, runResourceT)
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Data.Conduit (Source, Conduit, (=$=), ($$))
import qualified Data.Conduit as C
import qualified Data.Conduit.TQueue as C
import           Data.Char (isPrint, isSpace, chr, ord)
import           Data.Data (Typeable, Data)
import           Data.Foldable (toList)
import           Data.Function (on)
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


-- | Select an item from 'Wybor' once
--
-- The user can interrupt the process with @C-d@ and then you get 'Nothing'.
-- Exceptions result in @'Left' _@
select :: Wybor (ResourceT IO) a -> IO (Either TTYException (Maybe a))
select c = try . runResourceT $ selections c $$ C.await

-- | Continuously select items from 'Wybor'
--
-- Exceptions (see 'TTYException') aren't caught
selections :: (MonadResource m, MonadBaseControl IO m) => Wybor m a -> Source m a
selections = TTY.withTTY . pipeline


-- | The description of the alternative choices, see 'HasWybor'
data Wybor m a = Wybor
  { _alts :: Source m (NonEmpty (Text, a))
  , _conf :: Conf Text
#if __GLASGOW_HASKELL__ >= 708
  } deriving (Typeable)
#else
  }
#endif

instance Monad m => Functor (Wybor m) where
  fmap f = over alts (C.mapOutput (fmap (fmap f)))

alts :: Lens (Wybor m a) (Wybor n b) (Source m (NonEmpty (Text, a))) (Source n (NonEmpty (Text, b)))
alts f x = f (_alts x) <&> \y -> x { _alts = y }
{-# INLINE alts #-}

conf :: Lens' (Wybor m a) (Conf Text)
conf f x = f (_conf x) <&> \y -> x { _conf = y }
{-# INLINE conf #-}

data Conf a = Conf
  { __visible, __height :: Int
  , __initial, __prefix :: a
  } deriving (Show, Eq, Typeable, Data, Functor)

-- | A bunch of lenses to pick and configure Wybor
class HasWybor t m a | t -> m a where
  wybor :: Lens' t (Wybor m a)

  -- | How many alternative choices to show at once?
  visible :: Lens' t Int
  visible = wybor.conf._visible
  {-# INLINE visible #-}

  -- | How many lines every alternative takes on the screen?
  height :: Lens' t Int
  height = wybor.conf._height
  {-# INLINE height #-}

  -- | Initial search string
  initial :: Lens' t Text
  initial = wybor.conf._initial
  {-# INLINE initial #-}

  -- | Prompt prefix
  prefix :: Lens' t Text
  prefix = wybor.conf._prefix
  {-# INLINE prefix #-}

instance HasWybor (Wybor m a) m a where
  wybor = id
  {-# INLINE wybor #-}

_visible :: Lens' (Conf a) Int
_visible f x = f (__visible x) <&> \y -> x { __visible = y }
{-# INLINE _visible #-}

_height :: Lens' (Conf a) Int
_height f x = f (__height x) <&> \y -> x { __height = y }
{-# INLINE _height #-}

_initial :: Lens' (Conf a) a
_initial f x = f (__initial x) <&> \y -> x { __initial = y }
{-# INLINE _initial #-}

_prefix :: Lens' (Conf a) a
_prefix f x = f (__prefix x) <&> \y -> x { __prefix = y }
{-# INLINE _prefix #-}

-- | Construct 'Wybor' from the nonempty list of strings
--
-- The strings are used both as keys and values
fromTexts :: Monad m => NonEmpty Text -> Wybor m Text
fromTexts = fromAssoc . fmap (\x -> (x, x))

-- | Construct 'Wybor' from the nonempty list of key-value pairs
fromAssoc :: Monad m => NonEmpty (Text, a) -> Wybor m a
fromAssoc = fromSource . C.yield

-- | Construct 'Wybor' from the 'Source'
--
-- It's useful when the list of alternatives is populated over
-- time from multiple sources e.g. from HTTP responses
--
-- If choices are static, you will be served better by 'fromAssoc' and 'fromTexts'
fromSource :: Source m (NonEmpty (Text, a)) -> Wybor m a
fromSource xs = Wybor
  { _alts = xs
  , _conf = defaultConf
  }

defaultConf :: Conf Text
defaultConf = Conf
  { __visible = 10
  , __height = 1
  , __prefix = ">>> "
  , __initial = ""
  }

pipeline :: (MonadIO m, MonadBaseControl IO m) => Wybor m a -> TTY -> Source m a
pipeline w tty = do
  cue <- liftIO newTQueueIO
  pos <- prerenderUI (view conf w) tty
  ins <- lift . async' $ view alts w $$ C.sinkTQueue cue
  sourceInput cue tty =$= renderUI (liftIO (cancel ins)) tty pos (view conf w)

async' :: MonadBaseControl IO m => m a -> m (Async (StM m a))
async' m = liftBaseWith $ \run -> async (run m)

sourceInput :: MonadIO m => TQueue (NonEmpty (Text, a)) -> TTY -> Source m (Either (GenEvent a) KeyEvent)
sourceInput cue tty = interleaving
 where
  interleaving = tryReadTQueueIO cue >>= \case
    Nothing -> keyEvent
    Just p  -> do C.yield (Left (AppendChoices p)); keyEvent

  keyEvent = TTY.getCharNonBlocking tty >>= \case
    Nothing -> interleaving
    Just k  -> case parseChar k of
      Nothing       -> return ()
      Just Nothing  ->                       interleaving
      Just (Just e) -> do C.yield (Right e); interleaving

tryReadTQueueIO :: MonadIO m => TQueue a -> m (Maybe a)
tryReadTQueueIO = liftIO . atomically . tryReadTQueue

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
  { unChoices :: [(Text, a)]
  }

choices :: Iso (Choices a) (Choices b) [(Text, a)] [(Text, b)]
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
  AppendChar x -> Just (Right (append c s x))

done :: Query a b -> Maybe b
done = preview (matches.traverse.focus._2)

down :: Query a b -> Query a b
down = over (matches.traverse) Zipper.right

up :: Query a b -> Query a b
up = over (matches.traverse) Zipper.left

clear :: Choices a -> Query Text a
clear c = fromInput c mempty

append :: Choices a -> Query Text a -> Char -> Query Text a
append c s x = fromInput c (view input s |> x)

deleteChar :: Choices a -> Query Text a -> Query Text a
deleteChar c = maybe (clear c) (fromInput c . fst) . unsnoc . view input

deleteWord :: Choices a -> Query Text a -> Query Text a
deleteWord c = fromInput c . view (input.to (Text.dropWhileEnd (not . isSpace) . Text.stripEnd))

fromInput :: Choices a -> Text -> Query Text a
fromInput c q = Query { _matches = view (choices.to (computeMatches q)) c, _input = q }

fromNothing :: Conf Text -> Query Text a
fromNothing c = Query { _matches = Nothing, _input = view _initial c }

computeMatches :: Text -> [(Text, a)] -> Maybe (Zipper (Text, a))
computeMatches "" = Zipper.fromList
computeMatches q  = Zipper.fromList . sortOnByOf choiceScore (flip compare) positive . toList
 where
  choiceScore = score (Input q) . Choice . fst
  positive s  = s > minBound

sortOnByOf :: Ord b => (a -> b) -> (b -> b -> Ordering) -> (b -> Bool) -> [a] -> [a]
sortOnByOf f c p = map fst . sortBy (c `on` snd) . filter (p . snd) . map (\x -> (x, f x))


prerenderUI :: MonadIO m => Conf a -> TTY -> m Int
prerenderUI c tty = do
  row <- TTY.getCursorRow tty
  let offset = max 0 (linesTaken c - (TTY.winHeight tty - row))
  replicateM_ offset (TTY.putLine tty)
  return (row - offset)

renderUI
  :: MonadIO m
  => (forall n. MonadIO n => n ()) -> TTY -> Int -> Conf Text -> Conduit (Either (GenEvent a) KeyEvent) m a
renderUI stopInput tty p c = rendering (Choices []) (fromNothing c)
 where
  rendering cs s = renderQuery tty c p s >> C.await >>= \case
    Nothing ->
      cleanUp
    Just (Left (AppendChoices xs)) ->
      let cs' = over choices (++ toList xs) cs in rendering cs' (fromInput cs' (view input s))
    Just (Right e) -> case handleEvent cs s e of
      Nothing ->
        do cleanUp; stopInput
      Just (Left x) ->
        do cleanUp; C.yieldOr x stopInput; rendering cs s
      Just (Right s') ->
        rendering cs s'

  cleanUp = TTY.clearScreenBottom tty

renderQuery :: MonadIO m => TTY -> Conf Text -> Int -> Query Text a -> m ()
renderQuery tty c top s =
  liftIO . TTY.withHiddenCursor tty $ renderContent tty top (columnsTaken c s) (content tty c s)

renderContent :: TTY -> Int -> Int -> Text -> IO ()
renderContent tty x y t = do
  TTY.moveCursor tty x 0
  TTY.putText tty t
  TTY.moveCursor tty x y

content :: TTY -> Conf Text -> Query Text b -> Text
content tty c s =
  review lined . map (text . unline . clean . highlight tty . expand tty c . line) $ items c s

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

expand :: TTY -> Conf a -> Item [Text] -> Item [Text]
expand tty c = \case
  Plain  xs -> Plain  (take h (map (Text.take w) xs ++ repeat ""))
  Chosen xs -> Chosen (take h (map (Text.take w) xs ++ repeat ""))
  Prefix xs -> Prefix (take 1 xs)
 where
  w = TTY.winWidth tty
  h = view _height c

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

items :: Conf Text -> Query Text b -> [Item Text]
items c s = take (view _visible c + 1) $
     Prefix (view _prefix c <> view input s)
  :  maybe [] (zipperN (view _visible c) combine . fmap fst) (preview (matches.traverse) s)
  ++ repeat (Plain "")
 where
  combine xs y zs = map Plain xs ++ [Chosen y] ++ map Plain zs

linesTaken :: Conf a -> Int
linesTaken c = view _visible c * view _height c + 1

columnsTaken :: Conf Text -> Query Text a -> Int
columnsTaken c s = lengthOf (_prefix.each) c + lengthOf (input.each) s
