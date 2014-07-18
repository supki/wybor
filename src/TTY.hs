{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
module TTY
#ifdef TEST
  ( TTY(..)
  , nullDevice
#else
  ( TTY
  , winHeight
  , winWidth
#endif
  , TTYException(..)
  , withTTY
  , getChar
  , putStr
  , putStrLn
  , putLn
  , clearLn
  , withHiddenCursor
  , getCursorRow
  , moveCursor
  , rewindCursor
  ) where

import           Control.Exception (Exception(..), SomeException(..), IOException)
import qualified Control.Exception as E
import           Control.Monad
import           Control.Monad.IO.Class (MonadIO(..))
import           Data.Char (isDigit)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Typeable (Typeable, cast)
import           Prelude hiding (getChar, putStr, putStrLn)
import           System.Console.ANSI as Ansi
import           System.Console.Terminal.Size (Window(..), hSize)
import           System.Exit (ExitCode(..))
import           System.Process
import qualified System.IO as IO
import qualified System.IO.Error as IO
import           Text.Read (readMaybe)


data TTY = TTY
  { inHandle, outHandle :: IO.Handle
  , winHeight, winWidth :: Int
  } deriving (Show, Eq)

-- | Exceptions thrown while manipulating @\/dev\/tty@ device
data TTYException =
    -- | Generic I/O exception
    TTYIOException IOException
    -- | Not-a-TTY responded with nonsense to some query
  | TTYNotATTY
    -- | Failure to configure TTY with @stty@
  | TTYConfigurationError FilePath [String] ExitCode
    deriving (Show, Eq, Typeable)

instance Exception TTYException where
  fromException e@(SomeException se)
    | Just e' <- fromException e = Just (TTYIOException e')
    | otherwise                  = cast se

withTTY :: (TTY -> IO a) -> IO (Either TTYException a)
withTTY f = E.try $
  IO.withFile ttyDevice IO.ReadMode  $ \inHandle  ->
  IO.withFile ttyDevice IO.WriteMode $ \outHandle -> do
    IO.hSetBuffering outHandle IO.NoBuffering
    withConfiguredTTY $ do
      mw <- hSize outHandle
      case mw of
        Nothing -> E.throwIO TTYNotATTY
        Just w  -> f TTY { inHandle, outHandle, winHeight = height w, winWidth = width w }

withConfiguredTTY :: IO a -> IO a
withConfiguredTTY = E.bracket (do s <- state; configure; return s) unconfigure . const

newtype TTYState = TTYState String

state :: IO TTYState
state = fmap TTYState (stty ["-g"])

configure :: IO ()
configure = void (stty ["-echo", "-icanon"])

unconfigure :: TTYState -> IO ()
unconfigure (TTYState s) = void (stty [s])

stty :: [String] -> IO String
stty = run "stty"

run :: FilePath -> [String] -> IO String
run p args =
  IO.withFile ttyDevice IO.ReadMode   $ \inh ->
  IO.withFile nullDevice IO.WriteMode $ \errh -> do
    (_, Just h', _, h) <- createProcess (proc p args)
      { std_in  = UseHandle inh
      , std_out = CreatePipe
      , std_err = UseHandle errh
      }
    ec <- waitForProcess h
    case ec of
      ExitFailure _ -> E.throwIO (TTYConfigurationError p args ec)
      ExitSuccess   -> IO.hGetLine h' `IO.catchIOError` \_ -> return ""

getChar :: MonadIO m => TTY -> m Char
getChar = liftIO . IO.hGetChar . inHandle

putStr :: MonadIO m => TTY -> Text -> m ()
putStr TTY { outHandle } = liftIO . Text.hPutStr outHandle

putStrLn :: MonadIO m => TTY -> Text -> m ()
putStrLn TTY { outHandle } = liftIO . Text.hPutStrLn outHandle

putLn :: MonadIO m => TTY -> m ()
putLn TTY { outHandle } = liftIO (Text.hPutStrLn outHandle Text.empty)

clearLn :: MonadIO m => TTY -> m ()
clearLn TTY { outHandle } = liftIO $ do
  Ansi.hSetCursorColumn outHandle 0
  Ansi.hClearFromCursorToLineEnd outHandle

withHiddenCursor :: TTY -> IO a -> IO a
withHiddenCursor TTY { outHandle = h } = E.bracket_ (Ansi.hHideCursor h) (Ansi.hShowCursor h)

getCursorRow :: MonadIO m => TTY -> m Int
getCursorRow tty = do
  putStr tty magicCursorPositionSequence
  res <- parseAnsiResponse tty
  case res of
    Just r  -> return (r - 1) -- the response is 1-based
    Nothing -> liftIO (E.throwIO TTYNotATTY)
 where
  magicCursorPositionSequence = Text.pack "\ESC[6n"

parseAnsiResponse :: (MonadIO m, Read a) => TTY -> m (Maybe a)
parseAnsiResponse tty = liftM parse (go [])
 where
  go acc = do
    c <- getChar tty
    if c == 'R' then return (reverse acc) else go (c : acc)

  parse ('\ESC' : '[' : xs) = readMaybe (takeWhile isDigit xs)
  parse _                   = Nothing

moveCursor :: TTY -> Int -> Int -> IO ()
moveCursor = Ansi.hSetCursorPosition . outHandle

rewindCursor :: TTY -> IO ()
rewindCursor TTY { outHandle } = Ansi.hSetCursorColumn outHandle 0

ttyDevice, nullDevice :: FilePath
ttyDevice  = "/dev/tty"
nullDevice = "/dev/null"
