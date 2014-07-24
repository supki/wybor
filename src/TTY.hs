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
  , putText
  , putTextLine
  , putLine
  , clearScreenBottom
  , withHiddenCursor
  , getCursorRow
  , moveCursor
  ) where

import           Control.Exception (Exception(..), SomeException(..), IOException)
import qualified Control.Exception as E
import           Control.Monad
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Trans.Resource (MonadResource)
import           Data.Char (isDigit)
import           Data.Conduit (Conduit)
import qualified Data.Conduit as C
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Typeable (Typeable, cast)
import           Prelude hiding (getChar)
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

withTTY :: MonadResource m => (TTY -> Conduit i m o) -> Conduit i m o
withTTY f =
  withFile ttyDevice IO.ReadMode  $ \inHandle  ->
  withFile ttyDevice IO.WriteMode $ \outHandle -> do
    setBuffering outHandle IO.NoBuffering
    withConfiguredTTY $ do
      mw <- hWindow outHandle
      case mw of
        Nothing -> liftIO (E.throwIO TTYNotATTY)
        Just w  -> f TTY { inHandle, outHandle, winHeight = height w, winWidth = width w }

withFile :: MonadResource m => FilePath -> IO.IOMode -> (IO.Handle -> Conduit i m o) -> Conduit i m o
withFile name mode = C.bracketP (IO.openFile name mode) IO.hClose

withConfiguredTTY :: MonadResource m => Conduit i m o -> Conduit i m o
withConfiguredTTY = C.bracketP (do s <- state; configure; return s) unconfigure . const

setBuffering :: MonadIO m => IO.Handle -> IO.BufferMode -> m ()
setBuffering h m = liftIO (IO.hSetBuffering h m)

hWindow :: (Integral n, MonadIO m) => IO.Handle -> m (Maybe (Window n))
hWindow = liftIO . hSize

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

putText :: MonadIO m => TTY -> Text -> m ()
putText TTY { outHandle } = liftIO . Text.hPutStr outHandle

putTextLine :: MonadIO m => TTY -> Text -> m ()
putTextLine TTY { outHandle } = liftIO . Text.hPutStrLn outHandle

putLine :: MonadIO m => TTY -> m ()
putLine TTY { outHandle } = liftIO (Text.hPutStrLn outHandle Text.empty)

clearScreenBottom :: MonadIO m => TTY -> m ()
clearScreenBottom TTY { outHandle } = liftIO $ do
  Ansi.hSetCursorColumn outHandle 0
  Ansi.hClearFromCursorToScreenEnd outHandle

withHiddenCursor :: TTY -> IO a -> IO a
withHiddenCursor TTY { outHandle = h } = E.bracket_ (Ansi.hHideCursor h) (Ansi.hShowCursor h)

getCursorRow :: MonadIO m => TTY -> m Int
getCursorRow tty = do
  putText tty magicCursorPositionSequence
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

ttyDevice, nullDevice :: FilePath
ttyDevice  = "/dev/tty"
nullDevice = "/dev/null"
