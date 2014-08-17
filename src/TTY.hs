{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
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
  , Key(..)
  , getKey
  , putText
  , putTextLine
  , putLine
  , clearScreenBottom
  , withHiddenCursor
  , getCursorRow
  , moveCursor
  ) where

import           Control.Exception (Exception(..), IOException)
import qualified Control.Exception as E
import           Control.Monad
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Trans.Resource (MonadResource)
import           Data.Char (isPrint, isDigit, chr, ord)
import           Data.Conduit (Conduit)
import qualified Data.Conduit as C
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Typeable (Typeable)
import           Prelude hiding (getChar)
import           System.Console.ANSI as Ansi
import           System.Console.Terminal.Size (Window(..), hSize)
import qualified System.IO as IO
import qualified System.IO.Error as IO
import qualified System.Posix as Posix
import           System.Timeout (timeout)
import           Text.Read (readMaybe)


data TTY = TTY
  { inHandle, outHandle :: IO.Handle
  , winHeight, winWidth :: Int
  } deriving (Show, Eq)

-- | Exceptions thrown while manipulating @\/dev\/tty@ device
newtype TTYException = TTYIOException IOException deriving (Show, Eq, Typeable, Exception)

withTTY :: MonadResource m => (TTY -> Conduit i m o) -> Conduit i m o
withTTY f =
  withFile ttyDevice IO.ReadMode  $ \inHandle  ->
  withFile ttyDevice IO.WriteMode $ \outHandle -> do
    setBuffering outHandle IO.NoBuffering
    withConfiguredTTY $ do
      mw <- hWindow outHandle
      case mw of
        Nothing ->
          liftIO (ioError (notATTY outHandle))
        Just Window { height, width } ->
          f TTY { inHandle, outHandle, winHeight = height, winWidth = width }

withFile :: MonadResource m => FilePath -> IO.IOMode -> (IO.Handle -> Conduit i m o) -> Conduit i m o
withFile name mode = C.bracketP (IO.openFile name mode) IO.hClose

withConfiguredTTY :: MonadResource m => Conduit i m o -> Conduit i m o
withConfiguredTTY = C.bracketP
  (withFd ttyDevice Posix.ReadOnly $ \fd -> do s <- getAttrs fd; configure fd s; return s)
  (\as -> withFd ttyDevice Posix.ReadOnly (\fd -> setAttrs fd as)) . const

withFd :: FilePath -> Posix.OpenMode -> (Posix.Fd -> IO a) -> IO a
withFd name mode = E.bracket (Posix.openFd name mode Nothing Posix.defaultFileFlags) Posix.closeFd

setBuffering :: MonadIO m => IO.Handle -> IO.BufferMode -> m ()
setBuffering h m = liftIO (IO.hSetBuffering h m)

hWindow :: (Integral n, MonadIO m) => IO.Handle -> m (Maybe (Window n))
hWindow = liftIO . hSize

getAttrs :: Posix.Fd -> IO Posix.TerminalAttributes
getAttrs = Posix.getTerminalAttributes

configure :: Posix.Fd -> Posix.TerminalAttributes -> IO ()
configure fd as = setAttrs fd (withoutModes as [Posix.EnableEcho, Posix.ProcessInput])

withoutModes :: Posix.TerminalAttributes -> [Posix.TerminalMode] -> Posix.TerminalAttributes
withoutModes = foldr (flip Posix.withoutMode)

setAttrs :: Posix.Fd -> Posix.TerminalAttributes -> IO ()
setAttrs fd as = Posix.setTerminalAttributes fd as Posix.Immediately

data Key =
    Print Char
  | Ctrl Char -- invariant: this character is in ['A'..'Z'] range
  | Bksp
  | ArrowUp
  | ArrowDown
  | ArrowLeft
  | ArrowRight
    deriving (Show, Eq)

getKey :: MonadIO m => TTY -> m (Maybe Key)
getKey tty = liftIO . fmap join . timeout 100000 $
  getChar tty >>= \case
    '\DEL' -> return (Just Bksp)
    '\ESC' -> getChar tty >>= \case
      '[' -> getChar tty >>= \case
        'A' -> return (Just ArrowUp)
        'B' -> return (Just ArrowDown)
        'C' -> return (Just ArrowRight)
        'D' -> return (Just ArrowLeft)
        _   -> return Nothing
      _ -> return Nothing
    c | c `elem` ['\SOH'..'\SUB'] -> return (Just (Ctrl (chr (ord c + 64))))
      | isPrint c -> return (Just (Print c))
      | otherwise -> return Nothing

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
    Nothing -> liftIO (ioError (notATTY (inHandle tty)))
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

notATTY :: IO.Handle -> IOException
notATTY h = IO.mkIOError IO.illegalOperationErrorType "Not a TTY" (Just h) Nothing

ttyDevice, nullDevice :: FilePath
ttyDevice  = "/dev/tty"
nullDevice = "/dev/null"
