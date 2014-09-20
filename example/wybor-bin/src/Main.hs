{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
module Main (main) where

import           Control.Applicative
import           Control.Exception (catch)
import           Control.Lens
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Trans.Resource (runResourceT)
import           Data.List.NonEmpty (NonEmpty, nonEmpty)
import           Data.Conduit (Conduit, ($$))
import qualified Data.Conduit as C
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Version (showVersion)
import           Options.Applicative
import           Prelude hiding (init, lines)
import           System.Exit (exitFailure)
import qualified System.IO as IO
import           Text.Printf (printf)

import           Wybor (TTYException, selections, fromTexts, HasWybor(..))
import           Paths_wybor_bin (version)


main :: IO ()
main = do
  opts <- options
  ins  <- inputs
  output opts ins

data Options = Options
  { _visible, _height :: Maybe Int
  , _query, _prompt   :: Maybe Text
  , _mode             :: Mode
  , _endWith          :: EndWith
  }

newtype Mode = Mode (forall i m o. MonadIO m => (i -> Conduit i m o) -> Conduit i m o)

newtype EndWith = EndWith (forall m. MonadIO m => Text -> m ())

options :: IO Options
options = customExecParser (prefs showHelpOnError) parser
 where
  parser = info (helper <*> go)
    (fullDesc <> progDesc "CLI fuzzy text selector"
      <> header (printf "wybor - %s" (showVersion version)))

  go = Options
    <$> optional (option auto (long "visible" <> help "Choices to show"))
    <*> optional (option auto (long "height"  <> help "Choice height in lines"))
    <*> optional (textOption  (long "query"   <> help "Initial query string"))
    <*> optional (textOption  (long "prompt"  <> help "Prompt string"))
    <*> flag single multi     (long "multi"   <> help "Multiple selections mode")
    <*> flag ln _0            (short '0'      <> help "End the choice with the \\0 symbol and not a newline")

  single, multi :: Mode
  single = Mode (\f -> C.await >>= \case Nothing -> liftIO exitFailure; Just x -> f x)
  multi  = Mode C.awaitForever

  ln = EndWith println
  _0 = EndWith print0

textOption :: Mod OptionFields String -> Parser Text
textOption = fmap Text.pack . strOption

inputs :: IO (NonEmpty Text)
inputs = maybe exitFailure return . nonEmpty . Text.lines =<< Text.getContents

output :: Options -> NonEmpty Text -> IO ()
output Options { _visible, _height, _query, _prompt, _mode = Mode f, _endWith = EndWith g } ins = do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  runResourceT (selections alternatives $$ f g)
 `catchTTYException`
  \_ -> exitFailure
 where
  alternatives = fromTexts ins
    & visible %~ maybe id const _visible
    & height  %~ maybe id const _height
    & prefix  %~ maybe id const _prompt
    & initial %~ maybe id const _query

catchTTYException :: IO a -> (TTYException -> IO a) -> IO a
catchTTYException = catch

println :: MonadIO m => Text -> m ()
println = liftIO . Text.putStrLn

print0 :: MonadIO m => Text -> m ()
print0 m = liftIO $ do Text.putStr m; Text.putStr (Text.singleton '\0')
