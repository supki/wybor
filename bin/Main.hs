{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.Applicative
import           Control.Lens
import           Data.List.NonEmpty (NonEmpty, nonEmpty)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Version (showVersion)
import           Options.Applicative hiding ((&))
import           Prelude hiding (init, lines)
import           System.Exit (exitFailure)
import           Text.Printf (printf)

import           Wybor (HasWybor(..), TTYException, select, fromTexts)
import           Paths_wybor (version)


main :: IO ()
main = do
  opts <- options
  ins  <- inputs
  out  <- output opts ins
  case out of
    Left _         -> exitFailure
    Right Nothing  -> exitFailure
    Right (Just s) -> Text.putStrLn s

data Options = Options
  { lines :: Maybe Int
  , size  :: Maybe Int
  , pref  :: Maybe Text
  , init  :: Maybe Text
  } deriving (Show, Eq)

options :: IO Options
options = execParser parser
 where
  parser = info (helper <*> go)
    (fullDesc <> progDesc "CLI fuzzy text selector"
      <> header (printf "wybor - %s" (showVersion version)))

  go = Options
    <$> optional (option (long "lines" <> short 'n' <> help "Choices to show"))
    <*> optional (option (long "height" <> short 'm' <> help "Choice height"))
    <*> optional (textOption (long "prefix" <> short 'p' <> help "Prompt prefix"))
    <*> optional (textOption (long "init" <> short 'i' <> help "Initial input"))

textOption :: Mod OptionFields String -> Parser Text
textOption = fmap Text.pack . strOption

inputs :: IO (NonEmpty Text)
inputs = maybe exitFailure return . nonEmpty . Text.lines =<< Text.getContents

output :: Options -> NonEmpty Text -> IO (Either TTYException (Maybe Text))
output Options { lines, size, pref, init } ins =
  select $ fromTexts ins
    & visible %~ probably lines
    & height %~ probably size
    & prefix %~ probably pref
    & initial %~ probably init

probably :: Maybe a -> a -> a
probably = maybe id const
