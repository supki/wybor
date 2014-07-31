{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.Lens hiding (argument)
import           Control.Monad.Trans.Resource (MonadResource, MonadBaseControl, runResourceT)
import           Control.Monad.IO.Class (MonadIO(..))
import           Data.ByteString (ByteString)
import           Data.Conduit (ResumableSource, Conduit, ($$+-), ($$), (=$=))
import qualified Data.Conduit as C
import           Data.Conduit.Binary (sinkFile)
import           Data.Function (fix)
import           Data.List.NonEmpty (nonEmpty)
import           Data.Text (Text)
import           Data.Text.Lens (unpacked)
import           Prelude hiding (init, lines)
import           System.FilePath ((</>), takeFileName)
import qualified System.IO as IO
import qualified Network.HTTP.Conduit as Http
import           Text.HTML.DOM (sinkDoc)
import           Text.Printf (printf)
import           Text.Xml.Lens

import           Wybor (selections, fromTexts)


type Url = String

main :: IO ()
main = Http.withManager $ inquire ftp

inquire :: (MonadResource m, MonadBaseControl IO m) => Url -> Http.Manager -> m ()
inquire url m = do
  req <- Http.parseUrl url
  res <- Http.http req m
  case lookup "Content-Type" (Http.responseHeaders res) of
    Nothing -> error "WAT"
    Just "text/html;charset=UTF-8" ->
      choices url m =<< parseHtml res
    Just _ ->
      download res (takeFileName url)

choices :: (MonadResource m, MonadBaseControl IO m) => Url -> Http.Manager -> Document -> m ()
choices url m doc = case nonEmpty (deeper doc) of
  Just xs -> selections (fromTexts xs) $$ do
    C.awaitForever $ \s -> liftIO (runResourceT (inquire (url </> view unpacked s) m))
  Nothing -> return ()

deeper :: Document -> [Text]
deeper = toListOf (html.node "body".node "pre".dropping 4 (node "a".attr "href".folded))

parseHtml :: MonadResource m => Http.Response (ResumableSource m ByteString) -> m Document
parseHtml r = Http.responseBody r $$+- progress "Parsing HTML document.." =$= sinkDoc

download :: MonadResource m => Http.Response (ResumableSource m ByteString) -> FilePath -> m ()
download r p = Http.responseBody r $$+- progress (printf "Downloading ‘%s’.." p) =$= sinkFile p

progress :: MonadIO m => String -> Conduit a m a
progress m = do
  write m
  flip fix 0 $ \loop n -> do
    mx <- C.await
    case mx of
      Nothing -> return ()
      Just x  ->
        if n `rem` 100 == 0 then do write "."; C.yield x; loop 1; else do C.yield x; loop (n + 1)
 where
  write str = liftIO $ do putStr str; IO.hFlush IO.stdout

ftp :: Url
ftp = "http://ftp.gnu.org/gnu"
