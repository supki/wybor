{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module WyborSpec (spec) where

import           Data.Conduit (($$))
import qualified Data.Conduit as C
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Text (Text)
import qualified System.IO as IO
import qualified System.Posix as Posix
import           Prelude hiding (null)
import           Test.Hspec

import           TTY
import           Wybor


spec :: Spec
spec =
  describe "pipeline" $ do
    it "accepts the directly entered option" $
      Input
        [("foo", 4), ("bar", 7), ("baz", 11)]
        ("foo" ++ [keyEnter])
     `shouldSelect`
      Just 4

    it "accepts shortcuts" $
      Input
        [("foo", 4), ("bar", 7), ("baz", 11)]
        ("br" ++ [keyEnter])
     `shouldSelect`
      Just 7

    it "provides selection options" $
      Input
        [("foo", 4), ("bar", 7), ("baz", 11)]
        ("b" ++ [keyCtrl 'N', keyEnter])
     `shouldSelect`
      Just 11

    it "provides line-editing facilites" $
      Input
        [("foo", 4), ("bar", 7), ("baz", 11)]
        ("bzo" ++ [keyCtrl 'H', keyEnter])
     `shouldSelect`
      Just 11

    it "provides more line-editing facilites" $
      Input
        [("foo", 4), ("bar", 7), ("baz", 11)]
        ("bzo" ++ [keyCtrl 'U'] ++ "fo" ++ [keyEnter])
     `shouldSelect`
      Just 4

    it "supports infinite input (as long as the query is empty)" $
      Input
        (zip (repeat "foo") [1..])
        [keyCtrl 'N', keyCtrl 'N', keyEnter]
     `shouldSelect`
      Just 3

data Input a = Input [(Text, a)] String

type Expect a = (Show a, Eq a)

shouldSelect :: Expect a => Input a -> Maybe a -> Expectation
shouldSelect (Input xs s) r =
  IO.withFile nullDevice IO.WriteMode $ \null -> do
    (i, o) <- Posix.createPipe
    ih     <- Posix.fdToHandle i
    oh     <- Posix.fdToHandle o
    IO.hPutStr oh ("\ESC[1;1R" ++ s)
    IO.hFlush oh
    IO.hClose oh
    selectOnce (fromAssoc (NonEmpty.fromList xs)) TTY
      { outHandle = null
      , inHandle  = ih
      , winHeight = 4
      , winWidth  = 7
      } `shouldReturn` r

selectOnce :: Show a => Wybor IO a -> TTY -> IO (Maybe a)
selectOnce c tty = pipeline c tty $$ C.await
