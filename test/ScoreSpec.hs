{-# LANGUAGE OverloadedStrings #-}
module ScoreSpec (spec) where

import Prelude hiding (minimum)
import Test.Hspec

import Score (Input(..), Choice(..), minimum, score)


spec :: Spec
spec = do
  describe "minimum" $ do
    it "is a safe version of Prelude.minimum that does not choke on empty inputs" $
      minimum ([] :: [Int]) `shouldBe` Nothing

    it "finds a minimum value in the list" $
      minimum [4, 6, 7, 1, 8] `shouldBe` Just 1

  describe "score" $ do
    it "is maximal when the input is empty" $
      score (Input "") (Choice "foo") `shouldBe` maxBound

    it "is minimal when the choice is empty" $
      score (Input "foo") (Choice "") `shouldBe` minBound

    it "is minimal when the input is not a substring of the choice" $
      score (Input "foo") (Choice "bar") `shouldBe` minBound

    it "is greater than minimum when the input is a substring of the choice" $
      score (Input "foo") (Choice "fbarobazo") `compare` minBound  `shouldBe` GT

    it "is greater for the input with a shorter substring" $
      let
        s = score (Input "foo") (Choice "fbarobazo")
        t = score (Input "foo") (Choice "frozo")
      in
        compare s t `shouldBe` LT
