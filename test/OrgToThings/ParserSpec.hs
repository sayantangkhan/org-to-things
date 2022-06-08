{-# LANGUAGE OverloadedStrings #-}

module OrgToThings.ParserSpec (spec) where

import OrgToThings.Definitions
import OrgToThings.Parser
import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "parseScheduledString" $ do
    it "parses successfully when provided a well formed SCHEDULE date string" $ do
      let input = "<2022-06-14 Tue>"
      let output = Right (DateS (2022, 6, 14), "")
      (runParser parseScheduledString input) `shouldBe` output
    it "parses successfully when provided a well formed SCHEDULE datetime string" $ do
      let input = "<2022-06-14 Tue 11:34>"
      let output = Right (DateTimeS ((2022, 6, 14), (11, 34)), "")
      (runParser parseScheduledString input) `shouldBe` output

    it "fails to parse without consuming input when given an ill-formed input string" $ do
      let input = "<2022-06/14 Tue<"
      let output = Left $ "Expected schedule. Found " ++ input
      (runParser parseScheduledString input) `shouldBe` output
