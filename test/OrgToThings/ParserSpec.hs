{-# LANGUAGE OverloadedStrings #-}

module OrgToThings.ParserSpec (spec) where

import OrgToThings.Definitions
import OrgToThings.Parser
import Test.Hspec
import Text.Pandoc.Definition

spec :: Spec
spec = parallel $ do
  describe "parseScheduledString" $ do
    it "parses successfully when provided a well formed SCHEDULE date string" $ do
      let input = "<2022-06-14 Tue>"
      let output = Right (DateS (2022, 6, 14), "")
      runParser parseScheduledString input `shouldBe` output

    it "parses successfully when provided a well formed SCHEDULE datetime string" $ do
      let input = "<2022-06-14 Tue 11:34>"
      let output = Right (DateTimeS ((2022, 6, 14), (11, 34)), "")
      runParser parseScheduledString input `shouldBe` output

    it "fails to parse without consuming input when given an ill-formed input string" $ do
      let input = "<2022-06/14 Tue<"
      let output = Left $ "Expected schedule. Found " ++ input
      runParser parseScheduledString input `shouldBe` output

    it "fails to parse without consuming input when a well formed string with trailing data" $ do
      let input = "<2022-06-14 Tue> More stuff"
      let output = Left $ "Expected schedule. Found " ++ input
      runParser parseScheduledString input `shouldBe` output

  describe "parseDeadlineString" $ do
    it "parses successfully when provided a well formed DEADLINE date string" $ do
      let input = "<2022-06-14 Tue>"
      let output = Right (DateD (2022, 6, 14), "")
      runParser parseDeadlineString input `shouldBe` output

    it "fails to parse without consuming input when given an ill-formed input string" $ do
      let input = "<2022-06/14 Tue<"
      let output = Left $ "Expected deadline. Found " ++ input
      runParser parseDeadlineString input `shouldBe` output

    it "fails to parse without consuming input when a well formed string with trailing data" $ do
      let input = "<2022-06-14 Tue> More stuff"
      let output = Left $ "Expected deadline. Found " ++ input
      runParser parseDeadlineString input `shouldBe` output

  describe "parsePlanningInline" $ do
    it "parses planning info with only schedule" $ do
      let input =
            [ Strong [Str "SCHEDULED:"],
              Space,
              Emph [Str "<2022-06-15 Wed>"]
            ]
      let output = Right ((Just $ DateS (2022, 6, 15), Nothing), [])
      runParser parsePlanningInline input `shouldBe` output

    it "parses planning info with only deadline" $ do
      let input =
            [ Strong [Str "DEADLINE:"],
              Space,
              Emph [Str "<2022-06-15 Wed>"]
            ]
      let output = Right ((Nothing, Just $ DateD (2022, 6, 15)), [])
      runParser parsePlanningInline input `shouldBe` output

    it "parses planning info with schedule then deadline" $ do
      let input =
            [ Strong [Str "SCHEDULED:"],
              Space,
              Emph [Str "<2022-06-14 Tue>"],
              Space,
              Strong [Str "DEADLINE:"],
              Space,
              Emph [Str "<2022-06-18 Sat>"]
            ]
      let output = Right ((Just (DateS (2022, 6, 14)), Just (DateD (2022, 6, 18))), [])
      runParser parsePlanningInline input `shouldBe` output

    it "parses planning info with deadline then schedule " $ do
      let input =
            [ Strong [Str "DEADLINE:"],
              Space,
              Emph [Str "<2022-06-18 Sat>"],
              Space,
              Strong [Str "SCHEDULED:"],
              Space,
              Emph [Str "<2022-06-14 Tue>"]
            ]
      let output = Right ((Just (DateS (2022, 6, 14)), Just (DateD (2022, 6, 18))), [])
      runParser parsePlanningInline input `shouldBe` output

  describe "parseTodoAndTagsInline" $ do
    it "parses a TODO with no tags" $ do
      let input =
            [ Span ("", ["todo", "TODO"], []) [Str "TODO"],
              Space,
              Str "Todo",
              Space,
              Str "without",
              Space,
              Str "a",
              Space,
              Str "project",
              Space,
              Str "with",
              Space,
              Str "notes",
              Space,
              Str "without",
              Space,
              Str "checkboxes"
            ]
      let output = Right (("Todo without a project with notes without checkboxes", []), [])
      runParser parseTodoAndTagsInline input `shouldBe` output

    it "parses a TODO with a single tag" $ do
      let input =
            [ Span ("", ["todo", "TODO"], []) [Str "TODO"],
              Space,
              Str "Todo",
              Space,
              Str "with",
              Space,
              Str "notes",
              Space,
              Str "without",
              Space,
              Str "checkboxes",
              Space,
              Span
                ("", ["tag"], [("tag-name", "programming")])
                [SmallCaps [Str "programming"]]
            ]
      let output = Right (("Todo with notes without checkboxes", ["programming"]), [])
      runParser parseTodoAndTagsInline input `shouldBe` output

    it "parses a TODO with a multiple tag" $ do
      let input =
            [ Span ("", ["todo", "TODO"], []) [Str "TODO"],
              Space,
              Str "Todo",
              Space,
              Str "with",
              Space,
              Str "notes",
              Space,
              Str "without",
              Space,
              Str "checkboxes",
              Space,
              Span
                ("", ["tag"], [("tag-name", "programming")])
                [SmallCaps [Str "programming"]],
              Str "\160",
              Span
                ("", ["tag"], [("tag-name", "math")])
                [SmallCaps [Str "math"]]
            ]
      let output = Right (("Todo with notes without checkboxes", ["programming", "math"]), [])
      runParser parseTodoAndTagsInline input `shouldBe` output

    it "fails to parse TODO without a TODO marker" $ do
      let input =
            [ Str "Todo",
              Space,
              Str "without",
              Space,
              Str "a",
              Space,
              Str "project",
              Space,
              Str "with",
              Space,
              Str "notes",
              Space,
              Str "without",
              Space,
              Str "checkboxes"
            ]
      let output = Left ("Expected 'TODO'", Just $ Str "Todo")
      runParser parseTodoAndTagsInline input `shouldBe` output
