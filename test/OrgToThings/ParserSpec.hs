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

  describe "parseSpecialInline" $ do
    it "parses a hyperlink with single word title" $ do
      let input =
            [ Link
                ("", [], [])
                [Str "Things"]
                ( "https://culturedcode.com/things/support/articles/2803573/",
                  ""
                )
            ]
      let output = Right ("(Things)[https://culturedcode.com/things/support/articles/2803573/]", [])
      runParser parseSpecialInline input `shouldBe` output

    it "parses a hyperlink with multiple words in the title" $ do
      let input =
            [ Link
                ("", [], [])
                [Str "Things", Space, Str "API"]
                ( "https://culturedcode.com/things/support/articles/2803573/",
                  ""
                )
            ]
      let output = Right ("(Things API)[https://culturedcode.com/things/support/articles/2803573/]", [])
      runParser parseSpecialInline input `shouldBe` output

    it "fails to parse a hyperlink with no words in the title" $ do
      let input =
            [ Link
                ("", [], [])
                []
                ( "https://culturedcode.com/things/support/articles/2803573/",
                  ""
                )
            ]
      let output = Left ("Expected input Str", Nothing)
      runParser parseSpecialInline input `shouldBe` output

    it "parses emphasized text correctly " $ do
      let input =
            [ Emph [Str "italic", Space, Str "text"]
            ]
      let output = Right ("*italic text*", [])
      runParser parseSpecialInline input `shouldBe` output

    it "parses strong text correctly " $ do
      let input =
            [ Strong [Str "bold", Space, Str "text"]
            ]
      let output = Right ("**bold text**", [])
      runParser parseSpecialInline input `shouldBe` output

    it "parses code blocks correctly " $ do
      let input =
            [ Code ("", [], []) "code block"
            ]
      let output = Right ("`code block`", [])
      runParser parseSpecialInline input `shouldBe` output

  describe "parseNotesInline" $ do
    it "parses notes fields with all sorts of special data correctly" $ do
      let input =
            [ Str "Blah",
              Space,
              Str "notes",
              Space,
              Str "blah",
              Space,
              Link
                ("", [], [])
                [Str "Things", Space, Str "API"]
                ( "https://culturedcode.com/things/support/articles/2803573/",
                  ""
                ),
              Space,
              Code ("", [], []) "code block",
              Space,
              Emph [Str "italics"],
              Space,
              Strong [Str "bold"],
              Str ".",
              Space,
              Link
                ("", [], [])
                [Str "http://joeyh.name/blog/"]
                ("http://joeyh.name/blog/", "")
            ]
      let output = Right ("Blah notes blah (Things API)[https://culturedcode.com/things/support/articles/2803573/] `code block` *italics* **bold**. (http://joeyh.name/blog/)[http://joeyh.name/blog/]", [])
      runParser parseNotesInline input `shouldBe` output

  describe "parseSingleChecklistInline" $ do
    it "parses a simple checklist item with no fancy markup" $ do
      let input = [Str "\9744", Space, Str "Blah", Space, Str "2"]
      let output = Right ("Blah 2", [])
      runParser parseSingleChecklistInline input `shouldBe` output

    it "parses a checklist item with fancy markup" $ do
      let input =
            [ Str "\9744",
              Space,
              Str "Blah",
              Space,
              Str "3",
              Space,
              Code ("", [], []) "code block",
              Space,
              Strong [Str "bold"],
              Space,
              Str "and",
              Space,
              Emph [Str "italics"],
              Str "."
            ]
      let output = Right ("Blah 3 `code block` **bold** and *italics*.", [])
      runParser parseSingleChecklistInline input `shouldBe` output

    it "fails to parse a checklist item which does not start with a checkmark" $ do
      let input =
            [ Str "Blah",
              Space,
              Str "3",
              Space,
              Code ("", [], []) "code block",
              Space,
              Strong [Str "bold"],
              Space,
              Str "and",
              Space,
              Emph [Str "italics"],
              Str "."
            ]
      let output = Left ("Expected to start with a checkmark", Just $ Str "Blah")
      runParser parseSingleChecklistInline input `shouldBe` output

  describe "parseChecklistBlock" $ do
    it "parses a Block corresponding to a checklist" $ do
      let input =
            [ BulletList
                [ [Plain [Str "\9744", Space, Str "Blah"]],
                  [ Plain
                      [Str "\9744", Space, Str "Blah", Space, Str "2"]
                  ],
                  [ Plain
                      [ Str "\9744",
                        Space,
                        Str "Blah",
                        Space,
                        Str "3",
                        Space,
                        Code ("", [], []) "code block",
                        Space,
                        Strong [Str "bold"],
                        Space,
                        Str "and",
                        Space,
                        Emph [Str "italics"],
                        Str "."
                      ]
                  ]
                ]
            ]
      let output = Right ((input, ["Blah", "Blah 2", "Blah 3 `code block` **bold** and *italics*."]), [])
      runParser parseChecklistBlock input `shouldBe` output

    it "fails to parse an empty checklist" $ do
      let input =
            [ BulletList
                []
            ]
      let output = Left ("Failed to parse BulletList", Just (BulletList []))
      runParser parseChecklistBlock input `shouldBe` output

    it "fails to parse a BulletList with malformed entries" $ do
      let input =
            [ BulletList
                [ [Plain [Str "\9744", Space, Str "Blah"]],
                  [ Plain
                      [Str "Blah", Space, Str "2"]
                  ],
                  [ Plain
                      [ Str "\9744",
                        Space,
                        Str "Blah",
                        Space,
                        Str "3",
                        Space,
                        Code ("", [], []) "code block",
                        Space,
                        Strong [Str "bold"],
                        Space,
                        Str "and",
                        Space,
                        Emph [Str "italics"],
                        Str "."
                      ]
                  ]
                ]
            ]
      let output = Left ("Failed to parse BulletList", Just (head input))
      runParser parseChecklistBlock input `shouldBe` output

  describe "parseNotesBlock" $ do
    it "parses well formed [Inline] wrapped in Para" $ do
      let input =
            [ Para
                [ Str "Blah",
                  Space,
                  Str "notes",
                  Space,
                  Str "blah",
                  Space,
                  Link
                    ("", [], [])
                    [Str "Things", Space, Str "API"]
                    ( "https://culturedcode.com/things/support/articles/2803573/",
                      ""
                    ),
                  Space,
                  Code ("", [], []) "code block",
                  Space,
                  Emph [Str "italics"],
                  Space,
                  Strong [Str "bold"],
                  Str ".",
                  Space,
                  Link
                    ("", [], [])
                    [Str "http://joeyh.name/blog/"]
                    ("http://joeyh.name/blog/", "")
                ]
            ]
      let output = Right ((input, "Blah notes blah (Things API)[https://culturedcode.com/things/support/articles/2803573/] `code block` *italics* **bold**. (http://joeyh.name/blog/)[http://joeyh.name/blog/]"), [])
      runParser parseNotesBlock input `shouldBe` output

  describe "parseTodoWithProjectAndHeading" $ do
    it "parses a Todo nested in Project and Heading and returns the Todo block with link appended" $ do
      let input_todo =
            [ Para
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
                ],
              Plain
                [ Strong [Str "SCHEDULED:"],
                  Space,
                  Emph [Str "<2022-06-15 Wed 11:34>"]
                ],
              Para
                [Str "Some", Space, Str "example", Space, Str "notes"]
            ]

      let area = Area "Personal"
      let project = constructProject "Home setup" ["math"] Nothing Nothing area
      let heading = Heading "Heading 1" project area
      let output_todo =
            input_todo
              ++ [ Para
                     [ Link
                         ("", [], [])
                         [Str "Add", Space, Str "Todo"]
                         ("things:///add?title=Todo%20with%20notes%20without%20checkboxes&notes=Some%20example%20notes&when=2022-6-15@11:34&tags=programming&list=Home%20setup&heading=Heading%201", "")
                     ]
                 ]
      let input = [OrderedList (1, DefaultStyle, DefaultDelim) [input_todo]]
      let output = Right ([OrderedList (1, DefaultStyle, DefaultDelim) [output_todo]], [])
      runParser (parseTodoWithProjectAndHeading area project heading) input `shouldBe` output

    it "parses mutliples Todos nested in Project and Heading and returns the Todo blocks with link appended" $ do
      let input_todo =
            [ Para
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
                ],
              Plain
                [ Strong [Str "SCHEDULED:"],
                  Space,
                  Emph [Str "<2022-06-15 Wed 11:34>"]
                ],
              Para
                [Str "Some", Space, Str "example", Space, Str "notes"]
            ]

      let area = Area "Personal"
      let project = constructProject "Home setup" ["math"] Nothing Nothing area
      let heading = Heading "Heading 1" project area
      let output_todo =
            input_todo
              ++ [ Para
                     [ Link
                         ("", [], [])
                         [Str "Add", Space, Str "Todo"]
                         ("things:///add?title=Todo%20with%20notes%20without%20checkboxes&notes=Some%20example%20notes&when=2022-6-15@11:34&tags=programming&list=Home%20setup&heading=Heading%201", "")
                     ]
                 ]
      let input = [OrderedList (1, DefaultStyle, DefaultDelim) [input_todo, input_todo]]
      let output = Right ([OrderedList (1, DefaultStyle, DefaultDelim) [output_todo, output_todo]], [])
      runParser (parseTodoWithProjectAndHeading area project heading) input `shouldBe` output
