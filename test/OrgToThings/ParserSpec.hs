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
      let output = Right (("Todo without a project with notes without checkboxes", [], Active), [])
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
      let output = Right (("Todo with notes without checkboxes", ["programming"], Active), [])
      runParser parseTodoAndTagsInline input `shouldBe` output

    it "parses a TODO with multiple tags" $ do
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
      let output = Right (("Todo with notes without checkboxes", ["programming", "math"], Active), [])
      runParser parseTodoAndTagsInline input `shouldBe` output

    it "parses an INACTIVE with multiple tags" $ do
      let input =
            [ Str "INACTIVE",
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
      let output = Right (("Todo with notes without checkboxes", ["programming", "math"], Inactive), [])
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

    it "parses mutliple Todos nested in Project and Heading and returns the Todo blocks with link appended" $ do
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

    it "fails to parse an OrderedList with a malformed Todo" $ do
      let input_todo1 =
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
      let input_todo2 =
            [ Para
                [ Str "Todo",
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
      let input = [OrderedList (1, DefaultStyle, DefaultDelim) [input_todo1, input_todo2]]
      let output = Left ("Expected 'TODO'", Just $ head input_todo2)
      runParser (parseTodoWithProjectAndHeading area project heading) input `shouldBe` output

  describe "parseHeading" $ do
    it "parses a heading and nested Todos and appends the appropriate link after each Todo" $ do
      let input_blocks =
            [ Header
                3
                ("heading-1", [], [])
                [Str "Heading", Space, Str "1"],
              OrderedList
                (1, DefaultStyle, DefaultDelim)
                [ [ Para
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
                  ],
                  [ Para
                      [ Span ("", ["todo", "TODO"], []) [Str "TODO"],
                        Space,
                        Str "Todo",
                        Space,
                        Str "with",
                        Space,
                        Str "notes",
                        Space,
                        Str "and",
                        Space,
                        Str "checkboxes"
                      ],
                    Plain
                      [ Strong [Str "SCHEDULED:"],
                        Space,
                        Emph [Str "<2022-06-14 Tue>"]
                      ],
                    Para [Str "Some", Space, Str "notes"],
                    BulletList
                      [ [Plain [Str "\9744", Space, Str "Blah"]],
                        [ Plain
                            [Str "\9744", Space, Str "Blah", Space, Str "2"]
                        ]
                      ]
                  ]
                ]
            ]

      let area = Area "Personal"
      let project = constructProject "Home setup" ["math"] Nothing Nothing area
      let output_blocks =
            [ Header
                3
                ("heading-1", [], [])
                [Str "Heading", Space, Str "1"],
              OrderedList
                (1, DefaultStyle, DefaultDelim)
                [ [ Para
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
                      [Str "Some", Space, Str "example", Space, Str "notes"],
                    Para
                      [ Link
                          ("", [], [])
                          [Str "Add", Space, Str "Todo"]
                          ("things:///add?title=Todo%20with%20notes%20without%20checkboxes&notes=Some%20example%20notes&when=2022-6-15@11:34&tags=programming&list=Home%20setup&heading=Heading%201", "")
                      ]
                  ],
                  [ Para
                      [ Span ("", ["todo", "TODO"], []) [Str "TODO"],
                        Space,
                        Str "Todo",
                        Space,
                        Str "with",
                        Space,
                        Str "notes",
                        Space,
                        Str "and",
                        Space,
                        Str "checkboxes"
                      ],
                    Plain
                      [ Strong [Str "SCHEDULED:"],
                        Space,
                        Emph [Str "<2022-06-14 Tue>"]
                      ],
                    Para [Str "Some", Space, Str "notes"],
                    BulletList
                      [ [Plain [Str "\9744", Space, Str "Blah"]],
                        [ Plain
                            [Str "\9744", Space, Str "Blah", Space, Str "2"]
                        ]
                      ],
                    Para
                      [ Link
                          ("", [], [])
                          [Str "Add", Space, Str "Todo"]
                          ("things:///add?title=Todo%20with%20notes%20and%20checkboxes&notes=Some%20notes&checklist-items=Blah%0ABlah%202&when=2022-6-14&list=Home%20setup&heading=Heading%201", "")
                      ]
                  ]
                ]
            ]

      let output = Right (output_blocks, [])
      runParser (parseHeading area project) input_blocks `shouldBe` output

  describe "parseTodoWithProject" $ do
    it "parses a standalone Todo in a project" $ do
      let input_blocks =
            [ Header
                3
                ("sell-yoga-laptop", [], [])
                [ Span ("", ["todo", "TODO"], []) [Str "TODO"],
                  Space,
                  Str "Sell",
                  Space,
                  Str "yoga",
                  Space,
                  Str "laptop"
                ],
              Header
                3
                ("buy-mac-mini", [], [])
                [ Span ("", ["todo", "TODO"], []) [Str "TODO"],
                  Space,
                  Str "Buy",
                  Space,
                  Str "Mac",
                  Space,
                  Str "mini"
                ],
              Para
                [ Str "Also",
                  Space,
                  Str "buy",
                  Space,
                  Str "accessories",
                  Space,
                  Str "like",
                  Space,
                  Str "trackpad"
                ]
            ]
      let area = Area "Personal"
      let project = constructProject "Migrate desktop to Mac" ["programming", "math"] Nothing Nothing area
      let output_blocks =
            [ Header
                3
                ("sell-yoga-laptop", [], [])
                [ Span ("", ["todo", "TODO"], []) [Str "TODO"],
                  Space,
                  Str "Sell",
                  Space,
                  Str "yoga",
                  Space,
                  Str "laptop"
                ],
              Para [Link ("", [], []) [Str "Add", Space, Str "Todo"] ("things:///add?title=Sell%20yoga%20laptop&list=Migrate%20desktop%20to%20Mac", "")]
            ]
      let tail_blocks =
            [ Header
                3
                ("buy-mac-mini", [], [])
                [ Span ("", ["todo", "TODO"], []) [Str "TODO"],
                  Space,
                  Str "Buy",
                  Space,
                  Str "Mac",
                  Space,
                  Str "mini"
                ],
              Para
                [ Str "Also",
                  Space,
                  Str "buy",
                  Space,
                  Str "accessories",
                  Space,
                  Str "like",
                  Space,
                  Str "trackpad"
                ]
            ]
      let output = Right (output_blocks, tail_blocks)
      runParser (parseTodoWithProject area project) input_blocks `shouldBe` output

  describe "parseProject" $ do
    it "parses a Project block with metadata and nested Todos" $ do
      let input_blocks =
            [ Header
                2
                ("migrate-desktop-to-mac", [], [])
                [ Str "Migrate",
                  Space,
                  Str "desktop",
                  Space,
                  Str "to",
                  Space,
                  Str "Mac",
                  Space,
                  Span
                    ("", ["tag"], [("tag-name", "programming")])
                    [SmallCaps [Str "programming"]],
                  Str "\160",
                  Span
                    ("", ["tag"], [("tag-name", "math")])
                    [SmallCaps [Str "math"]]
                ],
              Plain
                [ Strong [Str "DEADLINE:"],
                  Space,
                  Emph [Str "<2022-06-18 Sat>"],
                  Space,
                  Strong [Str "SCHEDULED:"],
                  Space,
                  Emph [Str "<2022-06-14 Tue>"]
                ],
              Para
                [Str "Some", Space, Str "notes", Space, Str "here"],
              Header
                3
                ("sell-yoga-laptop", [], [])
                [ Span ("", ["todo", "TODO"], []) [Str "TODO"],
                  Space,
                  Str "Sell",
                  Space,
                  Str "yoga",
                  Space,
                  Str "laptop"
                ],
              Header
                3
                ("buy-mac-mini", [], [])
                [ Span ("", ["todo", "TODO"], []) [Str "TODO"],
                  Space,
                  Str "Buy",
                  Space,
                  Str "Mac",
                  Space,
                  Str "mini"
                ],
              Para
                [ Str "Also",
                  Space,
                  Str "buy",
                  Space,
                  Str "accessories",
                  Space,
                  Str "like",
                  Space,
                  Str "trackpad"
                ]
            ]
      let area = Area "Personal"
      let output_blocks =
            [ Header
                2
                ("migrate-desktop-to-mac", [], [])
                [ Str "Migrate",
                  Space,
                  Str "desktop",
                  Space,
                  Str "to",
                  Space,
                  Str "Mac",
                  Space,
                  Span
                    ("", ["tag"], [("tag-name", "programming")])
                    [SmallCaps [Str "programming"]],
                  Str "\160",
                  Span
                    ("", ["tag"], [("tag-name", "math")])
                    [SmallCaps [Str "math"]]
                ],
              Plain
                [ Strong [Str "DEADLINE:"],
                  Space,
                  Emph [Str "<2022-06-18 Sat>"],
                  Space,
                  Strong [Str "SCHEDULED:"],
                  Space,
                  Emph [Str "<2022-06-14 Tue>"]
                ],
              Para
                [Str "Some", Space, Str "notes", Space, Str "here"],
              Para [Link ("", [], []) [Str "Add", Space, Str "Project"] ("things:///add-project?title=Migrate%20desktop%20to%20Mac&notes=Some%20notes%20here&when=2022-6-14&deadline=2022-6-18&tags=programming%2Cmath&area=Personal", "")],
              Header
                3
                ("sell-yoga-laptop", [], [])
                [ Span ("", ["todo", "TODO"], []) [Str "TODO"],
                  Space,
                  Str "Sell",
                  Space,
                  Str "yoga",
                  Space,
                  Str "laptop"
                ],
              Para [Link ("", [], []) [Str "Add", Space, Str "Todo"] ("things:///add?title=Sell%20yoga%20laptop&list=Migrate%20desktop%20to%20Mac", "")],
              Header
                3
                ("buy-mac-mini", [], [])
                [ Span ("", ["todo", "TODO"], []) [Str "TODO"],
                  Space,
                  Str "Buy",
                  Space,
                  Str "Mac",
                  Space,
                  Str "mini"
                ],
              Para
                [ Str "Also",
                  Space,
                  Str "buy",
                  Space,
                  Str "accessories",
                  Space,
                  Str "like",
                  Space,
                  Str "trackpad"
                ],
              Para [Link ("", [], []) [Str "Add", Space, Str "Todo"] ("things:///add?title=Buy%20Mac%20mini&notes=Also%20buy%20accessories%20like%20trackpad&list=Migrate%20desktop%20to%20Mac", "")]
            ]
      let output = Right (output_blocks, [])
      runParser (parseProject area) input_blocks `shouldBe` output

    it "parses a Project with metadata, nested Todos and headings" $ do
      let input_blocks =
            [ Header
                2
                ("home-setup", [], [])
                [ Str "Home",
                  Space,
                  Str "setup",
                  Space,
                  Span
                    ("", ["tag"], [("tag-name", "math")])
                    [SmallCaps [Str "math"]]
                ],
              Header
                3
                ("cleanup-kitchen", [], [])
                [ Span ("", ["todo", "TODO"], []) [Str "TODO"],
                  Space,
                  Str "Cleanup",
                  Space,
                  Str "kitchen"
                ],
              Plain
                [ Strong [Str "DEADLINE:"],
                  Space,
                  Emph [Str "<2022-06-16 Thu>"],
                  Space,
                  Strong [Str "SCHEDULED:"],
                  Space,
                  Emph [Str "<2022-06-14 Tue>"]
                ],
              Header
                3
                ("heading-1", [], [])
                [Str "Heading", Space, Str "1"],
              OrderedList
                (1, DefaultStyle, DefaultDelim)
                [ [ Para
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
                  ],
                  [ Para
                      [ Span ("", ["todo", "TODO"], []) [Str "TODO"],
                        Space,
                        Str "Todo",
                        Space,
                        Str "with",
                        Space,
                        Str "notes",
                        Space,
                        Str "and",
                        Space,
                        Str "checkboxes"
                      ],
                    Plain
                      [ Strong [Str "SCHEDULED:"],
                        Space,
                        Emph [Str "<2022-06-14 Tue>"]
                      ],
                    Para [Str "Some", Space, Str "notes"],
                    BulletList
                      [ [Plain [Str "\9744", Space, Str "Blah"]],
                        [ Plain
                            [Str "\9744", Space, Str "Blah", Space, Str "2"]
                        ]
                      ]
                  ]
                ],
              Header
                3
                ("heading-2", [], [])
                [Str "Heading", Space, Str "2"],
              OrderedList
                (1, DefaultStyle, DefaultDelim)
                [ [ Para
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
                      ],
                    Plain
                      [ Strong [Str "SCHEDULED:"],
                        Space,
                        Emph [Str "<2022-06-14 Tue>"]
                      ],
                    Para
                      [Str "Some", Space, Str "example", Space, Str "notes"]
                  ]
                ]
            ]
      let area = Area "Personal"
      let output_blocks =
            [ Header
                2
                ("home-setup", [], [])
                [ Str "Home",
                  Space,
                  Str "setup",
                  Space,
                  Span
                    ("", ["tag"], [("tag-name", "math")])
                    [SmallCaps [Str "math"]]
                ],
              Para [Link ("", [], []) [Str "Add", Space, Str "Project"] ("things:///add-project?title=Home%20setup&tags=math&area=Personal", "")],
              Header
                3
                ("cleanup-kitchen", [], [])
                [ Span ("", ["todo", "TODO"], []) [Str "TODO"],
                  Space,
                  Str "Cleanup",
                  Space,
                  Str "kitchen"
                ],
              Plain
                [ Strong [Str "DEADLINE:"],
                  Space,
                  Emph [Str "<2022-06-16 Thu>"],
                  Space,
                  Strong [Str "SCHEDULED:"],
                  Space,
                  Emph [Str "<2022-06-14 Tue>"]
                ],
              Para [Link ("", [], []) [Str "Add", Space, Str "Todo"] ("things:///add?title=Cleanup%20kitchen&when=2022-6-14&deadline=2022-6-16&list=Home%20setup", "")],
              Header
                3
                ("heading-1", [], [])
                [Str "Heading", Space, Str "1"],
              OrderedList
                (1, DefaultStyle, DefaultDelim)
                [ [ Para
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
                      [Str "Some", Space, Str "example", Space, Str "notes"],
                    Para [Link ("", [], []) [Str "Add", Space, Str "Todo"] ("things:///add?title=Todo%20with%20notes%20without%20checkboxes&notes=Some%20example%20notes&when=2022-6-15@11:34&tags=programming&list=Home%20setup&heading=Heading%201", "")]
                  ],
                  [ Para
                      [ Span ("", ["todo", "TODO"], []) [Str "TODO"],
                        Space,
                        Str "Todo",
                        Space,
                        Str "with",
                        Space,
                        Str "notes",
                        Space,
                        Str "and",
                        Space,
                        Str "checkboxes"
                      ],
                    Plain
                      [ Strong [Str "SCHEDULED:"],
                        Space,
                        Emph [Str "<2022-06-14 Tue>"]
                      ],
                    Para [Str "Some", Space, Str "notes"],
                    BulletList
                      [ [Plain [Str "\9744", Space, Str "Blah"]],
                        [ Plain
                            [Str "\9744", Space, Str "Blah", Space, Str "2"]
                        ]
                      ],
                    Para [Link ("", [], []) [Str "Add", Space, Str "Todo"] ("things:///add?title=Todo%20with%20notes%20and%20checkboxes&notes=Some%20notes&checklist-items=Blah%0ABlah%202&when=2022-6-14&list=Home%20setup&heading=Heading%201", "")]
                  ]
                ],
              Header
                3
                ("heading-2", [], [])
                [Str "Heading", Space, Str "2"],
              OrderedList
                (1, DefaultStyle, DefaultDelim)
                [ [ Para
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
                      ],
                    Plain
                      [ Strong [Str "SCHEDULED:"],
                        Space,
                        Emph [Str "<2022-06-14 Tue>"]
                      ],
                    Para
                      [Str "Some", Space, Str "example", Space, Str "notes"],
                    Para [Link ("", [], []) [Str "Add", Space, Str "Todo"] ("things:///add?title=Todo%20with%20notes%20without%20checkboxes&notes=Some%20example%20notes&when=2022-6-14&tags=programming%2Cmath&list=Home%20setup&heading=Heading%202", "")]
                  ]
                ]
            ]
      let output = Right (output_blocks, [])
      runParser (parseProject area) input_blocks `shouldBe` output

  describe "parseArea" $ do
    it "parses an Area containing Todos or Projects" $ do
      let input_blocks =
            [ Header
                1
                ("professional", [], [])
                [Str "Professional"],
              Header
                2
                ("email-ethan", [], [])
                [ Span ("", ["todo", "TODO"], []) [Str "TODO"],
                  Space,
                  Str "Email",
                  Space,
                  Str "Ethan",
                  Space,
                  Span
                    ("", ["tag"], [("tag-name", "programming")])
                    [SmallCaps [Str "programming"]],
                  Str "\160",
                  Span
                    ("", ["tag"], [("tag-name", "maths")])
                    [SmallCaps [Str "maths"]]
                ],
              Para
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
                ],
              Header
                2
                ("read-up-on-the-counting-curves-in-strata", [], [])
                [ Span ("", ["todo", "TODO"], []) [Str "TODO"],
                  Space,
                  Str "Read",
                  Space,
                  Str "up",
                  Space,
                  Str "on",
                  Space,
                  Str "the",
                  Space,
                  Str "counting",
                  Space,
                  Str "curves",
                  Space,
                  Str "in",
                  Space,
                  Str "strata"
                ],
              Plain
                [ Strong [Str "SCHEDULED:"],
                  Space,
                  Emph [Str "<2022-06-14 Tue>"]
                ],
              Para
                [ Str "Perhaps",
                  Space,
                  Str "also",
                  Space,
                  Str "email",
                  Space,
                  Str "John."
                ],
              Header
                2
                ("update-personal-website", [], [])
                [ Span ("", ["todo", "TODO"], []) [Str "TODO"],
                  Space,
                  Str "Update",
                  Space,
                  Str "personal",
                  Space,
                  Str "website"
                ],
              Plain
                [ Strong [Str "SCHEDULED:"],
                  Space,
                  Emph [Str "<2022-06-14 Tue>"]
                ],
              Para
                [Str "Things", Space, Str "to", Space, Str "add"],
              BulletList
                [ [Plain [Str "\9744", Space, Str "CV"]],
                  [Plain [Str "\9744", Space, Str "Papers"]],
                  [ Plain
                      [ Str "\9744",
                        Space,
                        Str "Other",
                        Space,
                        Str "things",
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
                ],
              Header
                2
                ("figure-out-travel", [], [])
                [Str "Figure", Space, Str "out", Space, Str "travel"],
              Header
                3
                ("check-out-visa-requirements-for-canada", [], [])
                [ Span ("", ["todo", "TODO"], []) [Str "TODO"],
                  Space,
                  Str "Check",
                  Space,
                  Str "out",
                  Space,
                  Str "visa",
                  Space,
                  Str "requirements",
                  Space,
                  Str "for",
                  Space,
                  Str "Canada"
                ],
              Header
                3
                ("qr-study-group", [], [])
                [Str "QR", Space, Str "Study", Space, Str "Group"],
              OrderedList
                (1, DefaultStyle, DefaultDelim)
                [ [ Para
                      [ Span ("", ["todo", "TODO"], []) [Str "TODO"],
                        Space,
                        Str "Prep",
                        Space,
                        Str "material"
                      ]
                  ],
                  [ Para
                      [ Span ("", ["todo", "TODO"], []) [Str "TODO"],
                        Space,
                        Str "Record",
                        Space,
                        Str "time",
                        Space,
                        Str "spent"
                      ]
                  ]
                ]
            ]
      let output_blocks =
            [ Header
                1
                ("professional", [], [])
                [Str "Professional"],
              Header
                2
                ("email-ethan", [], [])
                [ Span ("", ["todo", "TODO"], []) [Str "TODO"],
                  Space,
                  Str "Email",
                  Space,
                  Str "Ethan",
                  Space,
                  Span
                    ("", ["tag"], [("tag-name", "programming")])
                    [SmallCaps [Str "programming"]],
                  Str "\160",
                  Span
                    ("", ["tag"], [("tag-name", "maths")])
                    [SmallCaps [Str "maths"]]
                ],
              Para
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
                ],
              Para [Link ("", [], []) [Str "Add", Space, Str "Todo"] ("things:///add?title=Email%20Ethan&notes=Blah%20notes%20blah%20%28Things%20API%29%5Bhttps%3A%2F%2Fculturedcode.com%2Fthings%2Fsupport%2Farticles%2F2803573%2F%5D%20%60code%20block%60%20%2Aitalics%2A%20%2A%2Abold%2A%2A.%20%28http%3A%2F%2Fjoeyh.name%2Fblog%2F%29%5Bhttp%3A%2F%2Fjoeyh.name%2Fblog%2F%5D&tags=programming%2Cmaths&list=Professional", "")],
              Header
                2
                ("read-up-on-the-counting-curves-in-strata", [], [])
                [ Span ("", ["todo", "TODO"], []) [Str "TODO"],
                  Space,
                  Str "Read",
                  Space,
                  Str "up",
                  Space,
                  Str "on",
                  Space,
                  Str "the",
                  Space,
                  Str "counting",
                  Space,
                  Str "curves",
                  Space,
                  Str "in",
                  Space,
                  Str "strata"
                ],
              Plain
                [ Strong [Str "SCHEDULED:"],
                  Space,
                  Emph [Str "<2022-06-14 Tue>"]
                ],
              Para
                [ Str "Perhaps",
                  Space,
                  Str "also",
                  Space,
                  Str "email",
                  Space,
                  Str "John."
                ],
              Para [Link ("", [], []) [Str "Add", Space, Str "Todo"] ("things:///add?title=Read%20up%20on%20the%20counting%20curves%20in%20strata&notes=Perhaps%20also%20email%20John.&when=2022-6-14&list=Professional", "")],
              Header
                2
                ("update-personal-website", [], [])
                [ Span ("", ["todo", "TODO"], []) [Str "TODO"],
                  Space,
                  Str "Update",
                  Space,
                  Str "personal",
                  Space,
                  Str "website"
                ],
              Plain
                [ Strong [Str "SCHEDULED:"],
                  Space,
                  Emph [Str "<2022-06-14 Tue>"]
                ],
              Para
                [Str "Things", Space, Str "to", Space, Str "add"],
              BulletList
                [ [Plain [Str "\9744", Space, Str "CV"]],
                  [Plain [Str "\9744", Space, Str "Papers"]],
                  [ Plain
                      [ Str "\9744",
                        Space,
                        Str "Other",
                        Space,
                        Str "things",
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
                ],
              Para [Link ("", [], []) [Str "Add", Space, Str "Todo"] ("things:///add?title=Update%20personal%20website&notes=Things%20to%20add&checklist-items=CV%0APapers%0AOther%20things%20%60code%20block%60%20%2A%2Abold%2A%2A%20and%20%2Aitalics%2A.&when=2022-6-14&list=Professional", "")],
              Header
                2
                ("figure-out-travel", [], [])
                [Str "Figure", Space, Str "out", Space, Str "travel"],
              Para [Link ("", [], []) [Str "Add", Space, Str "Project"] ("things:///add-project?title=Figure%20out%20travel&area=Professional", "")],
              Header
                3
                ("check-out-visa-requirements-for-canada", [], [])
                [ Span ("", ["todo", "TODO"], []) [Str "TODO"],
                  Space,
                  Str "Check",
                  Space,
                  Str "out",
                  Space,
                  Str "visa",
                  Space,
                  Str "requirements",
                  Space,
                  Str "for",
                  Space,
                  Str "Canada"
                ],
              Para [Link ("", [], []) [Str "Add", Space, Str "Todo"] ("things:///add?title=Check%20out%20visa%20requirements%20for%20Canada&list=Figure%20out%20travel", "")],
              Header
                3
                ("qr-study-group", [], [])
                [Str "QR", Space, Str "Study", Space, Str "Group"],
              OrderedList
                (1, DefaultStyle, DefaultDelim)
                [ [ Para
                      [ Span ("", ["todo", "TODO"], []) [Str "TODO"],
                        Space,
                        Str "Prep",
                        Space,
                        Str "material"
                      ],
                    Para [Link ("", [], []) [Str "Add", Space, Str "Todo"] ("things:///add?title=Prep%20material&list=Figure%20out%20travel&heading=QR%20Study%20Group", "")]
                  ],
                  [ Para
                      [ Span ("", ["todo", "TODO"], []) [Str "TODO"],
                        Space,
                        Str "Record",
                        Space,
                        Str "time",
                        Space,
                        Str "spent"
                      ],
                    Para [Link ("", [], []) [Str "Add", Space, Str "Todo"] ("things:///add?title=Record%20time%20spent&list=Figure%20out%20travel&heading=QR%20Study%20Group", "")]
                  ]
                ]
            ]
      let output = Right (output_blocks, [])
      runParser parseArea input_blocks `shouldBe` output
