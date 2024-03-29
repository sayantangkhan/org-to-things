{-# LANGUAGE OverloadedStrings #-}

-- |
-- This module generates the [Things links](https://culturedcode.com/things/support/articles/2803573/)
-- for Todos and Projects.
module OrgToThings.Linkgen (linkFromTodo, linkFromProject) where

import Data.Text (Text, pack)
import Network.URI.Encode (encodeText)
import OrgToThings.Definitions
  ( Deadline (..),
    Heading (..),
    Project (..),
    Scheduled (..),
    Todo (..),
    area_title,
    project_title,
  )
import Text.Pandoc.Definition

-- | Creates a link to add Todo according to [these specifications](https://culturedcode.com/things/support/articles/2803573/#add)
createTodoLink :: Todo -> Text
createTodoLink todo =
  mconcat
    [ "things:///add?",
      intersperse
        "&"
        ( filter
            (/= "")
            [ genTitle,
              genNotes,
              genChecklistItems,
              genWhen,
              genDeadline,
              genTags,
              genProjectOrArea,
              genHeading
            ]
        )
    ]
  where
    genTitle = mappend "title=" $ encodeText $ todo_title todo
    genNotes = case todo_notes todo of
      Just notes -> mappend "notes=" $ encodeText notes
      Nothing -> ""
    genChecklistItems = case todo_checklist todo of
      [] -> ""
      checklist -> mappend "checklist-items=" $ encodeText $ intersperse "\n" checklist
    genWhen = case todo_scheduled todo of
      Just scheduled -> mappend "when=" $ scheduledToString scheduled
      Nothing -> ""
    genDeadline = case todo_deadline todo of
      Just deadline -> mappend "deadline=" $ deadlineToString deadline
      Nothing -> ""
    genTags = case todo_tags todo of
      [] -> ""
      tags -> mappend "tags=" $ encodeText $ intersperse "," tags
    genProjectOrArea = case todo_project todo of
      Just project -> mappend "list=" $ encodeText $ project_title project
      Nothing -> mappend "list=" $ encodeText $ area_title $ todo_area todo
    genHeading = case todo_heading todo of
      Just heading -> mappend "heading=" $ encodeText $ heading_title heading
      Nothing -> ""

-- | Creates a link to add Project according to [these specifications](https://culturedcode.com/things/support/articles/2803573/#add-project)
createProjectLink :: Project -> Text
createProjectLink project =
  mconcat
    [ "things:///add-project?",
      intersperse
        "&"
        ( filter
            (/= "")
            [ genTitle,
              genNotes,
              genWhen,
              genDeadline,
              genTags,
              genArea
            ]
        )
    ]
  where
    genTitle = mappend "title=" $ encodeText $ project_title project
    genNotes = case project_notes project of
      Just notes -> mappend "notes=" $ encodeText notes
      Nothing -> ""
    genWhen = case project_scheduled project of
      Just scheduled -> mappend "when=" $ scheduledToString scheduled
      Nothing -> ""
    genDeadline = case project_deadline project of
      Just deadline -> mappend "deadline=" $ deadlineToString deadline
      Nothing -> ""
    genTags = case project_tags project of
      [] -> ""
      tags -> mappend "tags=" $ encodeText $ intersperse "," tags
    genArea = mappend "area=" $ encodeText $ area_title $ project_area project

intersperse :: Text -> [Text] -> Text
intersperse _ [] = ""
intersperse _ [x] = x
intersperse y (x : xs) = mconcat [x, y, intersperse y xs]

scheduledToString :: Scheduled -> Text
scheduledToString (DateS (year, month, day)) = pack $ show year <> "-" <> show month <> "-" <> show day
scheduledToString (DateTimeS ((year, month, day), (hour, minute))) = pack $ show year <> "-" <> show month <> "-" <> show day <> "@" <> show hour <> ":" <> show minute
scheduledToString Someday = "someday"

deadlineToString :: Deadline -> Text
deadlineToString (DateD (year, month, day)) = pack $ show year <> "-" <> show month <> "-" <> show day

linkFromTodo :: Todo -> [Block]
linkFromTodo todo =
  [ Para
      [ Link
          ("", [], [])
          [Str "Add", Space, Str "Todo"]
          (createTodoLink todo, "")
      ]
  ]

linkFromProject :: Project -> [Block]
linkFromProject project =
  [ Para
      [ Link
          ("", [], [])
          [Str "Add", Space, Str "Project"]
          (createProjectLink project, "")
      ]
  ]
