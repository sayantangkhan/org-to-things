{-# LANGUAGE OverloadedStrings #-}

module OrgToThings.Linkgen where

import Data.Text (Text, pack)
import Network.URI.Encode (encodeText)
import OrgToThings.Definitions (Deadline (..), Heading (heading_title), Scheduled (..), Todo (todo_area, todo_checklist, todo_deadline, todo_heading, todo_notes, todo_project, todo_scheduled, todo_tags, todo_title), area_title, project_title)

-- Also implement error detection
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

intersperse :: Text -> [Text] -> Text
intersperse _ [] = ""
intersperse _ [x] = x
intersperse y (x : xs) = mconcat [x, y, intersperse y xs]

scheduledToString :: Scheduled -> Text
scheduledToString (DateS (year, month, day)) = pack $ show year <> "-" <> show month <> "-" <> show day
scheduledToString (DateTimeS ((year, month, day), (hour, minute))) = pack $ show year <> "-" <> show month <> "-" <> show day <> "@" <> show hour <> ":" <> show minute

deadlineToString :: Deadline -> Text
deadlineToString (DateD (year, month, day)) = pack $ show year <> "-" <> show month <> "-" <> show day