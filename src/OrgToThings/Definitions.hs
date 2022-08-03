-- {-# LANGUAGE OverloadedStrings #-}

module OrgToThings.Definitions where

import Data.Text

data Todo = Todo
  { todo_title :: Text,
    todo_notes :: Maybe Text,
    todo_checklist :: [Text],
    todo_scheduled :: Maybe Scheduled,
    todo_deadline :: Maybe Deadline,
    todo_tags :: [Text],
    todo_heading :: Maybe Heading,
    todo_project :: Maybe Project,
    todo_area :: Area
  }
  deriving (Show, Eq)

data Heading = Heading
  { heading_title :: Text,
    heading_project :: Project,
    heading_area :: Area
  }
  deriving (Show, Eq)

data Project = Project
  { project_title :: Text,
    project_notes :: Maybe Text,
    project_scheduled :: Maybe Scheduled,
    project_deadline :: Maybe Deadline,
    project_tags :: [Text],
    project_area :: Area
  }
  deriving (Show, Eq)

newtype Area = Area {area_title :: Text}
  deriving (Show, Eq)

data Scheduled = DateTimeS ((Int, Int, Int), (Int, Int)) | DateS (Int, Int, Int)
  deriving (Show, Eq)

newtype Deadline = DateD (Int, Int, Int)
  deriving (Show, Eq)

constructTodo :: Text -> [Text] -> Maybe (Maybe Scheduled, Maybe Deadline) -> Maybe Text -> Maybe [Text] -> Maybe Heading -> Maybe Project -> Area -> Todo
constructTodo title tags optional_planning optional_notes optional_checklist heading project area =
  Todo
    { todo_title = title,
      todo_notes = optional_notes,
      todo_checklist = extractChecklist optional_checklist,
      todo_scheduled = fst =<< optional_planning,
      todo_deadline = snd =<< optional_planning,
      todo_tags = tags,
      todo_heading = heading,
      todo_project = project,
      todo_area = area
    }
  where
    extractChecklist (Just c) = c
    extractChecklist Nothing = []

constructProject :: Text -> [Text] -> Maybe (Maybe Scheduled, Maybe Deadline) -> Maybe Text -> Area -> Project
constructProject title tags optional_planning optional_notes area =
  Project
    { project_title = title,
      project_notes = optional_notes,
      project_scheduled = fst =<< optional_planning,
      project_deadline = snd =<< optional_planning,
      project_tags = tags,
      project_area = area
    }