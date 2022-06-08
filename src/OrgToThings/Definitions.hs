module OrgToThings.Definitions where

import Data.Text

data Todo = Todo
  { todo_title :: Text,
    todo_notes :: Text,
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
