{-# LANGUAGE OverloadedStrings #-}

module OrgToThings
  ( mainFunc,
  )
where

import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.IO.Class
import qualified Data.Text.IO as TIO
import OrgToThings.Parser (parseAndTransformBlocks)
import Safe (headMay)
import System.Environment
import System.Exit
import Text.Pandoc

mainFunc :: IO ()
mainFunc = do
  args <- getArgs
  let optHead = headMay args
  case optHead of
    Nothing -> do
      putStrLn "Expected an input file"
      exitFailure
    Just filename -> do
      agendaSource <- TIO.readFile filename
      result <- runIO $ do
        agenda <- readOrg def agendaSource
        let parsedAndTransformed = parseAndTransformBlocks agenda
        liftIO $ putStrLn $ show parsedAndTransformed
        case parsedAndTransformed of
          Left _ -> writeOrg def agenda
          Right transformedAgenda -> writeOrg def transformedAgenda
      html <- handleError result
      TIO.putStrLn html
