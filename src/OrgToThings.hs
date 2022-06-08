module OrgToThings
  ( convertToHTML,
    readFileAsArg,
    parseOrgToAST,
  )
where

import Control.Exception (try)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.IO.Exception (IOException)
import Safe (headMay)
import Text.Pandoc

-- | This should ideally complain len args > 1.
readFileAsArg :: [String] -> Maybe (IO (Either IOException T.Text))
readFileAsArg args = try . TIO.readFile <$> headMay args

parseOrgToAST :: T.Text -> Either PandocError Pandoc
parseOrgToAST rawOrg = runPure $ readOrg def rawOrg

convertToHTML :: IO ()
convertToHTML = putStrLn "someFunc"
