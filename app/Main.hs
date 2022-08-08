module Main where

import OrgToThings (filterFunc)
import Text.Pandoc.JSON

main :: IO ()
main = toJSONFilter filterFunc
