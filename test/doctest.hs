module Test.DocTest where

import           TestImport

main :: IO ()
main = doctest ["Handler/Root.hs"]
