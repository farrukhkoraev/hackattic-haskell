module Main where

import Diff

main :: IO ()
main = do
  runDiff "f1.json" "f2.json"
