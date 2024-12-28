module Main where

import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as L
import Diff

main :: IO ()
main = do
  in1 <- L.readFile "f1.json"
  in2 <- L.readFile "f2.json"
  case J.decode in1 of
    Just (J.Object v1) ->
      case J.decode in2 of
        Just (J.Object v2) ->
          let res = diffObj v1 v2
           in print $ toString res
        _ -> print "Failed to parse in2"
    _ -> print "Failed to parse in1"
