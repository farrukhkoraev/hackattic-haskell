{-# LANGUAGE OverloadedStrings #-}

module Diff (diffObj, toString) where

import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM

data Status = A | U | R | M deriving (Show) -- A -added, U-unchanged, R-removed, M-modified

type ResultMap = KM.KeyMap [(Status, ResultVal)]

data ResultVal = RVal J.Value | RMap ResultMap deriving (Show)

diffObj :: J.Object -> J.Object -> ResultMap
diffObj o1 o2 = foldr run KM.empty (KM.keys o1 ++ KM.keys o2)
  where
    run :: KM.Key -> ResultMap -> ResultMap
    run k = diff k (KM.lookup k o1) (KM.lookup k o2)

diff :: KM.Key -> Maybe J.Value -> Maybe J.Value -> ResultMap -> ResultMap
-- check diff in nested objects
diff k (Just (J.Object o1)) (Just (J.Object o2)) res
  | o1 /= o2 =
      let r = diffObj o1 o2
       in KM.insert k [(M, RMap r)] res
  | otherwise = KM.insert k [(U, RVal $ J.Object o1)] res
-- key absent from 2nd json
diff k (Just v1) Nothing r = KM.insert k [(R, RVal v1)] r
-- key absent from 1st json
diff k Nothing (Just v1) r = KM.insert k [(A, RVal v1)] r
-- check diff in flat values
diff k (Just v1) (Just v2) r
  | v1 /= v2 = KM.insert k [(R, RVal v1), (A, RVal v2)] r
  | otherwise = KM.insert k [(U, RVal v1)] r
--- unreachable case in theory ))
diff _ _ _ r = r

-- Printing
toStringVal :: J.Value -> String
toStringVal (J.Bool b) = if b then "true" else "false"
toStringVal (J.Number n) = show n
toStringVal (J.String s) = show s
toStringVal (J.Array a) = "[" ++ foldr (\x acc -> acc ++ toStringVal x ++ ", ") "" a ++ "]"
toStringVal J.Null = "null"
toStringVal _ = ""

toStringKV :: KM.Key -> [(Status, ResultVal)] -> String
toStringKV k = foldr run ""
  where
    run :: (Status, ResultVal) -> String -> String
    run (U, v) res =
      res ++ show k ++ ":" ++ case v of
        (RMap m) -> toString m ++ ", "
        (RVal x) -> toStringVal x ++ ", "
    run (M, v) res =
      res ++ show k ++ ":" ++ case v of
        (RMap m) -> toString m ++ ", "
        (RVal x) -> toStringVal x ++ ", "
    run (A, v) res =
      res ++ "+" ++ show k ++ ":" ++ case v of
        (RMap m) -> toString m ++ ", "
        (RVal x) -> toStringVal x ++ ", "
    run (R, v) res =
      res ++ "-" ++ show k ++ ":" ++ case v of
        (RMap m) -> toString m ++ ", "
        (RVal x) -> toStringVal x ++ ", "

toString :: ResultMap -> String
toString m = "{ " ++ foldr run "" (KM.keys m) ++ " }"
  where
    run :: KM.Key -> String -> String
    run k res = case KM.lookup k m of
      Just xs -> res ++ toStringKV k xs ++ " "
      _ -> res
