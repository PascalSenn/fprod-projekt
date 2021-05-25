module Kuery.Language.Result where

type RecordList = [Record]

type Record = [RecordField]

data RecordField = RecordField String RecordValue deriving (Show)

data RecordValue
  = RecordValueString String
  | RecordValueBool Bool
  | RecordValueNull
  | RecordValueDouble Double
  | RecordValueInt Int
  deriving (Show)

getValueFromRecord :: String -> Record -> Maybe RecordValue
getValueFromRecord _ [] = Nothing
getValueFromRecord label ((RecordField l v) : xs) = if l == label then Just v else getValueFromRecord label xs