module Kuery.Language.Base where

import Kuery.Language.Value

newtype Field = Field String deriving (Show)

data Filter = Eq Field Value | Gt Field Value | And Filter Filter deriving (Show)

data OrderBy = Asc Field Value | Desc Field Value deriving (Show)

data Query = Query {selections :: [Field], filters :: [Filter], source :: Maybe String} deriving (Show)

_query :: Query
_query = Query {selections = [], filters = [], source = Nothing}