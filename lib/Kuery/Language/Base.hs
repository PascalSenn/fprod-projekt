-- Contains all the base types of Kuery
module Kuery.Language.Base where

import Kuery.Language.Value

-- A field that can be selected in a query
newtype Field = Field String deriving (Show)

-- A filter that is used to express the `where` condition
data Filter
  = Eq Field Value
  | Ne Field Value
  | Gt Field Value
  | Gte Field Value
  | Lt Field Value
  | Lte Field Value
  | In Field [Value]
  | Contains Field Value
  | Or Filter Filter
  | And Filter Filter
  deriving (Show)

data OrderBy = Ascending Field | Descending Field deriving (Show)

data Order = Asc String | Desc String deriving (Show)

data Setter = Setter Field Value deriving (Show)

data VariableValue = VariableValue String Value deriving (Show)

data Query
  = Selection
      { selections :: [Field],
        skip :: Maybe Value,
        limit :: Maybe Value,
        filters :: Maybe Filter,
        ordering :: [OrderBy],
        target :: Maybe String
      }
  | Update
      { filters :: Maybe Filter,
        update :: [Setter],
        target :: Maybe String
      }
  | Insert
      { insert :: [[Setter]],
        target :: Maybe String
      }
  | Delete
      { filters :: Maybe Filter,
        target :: Maybe String
      }
  deriving (Show)

var :: QueryValue a => String -> a -> VariableValue
var field value = VariableValue field (format value)
