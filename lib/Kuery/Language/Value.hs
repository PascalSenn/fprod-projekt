{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Kuery.Language.Value where

data Value
  = ValueString String
  | ValueBool Bool
  | ValueNull
  | ValueDouble Double
  | ValueInt Int
  | Variable String
  deriving (Show)

class QueryValue a where
  format :: a -> Value
  parse :: Value -> a

instance QueryValue String where
  format a = ValueString a
  parse (ValueString s) = s
  parse _ = ""

instance QueryValue Bool where
  format a = ValueBool a
  parse (ValueBool s) = s
  parse _ = False

instance QueryValue Double where
  format a = ValueDouble a
  parse (ValueDouble s) = s
  parse _ = 0

instance QueryValue Int where
  format a = ValueInt a
  parse (ValueInt s) = s
  parse _ = 0

instance QueryValue Value where
  format a = a
  parse s = s