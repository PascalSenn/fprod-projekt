{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Kuery.Language.Value where

data Value = ValueString String | ValueInt Int | Variable String deriving (Show)

class QueryValue a where
  format :: a -> Value
  parse :: Value -> Maybe a

instance QueryValue String where
  format a = ValueString a
  parse (ValueString s) = Just s
  parse _ = Nothing

instance QueryValue Int where
  format a = ValueInt a
  parse (ValueInt s) = Just s
  parse _ = Nothing