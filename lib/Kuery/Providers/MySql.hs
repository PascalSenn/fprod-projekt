{-# LANGUAGE OverloadedStrings #-}

module Kuery.Providers.MySql (executeMySqlQuery) where

import Control.Monad.Cont
import Data.List (intercalate)
import Data.Text (pack)
import qualified Database.MongoDB as Mongo
import Kuery.Connection
import Kuery.Language.Base
  ( Field (..),
    Filter (..),
    OrderBy (..),
    Query (Delete, Insert, Selection, Update),
    Setter (..),
    VariableValue (..),
  )
import Kuery.Language.Value
import Kuery.Result

executeMySqlQuery :: [VariableValue] -> Query -> Result (String)
executeMySqlQuery variables q =
  do
    let l = variableLookup variables
    case q of
      -- Selection
      Selection selections' skip' limit' filter' ordering' target' ->
        do
          select <- createSelect selections'
          from <- createFrom target'
          _where <- createWhere l filter'
          orderBy <- createOrderBy ordering'
          limit <- createLimit l limit'
          offset <- createOffset l skip'
          Result $ intercalate "" [select, from, _where, orderBy, limit, offset]
      -- Update
      Update filter' update' target' -> do
        return ""
      -- Insert
      Insert insert' target' -> do
        return ""
      -- Delete
      Delete filter' target' -> do
        return ""

createSelect :: [Field] -> Result String
createSelect [] = Result "Cannot create select clause. Not fields specified."
createSelect fields = Result $ "SELECT " ++ intercalate "," (map (\(Field f) -> "`" ++ f ++ "`") fields)

createFrom :: Maybe String -> Result String
createFrom (Just target) = Result $ "FROM `" ++ target ++ "`"
createFrom Nothing = Error "Cannot create from clause. No table specified."

createWhere :: VariableLookup -> Maybe Filter -> Result String
createWhere l (Just f) =
  do
    w <- createSingleWhere l f
    pure $ " WHERE " ++ w
createWhere _ Nothing = Result ""

createSingleWhere :: VariableLookup -> Filter -> Result String
createSingleWhere lookup' (And l r) =
  do
    left <- createSingleWhere lookup' l
    right <- createSingleWhere lookup' r
    pure $ "(" ++ left ++ ") AND (" ++ right ++ ")"
createSingleWhere lookup' (Or l r) =
  do
    left <- createSingleWhere lookup' l
    right <- createSingleWhere lookup' r
    pure $ "(" ++ left ++ ") OR (" ++ right ++ ")"
createSingleWhere l (Eq (Field f) v) = createSimpleOperator "=" l f v
createSingleWhere l (Ne (Field f) v) = createSimpleOperator "<>" l f v
createSingleWhere l (Gt (Field f) v) = createSimpleOperator ">" l f v
createSingleWhere l (Gte (Field f) v) = createSimpleOperator ">=" l f v
createSingleWhere l (Lt (Field f) v) = createSimpleOperator "<" l f v
createSingleWhere l (Lte (Field f) v) = createSimpleOperator "<=" l f v
createSingleWhere _ (Contains (Field f) (ValueString val)) = pure $ "`" ++ f ++ "` LIKE '%" ++ val ++ "%'"
createSingleWhere l (Contains (Field f) (Variable var)) = do
  variableValue <- l var
  createSingleWhere l (Contains (Field f) variableValue)
createSingleWhere _ (Contains (Field _) v) = Error ("Contains operation only supports string values. But provided value is " ++ show v)
createSingleWhere _ v = Error ("Unkown operation: " ++ show v)

createSimpleOperator :: String -> VariableLookup -> String -> Value -> Result String
createSimpleOperator operator l field value =
  do
    val <- toSqlValue l value
    pure $ "`" ++ field ++ "`" ++ operator ++ val

toSqlValue :: VariableLookup -> Value -> Result String
toSqlValue _ (ValueString v) = Result $ "'" ++ v ++ "'"
toSqlValue _ (ValueBool v) = Result $ if v then "1" else "0"
toSqlValue _ ValueNull = Result "null"
toSqlValue _ (ValueDouble v) = Result $ show v
toSqlValue _ (ValueInt v) = Result $ show v
toSqlValue l (Variable v) = do
  variableValue <- l v
  toSqlValue l variableValue

createOrderBy :: [OrderBy] -> Result String
createOrderBy [] = Result ""
createOrderBy orderBys = do
  os <- mapM createSingleOrderBy orderBys
  pure $ " ORDER BY " ++ intercalate ", " os

createSingleOrderBy :: OrderBy -> Result String
createSingleOrderBy (Ascending (Field f)) = Result $ "`" ++ f ++ "` ASC"
createSingleOrderBy (Descending (Field f)) = Result $ "`" ++ f ++ "` DESC"

createLimit :: VariableLookup -> Maybe Value -> Result String
createLimit _ Nothing = Result ""
createLimit l (Just limit) = do
  iLimit <- toInt l limit "Only integers are allowed for limit."
  pure $ " LIMIT " ++ show iLimit

createOffset :: VariableLookup -> Maybe Value -> Result String
createOffset _ Nothing = Result ""
createOffset l (Just skip) = do
  iSkip <- toInt l skip "Only integers are allowed for skip."
  pure $ " OFFSET " ++ show iSkip

toInt :: VariableLookup -> Value -> String -> Result Int
toInt _ (ValueInt v) _ = Result v
toInt l (Variable v) errMsg = do
  val <- l v
  case val of
    ValueInt i -> Result i
    _ -> Error errMsg
toInt _ _ errMsg = Error errMsg

type VariableLookup = String -> Result Value

variableLookup :: [VariableValue] -> String -> Result Value
variableLookup [] search = Error ("Variable with name " ++ search ++ "was not found")
variableLookup ((VariableValue name val) : fs) search =
  if name == search
    then Result val
    else variableLookup fs search