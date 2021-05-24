{-# LANGUAGE OverloadedStrings #-}

module Kuery.Providers.Mongo (executeMongoQuery) where

import Control.Monad.Cont
import Data.Text (pack)
import qualified Database.MongoDB as Mongo
import Kuery.Connection
import Kuery.Language.Base
import Kuery.Language.Value
import Kuery.Result

executeMongoQuery :: DatabaseConnection -> [VariableValue] -> Query -> Result (Mongo.Action IO [Mongo.Document])
executeMongoQuery config variables q =
  do
    let l = variableLookup variables
    case q of
      -- Selection
      Selection selections' skip' limit' filter' ordering' target' ->
        do
          t <- readTarget target'
          select <- createSelect l filter'
          query <-
            Result
              (Mongo.select select (pack t))
                { Mongo.project = createProjection selections',
                  Mongo.sort = createSort ordering'
                }
              >>= applySkip l skip'
              >>= applyLimit l limit'
          return
            ( execute' query >>= Mongo.rest
            )
        where
          execute' query = do
            if logQueries config
              then do
                liftIO $ do
                  putStrLn "[Select]"
                  putStrLn "Query="
                  print query
              else pure ()
            Mongo.find query

      -- Update
      Update filter' update' target' -> do
        t <- readTarget target'
        select <- createSelect l filter'
        modify <- createModify l update'
        let query = Mongo.select select (pack t)
        return (execute' query modify >>= \() -> do pure [])
        where
          execute' query modify = do
            if logQueries config
              then do
                liftIO $ do
                  putStrLn "[Update]"
                  putStrLn "Query="
                  print query
                  putStrLn "Update="
                  print modify
              else pure ()
            Mongo.modify query modify

      -- Insert
      Insert insert' target' -> do
        t <- readTarget target'
        modify <- mapM (createInsert l) insert'
        return (execute' t modify >> pure [])
        where
          execute' t modify = do
            if logQueries config
              then do
                liftIO $ do
                  putStrLn "[Insert]"
                  putStrLn "Update="
                  print modify
              else pure ()
            Mongo.insertAll (pack t) modify

      -- Delete
      Delete filter' target' -> do
        t <- readTarget target'
        select <- createSelect l filter'
        let query = Mongo.select select (pack t)
        return (execute' query >>= \() -> pure [])
        where
          execute' query = do
            if logQueries config
              then do
                liftIO $ do
                  putStrLn "[Delete]"
                  putStrLn "query="
                  print query
              else pure ()
            Mongo.delete query

readTarget :: Maybe String -> Result String
readTarget (Just target') = Result target'
readTarget Nothing = Error "No collection/table specified. User _from to specify the target"

applySkip :: VariableLookup -> Maybe Value -> Mongo.Query -> Result Mongo.Query
applySkip _ Nothing q = Result q
applySkip _ (Just (ValueInt int)) q = Result q {Mongo.skip = fromIntegral int}
applySkip l (Just (Variable str)) q = do
  value <- l str
  case value of
    ValueInt int -> Result q {Mongo.skip = fromIntegral int}
    _ -> Error "Only integers are allowed for skip"
applySkip _ _ _ = Error "Only integers are allowed for skip"

applyLimit :: VariableLookup -> Maybe Value -> Mongo.Query -> Result Mongo.Query
applyLimit _ Nothing q = Result q
applyLimit _ (Just (ValueInt int)) q = Result q {Mongo.limit = fromIntegral int}
applyLimit l (Just (Variable str)) q = do
  value <- l str
  case value of
    ValueInt int -> Result q {Mongo.limit = fromIntegral int}
    _ -> Error "Only integers are allowed for limit"
applyLimit _ _ _ = Error "Only integers are allowed for limit"

createProjection :: [Field] -> Mongo.Projector
createProjection = map (\(Field f) -> pack f Mongo.=: (1 :: Int))

createSort :: [OrderBy] -> Mongo.Order
createSort = map createSingleSort

createSingleSort :: OrderBy -> Mongo.Field
createSingleSort (Ascending (Field f)) = pack f Mongo.=: (1 :: Int)
createSingleSort (Descending (Field f)) = pack f Mongo.=: (-1 :: Int)

createInsert :: VariableLookup -> [Setter] -> Result Mongo.Modifier
createInsert l = mapM (createSingleInsert l)

createSingleInsert :: VariableLookup -> Setter -> Result Mongo.Field
createSingleInsert l (Setter (Field f) v) = do
  value <- toBson l v
  return (pack f Mongo.=: value)

createModify :: VariableLookup -> [Setter] -> Result Mongo.Modifier
createModify l = mapM (createSingleModify l)

createSingleModify :: VariableLookup -> Setter -> Result Mongo.Field
createSingleModify l (Setter (Field f) v) = do
  value <- toBson l v
  return ("$set" Mongo.=: (pack f Mongo.=: value))

createSelect :: VariableLookup -> Maybe Filter -> Result Mongo.Selector
createSelect l Nothing = mapM (createSingleSelect l) []
createSelect l (Just f) = mapM (createSingleSelect l) [f]

createSingleSelect :: VariableLookup -> Filter -> Result Mongo.Field
createSingleSelect lookup' (And l r) =
  do
    left <- createSingleSelect lookup' l
    right <- createSingleSelect lookup' r
    return ("$and" Mongo.=: [[left], [right]])
createSingleSelect lookup' (Or l r) =
  do
    left <- createSingleSelect lookup' l
    right <- createSingleSelect lookup' r
    return ("$or" Mongo.=: [[left], [right]])
createSingleSelect l (Eq (Field f) v) = createSimpleOperator "$eq" l f v
createSingleSelect l (Ne (Field f) v) = createSimpleOperator "$ne" l f v
createSingleSelect l (Gt (Field f) v) = createSimpleOperator "$gt" l f v
createSingleSelect l (Gte (Field f) v) = createSimpleOperator "$gte" l f v
createSingleSelect l (Lt (Field f) v) = createSimpleOperator "$lt" l f v
createSingleSelect l (Lte (Field f) v) = createSimpleOperator "$lte" l f v
createSingleSelect _ (Contains (Field f) (ValueString str)) =
  do return (pack f Mongo.=: ("$regex" Mongo.=: concat [".*", str, ".*"]))
createSingleSelect l (Contains f (Variable v)) = do
  variableValue <- l v
  createSingleSelect l (Contains f variableValue)
createSingleSelect l (In (Field f) values) =
  do
    val <- mapM (toBson l) values
    return (pack f Mongo.=: ("$in" Mongo.=: val))
createSingleSelect _ v = Error ("Unkown operation: " ++ show v)

createSimpleOperator :: String -> VariableLookup -> String -> Value -> Result Mongo.Field
createSimpleOperator operator l field value =
  do
    val <- toBson l value
    return (pack field Mongo.=: [pack operator Mongo.=: val])

toBson :: VariableLookup -> Value -> Result Mongo.Value
toBson _ (ValueString v) = Result (Mongo.val v)
toBson _ (ValueBool v) = Result (Mongo.val v)
toBson _ ValueNull = Result Mongo.Null
toBson _ (ValueDouble v) = Result (Mongo.val v)
toBson _ (ValueInt v) = Result (Mongo.val v)
toBson l (Variable v) = do
  variableValue <- l v
  toBson l variableValue

type VariableLookup = String -> Result Value

variableLookup :: [VariableValue] -> String -> Result Value
variableLookup [] search = Error ("Variable with name " ++ search ++ "was not found")
variableLookup ((VariableValue name val) : fs) search =
  if name == search
    then Result val
    else variableLookup fs search