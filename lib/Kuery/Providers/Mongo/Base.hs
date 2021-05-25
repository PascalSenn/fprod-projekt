module Kuery.Providers.Mongo.Base where

import Control.Monad.State
import Data.Text (pack, unpack)
import qualified Database.MongoDB as Mongo
import Kuery.Connection
  ( DatabaseConnection (databaseName, host),
    Executor (..),
  )
import Kuery.Language.Base
import Kuery.Language.Result
import Kuery.Monad.Operations
import Kuery.Providers.Mongo
import Kuery.Result

toMongo :: DatabaseConnection -> IO (Executor Record)
toMongo conf = do
  return
    Executor
      { db = conf,
        run = mongoExecutor conf,
        runM = mongoExecutorM conf
      }

mongoExecutor :: DatabaseConnection -> Query -> [VariableValue] -> IO (Result RecordList)
mongoExecutor con q variables =
  do
    p <- Mongo.connect $ Mongo.host (host con)
    let r act = Mongo.access p Mongo.master (pack (databaseName con)) act
    case executeMongoQuery con variables q of
      Error a -> do return (Error a)
      Result query -> do
        res <- r query
        pure $ mapM documentToRecord res

mongoExecutorM :: DatabaseConnection -> State (Result Query) () -> [VariableValue] -> IO (Result RecordList)
mongoExecutorM con stateQuery variables = do
  let resultQuery = evalState (stateQuery >> _evaluate) (Error "Not valid query specified")
  case resultQuery of
    Error e -> return (Error e)
    Result query -> mongoExecutor con query variables

documentToRecord :: Mongo.Document -> Result Record
documentToRecord doc = mapM fieldToRecordField doc

fieldToRecordField :: Mongo.Field -> Result RecordField
fieldToRecordField f = do
  val <- fieldToRecordValue (Mongo.value f)
  pure $ RecordField (unpack (Mongo.label f)) val

fieldToRecordValue :: Mongo.Value -> Result RecordValue
fieldToRecordValue (Mongo.Float d) = Result $ RecordValueDouble d
fieldToRecordValue (Mongo.String t) = Result $ RecordValueString (unpack t)
fieldToRecordValue (Mongo.ObjId i) = Result $ RecordValueString (show i)
fieldToRecordValue (Mongo.Bool b) = Result $ RecordValueBool b
fieldToRecordValue (Mongo.Null) = Result $ RecordValueNull
fieldToRecordValue v = Error $ "Could not parse Mongo.Value " ++ (show v)
