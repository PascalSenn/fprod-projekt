module Kuery.Providers.Mongo.Base where

import Control.Monad.State
import Data.Text
import qualified Database.MongoDB as Mongo
import Kuery.Connection
import Kuery.Language.Base
import Kuery.Providers.Mongo
import Kuery.Monad.Operations
import Kuery.Result

toMongo :: DatabaseConnection -> IO (Executor Mongo.Document)
toMongo conf = do
  return
    Executor
      { db = conf,
        run = mongoExecutor conf,
        runM = mongoExecutorM conf
      }

mongoExecutor :: DatabaseConnection -> Query -> [VariableValue] -> IO (Result [Mongo.Document])
mongoExecutor con q variables =
  do
    p <- Mongo.connect $ Mongo.host (host con)
    let r act = Mongo.access p Mongo.master (pack (databaseName con)) act
    case executeMongoQuery con variables q of
      Error a -> do return (Error a)
      Result query -> do
        res <- r query
        return (Result res)

mongoExecutorM :: DatabaseConnection -> State (Result Query) () -> [VariableValue] -> IO (Result [Mongo.Document])
mongoExecutorM con stateQuery variables = do
  let resultQuery = evalState (stateQuery >> _evaluate) (Error "Not valid query specified")
  case resultQuery of
    Error e -> return (Error e)
    Result query -> mongoExecutor con query variables 