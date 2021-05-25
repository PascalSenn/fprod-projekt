module Kuery.Providers.MySql.Base where

import Control.Monad.State
import Data.Text
import Kuery.Connection
import Kuery.Language.Base
import Kuery.Language.Result
import Kuery.Monad.Operations
import Kuery.Providers.MySql
import Kuery.Result

toMySql :: DatabaseConnection -> IO (Executor Record)
toMySql conf = do
  return
    Executor
      { db = conf,
        run = mySqlExecutor conf,
        runM = mySqlExecutorM conf
      }

mySqlExecutor :: DatabaseConnection -> Query -> [VariableValue] -> IO (Result RecordList)
mySqlExecutor con q variables =
  do
    case executeMySqlQuery variables q of
      Error a -> do return (Error a)
      Result query -> do return (Result [[RecordField "_id" (RecordValueString "-1"), RecordField "firstName" (RecordValueString query), RecordField "lastName" (RecordValueString "test")]])

mySqlExecutorM :: DatabaseConnection -> State (Result Query) () -> [VariableValue] -> IO (Result RecordList)
mySqlExecutorM con stateQuery variables = do
  let resultQuery = evalState (stateQuery >> _evaluate) (Error "Not valid query specified")
  case resultQuery of
    Error e -> return (Error e)
    Result query -> mySqlExecutor con query variables