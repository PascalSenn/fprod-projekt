module Kuery.Providers.MySql.Base where

import Control.Monad.State
import Data.Text
import Kuery.Connection
import Kuery.Language.Base
import Kuery.Monad.Operations
import Kuery.Providers.MySql
import Kuery.Result

toMySql :: DatabaseConnection -> IO (Executor String)
toMySql conf = do
  return
    Executor
      { db = conf,
        run = mySqlExecutor conf,
        runM = mySqlExecutorM conf
      }

mySqlExecutor :: DatabaseConnection -> Query -> [VariableValue] -> IO (Result [String])
mySqlExecutor con q variables =
  do
    case executeMySqlQuery variables q of
      Error a -> do return (Error a)
      Result query -> do return (Result [query])

mySqlExecutorM :: DatabaseConnection -> State (Result Query) () -> [VariableValue] -> IO (Result [String])
mySqlExecutorM con stateQuery variables = do
  let resultQuery = evalState (stateQuery >> _evaluate) (Error "Not valid query specified")
  case resultQuery of
    Error e -> return (Error e)
    Result query -> mySqlExecutor con query variables