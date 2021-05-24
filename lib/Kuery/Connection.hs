module Kuery.Connection where

import Control.Monad.State
import Kuery.Language.Base
import Kuery.Result

connect :: String -> String -> IO DatabaseConnection
connect h d = do
  return
    DatabaseConnection
      { host = h,
        databaseName = d,
        logQueries = False
      }

data DatabaseConnection = DatabaseConnection
  { host :: String,
    databaseName :: String,
    logQueries :: Bool
  }

data Executor a = Executor
  { db :: DatabaseConnection,
    run :: Query -> [VariableValue] -> IO (Result [a]),
    runM :: State (Result Query) () -> [VariableValue] -> IO (Result [a])
  }

execute :: Query -> [VariableValue] -> Executor a -> IO (Result [a])
execute q v a = do
  run a q v

executeM :: State (Result Query) () -> [VariableValue] -> Executor a -> IO (Result [a])
executeM q v a = do
  runM a q v

enableLogging :: DatabaseConnection -> IO DatabaseConnection
enableLogging config = do
  return (config {logQueries = True})