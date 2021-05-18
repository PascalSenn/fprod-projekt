module Kuery.Connection where

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

enableLogging :: DatabaseConnection -> IO DatabaseConnection
enableLogging config = do
  return (config {logQueries = True})