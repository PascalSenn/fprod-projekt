module Main where

import qualified Example.Function as FnExample
import qualified Example.Monad as MonadExample
import Kuery.Connection
import Kuery.Result

main :: IO ()
main = do
  putStrLn "(m)ongo / (s)ql"
  dbName <- readNextChar
  case getProvider dbName of
    (Error e) -> putStrLn e
    (Result p) -> do
      putStrLn "(m)onad / (f)unction chain"
      kind <- readNextChar
      case kind of
        'm' -> MonadExample.pageUsersM p (getDatabaseConnection p) 0 10
        'f' -> FnExample.pageUsers p 0 10
        _ -> do
          putStrLn "Unknown input"
          main

readNextChar :: IO Char
readNextChar = do
  char <- getChar
  case char of
    '\n' -> readNextChar
    _ -> pure char

getProvider :: Char -> Result Provider
getProvider 'm' = Result MongoProvider
getProvider 's' = Result MySqlProvider
getProvider p = Error $ "Unknown provider " ++ show p

getDatabaseConnection :: Provider -> IO DatabaseConnection
getDatabaseConnection MongoProvider = connect "127.0.0.1" "kuery"
getDatabaseConnection MySqlProvider = connect "127.0.0.1" "kuery"
