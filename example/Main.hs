module Main where

import qualified Example.Mongo.Function as Mongo
import qualified Example.Mongo.Monad as MongoM
import qualified Example.MySql.Monad as MySqlM
import Kuery.Language.Base
import Kuery.Monad.Operations
import Kuery.Providers.MySql

main :: IO ()
main = do
  putStrLn "(m)ongo / (s)ql"
  db <- readNextChar
  case db of
    'm' -> do
      putStrLn "(m)onad / (f)unction chain"
      kind <- readNextChar
      case kind of
        'm' -> MongoM.pageUsersM 0 10
        'f' -> Mongo.pageUsers 0 10
        _ -> do
          putStrLn "Unknown input"
          main
    's' -> do
      MySqlM.pageUsersM_sql 0 10
    _ -> do
      putStrLn "Unknown input"
      main

readNextChar :: IO Char
readNextChar = do
  char <- getChar
  case char of
    '\n' -> readNextChar
    _ -> pure char
