module Main where

import qualified Example.Mongo.Function as Mongo
import qualified Example.Mongo.Monad as MongoM

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
        putStrLn "Not implemented"
        main
    _ -> do 
      putStrLn "Unknown input"
      main

readNextChar :: IO Char
readNextChar = do
  char <- getChar
  case char of 
    '\n' -> readNextChar
    _ -> pure char 
