module Main where

import Data.Text (pack)
import Database.MongoDB
import Kuery.Language.Operators
import Kuery.Operations
import Kuery.Providers.Mongo

main :: IO ()
main = do 
  mongoTest

mongoTest :: IO ()
mongoTest = do
  pipe <- connect $ host "127.0.0.1"
  let run act = access pipe master (pack "peergrading") act
  let a = _q `_select` ["foo", "bar"] `_from` "projects" `_where` ("foo" $== "1" $&& "bar" $== (2 :: Int))
  case execute a of
    Nothing -> putStrLn "error"
    Just res -> do
      r <- run $ res >>= rest
      putStrLn (show r)
