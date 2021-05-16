module Main where

import Data.Text
import Database.MongoDB (ObjectId, at)
import Kuery.Language.Base
import Kuery.Language.Operators
import Kuery.Language.Value
import Kuery.Operations
import Kuery.Providers.Mongo
import Kuery.Result

main :: IO ()
main = do
  pageUsers 0 10

pageUsers :: Int -> Int -> IO ()
pageUsers skip'' limit'' = do
  let limit' = if limit'' < 10 then 10 else limit''
  let skip' = if skip'' < 0 then 0 else skip''
  let conn = (Kuery.Providers.Mongo.connect "127.0.0.1" "kuery") {logQueries = True}
  putStrLn ("Users " ++ show skip' ++ " - " ++ show (skip' + limit'))
  res <- execute conn userPageQuery [var "skip" skip', var "limit" limit']
  case res of
    Error a -> putStrLn a
    Result res' -> do
      mapM_ printUser res'
      if limit' > 10
        then putStr "(l)ess/"
        else pure ()
      putStrLn "(m)ore"
      if skip' > 0
        then putStr "(p)revious/"
        else pure ()
      putStrLn "(n)ext"
      userInput <- getChar
      case userInput of
        'n' -> pageUsers (skip' + limit') limit'
        'l' -> pageUsers skip' (limit' - 10)
        'm' -> pageUsers skip' (limit' + 10)
        'p' -> pageUsers (skip' - limit') limit'
        _ -> pageUsers skip' limit'
  where
    userPageQuery =
      _select ["firstName", "lastName", "_id"] `_from` "users"
        `_orderBy` [Asc "firstName"]
        `_skip` Variable "skip"
        `_limit` Variable "limit"
    printUser user =
      putStrLn
        ( show (at (pack "_id") user :: ObjectId)
            ++ "    "
            ++ at (pack "firstName") user
            ++ "    "
            ++ at (pack "lastName") user
        )

mongoSelect :: IO ()
mongoSelect = do
  let conn = (Kuery.Providers.Mongo.connect "127.0.0.1" "kuery") {logQueries = True}
  res <- execute conn selectQuery [var "name" "Marc"]
  -- _ <- execute conn insertQuery []
  -- _ <- execute conn updateQuery []
  -- _ <- execute conn deleteQuery []
  case res of
    Error a -> putStrLn a
    Result res' -> putStr (show res')

selectQuery :: Query
selectQuery =
  _select ["firstName", "lastName"] `_from` "users"
    `_where` ("firstName" `_contains` Variable "name") --- $|| "lastName" $== "Test1")
    `_orderBy` [Desc "lastName", Asc "firstName"]
    `_skip` (20 :: Int)
    `_limit` (10 :: Int)

updateQuery :: Query
updateQuery =
  _update "users"
    `_set` ["firstName" $= "Test1_1"]
    `_where` ("firstName" $== "Test1")

insertQuery :: Query
insertQuery =
  _insert
    [ [ "firstName" $= "Test1",
        "lastName" $= "Test1"
      ],
      [ "firstName" $= "Test2",
        "lastName" $= "Test2"
      ]
    ]
    `_into` "users"

deleteQuery :: Query
deleteQuery =
  _delete
    `_from` "users"
    `_where` ("firstName" $== "Test2")