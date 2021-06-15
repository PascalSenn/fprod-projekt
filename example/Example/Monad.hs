module Example.Monad where

import Data.Text
import Database.MongoDB (ObjectId, at)
import Kuery.Connection
import Kuery.Language.Base
import Kuery.Language.Operators
import Kuery.Language.Result
import Kuery.Language.Value
import Kuery.Monad.Operations
import Kuery.Providers.Mongo.Base
import Kuery.Providers.MySql.Base
import Kuery.Result

pageUsersM :: Provider -> IO DatabaseConnection -> Int -> Int -> IO ()
pageUsersM provider con skip'' limit'' = do
  let limit' = if limit'' < 10 then 10 else limit''
  let skip' = if skip'' < 0 then 0 else skip''
  putStrLn ("Users " ++ show skip' ++ " - " ++ show (skip' + limit'))

  res <-
    case provider of
      MongoProvider ->
        con >>= enableLogging
          >>= toMongo
          >>= executeM userPageQuery [var "skip" skip', var "limit" limit']
      MySqlProvider ->
        con >>= enableLogging
          >>= toMySql
          >>= executeM userPageQuery [var "skip" skip', var "limit" limit']

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
      waitForNextInput skip' limit'
  where
    userPageQuery =
      do
        _select ["firstName", "lastName", "_id"]
        _from "users"
        _orderBy [Asc "firstName"]
        _skip (Variable "skip")
        _limit (Variable "limit")
    printUser user = do
      putStrLn
        ( show (getValueFromRecord "_id" user)
            ++ "    "
            ++ show (getValueFromRecord "firstName" user)
            ++ "    "
            ++ show (getValueFromRecord "lastName" user)
        )
    waitForNextInput skip' limit' = do
      userInput <- getChar
      case userInput of
        'n' -> pageUsersM provider con (skip' + limit') limit'
        'l' -> pageUsersM provider con skip' (limit' - 10)
        'm' -> pageUsersM provider con skip' (limit' + 10)
        'p' -> pageUsersM provider con (skip' - limit') limit'
        _ -> waitForNextInput skip' limit'

selectQuery :: MonadQuery
selectQuery =
  do
    _select ["firstName", "lastName"]
    _from "users"
    _where ("firstName" `_contains` Variable "name")
    _orderBy [Desc "lastName", Asc "firstName"]
    _skip (20 :: Int)
    _limit (10 :: Int)

updateQuery :: MonadQuery
updateQuery =
  do
    _update "users"
    _set ["firstName" $= "Test1_1"]
    _where ("firstName" $== "Test1")

insertQuery :: MonadQuery
insertQuery =
  do
    _insert
      [ [ "firstName" $= "Test1",
          "lastName" $= "Test1"
        ],
        [ "firstName" $= "Test2",
          "lastName" $= "Test2"
        ]
      ]
    _into "users"

deleteQuery :: MonadQuery
deleteQuery =
  do
    _delete
    _from "users"
    _where ("firstName" $== "Test2")