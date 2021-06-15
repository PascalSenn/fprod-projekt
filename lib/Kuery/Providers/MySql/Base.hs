{-# LANGUAGE OverloadedStrings #-}

module Kuery.Providers.MySql.Base where

import Control.Monad.State
import qualified Data.ByteString as B
import Data.ByteString.Lazy.Char8 as Char8
import Data.ByteString.Lazy.UTF8 as BLU
import Data.Char (ord)
import qualified Data.Text as Text
import qualified Database.MySQL.Base as MySQL
import Kuery.Connection
import Kuery.Language.Base
import Kuery.Language.Result
import Kuery.Monad.Operations
import Kuery.Providers.MySql
import Kuery.Result
import qualified System.IO.Streams as Streams

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
    conn <-
      MySQL.connect
        MySQL.defaultConnectInfo {MySQL.ciUser = "root", MySQL.ciPassword = "password", MySQL.ciDatabase = "fprod"}
    case createMySqlQuery variables q of
      Error a -> do return (Error a)
      Result query -> do
        if logQueries con
          then do
            Prelude.putStrLn "Query="
            print query
          else pure ()
        (defs, inputStream) <- MySQL.query_ conn (stringToQuery query)
        records <- Streams.toList inputStream
        pure $ mapM (mySqlResultToRecord defs) records

stringToQuery :: String -> MySQL.Query
stringToQuery x = MySQL.Query (BLU.fromString x)

mySqlExecutorM :: DatabaseConnection -> State (Result Query) () -> [VariableValue] -> IO (Result RecordList)
mySqlExecutorM con stateQuery variables = do
  let resultQuery = evalState (stateQuery >> _evaluate) (Error "Not valid query specified")
  case resultQuery of
    Error e -> return (Error e)
    Result query -> mySqlExecutor con query variables

mySqlResultToRecord :: [MySQL.ColumnDef] -> [MySQL.MySQLValue] -> Result Record
mySqlResultToRecord = zipWithM toRecordField

toRecordField :: MySQL.ColumnDef -> MySQL.MySQLValue -> Result RecordField
toRecordField def val = do
  v <- toRecordValue val
  pure $ RecordField (toFieldName def) v

toFieldName :: MySQL.ColumnDef -> String
toFieldName d = Prelude.tail (Prelude.init (show (MySQL.columnName d)))

toRecordValue :: MySQL.MySQLValue -> Result RecordValue
toRecordValue (MySQL.MySQLInt8 i) = Result $ RecordValueInt (read (show i) :: Int)
toRecordValue (MySQL.MySQLInt8U i) = Result $ RecordValueInt (read (show i) :: Int)
toRecordValue (MySQL.MySQLInt16 i) = Result $ RecordValueInt (read (show i) :: Int)
toRecordValue (MySQL.MySQLInt16U i) = Result $ RecordValueInt (read (show i) :: Int)
toRecordValue (MySQL.MySQLInt32 i) = Result $ RecordValueInt (read (show i) :: Int)
toRecordValue (MySQL.MySQLInt32U i) = Result $ RecordValueInt (read (show i) :: Int)
toRecordValue (MySQL.MySQLBit b) = Result $ RecordValueBool (read (show b) :: Bool)
toRecordValue (MySQL.MySQLText t) = Result $ RecordValueString (Text.unpack t)
toRecordValue MySQL.MySQLNull = Result RecordValueNull
toRecordValue v = Error $ "Could not parse MySQLValue " ++ show v