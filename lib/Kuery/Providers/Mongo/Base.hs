module Kuery.Providers.Mongo.Base where

import Data.Text
import qualified Database.MongoDB as Mongo
import Kuery.Connection

toMongo :: DatabaseConnection -> IO MongoConnection
toMongo conf = do
  p <- Mongo.connect $ Mongo.host (host conf)
  let r act = Mongo.access p Mongo.master (pack (databaseName conf)) act
  return
    MongoConnection
      { db = conf,
        executor = r
      }

data MongoConnection = MongoConnection
  { db :: DatabaseConnection,
    executor :: Mongo.Action IO [Mongo.Document] -> IO [Mongo.Document]
  }
