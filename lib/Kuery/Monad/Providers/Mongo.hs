{-# LANGUAGE OverloadedStrings #-}

module Kuery.Monad.Providers.Mongo (executeM) where

import Control.Monad.State
import qualified Database.MongoDB as Mongo
import Kuery.Language.Base
import qualified Kuery.Providers.Mongo as Provider
import Kuery.Providers.Mongo.Base
import Kuery.Result

executeM :: State (Result Query) () -> [VariableValue] -> MongoConnection -> IO (Result [Mongo.Document])
executeM stateQuery variables con = do
  let resultQuery = evalState (stateQuery >> _evaluate) (Error "Not valid query specified")
  case resultQuery of
    Error e -> return (Error e)
    Result query -> Provider.execute query variables con

_evaluate :: State (Result Query) (Result Query)
_evaluate = do Control.Monad.State.get