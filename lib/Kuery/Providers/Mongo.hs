{-# LANGUAGE OverloadedStrings #-}

module Kuery.Providers.Mongo (execute) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (pack)
import qualified Database.MongoDB as Mongo
import Kuery.Language.Base

execute :: Query -> Maybe (Mongo.Action IO Mongo.Cursor)
execute d =
  do
    case source d of
      Nothing -> Nothing
      Just src -> Just (Mongo.find $ Mongo.select [] (pack src))
