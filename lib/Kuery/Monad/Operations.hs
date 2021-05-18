module Kuery.Monad.Operations where

import Control.Monad.State
import Kuery.Language.Base
import qualified Kuery.Operations as Operations
import Kuery.Result

type MonadQuery = State (Result Query) ()

_select :: [String] -> MonadQuery
_select fields =
  do
    let new = Operations._select fields
    put (Result new)

_from :: String -> MonadQuery
_from src =
  do
    res <- Control.Monad.State.get
    put $ do
      query <- res
      let new = Operations._from query src
      Result new

_where :: Filter -> MonadQuery
_where f =
  do
    res <- Control.Monad.State.get
    put $ do
      query <- res
      let new = Operations._where query f
      Result new

_skip :: Operations.SkipOrLimit a => a -> MonadQuery
_skip s =
  do
    res <- Control.Monad.State.get
    put $ do
      query <- res
      let new = Operations._skip query s
      Result new

_limit :: Operations.SkipOrLimit a => a -> MonadQuery
_limit s =
  do
    res <- Control.Monad.State.get
    put $ do
      query <- res
      let new = Operations._limit query s
      Result new

_orderBy :: [Order] -> MonadQuery
_orderBy values =
  do
    res <- Control.Monad.State.get
    put $ do
      query <- res
      let new = Operations._orderBy query values
      Result new

_insert :: [[Setter]] -> MonadQuery
_insert setters =
  do
    put $ do
      let new = Operations._insert setters
      Result new

_into :: String -> MonadQuery
_into src =
  do
    res <- Control.Monad.State.get
    put $ do
      query <- res
      let new = Operations._into query src
      Result new

_update :: String -> MonadQuery
_update t =
  do
    put $ do
      let new = Operations._update t
      Result new

_set :: [Setter] -> MonadQuery
_set values =
  do
    res <- Control.Monad.State.get
    put $ do
      query <- res
      let new = Operations._set query values
      Result new

_delete :: MonadQuery
_delete =
  do
    put $ do
      let new = Operations._delete
      Result new
