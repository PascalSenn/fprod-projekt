module Kuery.Operations where

import Kuery.Language.Base
import Kuery.Language.Value

_select :: [String] -> Query
_select fields =
  Selection
    { selections = map Field fields,
      skip = Nothing,
      limit = Nothing,
      filters = [],
      target = Nothing,
      ordering = []
    }

_from :: Query -> String -> Query
_from query src = query {target = Just src}

_where :: Query -> Filter -> Query
_where query f = query {filters = f : filters query}

_skip :: SkipOrLimit a => Query -> a -> Query
_skip query s = query {skip = Just (get s)}

_limit :: SkipOrLimit a => Query -> a -> Query
_limit query s = query {limit = Just (get s)}

_orderBy :: Query -> [Order] -> Query
_orderBy query values = query {ordering = map orderingToOrderBy values}

orderingToOrderBy :: Order -> OrderBy
orderingToOrderBy (Asc v) = Ascending (Field v)
orderingToOrderBy (Desc v) = Descending (Field v)

_insert :: [[Setter]] -> Query
_insert setters =
  Insert
    { insert = setters,
      target = Nothing
    }

_into :: Query -> String -> Query
_into query src = query {target = Just src}

_update :: String -> Query
_update t =
  Update
    { update = [],
      filters = [],
      target = Just t
    }

_set :: Query -> [Setter] -> Query
_set query values = query {update = values}

_delete :: Query
_delete =
  Delete
    { filters = [],
      target = Nothing
    }

class QueryValue a => SkipOrLimit a where
  get :: a -> Value

instance SkipOrLimit Int where
  get a = ValueInt a

instance SkipOrLimit Value where
  get a = a