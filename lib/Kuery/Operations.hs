module Kuery.Operations where

import Kuery.Helpers
import Kuery.Language.Base

_q :: Query
_q = Query {filters = [], selections = [], source = Nothing}

_select :: Query -> [String] -> Query
_select query fields = query {selections = merge (selections query) (map Field fields)}

_from :: Query -> String -> Query
_from query src = query {source = Just src}

_where :: Query -> Filter -> Query
_where query filter = query {filters = filter : filters query}