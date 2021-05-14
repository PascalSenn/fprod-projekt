module Kuery.Language.Operators where

import Kuery.Language.Value
import Kuery.Language.Base

($==) :: (QueryValue a) => String -> a -> Filter
l $== r = Eq (Field l) (format r)

infix 4 $==

($&&) :: Filter -> Filter -> Filter
l $&& r = And l r

infixr 3 $&&