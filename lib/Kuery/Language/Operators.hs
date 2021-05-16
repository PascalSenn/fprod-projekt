module Kuery.Language.Operators where

import Kuery.Language.Base
import Kuery.Language.Value

($==) :: (QueryValue a) => String -> a -> Filter
l $== r = Eq (Field l) (format r)

infix 4 $==

($!=) :: (QueryValue a) => String -> a -> Filter
l $!= r = Ne (Field l) (format r)

infix 4 $!=

($>=) :: (QueryValue a) => String -> a -> Filter
l $>= r = Gte (Field l) (format r)

infix 4 $>=

($>) :: (QueryValue a) => String -> a -> Filter
l $> r = Gt (Field l) (format r)

infix 4 $>

($<=) :: (QueryValue a) => String -> a -> Filter
l $<= r = Lte (Field l) (format r)

infix 4 $<=

($<) :: (QueryValue a) => String -> a -> Filter
l $< r = Lt (Field l) (format r)

infix 4 $<

($&&) :: Filter -> Filter -> Filter
l $&& r = And l r

infixr 3 $&&

($||) :: Filter -> Filter -> Filter
l $|| r = Or l r

infixr 2 $||

($=) :: (QueryValue a) => String -> a -> Setter
l $= r = Setter (Field l) (format r)

infixr 3 $=

_in :: (QueryValue a) => String -> [a] -> Filter
l `_in` r = In (Field l) (map format r)

_contains :: (QueryValue a) => String -> a -> Filter
l `_contains` r = Contains (Field l) (format r)