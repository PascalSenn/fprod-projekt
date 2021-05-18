module Kuery.Helpers (merge) where

merge :: [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x : xs) (y : ys) = x : y : merge xs ys
