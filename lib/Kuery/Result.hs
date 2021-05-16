module Kuery.Result where

data Result a = Error String | Result a

instance Functor Result where
  fmap _ (Error a) = Error a
  fmap f (Result a) = Result (f a)

instance Applicative Result where
  pure a = Result a
  Result f <*> Result a = Result (f a)
  (Error a) <*> _ = Error a
  _ <*> (Error b) = Error b

instance Monad Result where
  Error a >>= _ = Error a
  Result a >>= f = f a