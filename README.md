# Kuery

## Presentations themen
1. Main target was to have a nice API
1. To Monad or not to monad
1. Having fun with operatiors
1.1 You can make it hard for others to use, awesome
1.1 Infix 
1. Type Classes for Values



Monadic Approach brings no benefits
```haskell

_select2 :: [String] -> State.StateT Query IO ()
_select2 selects = do
  q <- State.get
  State.put q {selections = merge (selections q) (map Field selects)}

_from2 :: String -> State.StateT Query IO ()
_from2 src = do
  q <- State.get
  State.put q {source = Just src}

_where2 :: Filter -> State.StateT Query IO ()
_where2 filter = do
  q <- State.get
  State.put q {filters = filter : filters q}

_query2 = Query {selections = [], filters = [], source = Nothing}

--- test2 = do _select2 ["foo", "bar"] <*> _from2 "collections" <*> _where Eq (Field "foo") (ValueString "bar")
```