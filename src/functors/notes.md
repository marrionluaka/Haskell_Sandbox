## General
- fmap :: Functor f => (a -> b) -> f a -> f b

```haskell
instance Functor Maybe where
  fmap func (Just n) = Just(func c)
  fmap func Nothing = Nothing
```

- <$> is the synonym for fmap, except it's a binary operator rather than a function

```haskell
successfulRequest :: Maybe Int
successfulRequest = Just 6

failedRequest :: Maybe Int
failedRequest = Nothing

successStr :: Maybe String
successStr = show <$> successfulRequest

failStr :: Maybe String
failStr = show <$> failedRequest

reverser :: Maybe String
reverser = reverse <$> Just "hello"

-- fmap (+1) successfulRequest
-- (+1) <$> successfulRequest
```

## Functors everywhere
- Types of a kind `* -> *` are parameterized types that take just one type parameter. All Functors must be of kind `* -> *`. It also turns out that many parameterized types of kind `* -> *` are instances of Functor.

- Some members of Functor include `List, Map, Maybe, and IO`.
