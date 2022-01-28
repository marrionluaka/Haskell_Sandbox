## General

- The `Functor` type class allows you to apply an ordinary function to values inside a container(for example, `List`) or context(for example, `IO` or `Maybe`)

- If you have a function `Int -> Double` and a value `Maybe Int`, you can use `Functor`'s `fmap` to apply the `Int -> Double` function to the `Maybe Int` value, resulting in a Maybe Double value.

- `Functors` allow you to reuse a single function with any type belonging to the `Functor` type class.

- `Functor`'s `<$>` provides a common interface to apply to any function to a value in a context.

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

- `fmap` for `List` is identical to `map`

- One way to think of the `Functor` type class is as "things that can be mapped over"

- `Functor` for `Map` is concerned only about the `Map`'s values and its keys.

- When `Map` is made an instance for `Functor`, you're concerned only about a single type variable, the one used for its values.

- For the purposed of `Map` being a member of `Functor`, you treat it as being of kind `* -> *`.

