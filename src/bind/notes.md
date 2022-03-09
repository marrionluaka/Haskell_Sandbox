- Whenever you find yourself having to write a wrapper function around a maybe type:
  ```haskell
    lookupCredits :: UserName -> Maybe PlayerCredits
    lookupCredits username = Map.lookup username creditsDB

    -- wrapper function (Bad)
    altLookupCredits :: Maybe UserName -> Maybe PlayerCredits
    altLookupCredits Nothing = Nothing
    altLookupCredits (Just username) = lookupCredits username
  ``
  This is a sure telling sign that there's a better alternative using `Functors`, `Applicatives` or `Bind`.

# Bind operator
- The bind operator 'cracks' open for you the value that's contained inside the initial monad (`m a`) and applies it (`a -> m b`) to the returning monad (`m b`):
  ```haskell
  (>>=) :: Monad m => m a -> (a -> m b) -> m b

  -- example
  creditFromId id = Just (id) >>= \a -> Just(a + 1)

  -- GHCi> creditFromId 1 // => Just 2
  -- GHCi> creditFromId 2 // => Just 3
  ```

- Another example using `IO`:
  ```haskell
  printDouble :: Int -> IO ()
  printDouble n = print (n*2)

  pp :: IO ()
  pp = readInt >>= printDouble

  -- GHCi> pp
  -- 2
  -- 4
  ```

- You use `>>=` to chain together functions of the type `(a -> m b)`

# Monad type class
- In the same way the `Applicative` type class extends the power of `Functor`, the `Monad` type class extends the power of `Applicative`:
  ```haskell
    -- Functor
    fmap  :: Functor f :: (a -> b) -> f a -> f b
    (<$>) :: Functor f :: (a -> b) -> f a -> f b
    |
    |
    -- Applicative extends Functor
    fmap  :: Functor f :: (a -> b) -> f a -> f b
    (<$>) :: Functor f :: (a -> b) -> f a -> f b
    (<*>) :: Applicative f :: f (a -> b) -> f a -> f b
    pure  :: Applicative f :: a -> f a
    |
    |
    -- Monad extends Applicative
    fmap  :: Functor f :: (a -> b) -> f a -> f b
    (<$>) :: Functor f :: (a -> b) -> f a -> f b
    (<*>) :: Applicative f :: f (a -> b) -> f a -> f b
    pure  :: Applicative f :: a -> f a
    (>>=) :: Monad m :: m a -> (a -> m b) -> m b
    (>>)  :: Monad m :: m a -> m b -> m b
    return :: Monad m :: a -> m a
    fail  :: Monad m :: String -> m a
  ```
- The only method required for the minimum definition of Monad is `>>=`
- `pure` and `return` are the same except that `pure` has a class restraint on `Applicative` (they have different names for historical reasons).

- `>>` throws away the first argument it receives, it's useful in contexts that produce side effects such as `IO`