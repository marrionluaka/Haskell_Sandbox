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

