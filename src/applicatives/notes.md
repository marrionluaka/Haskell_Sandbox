# General

- To calc a distance between two points on a globe, you use the Haversine formula.
- Reason backward from where you want to end up
- The `Applicative`'s `<*>` allows you to use functions that are themselves in a context.
- It allows us to extend `Functor` to use multi-argument functions.

# Functor reminder

```haskell
               --[1]   --[2]     --[3]  --[4]
  fmap :: Functor f => (a -> b) -> f a -> f b
```
1) The `f` type variable means any parameterized type, belonging to `Functor`.
2) This is just a function from type `a` to type `b`. Different variables mean only that the values *can* be different, not that they *have* to be.
3) Your first argument is in a functor, such as `Maybe`.
4) Your result remains in the same `Functor` as your argument. But now the function has been applied inside the context.

- The `fmap` function takes any function from type `a` to type `b`, and the value of type `a` in the context of a Functor (like `Maybe`), and returns a value of type `b` in the same context.

- One of the limitations of `Functor`'s `fmap` is that it only works on single-argument functions.

# Functor limitations

- The limitation of `Functor`'s `<$>` is that if you end up with a function in a context, through partial application, you have no way of using that function.

- For example, you can use `<$>, (+)` and the number 1 in a context to create a `maybeInc` function:
  ```haskell
    maybeInc = (+) <$> Just 1
  ```
- The `(+)` operator is a function that takes two values `:t (+) Num a => a -> a -> a`; by using `<$>` on a `Maybe` value, you created a function waiting for a missing value, but it's inside a `Maybe`. You now have a `Maybe` function, but there's no way to apply this function!

# Enter the <*> operator

- `<*>` operator is pronounced *app*

```haskell
                         --[1] --[2]    --[3]  --[4]
  (<*>) :: Applicative f => f (a -> b) -> f a -> f b
```
1) This `f` means any type that's an instance of `Applicative`. For example, `Maybe`.
2) You have a function in an `Applicative` from `a -> b`. For example, `Maybe(Int -> Double)`.
3) You take an argument in the context of an `Applicative`. For example, `Maybe Int`.
4) Finally, you get your result in the same applicative context you started with. In this case, a `Maybe Double`.

- Applicative's `<*>` allows you to *apply* a function in a context:

  ```haskell
    GHCi> :t maybeInc
    maybeInc :: Num a => Maybe (a -> a)

    GHCi> maybeInc <*> Just 6
    Just 7

    GHCi> maybeInc <*> Nothing
    Nothing
  ```

- Using existing binary functions in a `Maybe` context (chaining):

  ```haskell
    GHCi> (++) <$> Just "cats" <*> Just " and dogs"
    Just "cats and dogs"

    GHCi> (++) <$> Nothing <*> Just " and dogs"
    Nothing
  ```

- Combining `<$>` with `<*>` to compute haverstine in a `Maybe` context:
  ```haskell
    --[1]    --[2]                  --[3]
    haversine <$> startingCity <*> destcity
    ```
  1) The first part uses partial application which leaves you with a function of type `Maybe (LatLong -> Double)` waiting for a missing argument.
  2) The `<*>` operator **takes a function in a context**, in this case `Maybe (LatLong -> Double)`.
  3) And an argument in the same context `Maybe LatLong` **and applies the function to that argument**, returning a type still in the context, here a `Maybe Double`.

# Using <*> to create data in a context

- One of the most common uses of `Applicatives` in practice occurs when you want to create data, but all the information you need for that data is in a context such as a `Maybe` or `IO`.

### Creating a user in the context of a Maybe

- Remember that data constructors can work as functions:
  ```haskell
    data User = User
    { name :: String
    , gamerId :: Int
    , score :: Int
    } deriving Show

    serverUserName :: Maybe String
    serverUserName = Just "Sue"

    serverGamerId :: Maybe Int
    serverGamerId = Just 1337

    serverScore :: Maybe Int
    serverScore = Just 9001

    GHCi> User <$> serverUserName <*> serverGamerId <*> serverScore
    Just (User {name = "Sue", gamerId = 1337, score = 9001 })
  ```
- In this case, `User` is a function that expects three arguments

# Applicative Type Class

- Functor is the superclass of applicative.

- Type class definition for Applicative:
  ```haskell
    class Functor f => Applicative f where
      (<*>) :: f (a -> b) -> f a -> f b
      pure :: a -> f a
  ```
- The type constraint on the type variable f means that Functor is a superclass of Applicative. So all Applicatives are also Functors.
- The `pure` function takes an ordinary value or function and returns the same value wrapped in a context.
  ```haskell
    -- For values
    GHCi> pure 7 :: Maybe Int
    Just 7

    -- For functions
    GHCi> pure (6+) <*> Just 5
    Just 11
  ```

# Containers vs context

- What's the difference between a Container vs a Context?
  - Parameterized types that represent a container are types that represent a data structure.
  - When a type is a context, extra information is implied about the type, beyond its structure.

- Why is it important to understand the differences between a Container and a Context?

- How does one know when a type is a container?
  - The best test of whether a type is a container is whether you can tell what it does, independent of its name.
  - For example you can tell that `Blah` is a tuple despite not paying attention to its awful name:
    ```haskell
      data Blah a b = Blah a b
    ```
  - The meaning of the type is implied by the data structure itself

- The 2-tuple `(,)` and `Data.Map` are both instances of `Functor` but *not* instances of `Applicative`.

# List as a context

- The `List` type is both a container *and* a context.

- The `List` type is a member of `Applicative`
