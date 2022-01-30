# General

- To calc a distance between two points on a globe, you use the Haversine formula.
- Reason backward from where you want to end up

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