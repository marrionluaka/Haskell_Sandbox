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