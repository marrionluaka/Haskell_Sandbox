# Do notation
- Do-notation is syntactic sugar for using >>, >>=, and (\x -> return (func x))
  ```haskell
    helloNameDo :: IO ()
    helloNameDo = askForName >>
                  getLine >>=
                  (\name -> return (nameStatement name)) >>=
                  putStrLn
  ```
  is functionally equivalent to:
  ```haskell
    helloNameDo :: IO ()
    helloNameDo = do
      askForName
      name <- getLine
      putStrLn (nameStatement name)
  ``` 

- do-notation is strongly preferred for nontrivial use of monadic operators.