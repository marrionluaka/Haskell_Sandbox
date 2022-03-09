-- When working with IO, >> is useful anytime you need to perform an IO action that doesn't meaningfully return a value
helloName :: IO ()
helloName = askForName >> getLine >>= (\name -> return (nameStatement name)) >>= putStrLn

askForName :: IO ()
askForName = putStrLn "What is your name?"

nameStatement :: String -> String
nameStatement name = "Hello, " ++ name ++ "!"
