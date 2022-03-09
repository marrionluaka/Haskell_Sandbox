-- getLine :: IO String
-- putStrLn :: String -> IO ()

main :: IO ()
main = echo

echoVerbose :: IO ()
echoVerbose = putStrLn "Enter a String and we'll echo it!" >> getLine >>= putStrLn

echo :: IO ()
echo = getLine >>= putStrLn

readInt :: IO Int
readInt = read <$> getLine
