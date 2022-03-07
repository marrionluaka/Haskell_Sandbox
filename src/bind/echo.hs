-- getLine :: IO String
-- putStrLn :: String -> IO ()

echo :: IO ()
echo = getLine >>= putStrLn

main :: IO ()
main = echo

readInt :: IO Int
readInt = read <$> getLine
