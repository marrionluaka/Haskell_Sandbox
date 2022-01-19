main :: IO ()
main = do
  userInput <- getContents
  let numbers = toInts userInput
  print (sum numbers)

toInts :: String -> [Int]
toInts = map read . lines


mainSumSquares :: IO ()
mainSumSquares = do
  userInput <- getContents
  let numbers = toInts userInput
  let squares = map (^2) numbers
  print (sum squares)
