myGCD a b = if remainder == 0
            then b
            else myGCD b remainder
  where remainder = a `mod` b


-- with pattern matching
myTake _ [] = []
myTake 0 _ = []
myTake n (x:xs) = x:rest
  where rest = myTake (n - 1) xs
