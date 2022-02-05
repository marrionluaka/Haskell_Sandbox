primesToN :: Integer -> [Integer]
primesToN n = filter isNotComposite twoThroughN
  where twoThroughN = [2 .. n]
        composite = (*) <$> twoThroughN <*> twoThroughN
        isNotComposite = not . (`elem` composite)


data User = User
  { name :: String
  , gamerId :: Int
  , score :: Int
  } deriving Show

testIds :: [Int]
testIds = [1337, 0123, 999999]

testScores :: [Int]
testScores = [0, 100000, -99999]

testNames :: [String]
testNames = ["John Smith", "Robert'); DROP TABLE Students;--", "Christina NULL", "Randall Munroe"]

testData :: [User]
testData = User <$> testNames <*> testIds <*> testScores
