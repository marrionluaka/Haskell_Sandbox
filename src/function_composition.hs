import Data.List (sort)
import Data.Semigroup

myLast :: [a] -> a
myLast = head . reverse

myMin :: Ord a => [a] -> a
myMin = head . sort

myMax :: Ord a => [a] -> a
myMax = myLast . sort

myAll :: (a -> Bool) -> [a] -> Bool
myAll testFunc = (foldr (&&) True) . (map testFunc)

myAny :: (a -> Bool) -> [a] -> Bool
myAny testFunc = (foldr (||) True) . (map testFunc)



--------------
-- <> combines instances of the same type
-- Associative means that the order in which you apply your <> operator doesn’t matter (think additions)
-- elem === filter

data Color = Red | Yellow | Blue | Green | Purple | Orange | Brown deriving (Show, Eq)

instance Semigroup Color where
  (<>) Red Blue = Purple
  (<>) Blue Red = Purple
  (<>) Yellow Blue = Green
  (<>) Blue Yellow = Green
  (<>) Yellow Red = Orange
  (<>) Red Yellow = Orange
  (<>) a b  | a == b = a
            | all (`elem` [Red, Blue, Purple]) [a,b] = Purple -- a `elem` [Red, Blue, Purple]
            | all (`elem` [Blue, Yellow, Green]) [a,b] = Green
            | all (`elem` [Red, Yellow, Orange]) [a,b] = Orange
            | otherwise = Brown

-- Usage
-- (Green <> Blue) <> Yellow // Green
-- Green <> (Blue <> Yellow) // Green



--------------
type Events = [String]
type Probs = [Double]
data PTable = PTable Events Probs

createPTable :: Events -> Probs -> PTable
createPTable events probs = PTable events nomarlizedProbs
  where totalProbs = sum probs
        nomarlizedProbs = map (/ totalProbs) probs

showPair :: String -> Double -> String
showPair event prob = mconcat [event, "|", show prob, "\n"]

instance Show PTable where
  show (PTable events probs) = mconcat  pairs
    where pairs = zipWith showPair events probs

-- Usage
-- createPTable ["heads", "tails"] [0.5, 0.5] // => heads|0.5 tails|0.5



--------------
-- cartesian product
cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine func l1 l2 = zipWith func newL1 cycledL2
  where nToAdd = length l2
        repeatedL1 = map (replicate 2) l1 -- [[1,1], [2,2], [3,3]]
        newL1 = mconcat repeatedL1 -- [1,1,2,2,3,3]
        cycledL2 = cycle l2

combineEvents :: Events -> Events -> Events
combineEvents = cartCombine combiner
  where combiner = \x y -> mconcat [x, "-", y]

combineProbs :: Probs -> Probs -> Probs
combineProbs = cartCombine (*)

instance Semigroup PTable where
  (<>) ptable1 (PTable [] []) = ptable1
  (<>) (PTable [] []) ptable2 = ptable2
  (<>) (PTable e1 p1) (PTable e2 p2) = createPTable newEvents newProbs
    where newEvents = combineEvents e1 e2
          newProbs = combineProbs p1 p2

instance Monoid PTable where
  mempty = PTable [] []
  mappend = (<>)


-- Usage
coin :: PTable
coin = createPTable ["heads", "tails"] [0.5, 0.5]

spinner :: PTable
spinner = createPTable ["red", "blue", "green"] [0.1, 0.2, 0.7]

-- coin <> spinner
-- mconcat [coin, coin, coin]