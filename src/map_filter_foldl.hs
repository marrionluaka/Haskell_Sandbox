-- `++` concatenates one list to another. With `:` the first argument is of type a (an arbitrary type)
-- and the second argument is of type [a] which is the type of lists of elements of a. It's essentially R.prepend
-- If you're ever confused about a type just check the `ghci>` with `:t`
-- Example:
-- ghci> :t (++)
-- (++) :: [a] -> [a] -> [a]
-- ghci> :t (:)
-- (:) :: a -> [a] -> [a]

myMap f [] = []
myMap f (x:xs) = (f x):myMap f xs
-- myMap (^2) [1,2,3] => [1,4,9]

myFilter test [] = []
myFilter test (x:xs) = if test x
                       then x:myFilter test xs
                       else myFilter test xs
-- myFilter (\(x:xs) -> x == 'a') ["apple", "banana", "avocado"] => ["apple", "avocado"]

myFoldl f init [] = init
myFoldl f init (x:xs) = myFoldl f newInit xs
  where newInit = f init x
-- myFoldl (+) 0 [1,2,5] => 8

remove test [] = []
remove test (x:xs) = if test x
                     then remove test xs
                     else x:remove test xs
-- remove (\x -> x ^ 2 > 10) [1,3,4,5,6,7] => [1,3]

myProduct xs = foldl (*) 1 xs
-- myProduct [1,2,3,5] => 30

sumOfSquares xs = foldl (+) 0 (map (^2) xs)
-- sumOfSquares [2,4,6] => 56

rcons acc value = value:acc
myReverse xs = foldl rcons [] xs
-- myReverse [1,2,3] => [3,2,1]
