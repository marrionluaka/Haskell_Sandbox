myMap f [] = []
myMap f (x:xs) = (f x):myMap f xs
-- myMap (^2) [1,2,3] => [1,4,9]

myFilter test [] = []
myFilter test (x:xs) = if test x
                       then x:myFilter test xs
                       else myFilter test xs
-- myFilter (\(x:xs) -> x == 'a') ["apple", "banana", "avocado"] => ["apple", "avocado"]

remove test [] = []
remove test (x:xs) = if test x
                     then remove test xs
                     else x:remove test xs
-- remove (\x -> x ^ 2 > 10) [1,3,4,5,6,7] => [1,3]
