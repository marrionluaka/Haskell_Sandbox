isPalindrome word = word == reverse word

takeLast n aList = (take n (reverse aList))

ones n = take n (cycle [1])

assignToGroups n aList = zip groups aList
  where groups = cycle [1..n]
