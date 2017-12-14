-- Problem 1
myLast :: [a] -> a
myLast [] = error "no end"
myLast [x] = x
myLast (_:xs) = myLast xs

-- Problem 2
myButLast :: [a] -> a
myButLast x = reverse x !! 1

myButLast' :: [a] -> a
myButLast' [x,_] = x
myButLast' (_:xs) = myButLast' xs

myButLast'' :: [a] -> a
myButLast'' = head . tail . reverse

-- Problem 3
elementAt :: [a] -> Int -> a
elementAt xs k = xs !! (k-1)

-- Problem 4
myLength :: [a] -> Int
myLength = length

myLength' :: [a] -> Int
myLength' [] = 0
myLength' (x:xs) = 1 + myLength xs

myLengthFold :: [a] -> Int
myLengthFold = foldl (\acc _ -> 1 + acc) 0

-- Problem 5
myReverse :: [a] -> [a]
myReverse = foldl (\acc x -> x:acc) []

myReverse' :: [a] -> [a]
myReverse' [] = []
myReverse' (x:xs) = myReverse' xs ++ [x]

-- Problem 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x

-- Problem 7
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]

