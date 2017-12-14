-- module Main where
module H101 where

import Data.Maybe
import Data.Char
import Data.List hiding (concat, foldl, foldr)
import Prelude hiding (concat, head, tail, (++), map, foldr, foldl, length, const)


-- Recursion
fib :: Int -> Int
fib i = if i == 0 then 0
        else if i == 1 then 1
        else fib (i-1) + fib(i-2)

-- Guards
fib1 :: Int -> Int
fib1 i
    | i == 0 || i == 1
    = i
    | otherwise
    = fib1 (i-1) + fib1 (i-2)

-- Case analysis
fib2 :: Int -> Int
fib2 i = case i of
    i | i == 0 -> 0
    i | i == 1 -> 1
    i -> fib2 (i-1) + fib2(i-2)

-- Pattern matching
fib3 :: Int -> Int
fib3 0 = 0
fib3 1 = 1
fib3 i = fib3 (i-1) + fib3(i-2)

fib42 = fib3 42
-- Q: Is fib 42 actually evaluated?
-- No. Lazy evaluation.
-- Q: What happens if recursive fib3 i is defined first
-- infacce loop
-- Q: what is the value of fib on negative inputs
-- infacce loop

-- User Data Types
data IntError
    = Value {val :: Int}
    | Error {err :: String,
             code :: Int }

data Point a = Pt a a

get_code :: IntError -> Maybe Int 
get_code ie = case ie of
    Value _ -> Nothing
    Error a c -> Just c

-- Q: What is an advantage of using Maybe instead of user defined IntError?
-- Maybe is a polymorphic data type that we can use with many more types, whereas IntError can only be used with Ints

-- Lists
-- Q: What is type of empty list?
-- A: Empty list is [a]
-- Q: Type for cons constructor?
-- A: a -> [a] -> [a]

-- listCase :: [Int] -> Int
-- listCase xs = 
--   case xs of 
--     [] -> 1
--     [2] -> 2
--     [x,y,z] -> 3
--     x:xs -> 4
--     [x,y] -> 5


-- Q: Value of listcase [2,6]?
-- A: 4

-- List Comprehension
evens = [x | x<-[1..10], x `mod` 10 == 0]

sum_mult_3_5 = sum [x | x<-[1..999], x `mod` 3 == 0 || x `mod` 5 == 0]

pairs = [(x,y) | x <- [1..5], y<-[1..5]]

triangles = [(x,y,z) | x<-[1..9],y<-[1..9],z<-[1..9]]

right_triangles n = [(x,y,z) | x<-[1..n], y<-[1..x], z<-[1..y], z^2 + y^2 == x^2]

-- Recursion on Lists
head :: [a] -> a
head [] = error "head on empty list"
head (x:xs) = x

const :: a -> b -> b
const a b = b

-- Q: Define tail of list
tail :: [a] -> [a]
tail [] = error "no tail" 
tail (x:xs) = xs

-- Q: Concatenate two lists
concat :: [a] -> [a] -> [a]
concat [] bs = bs
concat (a:as) bs = a:(concat as bs)

-- Mapping
toLowerString :: String -> String
toLowerString = map toLower

plusOne :: [Int] -> [Int]
plusOne = map (+1)

-- Folding
-- foldr fun acc [] = acc
-- foldr fun acc (x:xs) = fun x (foldr fun acc xs)
foldr f acc []     = acc 
foldr f acc (x:xs) = f x (foldr f acc xs)

foldl fun acc [] = acc 
foldl fun acc (x:xs) = foldl fun (fun acc x) xs

-- Higher Order Haskell Dialect

-- Q: What is the type of '($ "me!")'?
-- A: ([Char] -> b) -> b
-- Q: What is the result of 'map ($ "me!") [map toUpper , (++"!?!?!")]'?
-- A: ["ME!","me!!?!?!"]

infixl 0 •
(•) :: (a->b) -> a -> b
f • b = f b

isFactor :: Int -> Int -> Bool
isFactor = (((>1).).gcd)

primes = nubBy (((>1).).gcd) [1..100]

-- Haskell in the real world
main :: IO ()
main = do
    putStrLn "Hello!"
    putStrLn "Hey!"
    i <- getLine
    let triangles = right_triangles (read i)
    putStrLn (show triangles)
