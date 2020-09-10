module Recursion where

import FunctionalPatterns (nDigit)
import Data.List (intersperse)

recursiveSum :: (Eq a, Num a) => a -> a
recursiveSum n 
    | n == 1    = 1
    | otherwise = n + recursiveSum (n - 1)

multiplication :: (Integral a) => a -> a -> a
multiplication x y 
    | y == 0    = 0
    | otherwise = x + multiplication x (y - 1)

dividedBy :: Integral a => a -> a -> (a, a) 
dividedBy num denom = go num denom 0
    where
        go n d count
            | n < d     = (count, n)
            | otherwise = go (n - d) d (count + 1)

data DividedResult = Result Integer | DividedByZero deriving Show 
fixedDividedBy :: (Integral a) => a -> a -> (DividedResult, DividedResult) 
fixedDividedBy num denom = negate (dividedBy (abs num) (abs denom))
    where
        dividedBy :: (Integral a) => a -> a -> (DividedResult, DividedResult) 
        dividedBy num denom = go num denom 0
        go :: (Integral a) => a -> a -> Integer -> (DividedResult, DividedResult) 
        go num denom count
            | denom == 0  = (DividedByZero, DividedByZero)
            | num < denom = (Result (toInteger count), Result (toInteger num))
            | otherwise   = go (num - denom) denom (count + 1)
        negate :: (DividedResult, DividedResult) -> (DividedResult, DividedResult)
        negate (Result fst, x)
            | (num < 0) /= (denom < 0) = (Result (- fst), x)
            | otherwise                = (Result fst, x)
        negate (DividedByZero, DividedByZero) = (DividedByZero, DividedByZero)

digitToWord :: Int -> String
digitToWord n
    | n == 0 = "zero" 
    | n == 1 = "one" 
    | n == 2 = "two" 
    | n == 3 = "three"
    | n == 4 = "four"
    | n == 5 = "five"
    | n == 6 = "six"
    | n == 7 = "seven"
    | n == 8 = "eigth"
    | n == 9 = "nine"

largestDigitPosition :: Integral a => a -> a
largestDigitPosition n = findLargestPosition n 0
findLargestPosition :: Integral a => a -> a -> a
findLargestPosition n c
    | n < 10 = c 
    | otherwise = findLargestPosition (n `div` 10) (c + 1)

digits :: Int -> [Int]
digits n = splitDigits n largestPosition []
    where largestPosition = (largestDigitPosition n)
splitDigits :: Int -> Int -> [Int] -> [Int]
splitDigits num position list
    | position < 0 = list
    | otherwise    = list ++ splitDigits num (position - 1) ((:[]) (nDigit num position))

wordNumber :: Int -> String
wordNumber n = concat $ intersperse "-" (map digitToWord (digits n))
