{-# OPTIONS_GHC -fwarn-tabs #-}

module HaskellIntro where

import Set
import GHC.Read (list)
import Data.Bits (Bits(xor))
import Data.Foldable (find)

-- Load this file into GHCi (say, with `ghci HaskellIntro.hs`) and type
-- `isThisWorking` at the prompt. GHCi will tell you whether it's working!

isThisWorking :: String
isThisWorking = "Yes"

--
-- Problem 1
--

lastDigit :: Integer -> Integer
lastDigit n = n `mod` 10

dropLastDigit :: Integer -> Integer
dropLastDigit n = div n 10

toDigits :: Integer -> [Integer]
toDigits n
        | n <= 0 = []
        | otherwise = toDigits (dropLastDigit n) ++ [lastDigit n]

doubleEveryOtherLeft :: [Integer] -> [Integer]
doubleEveryOtherLeft [] = []
doubleEveryOtherLeft [a] = [a]
doubleEveryOtherLeft (a:b:cs) = [a, b*2] ++ doubleEveryOtherLeft cs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther as = reverse (doubleEveryOtherLeft (reverse as))

findSum :: [Integer] -> Integer
findSum [] = 0
findSum (a:bs) = a + findSum bs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (a:bs) = findSum (toDigits a) + sumDigits bs

validate :: Integer -> Bool
validate a = sumDigits (doubleEveryOther (toDigits a)) `mod` 10 == 0

--
-- Problem 2
--
square :: Int -> Int
square x = x * x

pow :: (a -> a) -> Int -> a -> a
pow f n      | n == 1 = f
             | otherwise = f . pow f (n-1)

g :: Integer -> Integer
g 0 = 0
g n = n - pow g 2 (n - 1)

h :: Integer -> Integer
h 0 = 0
h n = n - pow h 3 (n-1)

d :: Int -> Integer -> Integer
d _ 0 = 0
d n a = a - pow (d n) n (a - 1)

--
-- Problem 3
--

first :: (a, b) -> a
first (n, _) = n

second :: (a, b) -> b
second (_, n) = n

powerSet :: (Ord a) => Set a -> Set (Set a)
powerSet as | isEmpty as = singleton as
            | otherwise = mapSet (insert (first (split as))) (powerSet (second (split as))) `union` powerSet (second (split as))