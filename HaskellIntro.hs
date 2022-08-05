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

-- validate :: Integer -> Bool
-- validate a = sumDigits (doubleEveryOther (toDigits a)) `mod` 10 == 0

--
-- Problem 2
--
square :: Int -> Int
square x = x * x

pow :: (a -> a) -> Int -> a -> a
pow f n      | n == 1 = f
             | otherwise = f . pow f (n-1)

g :: Integer -> Integer
g = error "h not yet defined"

h :: Integer -> Integer
h = error "h not yet defined"

d :: Int -> Integer -> Integer
d = error "d not yet defined"

--
-- Problem 3
--

powerSet = error "powerSet not yet defined"