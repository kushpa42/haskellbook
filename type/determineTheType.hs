{-# LANGUAGE NoMonomorphismRestriction #-}

module DetermineTheType where

example = 1

-- Num a => a 
a = (* 9) 6

-- Num a => (a, [Char])
b = head [(0, "doge"), (1, "kitteh")]

-- (Integer, [Char])
c = head [(0 :: Integer, "doge"), (1, "kitteh")]

-- Bool
d = if False then True else False

-- Int
e = length [1, 2, 3, 4, 5]

-- Bool
f = (length [1, 2, 3, 4]) > (length "TACOCAT")
