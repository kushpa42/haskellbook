module Cipher where

import Data.Char

caesar :: Int -> String -> String
caesar shift = map (caesarChar shift)

unCaesar :: Int -> String -> String
unCaesar shift = caesar (-shift) 

-- Ignore characters that are not alphabetic
caesarChar :: Int -> Char -> Char
caesarChar shift c
  | elem c ['a'..'z'] = chr $ wrap 26 (ord 'a') (ord c + shift) 
  | elem c ['A'..'Z'] = chr $ wrap 26 (ord 'A') (ord c + shift)
  | otherwise         = c

wrap :: Int -> Int -> Int -> Int 
wrap 0   base _   = base
wrap len base num = (num - base) `mod` len + base 
