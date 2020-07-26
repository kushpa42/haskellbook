module VigenereCipher where

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

zip' :: String -> String -> [(Char, Char)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys)
  | x == ' '  = (x, ' ') : zip' xs (y:ys)
  | y == ' '  = (' ', y) : zip' (x:xs) ys
  | otherwise = (x, y) : zip' xs ys 

getShift :: Char -> Int
getShift c
  | elem c ['a'..'z'] = ord c - ord 'a'
  | elem c ['A'..'Z'] = ord c - ord 'A'
  | otherwise         = 0

vigenere :: String -> String -> String
vigenere msg key = map f (zip' msg repeatedKey)
  where repeatedKey = key ++ repeatedKey
        f (x, y)    = caesarChar (getShift y) x 

interactiveVigenere :: IO () 
interactiveVigenere = do
        putStrLn "Enter message:"
        msg <- getLine
        putStrLn "Enter key:"
        key <- getLine 
        putStrLn "Encrypted message:"
        putStrLn $ vigenere msg key

interactiveCaesar :: IO ()
interactiveCaesar = do
        putStrLn "Enter message:"
        msg <- getLine
        putStrLn "Enter shift as integer:"
        input <- getLine
        let shift = (read input :: Int)
        putStrLn "Encrypted message:"
        putStrLn $ caesar shift msg
