module Exercises where

thirdLetter :: String -> Char
thirdLetter s = head $ drop 2 s

letterIndex :: Int -> Char
letterIndex n = head $ drop (n - 1) s
  where s = "Curry is awesome!"

rvrs :: String -> String
rvrs s = drop 9 s ++ " " ++ (take 2 $ drop 6 s) ++ " " ++ take 5 s
