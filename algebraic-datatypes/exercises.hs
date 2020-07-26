import Data.Char
import Data.List

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _  = True
isSubseqOf _  [] = False
isSubseqOf s@(x:xs) (y:ys)
  | x == y    = isSubseqOf xs ys 
  | otherwise = isSubseqOf s  ys 

capitalizeWords :: String -> [(String, String)]
capitalizeWords = map f . words
  where f s@(x:xs) = (s, toUpper x : xs)

capitalizeWord :: String -> String
capitalizeWord []     = []
capitalizeWord (x:xs) = toUpper x : xs

format :: String -> String
format [] = []
format (x:xs) = toUpper x : reverse (dropWhile (== ' ') $ reverse xs)

capitalizeParagraph :: String -> String
capitalizeParagraph = format . concat . intersperse " " . foldr capitalize [""] . words
  where capitalize curr (head : tail)
          | elem '.' curr = curr : capitalizeWord head : tail
          | otherwise     = curr : head : tail
