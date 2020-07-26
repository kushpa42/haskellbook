import Data.List
import Data.Char

replaceThe :: String -> String
replaceThe = concat . intersperse " " . map (go . notThe) . words
  where go Nothing   = "a"
        go (Just s)  = s

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe s     = Just s

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = go 0 . map notThe . words
  where go count []       = count
        go count [x]      = count
        go count (h:m:xs) = case h of
                              Nothing -> if isVowelInitial m then go (count + 1) xs else go count xs
                              Just _  -> go count (m:xs)

isVowel :: Char -> Bool
isVowel c = elem (toLower c) "aeiou" 

isVowelInitial :: Maybe String -> Bool
isVowelInitial Nothing   = False
isVowelInitial (Just []) = False
isVowelInitial (Just s)  = isVowel $ head s

countVowels :: String -> Integer
countVowels = sum . map (\_ -> 1) . filter isVowel 
