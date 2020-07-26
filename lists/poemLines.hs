module PoemLines where

firstSen  = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen  = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"

sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myBreaks :: String -> Char -> [String]
myBreaks s c = let string = dropWhile (== c) s in
                   case string of
                     ""        -> []
                     otherwise -> part : myBreaks tail c
                       where part = takeWhile (/= c) string
                             tail = dropWhile (/= c) string

myWords :: String -> [String]
myWords = flip myBreaks ' ' 

myLines :: String -> [String]
myLines s = myBreaks s '\n' 

shouldEqual = ["Tyger Tyger, burning bright", "In the forests of the night",
                "What immortal hand or eye", "Could frame thy fearful symmetry?"]

main :: IO ()
main = print $ "Are they equal? " ++ show (myLines sentences == shouldEqual)
