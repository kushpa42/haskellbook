import Data.Char

removeLowercase :: String -> String
removeLowercase = filter isUpper

capitalizeFirst :: String -> String
capitalizeFirst []     = []
capitalizeFirst (x:xs) = toUpper x : xs

allCaps :: String -> String
allCaps []     = []
allCaps (x:xs) = toUpper x : allCaps xs

capAndReturn :: String -> Char
capAndReturn s = toUpper $ head s

capAndReturnComposed :: String -> Char
capAndReturnComposed s = (toUpper . head) s

capAndReturnFree :: String -> Char
capAndReturnFree = toUpper . head
