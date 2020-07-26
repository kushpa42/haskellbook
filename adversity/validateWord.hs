newtype Word' = Word' String deriving (Eq, Show)

vowels :: String
vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord s = case vowelCount > consonantCount of
           True  -> Nothing
           False -> Just (Word' s)
        where vowelCount     = length $ filter (\c -> elem c vowels) s
              consonantCount = length s - vowelCount
