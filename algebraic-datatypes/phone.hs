import Data.Char
import Data.List

-- NOTE: I don't implement the "wrapping" functionality - not needed for exercises

data DaPhone = DaPhone [(Char, String)] deriving Show

phone :: DaPhone
phone = DaPhone [('1', "1"),    ('2', "abc2"), ('3', "def3")
                ,('4', "ghi4"), ('5', "jkl5"), ('6', "mno6")
                ,('7', "pqrs7"),('8', "tuv8"), ('9', "wzyz9")
                ,('*', "^*"),   ('0', " +_0"), ('#', ".,#")]

convo :: [String]
convo = ["Wanna play 20 questions"
        ,"Ya"
        ,"U 1st haha"
        ,"Lol ok. Have u ever tasted alcohol"
        ,"Lol ya"
        ,"Wow ur cool haha. Ur turn"
        ,"Ok. Do u think I am pretty Lol"
        ,"Lol ya"
        ,"Just makings ure rofl ur turn"]

type Digit = Char
type Presses = Int

-- assuming default phone definition
-- 'a' -> [('2', 1)]
-- 'A' -> [('*', 1), ('2', 1)]
reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps (DaPhone digitMapping) c
  | elem c ['A'..'Z'] = ('*', 1) : reverseTaps phone (toLower c)
  | otherwise         = [(digit, presses)]
    where presses     = length (takeWhile (/= c) s) + 1
          (digit, s)  = head $ filter (\(x, y) -> elem c y) digitMapping

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead phone s = concat $ map (reverseTaps phone) s

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldr (\(d, p) b -> p + b) 0

mostPopularLetter :: String -> Char
mostPopularLetter = fst . maximumBy (\(x, y) (w, z) -> compare y z) . map (\xs -> (head xs, length xs)) . group . sort

coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter . concat

coolestWord :: [String] -> String
coolestWord = max . foldr (\s b -> words s ++ b) [] 
  where max = fst . maximumBy (\(x, y) (w, z) -> compare y z) . map (\xs -> (head xs, length xs)) . group . sort
