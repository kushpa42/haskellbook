module LearnParsers where

import Control.Applicative
import Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

one :: Parser Char
one = char '1'

one' :: Parser a
one' = one >> stop

oneTwo :: Parser Char
oneTwo = char '1' >> char '2'

oneTwo' :: Parser a
oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "123"

testParse' :: Parser () -> IO ()
testParse' p = print $ parseString p mempty "123"

testStringParse :: Parser String -> String -> IO ()
testStringParse p s = print $ parseString p mempty s

-- Exercises: Parsing Practice
-- 1
oneEOF :: Parser ()
oneEOF = one >> eof

oneTwoEOF :: Parser ()
oneTwoEOF = oneTwo >> eof

-- 2
oneTwoThree :: Parser String
oneTwoThree = string "123" <|> string "12" <|> string "1"

-- 3
string' :: String -> Parser String
string' str = foldr (\c ps -> (:) <$> char c <*> ps) (return "") str


pNL :: String -> IO ()
pNL s = putStrLn ('\n' : s)

main :: IO ()
main = do
    pNL "stop:"
    testParse stop
    
    pNL "one:"
    testParse one

    pNL "one':"
    testParse one'

    pNL "oneTwo:"
    testParse oneTwo

    pNL "oneTwo':"
    testParse oneTwo'
