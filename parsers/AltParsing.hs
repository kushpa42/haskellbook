{-# LANGUAGE QuasiQuotes #-}

module AltParsing where

import Control.Applicative
import Text.RawString.QQ
import Text.Trifecta

type NumberOrString = Either Integer String

blah = "blah"
num = "123"
bothNumString = "123blah789"

eitherOr :: String
eitherOr = [r|
123
abc
456
def
|]

parseNos :: Parser NumberOrString
parseNos = skipMany (oneOf "\n") >> (Left <$> integer) <|> (Right <$> some letter)

parseNos' :: Parser NumberOrString
parseNos' = do
    skipMany (oneOf "\n")
    v <- (Left <$> integer) <|> (Right <$> some letter)
    skipMany (oneOf "\n")
    return v

main :: IO ()
main = do
    let p f i = parseString f mempty i
    print $ p (some parseNos') eitherOr
    -- print $ p (some letter) blah
    -- print $ p integer num
    -- print $ p parseNos blah
    -- print $ p parseNos num
    -- print $ p (many parseNos) bothNumString
    -- print $ p (some parseNos) bothNumString
