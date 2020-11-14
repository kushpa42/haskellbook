{-# LANGUAGE QuasiQuotes #-}

module DecimalOrFraction where

import Control.Applicative
import Text.RawString.QQ
import Text.Trifecta

import Fractions (virtuousFraction)

type DecimalOrFraction = Either Integer Rational

num :: String
num = "123"

fraction :: String
fraction = "1/2"

parseNos :: Parser DecimalOrFraction
parseNos = (Right <$> try virtuousFraction) <|> (Left <$> try decimal)

main :: IO ()
main = do
    let p f i = parseString f mempty i
    print $ p (some parseNos) num
    print $ p (some parseNos) fraction
    print $ p (some parseNos) "0/1"

