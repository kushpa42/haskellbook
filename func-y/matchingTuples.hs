module TupleFunctions where

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f (a, b) (c, d) = ((b, d), (a, c))
