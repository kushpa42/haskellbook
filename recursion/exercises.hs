cattyConny :: String -> String -> String
cattyConny x y = x ++ " meow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe = flippy "haha"

data DividedResult = Result Integer | DividedByZero deriving Show

fixedDividedBy :: Integer -> Integer -> DividedResult
fixedDividedBy num denom
  | denom == 0 = DividedByZero
  | num > 0 && denom > 0 = Result (fst $ dividedBy num denom)
  | num < 0 && denom < 0 = Result (fst $ dividedBy (-num) (-denom))
  | otherwise            = Result ((*) (-1) . fst $ dividedBy (abs num) (abs denom))

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go  n   d count
          | n < d     = (count, n)
          | otherwise = go (n - d) d (count + 1)

-- divideBy 15 2 == go 15 2 0
--                  otherwise = go 13 2 1
--                  otherwise = go 11 2 2
--                  otherwise = go  9 2 3
--                  otherwise = go  7 2 4
--                  otherwise = go  5 2 5
--                  otherwise = go  3 2 6
--                  otherwise = go  1 2 7
--                  1 < 2     = (7, 1)

sumTo :: (Eq a, Num a) => a -> a
sumTo 0 = 0
sumTo 1 = 1
sumTo n = n + sumTo (n - 1)

multiply :: (Integral a) => a -> a -> a
multiply x y
  | x > 0 && y > 0 = multiply' x y
  | x < 0 && y < 0 = multiply' (-x) (-y)
  | otherwise      = multiply' (abs x) (abs y) * (-1) 
  where 
    multiply' 0 _ = 0
    multiply' _ 0 = 0
    multiply' x y = x + multiply x (y - 1)

mc91 :: Integral a => a -> a
mc91 n
  | n >  100 = n - 10
  | n <= 100 = mc91 $ mc91 (n + 11)
