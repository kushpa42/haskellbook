eftBool :: Bool -> Bool -> [Bool]
eftBool True  True  = [True]
eftBool False False = [False]
eftBool True  False = []
eftBool False True  = [False, True]

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd LT LT = [LT]
eftOrd _  LT = []
eftOrd GT GT = [GT]
eftOrd GT _  = []
eftOrd x  y  = go x y []
  where go from to xs
          | from < to = go from (pred to) (to : xs)
          | otherwise = to : xs

eftInt :: Int -> Int -> [Int]
eftInt x y = go x y []
  where go x y xs
          | x == (minBound :: Int) && y == (minBound :: Int) = [minBound :: Int]
          | y == (minBound :: Int)                           = []
          | x == (maxBound :: Int) && y == (maxBound :: Int) = [maxBound :: Int]
          | x == (maxBound :: Int)                           = []
          | x <= y                                           = go x (pred y) (y : xs)
          | otherwise                                        = xs

eftChar :: Char -> Char -> [Char]
eftChar x y = go x y []
  where go x y xs
          | x == min && y == min = [min]
          | y == min             = []
          | x == max && y == max = [maxBound]
          | x == max             = []
          | x <= y               = go x (pred y) (y : xs)
          | otherwise            = xs
        max = maxBound :: Char
        min = minBound :: Char
