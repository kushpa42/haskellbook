import Data.List (sort)

i :: Num a => a
i = 1

f :: RealFrac a => a
f = 1.0

myX = 1 :: Int

sigmund :: Num a => a -> Int
sigmund x = myX


-- jung :: Ord a => [a] -> a
jung :: [Int] -> Int
jung xs = head (sort xs)

-- young :: [Char] -> Char
young :: Ord a => [a] -> a
young xs = head (sort xs)


mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
-- signifer :: Ord a => [a] -> a
signifier xs = head (mySort xs)
