myAnd :: [Bool] -> Bool
myAnd = foldr (&&) True

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
-- myAny f xs = myOr (map f xs)
-- myAny      = \f -> \xs -> myOr (map f xs) 
--            = \f -> \xs -> (myOr . (map f)) xs 
--            = \f -> myOr . (map f)
--            = \f -> ((.) myOr) (map f)
--            = \f -> (((.) myOr) . map) f
--            = ((.) myOr) . map

myAny = ((.) myOr) . map

-- myAny = (\f -> foldr (\a b -> f a || b) False) 

myElem :: Eq a => a -> [a] -> Bool
-- myElem x = foldr (\a b -> a == x || b) False 
myElem x = any (== x)

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\a b -> f a : b) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\a b -> if f a then a : b else b) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap = ((.) squish) . map

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f xs = foldr (\a b -> if f a b == GT then a else b) (head xs) xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f xs = foldr (\a b -> if f a b == LT then a else b) (head xs) xs
