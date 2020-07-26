myAnd :: [Bool] -> Bool
myAnd []     = True
myAnd (x:xs) = x && myAnd xs

myOr :: [Bool] -> Bool
myOr []     = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ []     = False
myAny f (x:xs) = f x || myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ []     = False
myElem e (x:xs)
  | e == x    = True
  | otherwise = myElem e xs 

myElemAny :: Eq a => a -> [a] -> Bool
myElemAny e xs = myAny (== e) xs

myReverse :: [a] -> [a]
myReverse []     = []
myReverse [x]    = [x]
myReverse (x:xs) = myReverse xs ++ [x] 

squish :: [[a]] -> [a]
squish []     = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f [x]    = f x ++ []
squishMap f (x:xs) = f x ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMinMaxBy :: Ordering -> (a -> a -> Ordering) -> [a] -> a
myMinMaxBy _        _ []  = error "empty list" 
myMinMaxBy _        _ [x] = x
myMinMaxBy ordering f ([x1, x2])
  | f x1 x2 == ordering = x1
  | otherwise           = x2

myMinMaxBy ordering f (x1:x2:xs)
  | f x1 x2 == ordering = myMinMaxBy ordering f (x1:xs)
  | otherwise           = myMinMaxBy ordering f (x2:xs)

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy = myMinMaxBy GT 

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy = myMinMaxBy LT 

myMaximum :: (Ord a) => [a] -> a
myMaximum [] = error "empty list" 
myMaximum [x] = x
myMaximum (x:xs) = undefined

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare 
