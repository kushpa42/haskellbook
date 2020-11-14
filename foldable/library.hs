import Data.Foldable
import Data.Monoid

sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

sum'' :: (Foldable t, Num a) => t a -> a
sum'' = foldr (+) 0

product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

product'' :: (Foldable t, Num a) => t a -> a
product'' = foldr (*) 1

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x = getAny . foldMap (\y -> Any (x == y))

elem'' :: (Foldable t, Eq a) => a -> t a -> Bool
elem'' x = foldr (\y b -> x == y || b) False

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = foldr f Nothing
  where f x Nothing  = Just x
        f x (Just y) = Just $ min x y

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = foldr f Nothing
  where f x Nothing  = Just x
        f x (Just y) = Just $ max x y

null' :: (Foldable t) => t a -> Bool
null' = (False ||) . getAny . foldMap (\_ -> Any True)

null'' :: (Foldable t, Ord a) => t a -> Bool
null'' = foldr (\_ b -> True || b) False

length' :: (Foldable t) => t a -> Int
length' = getSum . foldMap (\_ -> Sum 1)

length'' :: (Foldable t) => t a -> Int
length'' = foldr (\_ b -> 1 + b) 0

toList' :: (Foldable t) => t a -> [a]
toList' = foldMap (\x -> [x])

toList'' :: (Foldable t) => t a -> [a]
toList'' = foldr (\x b -> x : b) []

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\x b -> f x <> b) mempty
