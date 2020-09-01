import Data.List

added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

m :: Maybe Integer
m = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

n :: Maybe Integer
n = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> m <*> n


x :: Maybe Int
x = elemIndex 3 [1, 2, 3, 4, 5]

y :: Maybe Int
y = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x <*> y


xs = [1, 2, 3]
ys = [4, 5, 6]

a :: Maybe Integer
a = lookup 3 $ zip xs ys

b :: Maybe Integer
b = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = fmap sum $ (,) <$> a <*> b
