i :: a -> a
i = id

c :: a -> b -> a
c = decurry fst

c'' :: b -> a -> b
c'' = c

c' :: a -> b -> b
c' = decurry snd 

curry f (a, b) = f a b
decurry f a b = f (a, b)

r :: [a] -> [a]
r = reverse

co :: (b -> c) -> (a -> b) -> a -> c
co f g x = f $ g x

a :: (a -> c) -> a -> a
a f x = x

a' :: (a -> b) -> a -> b
a' f x = f x
