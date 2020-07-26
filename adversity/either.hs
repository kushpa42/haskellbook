-- Small library for Either

-- data Either a b = Left a | Right b

lefts' :: [Either a b] -> [a]
lefts' = foldr f []
  where f x xs = case x of
                   Left a    -> a : xs
                   otherwise -> xs

rights' :: [Either a b] -> [b]
rights' = foldr f []
  where f x xs = case x of
                   Right b   -> b : xs
                   otherwise -> xs

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = (\xs -> (lefts' xs, rights' xs))

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right b) = Just $ f b
eitherMaybe' _ _         = Nothing

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f g (Left a)  = f a
either' f g (Right b) = g b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f x = either' (\_ -> Nothing) (\x -> Just $ f x) x
