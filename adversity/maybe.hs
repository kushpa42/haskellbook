-- Small library for Maybe

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _        = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _       = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b f a = case a of
                Just x  -> f x
                Nothing -> b

fromMaybe :: a -> Maybe a -> a
fromMaybe y Nothing  = y
fromMaybe _ (Just x) = x

listToMaybe :: [a] -> Maybe a
listToMaybe []     = Nothing
listToMaybe (x:xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just x) = [x]

catMaybes :: [Maybe a] -> [a]
catMaybes = foldr f []
  where f x xs = case x of
                  Nothing -> xs
                  Just x  -> x : xs

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe xs
  | any isNothing xs = Nothing
  | otherwise        = Just $ catMaybes xs
