myIterate :: (a -> a) -> a -> [a]
myIterate f b = b : myIterate f (f b)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = case f b of
                  Nothing     -> []
                  Just (x, y) -> x : myUnfoldr f y 

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\x -> Just (x, f x)) x

data BinaryTree a = Leaf
                  | Node (BinaryTree a) a (BinaryTree a)
                  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f x = case f x of
               Nothing                  -> Leaf
               Just (left, root, right) -> Node (unfold f left) root (unfold f right)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold f 0
  where f :: Integer -> Maybe (Integer, Integer, Integer)
        f x
          | x >= n    = Nothing
          | otherwise = Just (x + 1, x, x + 1)
