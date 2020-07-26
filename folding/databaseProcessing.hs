import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase = [DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123)),
               DbNumber 9001,
               DbString "Hello, world!",
               DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123)),
               DbNumber 9009
              ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate  = foldr f [] 
  where f a b = case a of
                  DbDate time -> time : b
                  otherwise   -> b

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr f []
  where f a b  = case a of
                   DbNumber num -> num : b
                   otherwise    -> b

-- Assumption: db does not contain any dates larger than current time
-- Therefore, this will return the largest UTCTime value in database
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = foldr f minTime . filterDbDate
  where f a b   = if a > b then a else b
        minTime = UTCTime (fromGregorian 0 0 0) (secondsToDiffTime 0) 

sumDb :: [DatabaseItem] -> Integer
sumDb = foldr (+) 0 . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb db = (fromInteger $ sumDb db) / (fromInteger len)
  where len = foldr (\a b -> b + 1) 0 $ filterDbNumber db
