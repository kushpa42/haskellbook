import Control.Applicative

(<||>) :: (a -> Bool)
       -> (a -> Bool)
       -> a
       -> Bool

-- Combine two boolean functions
(<||>) = liftA2 (||)
