import Control.Monad
import Data.Char
import System.Exit (exitSuccess)

palindrome :: IO ()
palindrome = forever $ do
        line1 <- getLine
        let s = map toLower $ filter isAlpha  line1
        case (s == reverse s) of
          True -> putStrLn "It's a palindrome"
          False -> do
                  putStrLn "Nope!"
                  exitSuccess
