module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

import Test.Hspec
import Test.QuickCheck

newtype WordList = WordList [String] deriving (Eq, Show)

data Puzzle = Puzzle String [Maybe Char] [Char] deriving (Eq)

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
          (intersperse ' ' $ fmap renderPuzzleChar discovered)
          ++ " Guessed so far: " ++ guessed

-- Generate list of words
allWords :: IO WordList
allWords = do
        dict <- readFile "../data/dict.txt"
        return $ WordList (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
        WordList aw <- allWords
        return $ WordList (filter removeApostrophe $ filter gameLength aw)
          where gameLength w =
                  let l = length (w :: String)
                   in l >= minWordLength
                   && l <  maxWordLength
                removeApostrophe w = not $ elem '\'' w

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
        randomIndex <- randomRIO (0, length wl - 1)
        return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

-- Puzzle and associated logic
freshPuzzle :: String -> Puzzle
freshPuzzle s = Puzzle s (map (const Nothing) s) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle s _ _) c = elem c s

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) c = elem c guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing  = '_'
renderPuzzleChar (Just c) = c

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c = Puzzle word newFilledInSoFar (c : s)
  where zipper guessed wordChar guessChar =
          if wordChar == guessed
             then Just wordChar
             else guessChar
        newFilledInSoFar = zipWith (zipper c) word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
        putStrLn $ "Your guess was: " ++ [guess]
        case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
          (_, True) -> do
                  putStrLn "You already guessed that character, pick something else!"
                  return puzzle
          (True, _) -> do
                  putStrLn "This character was in the word, filling in accordingly."
                  return (fillInCharacter puzzle guess)
          (False, _) -> do
                  putStrLn "This character wasn't in the word, try again."
                  return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) =
        if (length $ filter (\c -> not $ elem c wordToGuess) guessed) > 7 then
                                                                          do putStrLn "You lose!"
                                                                             putStrLn $ "The word was: " ++ wordToGuess
                                                                             exitSuccess
        else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
        if all isJust filledInSoFar then
                                    do putStrLn "You win!"
                                       exitSuccess
        else return ()

-- Run game
runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
        gameOver puzzle
        gameWin puzzle
        putStrLn $ "Current puzzle is: " ++ show puzzle
        putStr "Guess a letter: "
        guess <- getLine
        case guess of
          [c] -> handleGuess puzzle (toLower c) >>= runGame
          _   -> putStrLn "Your guess must be a single character"

main :: IO ()
main = do
        word <- randomWord'
        let puzzle = freshPuzzle (fmap toLower word)
        runGame puzzle

-- Tests
prop_appendGuess :: String -> Char -> Bool
prop_appendGuess s c = length newList > length oldList
  where (Puzzle _ _ oldList) = puzzle
        (Puzzle _ _ newList) = fillInCharacter puzzle c
        puzzle               = freshPuzzle s

prop_fillInCorrectGuess :: String -> Char -> Bool
prop_fillInCorrectGuess s c = if elem c s then justLength newList >  justLength oldList
                                          else justLength newList == justLength oldList
  where (Puzzle _ oldList _) = puzzle
        (Puzzle _ newList _) = fillInCharacter puzzle c
        puzzle               = freshPuzzle s
        justLength           = length . filter isJust


runQuickCheck :: IO ()
runQuickCheck = do
    quickCheck prop_appendGuess
    quickCheck prop_fillInCorrectGuess

runHspec :: IO ()
runHspec = hspec $ do
    describe "handleGuess" $ do
        it "guessing a character again should return same puzzle" $ do
            let puzzle = (Puzzle "abcd" [] ['a']) in
                do
                    p <- handleGuess puzzle 'a'
                    puzzle `shouldBe` p 

        it "guessing a correct character should fill it in" $ do
            let puzzle = (Puzzle "ab" [Nothing, Nothing] []) in
                do
                    p <- handleGuess puzzle 'a'
                    p `shouldBe` (Puzzle "ab" [(Just 'a'), Nothing] ['a'])
        
        it "guessing an incorrect character should add to the guessed list" $ do
            let puzzle = (Puzzle "ab" [Nothing, Nothing] []) in
                do
                    p <- handleGuess puzzle 'e'
                    p `shouldBe` (Puzzle "ab" [Nothing, Nothing] ['e'])
