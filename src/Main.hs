{-
Main.hs

The main executable, for the hangman game

Dec 22, 2018
-}

module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

-- | main is the starting method for the hangman game
main :: IO ()
main = do
  word <- randomWord'
  let puzzle =
        freshPuzzle (fmap toLower word)
  runGame puzzle

-- Generate Word Methods --

-- | type Wordlist is Syntactic sugar
-- for a list of Strings
type WordList = [String]

-- | Reads words from 'dict.txt' into a list
-- dict.txt originates from MacOS's default dictionary
allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return (lines dict)

-- | The minimum length of a word
minWordLength :: Int
minWordLength = 5

-- | The maximum length of a word
maxWordLength :: Int
maxWordLength = 9

-- | Returns a list of words, based on criteria matching 
-- minWordLength, maxWordLength
gameWords :: IO WordList
gameWords = do
  aw <- allWords
  return (filter gameLength aw)
  where gameLength w =
          let l = length (w :: String)
          in l >= minWordLength
          && l < maxWordLength

-- | Randomly chooses a word from a list
randomWord :: WordList -> IO String
randomWord wl = do
  randomIndex <- randomRIO ( 0, (length wl) - 1)
  return $ wl !! randomIndex

-- | Chooses a random word from gameWord
randomWord' :: IO String
randomWord' = gameWords >>= randomWord

-- Puzzle Handler Methods --

-- | Datatype for Puzzle, contents in order are
-- The word to find, the list of discovered letters, and
-- the list of letters attempted
data Puzzle =
  Puzzle String [Maybe Char] [Char]

-- | defines how the Puzzle datatype is displayed
instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    (intersperse ' ' $
     fmap renderPuzzleChar discovered)
    ++ " Guessed so far: " ++ guessed

-- | Generates a new Puzzle datatype, for a new game
freshPuzzle :: String -> Puzzle
freshPuzzle str = Puzzle str guessed [] where
  guessed = take (length str) $ repeat Nothing

-- | Returns True if the character is in the word
charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _) char = elem char word

-- | Returns True if the character has already been guessed
alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) char = elem char guessed

-- | Renders the characters or blank spaces of the 
-- discovered word, 'Nothing' is displayed as '_'
renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just char) = char 

-- | Places the guessed character into the guess list,
-- and updates the discovered letters if it is in the word
fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle 
                  word 
                  filledInSoFar 
                  s) c =
  Puzzle word newFilledInSoFar (c : s) where 
        zipper guessed wordChar guessChar =
          if wordChar == guessed
            then Just wordChar
            else guessChar

        newFilledInSoFar =
          zipWith (zipper c)
            word filledInSoFar

-- | Handles the character based on if it was
-- previously chosen or is a character in the word,
-- and prints the appropriate message
handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess
      , alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that\
              \ character, pick \
              \ something else!"
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the\
              \ word, filling in the word\
              \ accordingly"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "This character wasn't in\
              \ the word, try again."
      return (fillInCharacter puzzle guess)

-- Game Methods --

-- | Checks if it is game over, based on number of
-- incorrect guesses.
gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess charsFound guessed) =
  if (incorrectGuesses > 7 
      || (length guessed >= length wordToGuess)) then
    do putStrLn "You Lose!"
       putStrLn $
         "The word was: " ++ wordToGuess
       exitSuccess
  else return () where
    incorrectGuesses =
      (length guessed) 
      - (length $ filter (/= Nothing) charsFound)

-- | Prints the win message and exits, if all
-- characters are correctly guessed
gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
  if all isJust filledInSoFar then
    do putStrLn "You win!"
       exitSuccess
  else return ()

-- | Runs the main portion of the game
runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $
    "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _   ->
      putStrLn "Your guess must\
              \ be a single character"