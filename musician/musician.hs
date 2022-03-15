-- Author:    Tabitha Ayudya <tayudya@student.unimelb.edu.au>
-- Purpose:   Play the logical guessing game of Musician
--            This program is written for Project 2 of COMP30020
--            created 21/11/2020
--
-- The program implements both composer and performer parts of the game.
-- The program takes a target chord at the beginning of the round. It then
-- makes a guess, and gives feedback for that guess which contains the number
-- of correct pitches, notes, and octaves respectively. The program then
-- makes another guess in light of that feedback. This process loops until
-- the correct target is guessed. At the end the program prints the number of
-- guesses needed to reach the target.

module Proj2 (Pitch, toPitch, feedback,
              GameState, initialGuess, nextGuess) where

import Data.List
import Data.Tuple

-- The type Pitch represents pitches in the Musician game
data Pitch = Pitch (Char,Char)
    deriving (Eq,Ord)

instance Show Pitch where
    show (Pitch (note,octave)) = [note] ++ [octave]

-- The type GameState represents the information maintained between guesses.
-- The info to be retained is the a list of all remaining possible chords,
-- as well as their respective scores when put through the feedback function
type GameState = [([Pitch],(Int,Int,Int))]

-- The toPitch function takes a string and returns Just the Pitch
-- named by the string, or Nothing if the string is not a valid pitch name.
toPitch :: String -> Maybe Pitch
toPitch (note:octave:[])
    | isMember note ['A'..'G'] &&
      isMember octave ['1'..'3'] = Just (Pitch (note,octave))
    | otherwise                  = Nothing
toPitch other                    = Nothing

-- The feedback function takes a target snd a guess, and returns the
-- approriate feedback as a triple of correct pitches, notes, and octaves
feedback :: [Pitch] -> [Pitch] -> (Int,Int,Int)
feedback target guess = (length pitch,length note,length octave)
    where pitch  = intersect target guess
          note   = minIntersect
                  (map fst (map getTuple (target \\ pitch)))
                  (map fst (map getTuple (guess \\ pitch)))
          octave = minIntersect
                  (map snd (map getTuple (target \\ pitch)))
                  (map snd (map getTuple (guess \\ pitch)))

-- The initialGuess function takes no arguments. It gives a pair of an initial
-- guess and the very first game state

-- The firstGuess is hardcoded here to be A1,B1,C1. The reason for that is
-- that a guess with just 1 octave and as many notes as possible seem to
-- eliminate the largest size of possible chords. The choice of the notes A,B,C
-- is arbitrary though, as it would give a similar result to any other 3
-- different notes. So is the choice of octave 1
initialGuess :: ([Pitch],GameState)
initialGuess = (firstGuess, zip allCombo (map (feedback firstGuess) allCombo))
    where firstGuess = [(Pitch ('A','1')),(Pitch ('B','1')),(Pitch ('C','1'))]
          allCombo   = subset 3 [Pitch (note,octave)
                     | note <- ['A'..'G'], octave <- ['1'..'3']]

-- The sameSnd takes two tuples of size 2 and checks whether or not the second
-- elements are equal
sameSnd :: Eq b => (a,b) -> (a,b) -> Bool
sameSnd t1 t2 = (snd t1) == (snd t2)

-- The nextGuess function takes a pair of the previous guess and game state,
-- and the feedback to this guess, and returns a pair of the next guess and
-- game state

-- General strategy for the next guesses:
-- Since the GameState all the remaining possible chords, we could compute
-- the score if the chords were indeed the target for our current guess.
-- After given the feedback, all chords with scores that are not the
-- same could be eliminated, and the next guess taken from the remaining of
-- the list. The next guess is just a random chord from the list which in this
-- case is the first chord in the list.

nextGuess :: ([Pitch],GameState) -> (Int,Int,Int) -> ([Pitch],GameState)
nextGuess (_,oldGameState) score = (newGuess,newGameState)
    where newCandidates = map fst (filter (\x -> snd x == score) oldGameState)
          (p:ps)        = newCandidates
          newGuess      = p
          newGameState  = zip newCandidates
                             (map (feedback newGuess) newCandidates)

-- The isMember function takes an element and a list and returns True iff
-- the element exists in the list
isMember :: Eq a => a -> [a] -> Bool
isMember n []   = False
isMember n (x:xs)
    | n == x    = True
    | otherwise = isMember n xs

-- The getTuple function takes a Pitch and returns it in its tuple form
getTuple :: Pitch -> (Char,Char)
getTuple (Pitch (note,octave)) = (note,octave)

-- The minIntersect function takes two lists and returns their intersection,
-- which does not include duplicates unless it is present in both inputs
minIntersect :: Eq a => [a] -> [a] -> [a]
minIntersect xs ys = xs \\ (xs \\ ys)

-- The subset function takes an int n and a list, and returns a list of lists
-- containing the subsets of the list that is of size n
subset :: Int -> [a] -> [[a]]
subset 0 _      = [[]]
subset _ []     = []
subset n (x:xs) = (map (x:) (subset (n-1) xs)) ++ subset n xs
