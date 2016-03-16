module Main where

import Binary
import qualified Chromosome as C
import Chromosome (Chromosome)

import Control.Monad (replicateM)
import Data.List (sortBy)
import System.Random (randomRIO)

type Solution = Float

-- a function that given a certain solution,
-- determines how close the chromesome (represented in binary)
-- comes to achieving this goal solution
data Score = SolutionFound Float | Fitness Float
  deriving Show

fitness :: Solution -> Binary -> Float
fitness s b = let diff = s - C.eval b
              in 1/diff


fitP :: (Score, Float, Chromosome) -> Bool
fitP (SolutionFound _, _, _) = True
fitP _ = False

-- given a fitness function, a desired solution,
-- and a list of possible chromosomes (actually just binary)
-- select 1 from them based on the relative probability of each
-- based off of there proximity to the real solution
rouletteWheel :: Solution -> [Binary] -> IO Binary
rouletteWheel s cs = let f = fitness s
                         fitSum = sum $ map f cs
                         normalizedFits = sortBy (flip compare) $ map (\c -> fitness s c/fitSum) cs
                         sumSoFar = map (\i-> sum (take i normalizedFits)) [1..length cs - 1]
                     in
                       do r <- randomRIO (0.0, 1.0)
                          let index = findAtLeast sumSoFar r
                          return $ cs !! index

  -- retuns the index of the first element of the list for which
  -- the given number is >= than it
findAtLeast :: [Float] -> Float -> Int
findAtLeast fs f = go fs f 0
  where
    go [] _ index = index
    go (f:fs) x index = if f >= x then index
                        else go fs x (index+1)


-- chance that two chromosomes will swap bits
crossoverRate :: Float
crossoverRate = 0.7

crossover :: (Binary, Binary) -> IO (Binary, Binary)
crossover bs@(Binary b1, Binary _) = do
  crossChance <- randomRIO (0.0, 1.0)

  if crossChance < crossoverRate then do
    crossPoint <- randomRIO (0, length b1-1)
    return $ crossAt bs crossPoint
    else return bs

crossAt :: (Binary, Binary) -> Int -> (Binary, Binary)
crossAt (Binary b1, Binary b2) point = let b1' = take point b1 ++ drop point b2
                                           b2' = take point b2 ++ drop point b1
                                       in (Binary b1', Binary b2')

mutationRate :: Float
mutationRate = 0.001

mutate :: Binary -> IO Binary
mutate (Binary b) = Binary <$>
  mapM (\bit -> do
    mChance <- randomRIO (0.0, 1.0)
    if mChance < mutationRate then
      return $ flipBit bit
    else return bit) b


createGeneration :: Int -> Int -> IO [Binary]
createGeneration size binLen = replicateM size $ randomBinary binLen

-- TODO: "mate" chromosomes together by
-- 1. pick two using roulette wheele
-- 2. cross them over eachother
-- 3. mutate both of the offspring
-- 4. repeat 1-3 untill a new pop of N is arived at.
--    This is the next generation
main :: IO ()
main = do
  let popSize = 1000
      chromosomeLength = 52
      solution = 30
  generation <- createGeneration popSize chromosomeLength
  print =<< rouletteWheel solution generation
