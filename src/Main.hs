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
fitness :: Solution -> Binary -> Float
fitness s b = 1/(s - C.eval b)

isSolution :: Solution -> Binary -> Bool
isSolution s b = abs (s - C.eval b) < 0.01

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

crossover :: Binary -> Binary -> IO (Binary, Binary)
crossover b1@(Binary b) b2 = do
  crossChance <- randomRIO (0.0, 1.0)

  if crossChance < crossoverRate then do
    crossPoint <- randomRIO (0, length b-1)
    return $ crossAt b1 b2 crossPoint
    else return (b1, b2)

crossAt :: Binary -> Binary -> Int -> (Binary, Binary)
crossAt (Binary b1) (Binary b2) point = let b1' = take point b1 ++ drop point b2
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

mate :: [Binary] -> IO (Binary, Binary)
mate gen = do
  partner1 <- rouletteWheel solution gen
  partner2 <- rouletteWheel solution gen
  (child1, child2) <- crossover partner1 partner2
  child1' <- mutate child1
  child2' <- mutate child2
  return (child1', child2')

-- keep the population mating until a new generation
-- of children has been made
reproduce :: [Binary] -> [Binary] -> IO [Binary]
reproduce gen children =
  if length children >= length gen then
    return children
  else do
    (child1, child2) <- mate gen
    reproduce gen (child1:child2:children)

solution :: Float
solution = 30.5

main :: IO ()
main = do
  let popSize = 1000
  generation <- createGeneration popSize 52
  answer <- loop generation popSize
  let ansStr = C.showChromosome $ C.decodeChromosome answer
  putStrLn $ ansStr ++ " = " ++ show solution

loop :: [Binary] -> Int -> IO Binary
loop gen size =
  let answers = filter (isSolution solution) gen
  in if not $ null answers then
       return $ head answers
  else do
       newGen <- reproduce [] gen
       loop newGen size
