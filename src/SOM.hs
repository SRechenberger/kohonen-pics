module SOM where

import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as Vector

import Data.List (minimumBy)

import Data.Function (on)

import Control.Monad.Random

import Control.Monad (forM)

-- import Data.Map (Map)
-- import Data.Map as Map

type Map key element = [(key, element)]

newtype SOM = SOM { somNeurons :: Map Int (Vector Double) }

randomVector :: MonadRandom m => Int -> (Double, Double) -> m (Vector Double)
randomVector len range = do
  vals <- getRandomRs range
  return $ Vector.fromList $ take len vals

initSOM :: MonadRandom m => Int -> Int -> (Double, Double) -> m SOM
initSOM len neurons range = do
  vecs <- forM [1..neurons] $ \i -> do
    v <- randomVector len range
    return (i,v)
  return $ SOM vecs

dot :: (Vector.Storable num, Num num) => Vector num -> Vector num -> num
dot v1 v2
  | Vector.length v1 == Vector.length v2 = Vector.sum $ Vector.zipWith (*) v1 v2
  | otherwise = error "Vector lengths do not match."

add :: (Vector.Storable num, Num num) => Vector num -> Vector num -> Vector num
add v1 v2
  | Vector.length v1 == Vector.length v2 = Vector.zipWith (+) v1 v2
  | otherwise = error "Vector lengths do not match."

sub :: (Vector.Storable num, Num num) => Vector num -> Vector num -> Vector num
sub v1 v2
  | Vector.length v1 == Vector.length v2 = Vector.zipWith (-) v1 v2
  | otherwise = error "Vector lengths do not match."

findNearest :: (Ord value) => Map position value -> position
findNearest = fst . minimumBy (compare `on` snd)

winnerNeuron :: SOM -> Vector Double -> Int
winnerNeuron (SOM neurons) input = findNearest . map (\(i,x) -> (i,dot input x)) $ neurons

dist :: Int -> Int -> Double
dist a b = toEnum (abs $ a-b) ^ 2

neighbour :: Int -> (Int -> Double) -> Int -> Int -> Double
neighbour t s g1 g2 = exp (-dist g1 g2/(2*s t^2))

learnStep :: SOM -> Int -> (Int -> Double) -> (Int -> Double) -> Vector Double -> SOM
learnStep som@(SOM neurons) t l s x = SOM $ map (\(i,c) -> (i,add c (Vector.map ((l t * neighbour t s i winner) *) (sub x c)))) neurons
  where
    winner = winnerNeuron som x

learn' :: SOM -> Int -> Int -> (Int -> Double) -> (Int -> Double) -> [Vector Double] -> [Vector Double] -> SOM
learn' som n t l s [] processed = learn' som n t l s (reverse processed) []
learn' som n t l s (x:xs) processed
  | n > t = learn' (learnStep som t l s x) n (t+1) l s xs (x:processed)
  | otherwise = som

learn :: SOM -> Int -> [Vector Double] -> SOM
learn som n inputs = learn' som n 1 l s inputs []
  where
    l t = 10 * (0.1/10)**(toEnum t / toEnum n)
    s t = 3 * (0.1/3)**(toEnum t / toEnum n)
