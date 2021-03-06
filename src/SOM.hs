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

distance :: (Floating num, Vector.Storable num, Num num) => Vector num -> Vector num -> num
distance v1 v2
  | Vector.length v1 == Vector.length v2 = sqrt $ Vector.sum $ Vector.map (^2) $ Vector.zipWith (-) v1 v2
  | otherwise = error "Vector lenght do not match."

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
winnerNeuron (SOM neurons) input = findNearest . map (\(i,x) -> (i,distance input x)) $ neurons

dist :: Int -> Int -> Double
dist a b = toEnum (abs $ a-b)

neighbour :: Int -> (Int -> Double) -> Int -> Int -> Double
neighbour t s g1 g2 = 1 -- 0.1 * exp (-dist g1 g2/(2*s t^2))

learnStep :: SOM -> Int -> (Int -> Double) -> (Int -> Double) -> Vector Double -> SOM
learnStep som@(SOM neurons) t l s x = SOM
    $ map (\(i,c)
      -> ( i
         , if i == winner
            then add c (Vector.map (l t *) (sub x c))
            else c))
    $ neurons
  where
    winner = winnerNeuron som x
-- SOM $ map (\(i,c) -> (i,add c (Vector.map ((l t * neighbour t s i winner) *) (sub x c)))) neurons

learn' :: MonadRandom m => SOM -> Int -> Int -> (Int -> Double) -> (Int -> Double) -> [Vector Double] -> m SOM
learn' som n t l s samples
  | n > t = do
      x <- uniform samples
      learn' (learnStep som t l s x) n (t+1) l s samples
  | otherwise = pure som

learn :: MonadRandom m => SOM -> Int -> [Vector Double] -> m SOM
learn som n inputs = learn' som n 1 l s inputs
  where
    l t = 1 / (1 + 2**(-toEnum t))
    s t = (toEnum t / toEnum n)^2
