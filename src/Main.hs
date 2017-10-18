module Main where

import SOM
import Codec.Picture
  ( convertRGB8
  , readImage
  , imageData
  , saveJpgImage
  , DynamicImage (ImageRGB8)
  , Image (Image))
import Codec.Picture.Extra (scaleBilinear)
import System.Random (getStdGen)
import System.Environment (getArgs)
import qualified Data.Vector.Storable as V

import Control.Monad (forM, forM_)

size = 128

main :: IO ()
main = do
  n:paths <- getArgs

  putStrLn "Reading..."
  vecs <- forM paths $ \path -> do
    i <- readImage path
    i' <- case i of
      Left err -> error err
      Right i' -> return
        $ V.map (toEnum . fromEnum)
        $ imageData
        $ scaleBilinear size size
        $ convertRGB8
        $ i'
    return (path, i')

  putStrLn "Initializing SOM..."
  som <- initSOM (size * size * 3) (read n) (0.0, 255.0)

  putStrLn "Learning..."
  let som' = learn som (length vecs * 300) (map snd vecs)

  putStrLn "Querying..."
  forM_ vecs $ \(path, vec) -> do
    let w = winnerNeuron som' vec
    putStrLn $ path ++ "\t" ++ show w

  putStrLn "Printing Prototypes..."
  forM_ (somNeurons som') $ \(index, vec) -> do
    let pic = ImageRGB8
            $ Image size size
            $ V.map (toEnum . fromEnum)
            $ vec
    saveJpgImage 100 ("neuron_" ++ show index ++ ".jpg") pic





