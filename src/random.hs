{-# OPTIONS_GHC -Wall #-}

import Data.Function (on)
import Data.List (partition)
import System.Random (random, split)
import System.Random.TF (TFGen, seedTFGen)

mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair = uncurry . on (,)

randBools :: TFGen -> [(Bool, TFGen)]
randBools g = iterate (random . snd) (random g)

partitionR :: [a] -> TFGen -> (([a], [a]), (TFGen, TFGen))
partitionR xs g = (f (partition (fst . snd) ys), g')
  where
    ys = zip xs (randBools g)
    f = mapPair (map fst)
    g' = (split . snd . snd) (foldl1 (curry snd) ys)

loop :: Int -> (a -> (b, (a, a))) -> (a, a) -> [b] -> [b]
loop n f x accu
    | n <= 0 = accu
    | otherwise = loop (n - 1) f x' (xs:accu)
  where
    (xs, x') = f (snd x)

main :: IO ()
main = (putStr . unlines . map show) (loop n (partitionR xs) seed [])
  where
    n = 10
    xs = [1 .. 10] :: [Int]
    seed = split $ seedTFGen (0, 0, 0, 0)
