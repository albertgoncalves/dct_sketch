{-# OPTIONS_GHC -Wall #-}

import Data.Function (on)
import Data.List (intercalate, partition)
import System.Random (next, random, split)
import System.Random.TF (TFGen, seedTFGen)

mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair = uncurry . on (,)

attach :: [a] -> TFGen -> [(a, Bool)] -> ([(a, Bool)], TFGen)
attach [] g accu = (accu, snd $ next g)
attach (x:xs) g accu = attach xs g'((x, b):accu)
  where
    (b, g') = random g

partitionR :: [a] -> TFGen -> (([a], TFGen), ([a], TFGen))
partitionR xs g = ((ls, lg), (rs, rg))
  where
    (ys, g') = attach xs g []
    (ls, rs) = mapPair (map fst) (partition snd ys)
    (lg, rg) = split g'

shuffle :: [a] -> TFGen -> ([a], TFGen)
shuffle [] g = ([], g)
shuffle [x] g = ([x], g)
shuffle xs g = (ls' ++ rs', rg')
  where
    ((ls', _), (rs', rg')) = mapPair (uncurry shuffle) (partitionR xs g)

loop :: Show a => Int -> (TFGen -> ([a], TFGen)) -> TFGen -> IO ()
loop n f g
    | n <= 0 = return ()
    | otherwise = pprint xs >> loop (n - 1) f g'
  where
    (xs, g') = f g
    pprint = putStrLn . intercalate "," . map show

main :: IO ()
main = loop n (shuffle xs) seed
  where
    n = 10
    xs = [1 .. 5] :: [Int]
    seed = seedTFGen (0, 0, 0, 0)
