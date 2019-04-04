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

shuffle :: ([a], TFGen) -> ([a], TFGen)
shuffle ([], g) = ([], g)
shuffle ([x], g) = ([x], g)
shuffle (xs, g) = (ls' ++ rs', rg')
  where
    ((ls', _), (rs', rg')) = mapPair shuffle (partitionR xs g)

loop :: Int -> (a -> (b, a)) -> a -> [b] -> [b]
loop n f x accu
    | n <= 0 = accu
    | otherwise = loop (n - 1) f x' (xs:accu)
  where
    (xs, x') = f x

main :: IO ()
main = pprint ys
  where
    n = 20
    xs = [1 .. 5] :: [Int]
    seed = seedTFGen (0, 0, 0, 0)
    ys = loop n (curry shuffle xs) seed []
    pprint = putStr . unlines . map (intercalate "," . map show)
