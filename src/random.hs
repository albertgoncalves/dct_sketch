{-# OPTIONS_GHC -Wall #-}

import Data.Function (on)
import Data.List (intercalate, partition)
import System.Random (random, split)
import System.Random.TF (TFGen, seedTFGen)

mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair = uncurry . on (,)

randBools :: TFGen -> [(Bool, TFGen)]
randBools g = iterate (random . snd) (random g)

partitionR :: [a] -> TFGen -> (([a], TFGen), ([a], TFGen))
partitionR xs g = ((ls, lg), (rs, rg))
  where
    ys = zip xs (randBools g)
    f = mapPair (map fst)
    (lg, rg) = (split . snd . snd) (foldl1 (curry snd) ys)
    (ls, rs) = f (partition (fst . snd) ys)

shuffle :: ([a], TFGen) -> ([a], TFGen)
shuffle ([], g) = ([], g)
shuffle ([x], g) = ([x], g)
shuffle (xs, g) = (ls' ++ rs', rg')
  where
    ((ls', _), (rs', rg')) = mapPair shuffle $ partitionR xs g

loop :: Int -> (a -> (b, a)) -> a -> [b] -> [b]
loop n f x accu
    | n <= 0 = accu
    | otherwise = loop (n - 1) f x' (xs:accu)
  where
    (xs, x') = f x

main :: IO ()
main = pprint ys
  where
    n = 10
    xs = [1 .. 5] :: [Int]
    seed = seedTFGen (0, 0, 0, 0)
    ys = loop n (curry shuffle xs) seed []
    pprint = putStr . unlines . map (intercalate "," . map show)
