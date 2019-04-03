{-# OPTIONS_GHC -Wall #-}

import Data.Word (Word64)
import System.Random (Random, random)
import System.Random.TF (TFGen, seedTFGen)

type Seed = (Word64, Word64, Word64, Word64)
type RandGen a = (a, TFGen)

randInit :: (TFGen -> (a, TFGen)) -> Seed -> (a, TFGen)
randInit g = g . seedTFGen

randFold :: Random a => (a, TFGen) -> (a, TFGen)
randFold = random . snd

randIter
    :: Random a => (RandGen a -> RandGen a)
    -> Int
    -> RandGen a
    -> [RandGen a]
randIter f n = take n . iterate f

main :: IO ()
main = print $ (f 10) seed
  where
    seed = (0, 0, 0, 0)
    random' = random :: TFGen -> RandGen Bool
    f n = map fst . randIter randFold n . randInit random'
