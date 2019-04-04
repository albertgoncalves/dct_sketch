{-# OPTIONS_GHC -Wall #-}

import Data.Function (on)
import Data.List (intercalate, partition)
import System.Random (mkStdGen, setStdGen, randomIO)

mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair = uncurry . on (,)

attach :: a -> IO (Bool, a)
attach x = (randomIO :: IO Bool) >>= \b -> return (b, x)

partitionR :: [a] -> IO ([a], [a])
partitionR xs = lr >>= \(l, r) -> return $ mapPair (map snd) (l, r)
  where
    lr = fmap (partition fst) (mapM attach xs)

shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle [x] = return [x]
shuffle xs = lr >>= \(l, r) -> shuffle l >>= \l' -> fmap (l' ++) (shuffle r)
  where
    lr = partitionR xs

loop :: Monad m => Int -> m a -> m a
loop n f
    | n <= 0 = f
    | otherwise = f >> loop (n - 1) f

main :: IO ()
main = (setStdGen . mkStdGen) 0 >> loop n f
  where
    n = 10 :: Int
    xs' = [1 .. 5] :: [Int]
    f = shuffle xs' >>= putStrLn . intercalate "," . map show
