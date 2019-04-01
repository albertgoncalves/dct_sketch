{-# OPTIONS_GHC -Wall #-}

import System.Random (mkStdGen, setStdGen, randomIO)

boolPath :: ([a], [a]) -> a -> Bool -> ([a], [a])
boolPath (a, b) x True = (x:a, b)
boolPath (a, b) x False = (a, x:b)

randSplit :: IO ([a], [a]) -> a -> IO ([a], [a])
randSplit ab x =
    ab >>= \ab' -> (randomIO :: IO Bool) >>= return . boolPath ab' x

randPart :: [a] -> IO ([a], [a])
randPart = foldl randSplit $ return ([], [])

shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle [x] = return [x]
shuffle xs =
    randPart xs >>= \(before, after) ->
    shuffle before >>= \before' ->
    shuffle after >>= return . (before' ++)

main :: IO ()
main = (setStdGen . mkStdGen) 2 >> shuffle ([1 .. 25] :: [Int]) >>= print
