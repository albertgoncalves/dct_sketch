{-# OPTIONS_GHC -Wall #-}

import System.Random (mkStdGen, setStdGen, randomIO)

split :: IO ([a], [a]) -> a -> IO ([a], [a])
split ab x = ab >>= \ab' -> rand >>= return . path ab' x
  where
    rand = randomIO :: IO Bool
    path (a, b) x' True = (x':a, b)
    path (a, b) x' False = (a, x':b)

shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle [x] = return [x]
shuffle xs =
    foldl split empty xs >>= \(before, after) ->
    shuffle before >>= \before' ->
    shuffle after >>=
    return . (before' ++)
  where
    empty = return ([], [])

main :: IO ()
main =
    (setStdGen . mkStdGen) 2 >>
    shuffle ([1 .. 25] :: [Int]) >>=
    print
