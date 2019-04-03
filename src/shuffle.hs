{-# OPTIONS_GHC -Wall #-}

import System.Random (mkStdGen, setStdGen, randomIO)

-- https://hackage.haskell.org/package/extra-1.5.1/docs/src/Control.Monad.Extra.html#partitionM
partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM _ [] = return ([], [])
partitionM f (x:xs) =
    f x >>= \y ->
    partitionM f xs >>= \(a, b) ->
    return ([x | y] ++ a, [x | not y] ++ b)

shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle [x] = return [x]
shuffle xs =
    partitionM (\_ -> randomIO :: IO Bool) xs >>= \(before, after) ->
    shuffle before >>= \before' ->
    shuffle after >>= return . (before' ++)

main :: IO ()
main =
    (setStdGen . mkStdGen) 2 >>
    shuffle ([1 .. 25] :: [Int]) >>=
    print
