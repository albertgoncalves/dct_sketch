{-# OPTIONS_GHC -Wall #-}

import Data.List (concat)
import Text.Read (readMaybe)
import Text.Regex (matchRegex, mkRegex, Regex)

scaleTo16bit :: Int -> (Int, Int, Int)
scaleTo16bit x = (x', x', x')
  where
    x' = x * 257

groups :: Regex
groups = mkRegex p
  where
    p =
        concat
            [ "([0-9]+),([0-9]+): +"
            , "\\([0-9]+,[0-9]+,[0-9]+\\) +"
            , "#[a-zA-Z0-9]+ "
            , "+gray\\(([0-9]+)\\)"
            ]

extractRegex :: String -> Maybe [Int]
extractRegex = (mapM readMaybe =<<) . matchRegex groups

main :: IO ()
main = do
    mapM_ (print . scaleTo16bit) [0, 1, 120, 2255]
    print $ extractRegex "0,1: (65535,65535,65535)  #FFFFFF  gray(255)"
