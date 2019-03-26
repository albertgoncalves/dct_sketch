{-# OPTIONS_GHC -Wall #-}

import Control.Monad (join)
import Data.List (intercalate)
import Text.Read (readMaybe)
import Text.Regex (matchRegex, mkRegex, Regex)

scaleTo16bit :: Int -> (Int, Int, Int)
scaleTo16bit x = (x', x', x')
  where
    x' = x * 257

extract :: Maybe [String] -> Maybe [Int]
-- extract = sequence . map join . sequence . fmap (map readMaybe)
extract = mapM join . traverse (map readMaybe)

pattern :: Regex
pattern = mkRegex p
  where
    p =
        intercalate
            ""
            [ "([0-9]+),([0-9]+): +"
            , "\\([0-9]+,[0-9]+,[0-9]+\\) +"
            , "#[a-zA-Z0-9]+ "
            , "+gray\\(([0-9]+)\\)"
            ]

extractRegex :: String -> Maybe [Int]
extractRegex = extract . matchRegex pattern

main :: IO ()
main = do
    mapM_ (print . scaleTo16Bit) [0, 1, 120, 255]
    print $ extractRegex "0,1: (65535,65535,65535)  #FFFFFF  gray(255)"
