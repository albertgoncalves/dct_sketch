{-# OPTIONS_GHC -Wall #-}

import Control.Monad ((<=<))
import Text.Read (readMaybe)
import Text.Regex (matchRegex, mkRegex, Regex)

scaleTo16bit :: Int -> (Int, Int, Int)
scaleTo16bit x = (x', x', x')
  where
    x' = x * 257

demoScale :: [Int] -> IO ()
demoScale = mapM_ (print . scaleTo16bit)

groups :: Regex
groups =
    mkRegex
        "([0-9]+),([0-9]+): +\\([0-9]+,[0-9]+,[0-9]+\\) +#[a-zA-Z0-9]+ \
        \+gray\\(([0-9]+)\\)"

coerce :: [Int] -> Maybe (Int, Int, Int)
coerce [a, b, c] = Just (a, b, c)
coerce _ = Nothing

extractRegex :: String -> Maybe (Int, Int, Int)
extractRegex = coerce <=< mapM readMaybe <=< matchRegex groups

invert :: Int -> Int
invert = (255 -)

demoRegex :: String -> IO ()
demoRegex = print . fmap (\(a, b, c) -> (a, b, invert c)) . extractRegex

main :: IO ()
main =
    sequence_
        [ demoScale [0, 1, 120, 2255]
        , demoRegex "0,1: (65535,65535,65535)  #FFFFFF  gray(119)"
        ]
