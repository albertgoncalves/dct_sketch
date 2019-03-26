{-# OPTIONS_GHC -Wall #-}

cos' :: Float -> Float -> Float -> Float
cos' n i j = cos (num / den)
  where
    num = ((2 * j) + 1) * i * pi
    den = 2 * n

t :: Float -> Float -> Float -> Float
t n i j
    | i == 0 = 1 / sqrt n
    | otherwise = sqrt (2 / n) * cos' n i j

dct :: Int -> [[Float]]
dct n = loop [] [] n' n' n'
  where
    n' = fromIntegral n
    loop :: [Float] -> [[Float]] -> Float -> Float -> Float -> [[Float]]
    loop row rows n'' i j
        | i <= 0 = rows
        | j <= 0 = loop [] (row : rows) n'' (i - 1) n''
        | otherwise = loop row' rows n'' i (j - 1)
      where
        row' = t n'' (i - 1) (j - 1) : row

matPrint :: Show a => [[a]] -> IO ()
matPrint = mapM_ (putStrLn . unwords . map show)

main :: IO ()
main = matPrint $ dct 8
