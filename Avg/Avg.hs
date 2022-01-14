{-
avg solver
-}
clean raw = map (\s -> read s :: Double) (lines raw)

avg :: [Double] -> Double
avg (x:xs) = (a * x) + ((1 - a) * avg xs)
  where a = 0.95
avg [] = 0.0

mean xs = sum xs / fromIntegral (length xs)

splitError :: [Double] -> Double
splitError xs = mean xs - avg xs
{-
main loop
-}
main = do
  streamData <- readFile "input.txt"
  let input = clean streamData
  putStrLn $ "the mean is: " ++ (show. mean) input
  putStrLn $ "the moving average is: " ++ (show. avg) input
  putStrLn $ "the difference between the two is: " ++ (show . splitError) input