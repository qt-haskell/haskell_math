--Lia Marie Thaila

import Data.List

mean :: [Int] -> [Char]

mean [] =    
    let zero = 0
        in "Total Sum: " ++ show zero ++ ", Average: " ++ show zero
mean [x] = 
    let total = 1
        avg = x
        in "Total Sum: " ++ show total ++ ", Average: " ++ show x
mean (x:xs) = 
    let len = length xs + 1
        total = x + sum xs
        avg =(fromIntegral $ total)/(fromIntegral $ len)
        in "Total Sum: " ++ show total ++ ", Average: " ++ show avg
