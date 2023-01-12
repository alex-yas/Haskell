lastN n xs = drop (length xs - n) xs
listMod n xs = (lastN (length xs - n) xs) ++ (take n xs)

append :: Int -> [Int] -> [Int]
append a [] = [a]
append a l = l ++ [a]

getPrimary :: Int -> [Int]
getPrimaryHelper:: Int -> Int -> [Int] -> [Int]

getPrimaryHelper num lastPrimary primaries = if num == 1 then primaries else (if (( mod num lastPrimary) == 0) then getPrimaryHelper (div num lastPrimary) lastPrimary (append lastPrimary primaries) else getPrimaryHelper num (lastPrimary + 1) primaries)
getPrimary num = getPrimaryHelper num 2 []

getFirst :: Int -> [Int] -> Int
getFirstHelper:: Int -> [Int] -> Int -> Int
getFirstHelper num (x:xs) curIndex = if num == x then curIndex else getFirstHelper num xs (curIndex + 1)
getFirst num l = getFirstHelper num l 0
