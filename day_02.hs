occurances :: Eq a => a -> [a] -> Int
occurances c xs = length [ x | x <- xs, x == c]

removeDash :: [Char] -> [Char]
removeDash (x:xs)
    | x == '-' = ' ' : xs
    | otherwise = x : removeDash xs

readPositions :: [Char] -> [Int]
readPositions = map read .words. removeDash

--puzzle 3
validateOccurances :: Num p => [[Char]] -> p
validateOccurances [range, cs, password]
    | min <= occ && occ <= max = 1
    | otherwise = 0
    where
        occ = occurances (head cs) password
        [min, max] = readPositions range

--puzzle 4
validatePositions :: Num p => [[Char]] -> p
validatePositions [range, cs, str]
    | str !! (min -1) == c && str !! (max - 1) == c = 0
    | str !! (min -1) == c || str !! (max - 1) == c = 1
    | otherwise = 0
    where
        c = head cs 
        [min, max] = readPositions range

-- compile as follows "ghc --make day_02.hs"
-- run as follows "cat day_02.input | ./day_02"
main :: IO ()
--puzzle 3
main = interact $ (++ "\n") . show . sum . map (validateOccurances . words) . lines        
--puzzle 4
--main = interact $ (++ "\n") . show . sum . map (validatePositions . words) . lines        