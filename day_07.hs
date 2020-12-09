import Data.List (nub)

filterBags :: [String] -> [String]
filterBags = filter (\x -> not $ elem x ["bag", "bag,", "bag.", "bags", "bags,", "bags.","contain"])

tuplify :: [[String]] -> [(String, [(Int, String)])]
tuplify [] =  []
tuplify (x:xs) = (unwords [y0,y1], zs):tuplify xs
    where
        (y0:y1:ys) = x
        zs =  tuplifyContainedBags ys

tuplifyContainedBags :: Read a => [String] -> [(a, String)]
tuplifyContainedBags [] = []
tuplifyContainedBags ls =  tuplifyContainedBags' 0 1 2 ls
    where
        tuplifyContainedBags' _ _ _ [] = []
        tuplifyContainedBags' n a c ls  
            | c > length ls -1 = []
            | otherwise =  (read (ls!!n), unwords [(ls!!a), (ls!!c)]) : tuplifyContainedBags' (n+3) (a+3) (c+3) ls

filterGivenBags :: String -> [(String, [(Int, String)])] -> [(String, [(Int, String)])]
filterGivenBags bag = filter (\(_, ls) -> (any (\x -> snd x == bag) ls ))

containingBags :: [String] -> [(String, [(Int, String)])] -> [String]
containingBags [] _ = []
containingBags _ [] = []
containingBags (x:xs) ls = nub $ filteredBags ++ containingBags xs ls ++ containingBags filteredBags ls
    where 
        filteredBags = map fst $ filterGivenBags x ls

main :: IO ()
main = interact $ (++ "\n") . show  . length. containingBags ["shiny gold"] . tuplify . map (filterBags . words) . lines