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

getBag :: String -> [(String, [(Int, String)])] -> [(String, [(Int, String)])]
getBag _ [] = []
getBag [] _ = []
getBag  bag ls = filter (\x -> fst x == bag) ls         




getAllBags :: String -> [(String, [(Int, String)])] -> [(String, [(Int, String)])]
getAllBags _ []  = []
getAllBags [] _ = []
getAllBags  bag ls =  xs ++ cs 
    where 
        xs = getBag  bag ls
        ys = map snd $ snd (head xs) 
        bs [] = []
        bs (z:zs) = getAllBags z ls ++ bs zs
        cs = bs ys


-- puzzle 1
-- main :: IO ()
-- main = interact $ (++ "\n") . show  . length. containingBags ["shiny gold"] . tuplify . map (filterBags . words) . lines

-- puzzle 2
-- <ToDo> have not resolved yet
main :: IO ()
main = interact $ (++ "\n") . show . getAllBags "shiny gold" .tuplify . map (filterBags . words) . lines