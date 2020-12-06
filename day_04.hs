import Data.List (groupBy, isPrefixOf, sort)

normalize :: String -> [[String]]
normalize = map (sort . words . unwords) . filter (/= [""]) . groupBy (\x y -> and [x /= "", y /= ""]) . lines

dropColon :: (a1, [a2]) -> (a1, [a2])
dropColon (x, (_ : ys)) = (x, ys)

convertToKeyValuePairs :: [[String]] -> [[(String, String)]]
convertToKeyValuePairs xs = [map (dropColon . break (== ':')) x | x <- xs]

puz1Predicate :: [([Char], b)] -> Bool
puz1Predicate = (\x -> (length x == 8 || (length x == 7 && fst (x !! 0) /= "cid") && (length x == 7 && fst (x !! 1) /= "cid")))

-- puzzel 1
countValidPassports :: String -> Int
countValidPassports = length . filter puz1Predicate . convertToKeyValuePairs . normalize

main :: IO ()
main = interact $ (++ "\n") . show . countValidPassports