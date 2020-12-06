import Data.List (groupBy, isPrefixOf, sort)

normalize :: String -> [[String]]
normalize = map (sort . words . unwords) . filter (/= [""]) . groupBy (\x y -> and [x /= "", y /= ""]) . lines

dropColon :: (a1, [a2]) -> (a1, [a2])
dropColon (x, (_ : ys)) = (x, ys)

convertToKeyValuePairs :: [[String]] -> [[(String, String)]]
convertToKeyValuePairs xs = [map (dropColon . break (== ':')) x | x <- xs]

-- puzzel 1
countValidPassports :: String -> Int
countValidPassports = length . filter (\x -> (length x == 8 || (length x == 7 && fst (x !! 0) /= "cid") && (length x == 7 && fst (x !! 1) /= "cid"))) . convertToKeyValuePairs . normalize

main :: IO ()
main = interact $ (++ "\n") . show . countValidPassports