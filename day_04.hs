import Data.List (groupBy)
import qualified Data.Map as M

normalize :: String -> [[String]]
normalize = map (words . unwords) . filter (/= [""]) . groupBy (\x y -> and [x /= "", y /= ""]) . lines

dropColon :: (a1, [a2]) -> (a1, [a2])
dropColon (x, (_ : ys)) = (x, ys)

convertToKeyValuePairs :: [[[Char]]] -> [M.Map [Char] [Char]]
convertToKeyValuePairs xs = map M.fromList $ [map (dropColon . break (== ':')) x | x <- xs]

requiredKeys :: [String]
requiredKeys = ["byr", "ecl", "eyr", "hcl", "hgt", "iyr", "pid"]

puzzel1Validators :: [M.Map String a -> Bool]
puzzel1Validators = map M.member requiredKeys

filterForPuzz1 :: Traversable t => t (M.Map [Char] [Char] -> Bool) -> String -> [Bool]
filterForPuzz1 validators = filter (== True) . map (and . sequenceA validators) . convertToKeyValuePairs . normalize

-- puzzel 1
main :: IO ()
main = interact $ (++ "\n") . show . length . filterForPuzz1 puzzel1Validators