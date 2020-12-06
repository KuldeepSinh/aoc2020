import Data.List (groupBy, isSuffixOf)
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

filterForPuzzle :: Traversable t => t (M.Map [Char] [Char] -> Bool) -> String -> [Bool]
filterForPuzzle validators = filter (== True) . map (and . sequenceA validators) . convertToKeyValuePairs . normalize

-- puzzle 1
main :: IO ()
main = interact $ (++ "\n") . show . length . filterForPuzzle puzzel1Validators

-- puzzle 2

between :: Ord a => a -> a -> a -> Bool
between a b v = a <= v && v <= b

validator :: (Eq t, Ord k) => (t -> Bool) -> k -> M.Map k t -> Bool
validator predicate key fromLst
  | val == Nothing = False
  | otherwise = predicate something
  where
    val = M.lookup key fromLst
    Just something = val

validateByr :: M.Map [Char] [Char] -> Bool
validateByr = validator (\something -> length something == 4 && between "1920" "2002" something) "byr"

validateIyr :: M.Map [Char] [Char] -> Bool
validateIyr = validator (\something -> length something == 4 && between "2010" "2020" something) "iyr"

validateEyr :: M.Map [Char] [Char] -> Bool
validateEyr = validator (\something -> length something == 4 && between "2020" "2030" something) "eyr"

validateHgt :: M.Map [Char] [Char] -> Bool
validateHgt = validator (\something -> (isSuffixOf "cm" something && between "150cm" "193cm" something) || (isSuffixOf "in" something && between "59in" "76in" something)) "hgt"

validateEcl :: M.Map [Char] [Char] -> Bool
validateEcl = validator (\something -> elem something ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]) "ecl"

validateHcl :: M.Map [Char] [Char] -> Bool
validateHcl = validator (\something -> length something == 7 && head something == '#' && (and $ elem <$> tail something <*> ["0123456789abcdef"])) "hcl"

validatePid :: M.Map [Char] [Char] -> Bool
validatePid = validator (\something -> length something == 9 && (and $ elem <$> something <*> ["0123456789"])) "pid"

puzzel2Validators :: [M.Map [Char] [Char] -> Bool]
puzzel2Validators = [validateByr, validateEcl, validateEyr, validateHcl, validateHgt, validateIyr, validatePid]

-- puzzle 2
-- main :: IO ()
-- main = interact $ (++ "\n") . show . length . filterForPuzzle puzzel2Validators
