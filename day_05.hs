import Data.Char (digitToInt)
import Data.List (sort)

convertToBin :: [Char] -> [Char]
convertToBin [] = []
convertToBin (x : xs)
  | x == 'F' || x == 'L' = '0' : convertToBin xs
  | x == 'B' || x == 'R' = '1' : convertToBin xs

toDecimal :: [Char] -> Int
toDecimal [] = 0
toDecimal ls@(x : xs) = ((digitToInt x) * (2 ^ (length ls - 1))) + toDecimal xs

seatID :: [Char] -> Int
seatID xs = (toDecimal . convertToBin $ take 7 xs) * 8 + (toDecimal . convertToBin $ drop 7 xs)

-- puzzle 1
-- main :: IO ()
-- main = interact $ (++ "\n") . show . maximum . map seatID . lines

-- puzzle 2
main :: IO ()
main = interact $ (++ "\n") . show . head . mySeat . sort . map seatID . lines

mySeat :: (Eq a, Num a) => [a] -> [a]
mySeat [] = []
mySeat [x] = [x]
mySeat (x : y : xs)
  | x + 2 == y = [x + 1]
  | otherwise = mySeat (y : xs)