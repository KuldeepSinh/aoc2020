import Data.Char (digitToInt)
import Data.List (sort)

stringToBin :: [Char] -> [Char]
stringToBin [] = []
stringToBin (x : xs)
  | x == 'F' || x == 'L' = '0' : stringToBin xs
  | x == 'B' || x == 'R' = '1' : stringToBin xs

binToDecimal :: [Char] -> Int
binToDecimal [] = 0
binToDecimal ls@(x : xs) = ((digitToInt x) * (2 ^ (length ls - 1))) + binToDecimal xs

seatID :: [Char] -> Int
seatID xs = (binToDecimal . stringToBin $ take 7 xs) * 8 + (binToDecimal . stringToBin $ drop 7 xs)

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