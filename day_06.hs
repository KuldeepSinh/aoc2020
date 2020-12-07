import Data.List (groupBy, intersect, union)
import qualified Data.Set as Set

-- puzzle 1
main :: IO ()
main = interact $ (++ "\n") . show . sum . map (length . foldr1 union) . groupBy (\x y -> and [x /= "", y /= ""]) . lines

-- puzzle 2
-- main :: IO ()
-- main = interact $ (++ "\n") . show . sum . map (length . foldr1 intersect) . groupBy (\x y -> and [x /= "", y /= ""]) . lines
