import Data.List (tails)

-- Puzzle - 01
pairs :: [b] -> [(b, b)]
pairs ls = [(x, y) | (x : ys) <- tails ls, y <- ys]

solvePairs :: String -> String
solvePairs = show . (\(a, b) -> a * b) . head . filter (\(a, b) -> a + b == 2020) . pairs . map read . words

-- Puzzle - 02
triples :: [c] -> [(c, c, c)]
triples ls = [(x, y, z) | (x : ys) <- tails ls, (y : zs) <- tails ys, z <- zs]

solveTriples :: String -> String
solveTriples = show . (\(a, b, c) -> a * b * c) . head . filter (\(a, b, c) -> a + b + c == 2020) . triples . map read . words

-- main
-- compile as follows "ghc --make day_01.hs"
-- run as follows "cat day_01.input | ./day_01"
main :: IO ()
-- puzzle 1
main = interact $ solvePairs
-- puzzle 2
-- main = interact $ solveTriples
