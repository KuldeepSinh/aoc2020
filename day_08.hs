data CurrentState = CurrentState {accumulator :: Int, linesExecuted :: [Int]} deriving (Show)

executeInstruction :: Int -> ([Char], Int) -> CurrentState -> [([Char], Int)] -> CurrentState
executeInstruction _ _ state [] = state
executeInstruction lineNumber (instruction, value) state ls
  | elem lineNumber (linesExecuted state) = state
  | instruction == "acc" = executeInstruction (lineNumber + 1) (ls !! (lineNumber + 1)) CurrentState {accumulator = (accumulator state + value), linesExecuted = lineNumber : (linesExecuted state)} ls
  | instruction == "nop" = executeInstruction (lineNumber + 1) (ls !! (lineNumber + 1)) CurrentState {accumulator = accumulator state, linesExecuted = lineNumber : (linesExecuted state)} ls
  | instruction == "jmp" = executeInstruction (lineNumber + value) (ls !! (lineNumber + value)) CurrentState {accumulator = accumulator state, linesExecuted = lineNumber : (linesExecuted state)} ls

processWords :: [String] -> [(String, Int)]
processWords [x, y]
  | head y == '+' = [(x, read $ tail y)]
  | otherwise = [(x, read y)]

main :: IO ()
main = do
  contents <- getContents
  putStr (execute contents)

execute :: [Char] -> [Char]
execute contents =
  let ls = concat . map (processWords . words) . lines $ contents
      state = executeInstruction 0 (ls !! 0) CurrentState {accumulator = 0, linesExecuted = []} ls
   in (++ "\n") $ show $ accumulator state
