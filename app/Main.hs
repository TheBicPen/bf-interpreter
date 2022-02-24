module Main where


main :: IO ()
main = do
  bf <- getLine
  putStrLn (interpretationResult (interpret (initialState bf)))

data InterpreterState = InterpreterState String Int Int [Int] String

initialState :: String -> InterpreterState
initialState s = InterpreterState s 0 0 [0] ""

interpretationResult :: InterpreterState -> String
interpretationResult (InterpreterState _ _ _ _ s) = s

-- Takes string, 2 chars, char count, num brackets, returns char count
matchChar :: String -> Char -> Char -> Int -> Int -> Int
matchChar _ _ _ x 0 = x
matchChar [] _ _ x i = x
matchChar (c : cs) plus1 minus1 x i
  | c == plus1 = matchChar cs plus1 minus1 (x + 1) (i + 1)
  | c == minus1 = matchChar cs plus1 minus1 (x + 1) (i -1)
  | otherwise = matchChar cs plus1 minus1 (x + 1) i

-- Takes string, returns index of matching left bracket (looks leftward)
matchLBracket :: String -> Int
matchLBracket s = length s - matchChar (reverse s) ']' '[' 0 1

-- Takes string, returns index of matching right bracket (looks rightward)
matchRBracket :: String -> Int
matchRBracket s = matchChar s '[' ']' 0 1

addValue :: [Int] -> Int -> Int -> [Int]
addValue xs i val = take i xs ++ [xs !! i + val] ++ drop (i + 1) xs

-- interpret BF string.
interpret :: InterpreterState -> InterpreterState
interpret (InterpreterState str strPtr ptr cells output) =
  if strPtr >= length str
    then InterpreterState str strPtr ptr cells output
    else interpret (interpretChar (InterpreterState str strPtr ptr cells output))

interpretChar :: InterpreterState -> InterpreterState
interpretChar (InterpreterState str strPtr ptr cells output) =
  let c = str !! strPtr
   in case c of
        '>' -> InterpreterState str (strPtr + 1) (ptr + 1) (if ptr + 1 < length cells then cells else cells ++ [0]) output
        '<' -> InterpreterState str (strPtr + 1) (if ptr > 0 then ptr - 1 else ptr) cells output
        '+' -> InterpreterState str (strPtr + 1) ptr (addValue cells ptr 1) output
        '-' -> InterpreterState str (strPtr + 1) ptr (addValue cells ptr (-1)) output
        '.' -> InterpreterState str (strPtr + 1) ptr cells (output ++ " " ++show (cells !! ptr))
        ',' -> InterpreterState str (strPtr + 1) ptr cells output -- reading from stdin not implemented
        '[' -> InterpreterState str (if cells !! ptr == 0 then strPtr + matchRBracket (drop (strPtr + 1) str) else strPtr + 1) ptr cells output
        ']' -> InterpreterState str (if cells !! ptr == 0 then strPtr + 1 else matchLBracket (take strPtr str)) ptr cells output
        c -> InterpreterState str (strPtr + 1) ptr cells output -- ignore invalid chars