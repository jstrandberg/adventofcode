import Data.Either
import Data.List

example :: [String]
example =
    ["[({(<(())[]>[[{[]{<()<>>"
    ,"[(()[<>])]({[<{<<[]>>("
    ,"{([(<{}[<>[]}>{[]{[(<()>"
    ,"(((({<>}<{<{<>}{[]{[]{}"
    ,"[[<[([]))<([[{}[[()]]]"
    ,"[{[{({}]{}}([{[{{{}}([]"
    ,"{<[[]]>}<{[{[{[]{()[[[]"
    ,"[<(<(<(<{}))><([]([]()"
    ,"<{([([[(<>()){}]>(<<{{"
    ,"<{([{{}}[<[[[<>{}]]]>[]]"
    ]

isOpen :: Char -> Bool
isOpen '(' = True
isOpen '[' = True
isOpen '{' = True
isOpen '<' = True
isOpen _   = False

close :: Char -> Char
close '(' = ')'
close '[' = ']'
close '{' = '}'
close '<' = '>'
close _   = error "Illegal opening character"

score :: Either Char [Char] -> Int
score (Left ')') = 3
score (Left ']') = 57
score (Left '}') = 1197
score (Left '>') = 25137
score _          = 0

score' :: Char -> Int
score' ')' = 1
score' ']' = 2
score' '}' = 3
score' '>' = 4
score' _   = error "Scoring that is cheating"

-- closing previous opening bracket or open new
-- otherwise store left faulty, lefts end process
step :: Either Char [Char] -> Char -> Either Char [Char]
step (Left x) _   = Left x
step (Right []) c = Right [close c]
step (Right (x:xs)) c
    | c == x    = Right xs
    | isOpen c  = Right (close c : x : xs)
    | otherwise = Left c

-- parse to find right remainder or left faulty bracket
parse :: String -> Either Char [Char]
parse s = foldl step (Right []) s

-- parse remainder to a score
parse' :: String -> Int
parse' s = foldl (\a c -> a * 5 + (score' c)) 0 s

result :: [String] -> Int
result s = sum . map (score . parse) $ s

result' :: [String] -> Int
result' s = middle . sort . map parse' . rights . map parse $ s
    where middle s = s !! (div (length s) 2)

main :: IO ()
main = do
  putStrLn "What to do..."
  putStrLn . show . result $ example
  putStrLn . show . result' $ example
  {-
  file <- readFile "input.txt"
  putStrLn . show . result . lines $ file
  putStrLn . show . result' . lines $ file
  -}
