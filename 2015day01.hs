examples :: [(String,Int)]
examples =
    [("(())",0)
    ,("()()",0)
    ,("(((",3)
    ,("(()(()(",3)
    ,("))(((((",3)
    ,("())",-1)
    ,("))(",-1)
    ,(")))",-3)
    ,(")())())",-3)
    ]

parse :: Int -> Char -> Int
parse n '(' = n + 1
parse n ')' = n - 1
parse n _   = n

calc :: [Char] -> Int
calc = foldl parse 0

calc' :: [Char] -> Int
calc' = fst . head . filter ((>) 0 . snd) . zip [0..] . scanl parse 0

main :: IO ()
main = do
    putStrLn "What to do..."
    {-
    file <- readFile "input.txt"
    putStrLn $ show $ calc file
    putStrLn $ show $ calc' file
    -}
