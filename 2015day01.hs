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

what_floor :: String -> Int
what_floor s =
    let
        count_up s = length $ filter (=='(') s
        count_down s = length $ filter (==')') s
    in
        (count_up s) - (count_down s)

main :: IO ()
main = do
    putStrLn "What to do..."
    {-
    file <- readFile "input.txt"
    putStrLn $ show $ what_floor file
    -}
