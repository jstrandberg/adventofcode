example :: [String]
example = ["2x3x4"
          ,"1x1x10"
          ]

parse :: String -> [Int]
parse s = map read $ words [if c == 'x' then ' ' else c | c <- s]

calc :: String -> Int
calc s = let
  [l,w,h] = parse s
  ss = [l*w,w*h,h*l]
  x  = foldl min maxBound ss
  in (+) x $ sum $ map ((*) 2) ss

main :: IO ()
main = do
    putStrLn "What to do..."
    putStrLn $ show $ sum $ map calc example
    {-
    file <- readFile "input.txt"
    putStrLn $ show $ sum $ map calc $ lines file
    -}
