data Cmd = Up Int | Down Int | Forward Int
    deriving Show

example :: [Cmd]
example = [Forward 5
          ,Down 5
          ,Forward 8
          ,Up 3
          ,Down 8
          ,Forward 2
          ]

calcCmd :: Cmd -> [Int]
calcCmd (Forward n) = [n,0]
calcCmd (Down n)    = [0,n]
calcCmd (Up n)      = [0,-n]

calcCmds :: [Cmd] -> [Int]
calcCmds cs = foldl (zipWith (+)) [0,0] $ map calcCmd cs

result :: [Cmd] -> Int
result cs = product $ calcCmds cs

parseCmd :: [String] -> Cmd
parseCmd [cmd,n] =
    case cmd of
        "forward" -> Forward $ read n
        "down"    -> Down    $ read n
        "up"      -> Up      $ read n
        otherwise -> error "Error parsing command!"
parseCmd _ = error "Error parsing command!"

parse :: String -> [Cmd]
parse s = map (parseCmd . words) $ lines s

main :: IO ()
main = do
    putStrLn "What to do..."
    putStrLn $ show $ result example
    {-
    file <- readFile "input.txt"
    putStrLn $ show $ result $ parse file
    -}
