import Data.List
import Data.Maybe

example :: [String]
example = ["7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1"
          ,""
          ,"22 13 17 11  0"
          ," 8  2 23  4 24"
          ,"21  9 14 16  7"
          ," 6 10  3 18  5"
          ," 1 12 20 15 19"
          ,""
          ," 3 15  0  2 22"
          ," 9 18 13 17  5"
          ,"19  8  7 25 23"
          ,"20 11 10 24  4"
          ,"14 21 16 12  6"
          ,""
          ,"14 21 17 24  4"
          ,"10 16 15  9 19"
          ,"18  8 23 26 20"
          ,"22 11 13  6  5"
          ," 2  0 12  3  7"
          ]

parseNumbers :: [String] -> [Int]
parseNumbers s = map read $ words [if c == ',' then ' ' else c | c <- (head s)]

parseBoards :: [String] -> [[[Int]]]
parseBoards s =
    let bs [] = []
        bs (_:a:b:c:d:e:xs) = [a,b,c,d,e] : bs xs
    in  bs $ map (map read . words) $ tail s

-- -1 represent called numebers, flipping board to check columns
hasBingo :: [[Int]] -> Bool
hasBingo board = let
    board' = board ++ (transpose board)
    in (length $ filter (all (==(-1))) board') > 0

-- how to not use the Maybe type
findHasBingo :: [[[Int]]] -> [[Int]]
findHasBingo bs = fromMaybe [[]] $ find hasBingo bs

-- needed for multiple winners in part 2
filterHasBingo :: [[[Int]]] -> [[[Int]]]
filterHasBingo bs = let
    bingo = fromMaybe [[]] $ find hasBingo bs
    bs' = filter (/=bingo) bs
    in if bingo /= [[]]
        then filterHasBingo bs'
        else bs'

-- what a mess...-1 is crossed out number
callNumber :: Int -> [[[Int]]] -> [[[Int]]]
callNumber n bs = map (\b -> map (map (\x -> if x==n then (-1) else x)) b) bs

-- just reaching for it...
play :: [[[Int]]] -> [Int] -> Int
play bs (n:ns) = let
    bs' = callNumber n bs
    winner = findHasBingo bs'
    in if winner == [[]]
        then play bs' ns
        else (*) n $ sum $ filter (/=(-1)) $ winner >>= (\x -> x)

-- find last winner, everything should be refactored but time...
-- bingo boards needs different type to hold numbers and track called numbers?
-- maybe a type to represent the boards
play' :: [[[Int]]] -> [Int] -> Int
play' bs (n:ns) = let
    bs' = callNumber n bs
    winner = findHasBingo bs'
    bs'' = filterHasBingo bs'
    in if (length bs'' > 0)
        then play' bs'' ns
        else (*) n $ sum $ filter (/=(-1)) $ winner >>= (\x -> x)


result :: [String] -> Int
result s = play (parseBoards s) (parseNumbers s)

result' :: [String] -> Int
result' s = play' (parseBoards s) (parseNumbers s)

main :: IO ()
main = do
    putStrLn "What to do..."
    putStrLn $ show $ result example
    putStrLn $ show $ result' example
    {-
    file <- readFile "input.txt"
    putStrLn $ show $ result $ lines file
    putStrLn $ show $ result' $ lines file
    -}
