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
        bs (_:a:b:c:d:e:xs) = [a,b,c,d,e] : (transpose [a,b,c,d,e]) : bs xs
    in  bs $ map (map read . words) $ tail s

hasBingo :: [[Int]] -> Bool
hasBingo board = (length $ filter (==[]) board) > 0

findHasBingo :: [[[Int]]] -> Maybe [[Int]]
findHasBingo bs = find hasBingo bs

callNumber :: Int -> [[[Int]]] -> [[[Int]]]
callNumber n bs = map (\b -> map (filter (/=n)) b) bs

-- just reaching for it...
play :: [[[Int]]] -> [Int] -> Int
play bs (n:ns) = let
    bs' = callNumber n bs
    winner = findHasBingo bs'
    in if winner == Nothing
        then play bs' ns
        else (*) n $ sum $ (fromMaybe [[]] winner) >>= (\x -> x)

result :: [String] -> Int
result s = play (parseBoards s) (parseNumbers s)

main :: IO ()
main = do
    putStrLn "What to do..."
    putStrLn $ show $ result example
    {-
    file <- readFile "input.txt"
    putStrLn $ show $ result $ lines file
    -}
