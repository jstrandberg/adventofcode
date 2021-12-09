import Data.List

example :: [String]
example = ["0,9 -> 5,9"
          ,"8,0 -> 0,8"
          ,"9,4 -> 3,4"
          ,"2,2 -> 2,1"
          ,"7,0 -> 7,4"
          ,"6,4 -> 2,0"
          ,"0,9 -> 2,9"
          ,"3,4 -> 1,4"
          ,"0,0 -> 8,8"
          ,"5,5 -> 8,2"
          ]

data Line = Line { x1 :: Int
                 , y1 :: Int
                 , x2 :: Int
                 , y2 :: Int
                 }
    deriving Show

data Dot = Dot { x :: Int, y :: Int }
    deriving (Eq,Ord,Show)

parseLine :: String -> Line
parseLine s = Line (read a) (read b) (read c) (read d)
    where [a,b,c,d] = words [if c == ',' || c == '>' then ' ' else c | c <- s, c /= ' ' && c /= '-']

parseLines :: [String] -> [Line]
parseLines s = map parseLine s

isHorizontalOrVertical :: Line -> Bool
isHorizontalOrVertical (Line x1 y1 x2 y2) = x1 == x2 || y1 == y2

horizontalAndVerticalLines :: [Line] -> [Line]
horizontalAndVerticalLines l = filter isHorizontalOrVertical l

lineToDots :: Line -> [Dot]
lineToDots (Line x1 y1 x2 y2)
    | x1 == x2 && y2 < y1 = map (Dot x1) [y2 .. y1]
    | x1 == x2 = map (Dot x1) [y1 .. y2]
    | y1 == y2 && x2 < x1 = map (\x -> Dot x y1) [x2 .. x1]
    | y1 == y2 = map (\x -> Dot x y1) [x1 .. x2]
    | x2 < x1 && y2 < y1 = [Dot (x2+x) (y2+x) | x <- [0 .. x1-x2]]
    | x2 > x1 && y2 < y1 = [Dot (x1+x) (y1-x) | x <- [0 .. x2-x1]]
    | x2 < x1 && y2 > y1 = [Dot (x1-x) (y1+x) | x <- [0 .. x1-x2]]
    | x2 > x1 && y2 > y1 = [Dot (x1+x) (y1+x) | x <- [0 .. x2-x1]]

linesToDots :: [Line] -> [Dot]
linesToDots l = foldMap lineToDots l

compressDots :: [Dot] -> [(Dot,Int)]
compressDots ds = foldl addDot [] $ sort ds
    where addDot [] d = [(d,1)]
          addDot ((a,n):xs) d =
              if a == d
                  then (a,n + 1) : xs
                  else (d,1) : (a,n) : xs

result :: [String] -> Int
result s = foldl (\a d -> if snd d > 1 then a + 1 else a) 0 $ compressDots dots
    where dots = linesToDots $ horizontalAndVerticalLines $ parseLines s

result' :: [String] -> Int
result' s = foldl (\a d -> if snd d > 1 then a + 1 else a) 0 $ compressDots dots
    where dots = linesToDots $ parseLines s

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
