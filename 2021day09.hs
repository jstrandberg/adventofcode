import Data.Maybe

example :: [[Int]]
example = [[2,1,9,9,9,4,3,2,1,0]
          ,[3,9,8,7,8,9,4,9,2,1]
          ,[9,8,5,6,7,8,9,8,9,2]
          ,[8,7,6,7,8,9,6,7,8,9]
          ,[9,8,9,9,9,6,5,6,7,8]
          ]

type Position = (Int,Int)

maxX :: [[Int]] -> Int
maxX x = (length . head $ x) - 1

maxY :: [[Int]] -> Int
maxY x = (length x) - 1

allPositions :: [[Int]] -> [Position]
allPositions m = [(x,y) | x <- [0 .. maxX m], y <- [0 .. maxY m]]

height :: [[Int]] -> Position -> Maybe Int
height m (x,y)
    | x < 0      = Nothing
    | x > maxX m = Nothing
    | y < 0      = Nothing
    | y > maxY m = Nothing
    | otherwise  = Just $ m' !! pos
    where m'  = m >>= id
          pos = y * (maxX m + 1) + x

adjacent :: Position -> [Position]
adjacent (x,y) =
    [(x, y - 1)
    ,(x, y + 1)
    ,(x - 1, y)
    ,(x + 1, y)
    ]

isLow :: [[Int]] -> Position -> Bool
isLow m p = all ((<) height') adjacent'
    where height'   = height m p
          adjacent' = filter (/= Nothing) . map (height m) . adjacent $ p

result :: [[Int]] -> Int
result x = sum . map ((+) 1 . snd) . filter (fst) . map isLow' . allPositions $ x
    where height'  = fromMaybe 0 . height x -- better way to manage the maybe?
          isLow' p = (isLow x p, height' p)

parse :: String -> [[Int]]
parse = undefined

main :: IO ()
main = do
  putStrLn "What to do..."
  putStrLn . show . result $ example
  {-
  file <- readFile "input.txt"
  putStrLn . show . result . parse $ file
  -}
