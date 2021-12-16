import Data.List

example :: [String]
example =
  ["6,10"
  ,"0,14"
  ,"9,10"
  ,"0,3"
  ,"10,4"
  ,"4,11"
  ,"6,0"
  ,"6,12"
  ,"4,1"
  ,"0,13"
  ,"10,12"
  ,"3,4"
  ,"3,0"
  ,"8,4"
  ,"1,10"
  ,"2,14"
  ,"8,10"
  ,"9,0"
  ,""
  ,"fold along y=7"
  ,"fold along x=5"
  ]

type Dot = (Int,Int)
data Fold = X Int | Y Int
  deriving Show

parseDots :: [String] -> [Dot]
parseDots ss = map dot ss'
  where
    ss' = takeWhile (/="") ss
    dot s =
      let [x,y] = words [if c == ',' then ' ' else c | c <- s]
      in  (read x, read y)

parseFolds :: [String] -> [Fold]
parseFolds ss = map fold ss'
  where
    ss' = tail . dropWhile (/="") $ ss
    fold (_:_:_:_:_:_:_:_:_:_:_:a:_:n)
      | a == 'x' = X (read n)
      | a == 'y' = Y (read n)
      | otherwise = error "could not parse"

fold :: Fold -> Dot -> Dot
fold (Y n) (x,y) = if y > n
  then (x,abs $ y - n * 2)
  else (x,y)
fold (X n) (x,y) = if x > n
  then (abs $ x - n * 2,y)
  else (x,y)

compress :: [Dot] -> [Dot]
compress = foldr compress' [] . sort
  where
    compress' a [] = [a]
    compress' a (x:xs) = if x == a
      then (x : xs)
      else (a : x : xs)

result :: [String] -> Int
result x = length . compress . map (fold . head $ parseFolds x) $ parseDots x

-- takes width of grid and a dot
indexFrom :: Int -> Dot -> Int
indexFrom w (x,y) = y * w + x

splitLength :: Int -> String -> [String]
splitLength n s = split' s
  where
    split' [] = []
    split' s = take n s : (split' $ drop n s)

result' :: [String] -> String
result' x = unlines . splitLength width $ chars
  where
    dots = parseDots x
    folds = parseFolds x
    final = foldl (\ds f -> compress $ map (fold f) ds) dots folds
    width = (+) 1 . last . sort . map fst $ final
    height = (+) 1 . last . sort . map snd $ final
    indexes = map (indexFrom width) final
    chars = map (\x -> if elem x indexes then '#' else '.') [0..(width*height-1)]

main :: IO ()
main = do
  putStrLn "What to do..."
  putStrLn . show . result $ example
  putStrLn . result' $ example
  {-
  file <- readFile "input.txt"
  putStrLn . show . result . lines $ file
  putStrLn . result' . lines $ file
  -}
