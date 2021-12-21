example :: [String]
example =
  ["1163751742"
  ,"1381373672"
  ,"2136511328"
  ,"3694931569"
  ,"7463417111"
  ,"1319128137"
  ,"1359912421"
  ,"3125421639"
  ,"1293138521"
  ,"2311944581"
  ]

type Location = (Int,Int)

parse :: [String] -> [(Location,Int)]
parse s
  = zip [(x,y) | y <- [0..maxY s], x <- [0..maxX s]]
  . map (read . pure)
  $ concat s
  where
    maxX s = (length . head $ s) - 1
    maxY s = (length s) - 1

width :: [(Location,Int)] -> Int
width = maximum . map fst . map fst

height :: [(Location,Int)] -> Int
height = maximum . map snd . map fst

weight :: Location -> [(Location,Int)] -> Maybe Int
weight l xs = lookup l xs

heuristic :: Location -> Location -> Int
heuristic (x1,y1) (x2,y2) = (x1 - x2)^2 + (y1 - y2)^2

cost :: Int -> Location -> [(Location,Int)] -> Maybe Int
cost s l xs = fmap (+) weight' <*> pure heuristic'
  where
    weight' = fmap ((+) s) $ weight l xs
    heuristic' = heuristic l (width xs,height xs)
