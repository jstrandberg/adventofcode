import Data.List

type Location = (Int,Int)

example :: [(Location,Int)]
example = zip locations
  [5,4,8,3,1,4,3,2,2,3
  ,2,7,4,5,8,5,4,7,1,1
  ,5,2,6,4,5,5,6,1,7,3
  ,6,1,4,1,3,3,6,1,4,6
  ,6,3,5,7,3,8,5,4,7,8
  ,4,1,6,7,5,2,4,6,4,5
  ,2,1,7,6,8,4,1,7,2,1
  ,6,8,8,2,8,8,1,1,3,4
  ,4,8,4,6,8,4,8,5,5,4
  ,5,2,8,3,7,5,1,5,2,6
  ]

locations :: [Location]
locations = [(x,y) | y <- [0..9], x <- [0..9]]

adjacent :: Location -> [Location]
adjacent (x,y) = filter (flip elem locations)
  [(x-1,y-1)
  ,(x-1,y)
  ,(x-1,y+1)
  ,(x+1,y-1)
  ,(x+1,y)
  ,(x+1,y+1)
  ,(x,y-1)
  ,(x,y+1)
  ]

flash :: [(Location,Int)] -> (Int,[(Location,Int)])
flash s = (n,calc s')
  where
    (n,s') = foldr flash' (0,[]) s
    flash' (i,x) (n,a) = if x > 9
      then (n + 1,) $ map (,1) (adjacent i) ++ (i,0) : a
      else (n,) $ (i,x) : a

calc :: [(Location,Int)] -> [(Location,Int)]
calc = foldr calc' [] . sort
  where
    add a b
      | a == 0 = 0
      | b == 0 = 0
      | otherwise = a + b
    calc' x [] = [x]
    calc' x (y:ys) =
      if fst x == fst y
        then (fst x, add (snd x) (snd y)) : ys
        else x : y : ys

step :: [(Location,Int)] -> (Int,[(Location,Int)])
step s = step' (0, increase s)
  where
    increase = map (\x -> (fst x,snd x + 1))
    step' (n,s') =
      let (n',s'') = flash s'
      in if n' == 0
        then (n+n',s'')
        else step' (n+n',s'')

result :: [(Location,Int)] -> Int
result s = fst (iterate result' (0,s) !! 100)
  where
    result' (n,s') =
      let (n',s'') = step s'
      in (n+n',s'')

parse :: String -> [(Location,Int)]
parse s = zip locations s'
  where
    s' = map (\c -> read [c]) . filter (/= '\n') $ s

main :: IO ()
main = do
  putStrLn "What to do..."
  putStrLn . show . result $ example
  {-
  file <- readFile "input.txt"
  putStrLn . show . result . parse $ file
  -}
