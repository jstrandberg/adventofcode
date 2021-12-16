import Data.List

type Location = Int

example :: [(Location,Int)]
example = zip [0..] [4,5,9,7,8]

adjacent :: Location -> [Location]
adjacent i
  | i == 0 = [i+1]
  | i == 4 = [i-1]
  | otherwise = [i-1,i+1]

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
