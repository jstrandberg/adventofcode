import Data.Maybe
import Data.List

-- playing with a 1d version for simplicity
example :: [(Int,Int)]
example = zip [0..] [4,5,9,10,8,3]

-- adjacent indexes, needing boundaries
adjacent :: Int -> [Int]
adjacent i =
    [i-1
    ,i+1
    ]

-- runs one iteration of checking flashes
-- increase adjacent locations and track
-- number of flashes
-- i have a strong feeling this should use
-- a state monad or something
-- no idea how to do that currently
flash :: [(Int,Int)] -> (Int,[(Int,Int)])
flash = foldr flash' (0,[]) . calc
    where flash' (i,x) (n,a) = if x > 9
            then (n + 1,) $ map (,1) (adjacent i) ++ (i,0) : a
            else (n,) $ (i,x) : a

-- compressing/calculating the result after flashing
-- skipping already flashed locations
calc :: [(Int,Int)] -> [(Int,Int)]
calc = foldr calc' [] . sort
    where add a b
              | a == 0 = 0
              | b == 0 = 0
              | otherwise = a + b
          calc' x [] = [x]
          calc' x (y:ys) =
              if fst x == fst y
                  then (fst x, add (snd x) (snd y)) : ys
                  else x : y : ys
