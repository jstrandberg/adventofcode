import Prelude hiding (lookup)
import Data.Maybe
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

example :: Map Vector Int
example
  = Map.fromList
  $ zip [(x,y) | y <- [0..9], x <- [0..9]]
  $ map (read . pure)
  $ concat
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

type Vector = (Int,Int)

start  = (0,0)
end    = (9,9)

open :: Map Vector Vector
open = Map.fromList $ map (,start) $ adj start
closed :: Map Vector Vector
closed = Map.singleton start start

adj :: Vector -> [Vector]
adj (x,y) = [(x,y-1), (x-1,y  ), (x+1,y  ), (x,y+1)]

dist :: Vector -> Int
dist (x,y) = (x - fst end)^2 + (y - snd end)^2

cost :: Vector -> Int
cost v = fromMaybe 999 $ Map.lookup v example

costTotal :: Map Vector Vector -> Vector -> Int
costTotal mv (0,0) = 0
costTotal mv v = c + (costTotal mv $ (Map.!) mv v)
  where
    c = cost v

insertMaybe parent o c v = if costTotal c parent' < costTotal c parent
  then Map.insert v parent' o
  else Map.insert v parent o
  where
    parent' = fromMaybe parent $ Map.lookup v o
    --m = Map.union o c

-- insertMaybe parent o v = Map.insert v parent' o
--   where parent' = fromMaybe parent $ Map.lookup v o

next
  = fst
  $ Map.foldlWithKey (\a v c -> if snd a > c then (v,c) else a) (end,maxBound)
  $ Map.mapWithKey (\v _ -> cost v + dist v)
  $ open

step o c
  | Map.member end c = (o,c)
  | Map.member (0,2) c =(o,c)
  | otherwise = step o'' c'
  where
    current
      = fst
      $ Map.foldlWithKey (\a v c -> if snd a > c then (v,c) else a) (end,maxBound)
      $ Map.mapWithKey (\v _ -> cost v + dist v)
      $ o
    o' = Map.delete current o
    c' = let Just p = Map.lookup current o in Map.insert current p c
    adj' = adj current
    o'' = foldl (\a x -> insertMaybe current a c' x) o' adj'
