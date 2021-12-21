example :: String
example = "target area: x=20..30, y=-10..-5"

type Pos  = (Int,Int)
type Rect = (Pos,Pos)

-- this was not necessary for part one
parse :: String -> Rect
parse s = ((fst $ fromString xs,fst $ fromString ys),(snd $ fromString xs, snd $ fromString ys))
  where
    afterEq = tail . dropWhile (/= '=')
    fromString s' =
      let [a,b] = map read . words $ [if c == '.' then ' ' else c | c <- s']
      in  (a,b)
    xs = takeWhile (/= ',') . afterEq $ s
    ys = afterEq . afterEq $ s

xsFrom :: Rect -> [Int]
xsFrom r = let x1 = (fst . fst) r; x2 = (fst . snd) r in [x1,x2]

ysFrom :: Rect -> [Int]
ysFrom r = let y1 = (snd . fst) r; y2 = (snd . snd) r in [y1,y2]

minY :: Rect -> Int
minY = minimum . ysFrom

maxX :: Rect -> Int
maxX = maximum . xsFrom

notMissed :: Rect -> Pos -> Bool
notMissed r p = fst p <= maxX r && snd p >= minY r

inRect :: Rect -> Pos -> Bool
inRect ((rx1,ry1),(rx2,ry2)) (x,y)
  = rx1 <= x && rx2 >= x && ry1 <= y && ry2 >= y

trajs :: (Int,Int) -> [Pos]
trajs (xv,yv) = zip trajXs trajYs
  where
    trajXs = scanl (\x m -> x + (max 0 (xv - m))) 0 [0..]
    trajYs = scanl (\y m -> y + (yv - m)) 0 [0..]

fact :: Int -> Int
fact 0 = 0
fact n = n + fact (n-1)

result :: String -> Int
result x = fact . abs $ y + 1
  where
    y = let ((_,y1),(_,y2)) = parse x
        in  min y1 y2

result' x = length . filter (inRect rect) . map last' $ vs
  where
    last' = last . takeWhile (notMissed rect) . trajs
    rect = parse x
    vs = [(vx,vy) | vy <- vys, vx <- vxs]
    vys = [(minY rect)..(abs . minY $ rect)]
    vxs = [1..(maxX rect)]

main :: IO ()
main = do
  putStrLn "What to do..."
  putStrLn . show . result $ example
  putStrLn . show . result' $ example
  {-
  file <- readFile "input.txt"
  putStrLn . show . result $ file
  putStrLn . show . result' $ file
  -}
