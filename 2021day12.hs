import Data.Char
import Data.Tree

example1 :: [String]
example1 =
  ["start-A"
  ,"start-b"
  ,"A-c"
  ,"A-b"
  ,"b-d"
  ,"A-end"
  ,"b-end"
  ]

example2 :: [String]
example2 =
  ["dc-end"
  ,"HN-start"
  ,"start-kj"
  ,"dc-start"
  ,"dc-HN"
  ,"LN-dc"
  ,"HN-end"
  ,"kj-sa"
  ,"kj-HN"
  ,"kj-dc"
  ]

example3 :: [String]
example3 =
  ["fs-end"
  ,"he-DX"
  ,"fs-he"
  ,"start-DX"
  ,"pj-DX"
  ,"end-zg"
  ,"zg-sl"
  ,"zg-pj"
  ,"pj-he"
  ,"RW-he"
  ,"fs-DX"
  ,"pj-RW"
  ,"zg-RW"
  ,"start-pj"
  ,"he-WI"
  ,"zg-he"
  ,"pj-fs"
  ,"start-RW"
  ]

type Caves = [(String,String)]

parseCaves :: [String] -> Caves
parseCaves = foldl addCave []
  where
    addCave cs s =
      let [a,b] = words $ [if x == '-' then ' ' else x | x <- s]
      in  (a,b) : (b,a) : cs

connectsTo :: String -> Caves -> [String]
connectsTo c = map snd . filter ((==) c . fst)

buildTree :: String -> Caves -> [String] -> Tree String
buildTree "end" _ _ = Node "end" []
buildTree s cs vs = Node s $ map (\x -> buildTree x cs vs') subTrees
  where
    subTrees = filter (flip notElem vs) $ connectsTo s cs
    vs' = if all isLower s
      then s : vs
      else vs

result x = foldTree (\n ns -> if n == "end" then 1 else sum ns) tree
  where
    tree = buildTree "start" (parseCaves x) []

main :: IO ()
main = do
  putStrLn "What to do..."
  putStrLn . show . result $ example1
  putStrLn . show . result $ example2
  putStrLn . show . result $ example3
  {-
  file <- readFile "input.txt"
  putStrLn . show . result . lines $ file
  -}
