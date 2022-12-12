import Control.Monad.State.Lazy
import Data.Tree

example :: String
example = "$ cd /\n$ ls\ndir a\n14848514 b.txt\n8504156 c.dat\ndir d\n$ cd a\n$ ls\ndir e\n29116 f\n2557 g\n62596 h.lst\n$ cd e\n$ ls\n584 i\n$ cd ..\n$ cd ..\n$ cd d\n$ ls\n4060174 j\n8033020 d.log\n5626152 d.ext\n7214296 k"

type File = (String, Int)
type ParserState = ([String], Tree File)

fs :: Tree File
fs = Node ("/", 0) []

add :: [String] -> File -> Tree File -> Tree File
add path file tree
  | path == [] = tree { subForest = Node file [] : subForest tree }
  | otherwise  = tree { subForest = [ if (fst . rootLabel) x == head path then add (tail path) file x else x | x <- subForest tree ] }

runCd :: String -> State ParserState ()
runCd s = do
  (path,tree) <- get
  let folder = (last . words) s
  case folder of
    "/"       -> put ([],            tree)
    ".."      -> put (tail path,     tree)
    otherwise -> put (folder : path, tree)

runAdd :: String -> State ParserState ()
runAdd s = do
  (path,tree) <- get
  let s'    = words s
  let file  = last s'
  let size  = if head s' == "dir" then 0 else (read . head) s'
  let tree' = add (reverse path) (file, size) tree
  put (path, tree')

runLn :: String -> State ParserState ()
runLn s = do
  (path,files) <- get
  case take 3 s of
    "$ c"     -> runCd s
    "$ l"     -> return ()
    otherwise -> runAdd s

main :: IO ()
main = do
  putStrLn "What to do..."
  putStrLn $ example
  {-
  file <- readFile "input.txt"
  putStrLn $ file
  -}
