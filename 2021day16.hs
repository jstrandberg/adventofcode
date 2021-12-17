import Control.Monad.State

binaryFrom :: Char -> String
binaryFrom c
  | c == '0'  = "0000"
  | c == '1'  = "0001"
  | c == '2'  = "0010"
  | c == '3'  = "0011"
  | c == '4'  = "0100"
  | c == '5'  = "0101"
  | c == '6'  = "0110"
  | c == '7'  = "0111"
  | c == '8'  = "1000"
  | c == '9'  = "1001"
  | c == 'A'  = "1010"
  | c == 'B'  = "1011"
  | c == 'C'  = "1100"
  | c == 'D'  = "1101"
  | c == 'E'  = "1110"
  | c == 'F'  = "1111"
  | otherwise = ""

-- stolen from iceman@stackoverflow
intFrom :: String -> Int
intFrom = foldl (\a n -> a * 2 + (read [n])) 0

-- examples
example1 = "D2FE28"
example2 = "38006F45291200"
example3 = "EE00D40C823060"
example4 = "8A004A801A8002F478"
example5 = "620080001611562C8802118E34"
example6 = "C0015000016115A2E0802F182340"
example7 = "A0016C880162017C3686B18A3D4780"

example8  = "C200B40A82"
example9  = "04005AC33890"
example10 = "880086C3E88112"
example11 = "CE00C43D881120"
example12 = "D8005AC2A8F0"
example13 = "F600BC2D8F"
example14 = "9C005AC2F8F0"
example15 = "9C0141080250320F1802104A08"

data Header = Header { version :: Int, typeid :: Int }
  deriving Show

data Packet
  = Literal Header Int
  | Operator Header [Packet]
  deriving Show

getInt :: Int -> State String Int
getInt n = state (\s -> (intFrom . take n $ s, drop n s))

getChunk :: Int -> State String String
getChunk n = state (\s -> (take n s, drop n s))

getLength :: State String Int
getLength = state (\s -> (length s, s))

readHeader :: State String Header
readHeader = do
  version' <- getInt 3
  typeid'  <- getInt 3
  return $ Header version' typeid'

readValue :: State String String
readValue = do
  flag  <- getInt 1
  chunk <- getChunk 4
  if flag == 0
    then return chunk
    else do
      next <- readValue
      return $ chunk ++ next

readChunk :: State String [Packet]
readChunk = do
  packet  <- readPacket
  clength <- getLength
  if clength == 0
    then return $ packet : []
    else do
      next <- readChunk
      return $ packet : next

readSubPackets :: State String [Packet]
readSubPackets = do
  flag <- getInt 1
  if flag == 0
    then do
      clength <- getInt 15
      chunk   <- getChunk clength
      return $ evalState readChunk chunk
    else do
      num     <- getInt 11
      packets <- readSubPackets' num
      return packets
  where
    readSubPackets' 0 = return []
    readSubPackets' n = do
      packet <- readPacket
      next   <- readSubPackets' (n - 1)
      return $ packet : next

readPacket :: State String Packet
readPacket = do
  header <- readHeader
  if typeid header == 4
    then do
      value <- readValue
      return $ Literal header $ intFrom value
    else do
      content <- readSubPackets
      return $ Operator header content

mapPkt :: (Packet -> a) -> [Packet] -> [a]
mapPkt _ [] = []
mapPkt f ((Literal h v)  : ps) = f (Literal h v) : mapPkt f ps
mapPkt f ((Operator h c) : ps) = f (Operator h c) : mapPkt f c ++ mapPkt f ps

pktVersion :: Packet -> Int
pktVersion (Literal  header _) = version header
pktVersion (Operator header _) = version header

gt :: [Int] -> Int
gt [a,b] = if a > b then 1 else 0

lt :: [Int] -> Int
lt [a,b] = if a < b then 1 else 0

eq :: [Int] -> Int
eq [a,b] = if a == b then 1 else 0

runPkt :: Packet -> Int
runPkt (Literal  _ v) = v
runPkt (Operator h c) = case typeid h of
  0 -> sum . map runPkt $ c
  1 -> product . map runPkt $ c
  2 -> minimum . map runPkt $ c
  3 -> maximum . map runPkt $ c
  5 -> gt . map runPkt $ c
  6 -> lt . map runPkt $ c
  7 -> eq . map runPkt $ c

parse :: String -> String
parse = concat . map binaryFrom

result :: String -> Int
result s = sum . mapPkt pktVersion $ [evalState readPacket s]

result' :: String -> Int
result' = runPkt . evalState readPacket

main :: IO ()
main = do
  putStrLn "What to do..."
  putStrLn . show . result . parse $ example1
  putStrLn . show . result . parse $ example2
  putStrLn . show . result . parse $ example3
  putStrLn . show . result . parse $ example4
  putStrLn . show . result . parse $ example5
  putStrLn . show . result . parse $ example6
  putStrLn . show . result . parse $ example7
  putStrLn . show . result' . parse $ example8
  putStrLn . show . result' . parse $ example9
  putStrLn . show . result' . parse $ example10
  putStrLn . show . result' . parse $ example11
  putStrLn . show . result' . parse $ example12
  putStrLn . show . result' . parse $ example13
  putStrLn . show . result' . parse $ example14
  putStrLn . show . result' . parse $ example15
  {-
  file <- readFile "input.txt"
  putStrLn . show . result . parse $ file
  putStrLn . show . result' . parse $ file
  -}
