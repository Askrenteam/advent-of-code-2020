import System.IO
import Data.List

readInput :: String -> [String]
readInput s = lines s 

readBinary :: String -> Int
readBinary s = sum [2^(length s - 1 - x) | x <- [0..length s - 1] , s!!x == 'R' || s!!x == 'B']

rowId :: String -> Int
rowId s = readBinary $ take 7 s

colId :: String -> Int
colId s = readBinary $ drop 7 s

seatId :: String -> Int
seatId s = rowId s * 8 + colId s

-- TODO : deduplicate and isolate the pair in the middle
solve :: [String] -> [(Int, Int)]
solve l = let seats = map (\s -> (rowId s, colId s)) l
            in [(x, y) | x <- map fst seats, y <- map snd seats, not ((x, y) `elem` seats)] 

getAnswer :: String -> String
getAnswer s = show $ solve $ readInput s

main :: IO ()
main = do 
    input <- readFile "input"
    putStrLn $ getAnswer input
