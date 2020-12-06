import System.IO
import Data.List

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn delim l = filter (/= [delim]) $ groupBy (\x y -> (x == delim) == (y == delim)) l

readInput :: String -> [[String]]
readInput s = splitOn "" $ lines s 

solve :: [[String]] -> Int
solve l = sum $ map (length . foldl intersect ['a'..'z']) l

getAnswer :: String -> String
getAnswer s = show $ solve $ readInput s

main :: IO ()
main = do 
    input <- readFile "input"
    putStrLn $ getAnswer input
