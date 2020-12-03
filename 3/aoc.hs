import System.IO

readInput :: String -> [String]
readInput s = lines s 

countTrees :: Int -> Int -> [String] -> Int
countTrees right down l = length $ filter (== '#') $ map (\(x, y) -> x!!y) $ zip (map (l!!) [0, down..(length l - 1)]) [(right * x) `mod` (length $ l!!0) | x <- [0..]]

solve :: [String] -> Int
solve l = countTrees 1 1 l * countTrees 3 1 l * countTrees 5 1 l * countTrees 7 1 l * countTrees 1 2 l 

getAnswer :: String -> String
getAnswer s = show $ solve $ readInput s

main :: IO ()
main = do 
    input <- readFile "input"
    putStrLn $ getAnswer input
