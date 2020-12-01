import System.IO

getList :: String -> [Int]
getList s = map read $ lines s 

getAnswer :: [Int] -> Int
getAnswer l = head [x * y * z | x <- l, 
                                y <- l, 
                                z <- l, 
                                x + y + z == 2020]

solve :: String -> String
solve s = show $ getAnswer $ getList s

main :: IO ()
main = do 
    input <- readFile "input"
    putStrLn $ solve input
