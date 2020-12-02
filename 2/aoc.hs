import System.IO

data Password = Password { range :: (Int, Int), char :: Char, psswd :: String } deriving (Show, Eq)

readRange :: String -> (Int, Int)
readRange s = (\p -> (read $ fst p, read $ tail $ snd p)) $ break (== '-') s

readInput :: String -> [Password]
readInput s = map (\w -> Password (readRange (w!!0)) (w!!1!!0) (w!!2)) $ map (\l -> words l) $ lines s 

solve :: [Password] -> Int
solve l = length [x | x <- l, (psswd x !!(fst (range x) - 1) == char x) /= 
                        (psswd x !!(snd (range x) - 1) == char x) ]

getAnswer :: String -> String
getAnswer s = show $ solve $ readInput s

main :: IO ()
main = do 
    input <- readFile "input"
    putStrLn $ getAnswer input
