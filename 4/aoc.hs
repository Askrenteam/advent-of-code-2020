import System.IO
import Data.List
import Text.Regex.Posix

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn delim l = filter (/= [delim]) $ groupBy (\x y -> (x == delim) == (y == delim)) l

readInput :: String -> [[(String, String)]]
readInput s = map (\y -> map (\x -> let [k, v] = splitOn ':' x in (k, v)) y) $ map (concatMap words) $ splitOn "" $ lines s 

fields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

isValidPassport :: [(String, String)] -> Bool
isValidPassport p = (all (flip elem (map fst p)) fields) && (all isValidField p)


isValidField :: (String, String) -> Bool
isValidField ("byr", f) = (f =~ "^[0-9]{4}$") && read f >= 1920 && read f <= 2002
isValidField ("iyr", f) = (f =~ "^[0-9]{4}$") && read f >= 2010 && read f <= 2020
isValidField ("eyr", f) = (f =~ "^[0-9]{4}$") && read f >= 2020 && read f <= 2030
isValidField ("hgt", f) | f =~ "^[0-9]{3}cm$" = read (take 3 f) >= 150 && read (take 3 f) <= 193
                        | f =~ "^[0-9]{2}in$" = read (take 2 f) >= 59 && read (take 2 f) <= 76
                        | otherwise = False
isValidField ("hcl", f) = f =~ "^#[0-9a-f]{6}$"
isValidField ("ecl", f) = f =~ "^(amb|blu|brn|gry|grn|hzl|oth)$"
isValidField ("pid", f) = f =~ "^[0-9]{9}$"
isValidField (_, f) = True

solve :: [[(String, String)]] -> Int
solve l = length $ filter id $ map isValidPassport l

getAnswer :: String -> String
getAnswer s = show $ solve $ readInput s

main :: IO ()
main = do 
    input <- readFile "input"
    putStrLn $ getAnswer input
