module Main where

-- DAY 2

checkIncreasing :: [Integer] -> Bool
checkIncreasing [] = True
checkIncreasing [_] = True
checkIncreasing (x:y:xs) = 
    if x < y && x >= y - 3
        then checkIncreasing (y:xs)
    else False

checkDecreasing :: [Integer] -> Bool
checkDecreasing [] = True
checkDecreasing [_] = True
checkDecreasing (x:y:xs) = 
    if x > y && x <= y + 3
        then checkDecreasing (y:xs)
    else False

removeAndCheck :: [Integer] -> Bool
removeAndCheck [] = True
removeAndCheck [_] = True
removeAndCheck xs = any valid listops
  where
    idxs = [0 .. length xs - 1]
    listops = map (\i -> remove xs i) idxs

remove :: [a] -> Int -> [a]
remove xs i = take i xs ++ drop (i + 1) xs

valid :: [Integer] -> Bool
valid xs = checkIncreasing xs || checkDecreasing xs

parseFile :: FilePath -> IO [[Integer]]
parseFile filePath = do
    contents <- readFile filePath
    return $ map (map read . words) (lines contents)

main :: IO ()
main = do
    putStrLn "Day 2"
    let filePath = "input.txt"
    rows <- parseFile filePath
    let part1 = length (filter id (map valid rows))
    print part1
    let part2 = length (filter id (map removeAndCheck rows))
    print part2
