module Main where
import Data.List (transpose, sortOn)

parseFile :: FilePath -> IO ([Integer], [Integer])
parseFile filePath = do
    contents <- readFile filePath
    let rows = map (map read . words) (lines contents) :: [[Integer]]
    let [col1, col2] = transpose rows  -- Separate columns
    return (col1, col2)

--     ->   difference (number on first list, number on second list, 
pairall :: [Integer] -> [Integer] -> [(Integer, (Integer, Integer, Integer, Integer))]
pairall xs ys =
    let xs' = zip xs [0..]            
        ys' = zip ys [0..]            
        sortedXs = sortOn fst xs'     
        sortedYs = sortOn fst ys'     
        pairs = zip sortedXs sortedYs 
    in map (\((x, i), (y, j)) -> (abs (x - y), (x, y, i, j))) pairs


remove_and_count :: Integer -> [Integer] -> (Integer, [Integer])
remove_and_count _ [] = (0, [])
remove_and_count n (x:xs) = 
    let(count, rest) = remove_and_count n xs 
    in
        if n == x then (1 + count, rest) 
        else (0 + count, x : rest)

part2 :: [Integer] -> [Integer] -> Integer
part2 [] _ = 0
part2 _ [] = 0
part2 (x:xs) y = 
    let 
        (numOfX, restOfXs) = remove_and_count x xs 
        (numOfY, restOfY) = remove_and_count x y  
    in 
        ((1 + numOfX) * numOfY * x) + part2 restOfXs restOfY  



main :: IO ()
main = do
    let filePath = "input.txt"  
    (list1, list2) <- parseFile filePath
    let f = pairall list1 list2
    let p1 = sum $ map fst f
    let p2 = part2 list1 list2
    print p1
    print p2