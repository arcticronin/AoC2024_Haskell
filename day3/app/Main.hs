module Main where
import System.IO
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()
import Data.List
import Data.Maybe
import Data.Char (isDigit)

-- regex
mulPattern :: String
mulPattern = "mul\\(([0-9]+),([0-9]+)\\)"

extractMulPairs :: String -> [(Integer, Integer)]
extractMulPairs input = 
    let matches = input =~ mulPattern :: [[String]]
    in map (\[_, x, y] -> (read x, read y)) matches

parseFile :: FilePath -> IO [(Integer, Integer)]
parseFile filePath = do
    contents <- readFile filePath
    return (extractMulPairs contents)

part1:: [(Integer, Integer)] -> Integer
part1 = sum . map (uncurry (*))

main :: IO ()
main = do
    let filePath = "input.txt"  -- Change the file path if needed
    mulPairs <- parseFile filePath
    print $ part1 mulPairs
