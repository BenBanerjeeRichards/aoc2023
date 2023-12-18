import Data.Text (splitOn, pack, Text, unpack)
import Debug.Trace

main :: IO ()
main = do 
    l <- readFile "input.txt" >>= return . lines
    let numsStrs = map (splitOn (pack " ") . pack) l
    let nums = (map . map) (read . unpack :: Text -> Int) numsStrs 
    putStrLn $ show $ part1 nums
    putStrLn $ show $ part2 nums


part1 :: [[Int]] -> Int 
part1 input = sum $  map history input

part2 :: [[Int]] -> Int 
part2 input = sum $  map reverseHistory input


differences :: [Int] -> [Int]
differences [] = []
differences (x:xs) = zipWith (-) xs (x:xs)

history :: [Int] -> Int
history xs =
    let diffs = differences xs 
     in if all (== 0) diffs || null diffs
        then last xs 
        else last xs + history diffs

reverseHistory :: [Int] -> Int 
reverseHistory xs =
    let diffs = differences xs 
     in if all (== 0) diffs || null diffs
        then head xs 
        else head xs - reverseHistory diffs
