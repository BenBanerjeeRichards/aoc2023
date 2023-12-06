import Data.Char

numStrings = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

main :: IO ()
main = do 
    words <- (readFile "input.txt") >>= (\x -> return (lines x))
    putStrLn $ show $ sum $ map calibrationValue words

calibrationValue :: String -> Int 
calibrationValue str = read $ (show $ head nums) ++ (show $ last nums)
    where nums = numbers str


numbers :: String -> [Int]
numbers str = reverse $ numbers' str []
    where
        numbers' :: String -> [Int] -> [Int]
        numbers' "" result = result
        numbers' (s:ss) result =
            if isDigit s 
                then numbers' ss result ++ [digitToInt s] 
                else case startsWithAny (s:ss) numStrings of 
                    Just x -> numbers' ss result ++ [x]
                    Nothing -> numbers' ss result 

startsWithAny :: String -> [String] -> Maybe Int
startsWithAny str prefixes = 
    case filter snd $ zip [1..] $ map (startsWith str) prefixes of
        []     -> Nothing 
        (x:xs) -> Just $ fst x

startsWith str prefix = 
    length (filter (\(a, b) -> a == b)$ zip str prefix) == length prefix
