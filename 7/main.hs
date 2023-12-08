import Data.List
import Debug.Trace

main :: IO ()
main = do 
    l <- (readFile "input.txt") >>= (\x -> return (lines x))
    putStrLn $ show $ part1 $ map parseLine l
    putStrLn $ show $ part2 $ map parseLine l

part1Chars = ['2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A']
part2Chars = ['J', '2', '3', '4', '5', '6', '7', '8', '9', 'T', 'Q', 'K', 'A']

parseLine :: String -> (String, Int)
parseLine (c1:c2:c3:c4:c5:_:rest) = ((c1:c2:c3:c4:c5:""), read rest)

part1 :: [(String, Int)] -> Int 
part1 hand = sum $ map (\(a, b) -> a * b) $ zip [1..] $ map snd $ sortHand part1Chars handType  hand

part2 :: [(String, Int)] -> Int 
part2 hand = sum $ map (\(a, b) -> a * b) $ zip [1..] $ map snd $ sortHand part2Chars handTypePart2  hand


sortHand :: [Char] -> (String -> Int)-> [(String, Int)] -> [(String, Int)]
sortHand partChars handFn hand = sortBy sortFn hand
    where 
        sortFn :: (String, Int) -> (String, Int) -> Ordering
        sortFn (a, _) (b, _) = 
            let 
                h1 = handFn a 
                h2 = handFn b
            in if h1 == h2 
                then baseCompare partChars a b 
                else compare h1 h2 

-- 7 = strongest (Give of a kind), 1 = weakest (High card)
handType :: String -> Int 
handType str = 
    case letterCounts str of 
        [5] -> 7        
        [4, 1] -> 6
        [3, 2] -> 5
        [3, 1, 1] -> 4
        [2, 2, 1] -> 3
        [2, 1, 1, 1] -> 2
        [1, 1, 1, 1, 1] -> 1

handTypePart2 :: String -> Int 
handTypePart2 hand = 
    let 
        possibleHands = generatePossibleHands hand
        possibleHandsWithType = zip possibleHands $ map handType $ possibleHands
        sortedHands = sortBy (\a b -> compare (snd a) (snd b)) $  possibleHandsWithType
    in
        snd $ (head.reverse) $ sortedHands

generatePossibleHands :: String -> [String]
generatePossibleHands hand = 
    let numJokers = length $ filter (== 'J') hand
        getHand replacement = map (\c -> if c == 'J' then replacement else c) hand
        otherChars = filter (/= 'J') part2Chars
     in
        if numJokers == 0
            then [hand]
            else map getHand otherChars


letterCounts :: String -> [Int]
letterCounts str = reverse.sort $ map length $ splitCommonLetters (sort str)

splitCommonLetters :: String -> [String]
splitCommonLetters = foldr combiner []

combiner :: Char -> [[Char]] -> [[Char]]
combiner c [] = [[c]]
combiner c (current:rest) = 
    case current of 
        [] -> [[c]]
        (currentChar:_) ->
            if currentChar == c
                then  [(c:current)] ++ rest  
                else [[c]] ++ (current:rest)


baseCompare :: [Char] -> String -> String -> Ordering 
baseCompare _ "" _ = EQ 
baseCompare _ _ "" = EQ
baseCompare partChars (c1:rest1) (c2:rest2) = 
    case compareChar partChars c1 c2 of 
        EQ -> baseCompare partChars rest1 rest2
        r -> r


compareChar :: [Char] -> Char -> Char -> Ordering 
compareChar partChars c1 c2 = compare (index partChars c1) (index partChars c2)

index :: [Char] -> Char -> Int 
index partChars c = case elemIndex c partChars of 
    Just x -> x 

