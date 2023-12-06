import Data.Char
import Data.Maybe
import Debug.Trace

main :: IO()
main = do 
    l <- (readFile "input.txt") >>= (\x -> return (lines x))
    putStrLn $ show $ sumParts l
    putStrLn $ show $ counts $ starCoords l

sumParts :: [String] -> Int
sumParts [] = 0
sumParts input = sum  $ map read $ foldr (++) [] $ map (partsInLine input) $ [0..(length input - 1)]

partsInLine :: [String] -> Int -> [String]
partsInLine input y = 
    partsInLine' 0 (input !! y) 
    where 
        partsInLine' :: Int -> String -> [String]
        partsInLine' _ "" = [] 
        partsInLine' x line = 
            case readNum line of 
                ""  -> partsInLine' (x+1) (skip line 1) 
                num -> 
                    if isAdjacentToSymbol input (x, y) (length num) 
                        then num:partsInLine' (x + (length num)) (skip line (length num))
                        else partsInLine' (x + (length num)) (skip line (length num))

counts :: [(Int, [(Int, Int)])] -> Int
counts stars =
    let
        allCoords = map snd stars
        allNumbers = map fst stars
        uniqueCoords = unique $ foldr (++) [] $ allCoords 
        indexesCoordsOccurIn = map ((flip findOccurences) allCoords) uniqueCoords
        adjToTwo = filter (\x -> length x == 2) indexesCoordsOccurIn
        indexesToNumber = map (\(first:second:_) -> (allNumbers !! first, allNumbers !! second)) adjToTwo
    in  sum $ map (\(a, b) -> a * b) indexesToNumber


starCoords :: [String] -> [(Int, [(Int, Int)])]
starCoords input = foldr (++) [] $  map (\(y, line) -> starCoords' (0, y) line) $  zip [0..] input 
    where 
        starCoords' :: (Int, Int) -> String -> [(Int, [(Int, Int)])]
        starCoords' _  "" = []
        starCoords' (x, y) line = 
            case readNum line of 
                "" -> starCoords' (x+1, y) (skip line 1)
                num -> (read num, starsAdjacentTo input (x, y) (length num)) : starCoords' (x+length num,y) (skip line (length num))


findOccurences :: (Eq a) => a -> [[a]] -> [Int]
findOccurences item list = findOccurences' item (zip [0..(length list)] list)
    where
        findOccurences' :: (Eq a) => a -> [(Int, [a])] -> [Int]
        findOccurences' _ [] = []
        findOccurences' item ((i, sublist):rest) = 
            if item `elem` sublist 
                then i:findOccurences' item rest 
                else findOccurences' item rest 


unique :: (Eq a) => [a] -> [a]
unique [] = []
unique (x:xs) = x:unique (filter ((/=) x) xs)


isAdjacentToSymbol = isAdjacent symbol

isAdjacent :: (Char -> Bool) -> [String] -> (Int, Int) -> Int -> Bool 
isAdjacent predicate input coords len = any predicate $ adjacentChars input coords len 

skip :: String -> Int -> String 
skip "" _ = ""
skip str 0 = str 
skip (_:cs) count = skip cs $ count - 1  

readNum :: String -> String 
readNum = takeWhile isDigit 

starsAdjacentTo :: [String] -> (Int, Int) -> Int -> [(Int, Int)]
starsAdjacentTo input coord len = 
    let
        adjCoords = adjacentCoords coord len
        adjCharsAreStars =  map (fmap (== '*')) $ map (charAt input) $ adjacentCoords coord len

        isJustTrue (Just True) = True
        isJustTrue _ = False
    in map fst $ filter (isJustTrue.snd) $ zip adjCoords adjCharsAreStars


adjacentChars :: [String] -> (Int, Int) -> Int -> [(Char)]
adjacentChars input coord len = catMaybes $ map (charAt input) $ adjacentCoords coord len


adjacentCoords :: (Int, Int) -> Int -> [(Int, Int)]
adjacentCoords (x, y) len = 
    let ends   = [(x-1, y), (x + len, y)]
        diags  = [(x-1, y-1), (x-1, y+1), (x+len, y-1), (x+len, y+1)]
        upDown = foldr (++) [] $ map (\x -> [(x, y+1), (x, y-1)]) [x..x+len-1]
    in  ends ++ diags ++ upDown
 
charAt :: [String] -> (Int, Int) -> Maybe Char 
charAt input (x,y)  = do 
    row <- get input y 
    char <- get row x 
    return char

symbol :: Char -> Bool
symbol c = not $ (c == '.') || (isDigit c)

get :: [a] -> Int -> Maybe a 
get list index = 
    if index < 0
        then Nothing 
        else if index >= length list 
            then Nothing 
            else Just $ list !! index
