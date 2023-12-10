import Data.List 
import Data.Maybe 
import qualified Data.Map.Strict as M
import Debug.Trace


main :: IO ()
main = do 
    l <- (readFile "input.txt") >>= (\x -> return (lines x))
    -- (direction, nodes, start, goal) <- return $ parseInput l
    -- putStrLn $ show $ findGoal nodes direction start goal
    putStrLn $ show $ part2 l


part2 :: [String] -> Int
part2 (directions:_:rest) = 
    let 
        nodes = parseInputToMap rest 
        startingNodes = filter isStartNode  $ M.keys nodes 
    in multiLcm $ map (\pos -> countMovesToEnd nodes pos directions 0) startingNodes

parseInput :: [String] -> (String, [(Int, Int)], Int, Int)
parseInput (direction:_:rest) =
    let
        (nodes, start, goal) = toListNotation (map parseLine rest)
    in (direction, nodes, start, goal)

parseInputToMap :: [String] -> M.Map String (String, String)
parseInputToMap input = M.fromList $ map (\(a, b, c) -> (a, (b, c))) $ map parseLine input

parseLine :: String -> (String, String, String)
parseLine line = (take 3 line, take 3 (drop 7 line), take 3 (drop 12 line))

multiLcm :: [Int] -> Int
multiLcm = foldr lcm 1 

countMovesToEnd :: M.Map String (String, String) -> String -> String -> Int -> Int 
countMovesToEnd nodes position directions count = 
    let
        newPosition = ghostTraverse nodes position directions 
     in if isEndNode newPosition
        then count + length directions 
        else countMovesToEnd nodes newPosition directions (count + length directions)


ghostTraverse :: M.Map String (String, String) -> String -> String -> String 
ghostTraverse _ startNode "" = startNode 
ghostTraverse nodes startNode (dir:restDir) = 
    case dir of 
        'L' -> ghostTraverse nodes (fst $ fromJust $ M.lookup startNode nodes) restDir
        'R' -> ghostTraverse nodes (snd $ fromJust $ M.lookup startNode nodes) restDir

isEndNode :: String -> Bool 
isEndNode = endsWith 'Z'

isStartNode :: String -> Bool 
isStartNode = endsWith 'A'

endsWith :: Char -> String -> Bool 
endsWith ch (_:_:c:[]) = c == ch


toListNotation :: [(String, String, String)] -> ([(Int, Int)], Int, Int)
toListNotation lines = 
    let 
        codes = map (\(a,_,_) -> a) lines
        codeToIndex x = (fromJust.(elemIndex x)) codes
        leftIndexes = map (\(_, l, _) -> codeToIndex l) lines
        rightIndexes = map (\(_, _, r) -> codeToIndex r) lines
        goalIndex = (fromJust.(elemIndex "ZZZ")) codes
        startIndex = (fromJust.(elemIndex "AAA")) codes
     in (zip leftIndexes rightIndexes, startIndex, goalIndex)


navigate :: [(Int, Int)] -> String -> Int -> Int 
navigate _ "" current = current
navigate nodes (dir:rest) current =
    case dir of 
        'L' -> navigate nodes rest (fst (nodes !! current))
        'R' -> navigate nodes rest (snd (nodes !! current))

findGoal :: [(Int, Int)] -> String -> Int -> Int -> Int 
findGoal nodes direction start goal = 
    findGoal' 0 start
    where 
        findGoal' :: Int -> Int -> Int 
        findGoal' numSteps currentNode = 
            let 
                pos = navigate nodes direction currentNode 
             in 
                 if pos == goal
                    then numSteps + length direction 
                    else findGoal' (numSteps + length direction) pos

