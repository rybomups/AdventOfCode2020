import System.Environment (getArgs, getProgName)
import System.Directory (doesFileExist)
import Data.List (tails, nub)

-- ====================================

preambleSize :: Int
preambleSize = 25

day_program :: String -> IO ()
day_program f = do
    let nums = map read $ lines f :: [Integer]
    case getInvalidNums preambleSize nums of
        (ans:_) -> 
            case getRangeThatSums (0, 1) nums ans of
                Nothing -> putStrLn "Could not find answer"
                Just idxRange -> 
                    let (minVal, maxVal) = minAndMaxInRange idxRange nums
                    in putStrLn $ "Encryption weakness = " ++ show (minVal + maxVal)
        _ -> putStrLn "no answer found"
    return ()

-- ====================================

getDistinctSums :: (Num a, Eq a) => [a] -> [a]
getDistinctSums xs = nub [x + y | (x:ys) <- tails xs, y <- ys]

getInvalidNums :: (Num a, Eq a) => Int -> [a] -> [a]
getInvalidNums _ []       = []
getInvalidNums idx nums 
    | idx >= length nums  = []
    | otherwise           = let (before, curr : _) = splitAt idx nums
                            in if curr `elem` (getDistinctSums $ drop (idx - preambleSize) before) then
                                getInvalidNums (idx + 1) nums
                            else
                                curr : getInvalidNums (idx + 1) nums

getRangeThatSums :: (Num a, Ord a) => (Int, Int) -> [a] -> a -> Maybe (Int, Int)
getRangeThatSums _ [] _ = Nothing
getRangeThatSums idxRange@(minIdx, maxIdx) nums target
    | maxIdx >= length nums || minIdx >= length nums = Nothing
    | currSum == target = Just idxRange
    | currSum < target  = getRangeThatSums (minIdx, maxIdx + 1) nums target
    | currSum > target  = getRangeThatSums (minIdx + 1, maxIdx) nums target
    where currSum = sum $ drop minIdx $ take maxIdx nums

minAndMaxInRange :: (Num a, Ord a) => (Int, Int) -> [a] -> (a, a)
minAndMaxInRange (minIdx, maxIdx) nums = (minimum currRange, maximum currRange)
    where currRange = drop minIdx $ take maxIdx nums

-- ====================================

main :: IO ()
main = do
    args <- getArgs; progName <- getProgName
    case args of
        (fileName:_) -> do
            fileExists <- doesFileExist fileName
            if fileExists then do
                fileContents <- readFile fileName
                putStrLn $ "Opened file: " ++ fileName
                putStrLn ""; day_program fileContents; putStrLn ""
            else do
                putStrLn $ "File " ++ fileName ++ " does not exist."

        _            -> putStrLn $ "Usage: " ++ progName ++ " <filename>"
