import System.Environment (getArgs, getProgName)
import System.Directory (doesFileExist)
import Data.List (tails, nub)

-- ====================================

preambleSize :: Int
preambleSize = 25

day_program :: String -> IO ()
day_program f = do
    let nums = map read $ lines f :: [Integer]
    let (ans:_) = getInvalidNums preambleSize nums
    putStrLn $ "First number not having the property = " ++ show ans
    return ()

-- ====================================

getDistinctSums :: (Num a, Ord a) => [a] -> [a]
getDistinctSums xs = nub [x + y | (x:ys) <- tails xs, y <- ys]

getInvalidNums :: (Num a, Ord a) => Int -> [a] -> [a]
getInvalidNums _ []       = []
getInvalidNums idx nums 
    | idx >= length nums  = []
    | otherwise           = let (before, curr : _) = splitAt idx nums
                            in if curr `elem` (getDistinctSums $ drop (idx - preambleSize) before) then
                                getInvalidNums (idx + 1) nums
                            else
                                curr : getInvalidNums (idx + 1) nums

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
