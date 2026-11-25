import System.Environment (getArgs, getProgName)
import System.Directory (doesFileExist)
import Data.List (group, sort)

-- ====================================

day_program :: String -> IO ()
day_program f = do
    let nums = map read (lines f) :: [Int]
    let deviceJoltage = maximum nums + 3
    let diffCounts = countsOfDifferences (0 : nums ++ [deviceJoltage])
    print diffCounts

    let ans = (*) <$> lookup 1 diffCounts <*> lookup 3 diffCounts
    print ans

    return ()

-- ====================================

countsOfDifferences :: (Num a, Ord a, Eq a) => [a] -> [(a, Int)]
countsOfDifferences = map (\g -> (g !! 0, length g))
                    . group
                    . sort
                    . map (\(x,y) -> y - x)
                    . pairwise
                    . sort

pairwise :: [a] -> [(a, a)]
pairwise xs = zip xs $ drop 1 xs

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
