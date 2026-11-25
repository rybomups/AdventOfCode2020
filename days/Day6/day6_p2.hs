import System.Environment (getArgs, getProgName)
import System.Directory (doesFileExist)
import Data.List (groupBy, intersect)

-- ====================================

day_program :: String -> IO ()
day_program f = do
    let paragraphs = filter (/= [""]) 
                   $ groupBy (\a b -> not (null a) && not (null b)) 
                   $ lines f
    let answers = map commonElements paragraphs
    putStrLn $ "Sum of those counts = " ++ show (sum $ map length answers)

-- ====================================

commonElements :: (Ord a) => [[a]] -> [a]
commonElements [] = []
commonElements xss = foldr1 intersect xss

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
