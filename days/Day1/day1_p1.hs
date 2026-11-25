import System.Environment (getArgs, getProgName)
import Data.List (tails)

-- ====================================

day_program :: String -> IO ()
day_program f = do
    let nums = map read $ words f :: [Int]
    print $ map (\(x, y) -> x * y) $ find2ThatSum 2020 nums

-- ====================================

find2ThatSum :: (Num a, Eq a) => a -> [a] -> [(a, a)]
find2ThatSum num arr = [(x, y) | (x:rest) <- tails arr, y <- rest, x + y == num]

-- ====================================

main :: IO ()
main = do
    args <- getArgs
    progName <- getProgName
    case args of
        [fileName] -> do
            fileContents <- readFile fileName
            day_program fileContents
        _          -> putStrLn $ "Usage: " ++ progName ++ " <filename>"
