import System.Environment (getArgs, getProgName)
import Data.List (tails)

-- ====================================

day_program :: String -> IO ()
day_program f = do
    let nums = map read $ words f :: [Integer]
    print $ map (\(x, y, z) -> x * y * z) $ find3ThatSum 2020 nums

-- ====================================

find3ThatSum :: (Num a, Eq a) => a -> [a] -> [(a, a, a)]
find3ThatSum num arr = [(x, y, z) | (x:xs) <- tails arr
                                  , (y:ys) <- tails xs
                                  , z      <- ys
                                  , x + y + z == num]

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
