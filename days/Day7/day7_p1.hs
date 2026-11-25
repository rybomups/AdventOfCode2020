import System.Environment (getArgs, getProgName)
import System.Directory (doesFileExist)
import qualified Data.Map as Map
import Data.Char (isAlpha)

-- ====================================

desiredBag :: String
desiredBag = "shinygold"

day_program :: String -> IO ()
day_program f = do
    let bags = Map.fromList $ map parseBag $ lines f
    let ans = sum $ map (fromEnum . containsBag bags desiredBag) $ filter (/=desiredBag) $ Map.keys bags
    putStrLn $ "Num of bags that eventually contain " ++ desiredBag ++ " is " ++ show ans

-- ====================================

parseBag :: String -> (String, [(String, Int)])
parseBag s = let word  = words s
                 bag   = concat $ take 2 $ word
                 other = parseHolding $ concat $ drop 4 $ word
             in (bag, other)
    where parseHolding "" = []
          parseHolding "nootherbags." = []
          parseHolding str = let (fir, oth) = break (==',') str
                                 (n, style) = break isAlpha $ reverse $ drop 1 $ dropWhile (/='b') $ reverse fir
                             in [(style, read n)] <> parseHolding (drop 1 oth)

containsBag :: Map.Map String ([(String, Int)]) -> String -> String -> Bool
containsBag table target curr
    | target == curr = True
    | otherwise = case Map.lookup curr table of
        Nothing          -> False
        Just bags -> any id $ map (containsBag table target . fst) bags

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
