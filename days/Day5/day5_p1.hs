import System.Environment (getArgs, getProgName)
import System.Directory (doesFileExist)

-- ====================================

day_program :: String -> IO ()
day_program f = do
    let seatDescriptions = words f
    let ans = map ((\(r,c) -> r * 8 + c) . findSeat (0,127) (0,7)) seatDescriptions
    -- _ <- mapM print ans
    putStrLn $ "Highest seat ID is " ++ show (maximum ans)


-- ====================================

-- (minRow, maxRow) (minCol, maxCol) boardingPass -> (row, col)
findSeat :: (Int, Int) -> (Int, Int) -> String -> (Int, Int)
findSeat (minRow,_) (minCol,_) "" = (minRow,minCol)
findSeat rows@(minRow,maxRow) cols@(minCol,maxCol) (curr:other) = case curr of
        'F' -> findSeat (minRow, halfOf rows)     cols other  -- Lower half of rows
        'B' -> findSeat (halfOf rows + 1, maxRow) cols other  -- Upper half of rows
        'L' -> findSeat rows (minCol, halfOf cols)     other  -- Lower half of cols
        'R' -> findSeat rows (halfOf cols + 1, maxCol) other  -- Upper half of cols
        _   -> error "Unknown character in seat description"
    where halfOf (mn, mx) = (mx - mn) `div` 2 + mn

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
