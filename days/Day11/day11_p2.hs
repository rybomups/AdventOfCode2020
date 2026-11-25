import System.Environment (getArgs, getProgName)
import System.Directory (doesFileExist)
import Data.List ((!?))

-- ====================================

day_program :: String -> IO ()
day_program f = do
    let seats = map (map readSeatStatus) $ lines f
    let ans = sum $ map (sum . map (fromEnum . (==Occupied))) $ runSimTillSteady seats
    putStrLn $ "Num occupied seats = " ++ show ans
    return ()

-- ====================================

data SeatStatus = Floor | Empty | Occupied
    deriving (Eq)

instance Show SeatStatus where
    show :: SeatStatus -> String
    show Floor    = "."
    show Empty    = "L"
    show Occupied = "#"

type SeatArrangement = [[SeatStatus]]

readSeatStatus :: Char -> SeatStatus
readSeatStatus '.' = Floor
readSeatStatus 'L' = Empty
readSeatStatus '#' = Occupied
readSeatStatus c   = error $ "Unknown char " ++ [c] ++ " while parsing SeatStatus."

runSimTillSteady :: SeatArrangement -> SeatArrangement
runSimTillSteady arr 
    | arr == nextGen = arr
    | otherwise      = runSimTillSteady nextGen
    where nextGen = nextGeneration arr

nextGeneration :: SeatArrangement -> SeatArrangement
nextGeneration chart = _nextGeneration_rec chart (0, 0)

_nextGeneration_rec :: SeatArrangement -> (Int, Int) -> SeatArrangement
_nextGeneration_rec [] _ = []
_nextGeneration_rec chart pos@(x, y)
    | y >= length chart        = chart
    | x >= length (chart !! 0) = _nextGeneration_rec chart (0, y + 1)
    | otherwise = case chart !! y !! x of
        Floor    -> goNext
        Empty    -> if (adjOccupation == 0) then replaceAt pos Occupied goNext 
                    else goNext
        Occupied -> if (adjOccupation >= 4) then replaceAt pos Empty goNext
                    else goNext
    where chartAt x' y' = (chart !? y') >>= (!? x')
          goNext = _nextGeneration_rec chart (x + 1, y)
          adjOccupation = sum $ map (fromEnum . (==(Just Occupied))) 
            [chartAt (x + dx) (y + dy) 
                | dx <- [-1..1], dy <- [-1..1]
                , (dx, dy) /= (0, 0)]
          replaceAt (x', y') status arrangement = 
            let (befRow, currRow : aftRow) = splitAt y' arrangement
                (befCol, _ : aftCol) = splitAt x' currRow
            in (befRow ++ [befCol ++ [status] ++ aftCol] ++ aftRow)



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
