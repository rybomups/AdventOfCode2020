import System.Environment (getArgs, getProgName)

-- ====================================

paths_to_bottom :: [(Int, Int)]
paths_to_bottom = [ (1, 1), (3, 1), (5, 1), (7, 1), (1, 2) ]

day_program :: String -> IO ()
day_program f = do
    let slope = parseMountain f
    let ans = product $ map (\dir -> countTreesHit (0,0) dir slope) paths_to_bottom
    putStrLn $ "The product of the trees you would hit is " ++ show ans

-- ====================================

data Slope = Clear | Tree deriving (Show, Enum)
type MountainSlope = [[Slope]]

parseMountain :: String -> MountainSlope
parseMountain s = map (map (parseSlope)) $ lines s
    where parseSlope '.' = Clear
          parseSlope  _  = Tree

-- Position, Direction, Slope
countTreesHit :: (Int, Int) -> (Int, Int) -> MountainSlope -> Int
countTreesHit _ _ ([] : _)            = 0
countTreesHit (x,y) dir@(dx,dy) slope 
    | y >= yLen = 0
    | otherwise = fromEnum (slope !! y !! (x `mod` xLen)) + countTreesHit (x+dx,y+dy) dir slope
    where xLen = length $ slope !! 0
          yLen = length slope

-- ====================================

main :: IO ()
main = do
    args <- getArgs; progName <- getProgName
    case args of
        [fileName] -> do
            fileContents <- readFile fileName
            putStrLn ""
            day_program fileContents
            putStrLn ""
        _          -> putStrLn $ "Usage: " ++ progName ++ " <filename>"
