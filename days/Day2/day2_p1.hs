import System.Environment (getArgs, getProgName)

-- ====================================

day_program :: String -> IO ()
day_program f = do
    let passwords = map readPassword $ lines f
    let ans = sum $ map (fromEnum . checkPassword) passwords
    putStrLn $ "Count of good passwords = " ++ show ans

-- ====================================

data Password = Password 
    { minCount :: Int
    , maxCount :: Int
    , letter   :: Char
    , pass     :: String
    } deriving (Show)

readPassword :: String -> Password
readPassword s = Password { minCount = read $ takeWhile (/='-') s
                          , maxCount = read $ takeWhile (/=' ') $ drop 1 $ dropWhile (/='-') s
                          , letter   = last $ takeWhile (/=':') s
                          , pass     = last $ words $ s
                          }

checkPassword :: Password -> Bool
checkPassword (Password minC maxC c s) = count >= minC && count <= maxC
    where count = sum $ map (fromEnum . (==c)) s

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
