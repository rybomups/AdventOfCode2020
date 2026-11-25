import System.Environment (getArgs, getProgName)
import Data.List ((!?))
import Data.Maybe (fromMaybe)

-- ====================================

day_program :: String -> IO ()
day_program f = do
    let passwords = map readPassword $ lines f
    let ans = sum $ map (fromEnum . checkPassword) passwords
    putStrLn $ "Count of good passwords = " ++ show ans

-- ====================================

data Password = Password 
    { pos1   :: Int
    , pos2   :: Int
    , letter :: Char
    , pass   :: String
    } deriving (Show)

readPassword :: String -> Password
readPassword s = Password { pos1   = read $ takeWhile (/='-') s
                          , pos2   = read $ takeWhile (/=' ') $ drop 1 $ dropWhile (/='-') s
                          , letter = last $ takeWhile (/=':') s
                          , pass   = last $ words $ s
                          }

checkPassword :: Password -> Bool
checkPassword (Password pos1 pos2 c s) = (cAt pos1) /= (cAt pos2)
    where cAt x = fromMaybe False $ fmap (==c) (s !? (x - 1))

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
