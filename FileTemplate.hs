import System.Environment (getArgs, getProgName)
import System.Directory (doesFileExist)

-- ====================================

day_program :: String -> IO ()
day_program f = do

    putStrLn "Hello World!"

    return ()

-- ====================================



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

