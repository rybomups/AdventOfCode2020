import System.Environment (getArgs, getProgName)
import Data.List (groupBy)

-- ====================================

day_program :: String -> IO ()
day_program f = do
    let paragraphs = map unlines 
                   $ filter (/= [""]) 
                   $ groupBy (\a b -> not (null a) && not (null b)) 
                   $ lines f
    let passports = map (parsePassport . words) paragraphs
    let ans = sum $ map (fromEnum . containsAll . (CidTag:) . map passportToTag) passports
    putStrLn $ "Num valid passports = " ++ show ans


-- ====================================

data PassportField = Byr Int    | Iyr Int | Eyr Int
                   | Hgt String
                   | Hcl String | Ecl String
                   | Pid Int    | Cid Int
                   deriving (Show)

data PassportFieldTag = ByrTag | IyrTag | EyrTag | HgtTag 
                      | HclTag | EclTag | PidTag | CidTag
                      deriving (Show, Eq, Ord, Enum, Bounded)

parsePassport :: [String] -> [PassportField]
parsePassport [] = []
parsePassport (s:sx) = 
    let (key, val) = break (==':') s
    in (parseField key val : parsePassport sx)
        
    where parseField field val = case field of
            "byr" -> Byr (read $ drop 1 val)
            "iyr" -> Iyr (read $ drop 1 val)
            "eyr" -> Eyr (read $ drop 1 val)
            "hgt" -> Hgt (drop 1 val)
            "hcl" -> Hcl (drop 1 val)
            "ecl" -> Ecl (drop 1 val)
            "pid" -> Pid (read $ drop 1 val)
            "cid" -> Cid (read $ drop 1 val)
            _     -> error "Unknown Passport Field"

passportToTag :: PassportField -> PassportFieldTag
passportToTag (Byr _) = ByrTag
passportToTag (Iyr _) = IyrTag
passportToTag (Eyr _) = EyrTag
passportToTag (Hgt _) = HgtTag
passportToTag (Hcl _) = HclTag
passportToTag (Ecl _) = EclTag
passportToTag (Pid _) = PidTag
passportToTag (Cid _) = CidTag


containsAll :: (Eq a, Enum a, Bounded a) => [a] -> Bool
containsAll xs = all (`elem` xs) [minBound..maxBound]

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
