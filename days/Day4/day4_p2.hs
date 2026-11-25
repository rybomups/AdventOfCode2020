import System.Environment (getArgs, getProgName)
import Data.List (groupBy)
import Data.Char (isDigit, isHexDigit)

-- ====================================

day_program :: String -> IO ()
day_program f = do
    let paragraphs = map unlines 
                   $ filter (/= [""]) 
                   $ groupBy (\a b -> not (null a) && not (null b)) 
                   $ lines f
    let passports = map (parsePassport . words) paragraphs
    let ans = sum $ map (fromEnum . (\x -> passportHasAllFields x && verifyPassport x)) passports
    -- _ <- mapM print passports
    -- _ <- mapM (print . verifyPassport) passports
    -- _ <- mapM (print . passportHasAllFields) passports
    putStrLn $ "Num valid passports = " ++ show ans


-- ====================================

data PassportField = Byr Int    | Iyr Int | Eyr Int
                   | Hgt String
                   | Hcl String | Ecl String
                   | Pid String | Cid Int
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
            "pid" -> Pid (drop 1 val)
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

data EyeColor = Amb | Blu | Brn | Gry | Grn | Hzl | Oth | None
                deriving (Eq)

parseEyeColor :: String -> EyeColor
parseEyeColor str = case str of
    "amb" -> Amb
    "blu" -> Blu
    "brn" -> Brn
    "gry" -> Gry
    "grn" -> Grn
    "hzl" -> Hzl
    "oth" -> Oth
    _     -> None

verifyPassportField :: PassportField -> Bool
verifyPassportField (Byr yr) = yr >= 1920 && yr <= 2002
verifyPassportField (Iyr yr) = yr >= 2010 && yr <= 2020
verifyPassportField (Eyr yr) = yr >= 2020 && yr <= 2030
verifyPassportField (Hgt hg) = let (num, unit) = break (not . isDigit) hg
                                   num' = read num :: Int
                                in case unit of
                                    "cm" -> num' >= 150 && num' <= 193
                                    "in" -> num' >= 59 && num' <= 76
                                    _    -> False
verifyPassportField (Hcl (p:cl)) = p == '#' && all isHexDigit cl
verifyPassportField (Ecl cl) = parseEyeColor cl /= None
verifyPassportField (Pid pid) = length pid == 9 && (and $ map isDigit pid)
verifyPassportField (Cid _)  = True
verifyPassportField _        = False


verifyPassport :: [PassportField] -> Bool
verifyPassport = and . map verifyPassportField

passportHasAllFields :: [PassportField] -> Bool
passportHasAllFields = containsAll . (CidTag:) . map passportToTag


containsAll :: (Eq a, Enum a, Bounded a) => [a] -> Bool
containsAll xs = all (`elem` xs) [minBound..maxBound]

-- ====================================

main :: IO ()
main = do
    args <- getArgs; progName <- getProgName
    case args of
        [fileName] -> do
            fileContents <- readFile fileName
            putStrLn $ "Opened file: " ++ fileName
            putStrLn ""; day_program fileContents; putStrLn ""
        _          -> putStrLn $ "Usage: " ++ progName ++ " <filename>"
