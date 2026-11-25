import System.Environment (getArgs, getProgName)
import System.Directory (doesFileExist)
import Data.Char (isSpace)

-- ====================================

day_program :: String -> IO ()
day_program f = do
    let program = parseCode $ lines f
    let ans = filter (\(ConsoleState _ progCount) -> progCount >= length program)
            $ map (runCode [0] (ConsoleState 0 0)) 
            $ getPossibleCodeFixes program 0
    -- mapM print ans
    -- print ans
    putStrLn $ "Value of accumulator at program completion = " ++ show (acc_val $ ans !! 0)
    return ()

-- ====================================

data Opcode       = Acc | Jmp | Nop
                  deriving (Show, Eq)
data Inst         = Inst Opcode Int
                  deriving (Show)
type ConsoleCode  = [Inst]

data ConsoleState = ConsoleState { acc_val :: Int, pc :: Int }
                  deriving (Show)

parseCode :: [String] -> ConsoleCode
parseCode []          = []
parseCode (inst:rest) = parseInstruction inst : parseCode rest

parseInstruction :: [Char] -> Inst
parseInstruction str =  let (op, val) = break isSpace str
                        in case op of 
                            "acc" -> Inst Acc (readSignedInt $ drop 1 val)
                            "jmp" -> Inst Jmp (readSignedInt $ drop 1 val)
                            "nop" -> Inst Nop (readSignedInt $ drop 1 val)
                            _     -> error $ "Unknown instruction " ++ str

readSignedInt :: String -> Int
readSignedInt ('+':xs) = read xs
readSignedInt xs       = read xs

runInst :: ConsoleState -> Inst -> ConsoleState
runInst (ConsoleState acc pc) (Inst Acc val) = ConsoleState { acc_val = (acc + val), pc = (pc + 1) }
runInst (ConsoleState acc pc) (Inst Jmp val) = ConsoleState { acc_val = acc,         pc = (pc + val) }
runInst (ConsoleState acc pc) (Inst Nop _)   = ConsoleState { acc_val = acc,         pc = (pc + 1) }

runCode :: [Int] -> ConsoleState -> ConsoleCode -> ConsoleState
runCode seen state@(ConsoleState _ progCount) instList
    | progCount >= length instList = state
    | otherwise   = let state' = runInst state $ instList !! progCount
                    in if pc state' `elem` seen then
                        state'
                    else
                        runCode (progCount:seen) state' instList

getPossibleCodeFixes :: ConsoleCode -> Int -> [ConsoleCode]
getPossibleCodeFixes code i 
    | i >= length code = []
    | opcode == Jmp    = proceed $ replaceAt i (Inst Nop val) code
    | opcode == Nop    = proceed $ replaceAt i (Inst Jmp val) code
    | otherwise        = getPossibleCodeFixes code (i + 1)
    where (Inst opcode val)    = code !! i
          replaceAt idx new xs = let (before, _ : after) = splitAt idx xs 
                                 in before ++ (new : after)
          proceed gen          = gen : getPossibleCodeFixes code (i + 1)

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
