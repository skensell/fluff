module Main where

-- built-in
import Control.Applicative
import Control.Monad
import Data.List.Split
import Data.Map as Map(keys,lookup)
import Data.Maybe
import Data.Time.Clock
import System.Directory (getHomeDirectory)
import System.Environment
import System.Exit (exitFailure)
import System.FilePath (joinPath, splitPath)
import System.FilePath.Find
import System.FilePath.Posix
import System.IO

-- 3rd party
import Control.DeepSeq
import Options

-- local
import Rule
import RuleSets (ruleSets)
import Utils
import Violation

data MainOptions = MainOptions
    { optListRules :: Bool
    , optTest :: Bool
    , optSearchDir :: String
    , optExcludeDirs :: String
    , optFileExtensions :: String
    , optAutoCorrect :: Bool
    , optRuleSet :: String
    , optExcludeRules :: String
    , optRules :: String
    , optMinPriority :: Int
    }

instance Options MainOptions where
    defineOptions = pure MainOptions
        <*> simpleOption "list-rules" False
            "List all rules which would be enforced and quit."
        <*> simpleOption "test" False
            "Run the test suite to verify the soundness of rules and quit."
        <*> simpleOption "search-dir" ""
            (  "Specify a directory where all .h, .m, .mm files" 
            ++ " will be recursively checked in addition to the files provided as arguments."
            ++ " Note that you can optionally specify different extensions with --file-extensions.")
        <*> simpleOption "exclude-dirs" ""
            (  "Specify a comma-separated list of directories to exclude from consideration." 
            ++ " Applicable only when --search-dir has been given.")
        <*> simpleOption "file-extensions" "h,m,mm"
           (  "A comma-separated list of file extensions which will be used to filter files for consideration."
           ++ " Only valid when --search-dir has been specified.") 
        <*> simpleOption "auto-correct" False
            "Try to auto-correct rules for which the auto-correct functionality is defined."
        <*> simpleOption "rule-set" "pn-objc"
            "Specify the collection of rules to be applied by the collection's name. Try --list-rules to see all possible collection names."
        <*> simpleOption "exclude-rules" ""
            (  "Specify a comma-separated list of rules to exclude from consideration. The end set of rules to be applied is "
            ++ "the (--rule-set) minus (--exclude-rules), unless (--rules) has been specified which trumps everything.")
        <*> simpleOption "rules" ""
            (  "Specify a comma-separated list of rules to apply. No other rules will be used. "
            ++ "The rules must exist in the collection specified with (--rule-set).")
        <*> simpleOption "min-priority" 25
            (  "Specify an integer k between 1 and 100. Only rules with priority greater than or equal to k will be considered. "
            ++ "0=disabled, 25=low, 50=medium, 75=high, 100=highest. Try --list-rules to see the priorities of rules.")

main :: IO ()
main = runCommand $ \opts args -> do 
    let ruleSet = optRuleSet opts
    rules <- parseRules opts
    searchDirFileNames <- getFilenamesInSearchDir (optSearchDir opts) (optFileExtensions opts) (optExcludeDirs opts)
    let fileNames = args ++ searchDirFileNames
    case opts of
        MainOptions {optListRules=True}    -> listRules ruleSet rules 
        MainOptions {optTest=True}         -> runTestsOnRules rules
        MainOptions {optAutoCorrect=False} -> searchFilesForViolationsOfRules fileNames rules
        MainOptions {optAutoCorrect=True}  -> autoCorrectViolationsInFiles fileNames rules

parseRules :: MainOptions -> IO ([Rule])
parseRules MainOptions{optRuleSet=ruleSet,optExcludeRules=e,optRules=o,optMinPriority=m} =
    let 
        prioFilter = filter ((>= m) . priorityOfRule)
    in
        case (Map.lookup ruleSet ruleSets, e, o) of
            (Nothing, _, _)             -> do
                printError $ "No rule set found with name " ++ ruleSet
                return []
            (Just rules, [], [])        -> return $ prioFilter rules
            (Just rules, _, s@(x:xs))   ->
                let
                    onlyThese = splitOn "," s
                    isIncluded Rule{nameOfRule=n} = any (== n) onlyThese 
                in
                    return $ prioFilter $ filter (isIncluded) rules 
            (Just rules, s@(x:xs), [])  ->
                let
                    excludedRules = splitOn "," s
                    isExcluded Rule{nameOfRule=n} = any (== n) excludedRules 
                in
                    return $ prioFilter $ filter (not . isExcluded) rules 

listRules :: String -> [Rule] -> IO ()
listRules ruleSet rules = do
    putStrLn $ "Active rule set: " ++ ruleSet
    putStrLn $ "\nAll possible rule sets:\n"
    mapM_ (putStrLn . indent) (Map.keys ruleSets)
    putStrLn "\nActive rules and their priorities:\n"
    mapM_ (putStrLn . indent . (\r -> (nameOfRule r) ++ " " ++ (show (priorityOfRule r)))) rules
    putStrLn "\nActive rule summaries:"
    forM_ rules (\rule -> do
        let separator = join $ take 60 $ repeat "="
        putStrLn $ "\n" ++ separator ++ "\n"
        putStrLn $ "    Rule: " ++ (nameOfRule rule)
        putStrLn $ "   Brief: " ++ (violationDescriptionOfRule rule)
        putStrLn $ "Priority: " ++ (show (priorityOfRule rule))
        putStrLn $ " Summary: " ++ (unlines $ map (\line -> ("         " ++ line)) $ lines $ longDescriptionOfRule rule)
        )

getFilenamesInSearchDir :: String -> String -> String -> IO [String]
getFilenamesInSearchDir [] fileExtensionsString excludeDirsString = return []
getFilenamesInSearchDir searchDir fileExtensionsString excludeDirsString = do
    let allowableFileExtensions = map (\x -> "." ++ x) (splitOn "," fileExtensionsString)
    let excludedDirs = if (length excludeDirsString) == 0 then [] else (splitOn "," excludeDirsString)
    let isNoneOf x aList = not $ any ((takeFileName x) ==) aList
    let anyEqualExtensions ext xs = any (ext ==) xs
    homeDirectory <- getHomeDirectory
    searchDirFileNames <- find 
             (depth <? 20)
             ((liftOp anyEqualExtensions) extension allowableFileExtensions
             &&? (liftOp isNoneOf) directory excludedDirs)
             (expandTilde homeDirectory searchDir)
    return searchDirFileNames

executionContainer :: [String] -> [Rule] -> IO Bool -> IO ()
executionContainer fileNames rules operation = do
    startTime <- getCurrentTime
    case (fileNames, rules) of
        ([], _)            -> die "You must supply at least one file as an argument or with the --search-dir option"
        (_, [])            -> die "You must specify at least one rule"
        (fileNames, rules) -> do
            shouldExitSuccessfully <- operation
            endTime <- getCurrentTime
            printInfo $ "Total execution time: " ++ (show (diffUTCTime endTime startTime))
            if shouldExitSuccessfully then return () else exitFailure
                    
searchFilesForViolationsOfRules :: [String] -> [Rule] -> IO ()
searchFilesForViolationsOfRules fileNames rules =
    let
        operation = do
            printInfo $ "Searching for code violations in " ++ show (length fileNames) ++ " files..."
            let allViolationsIOActions = [searchFileForViolationsOfRules f rules | f <- fileNames]
            listOfListOfViolations <- sequence allViolationsIOActions
            let allViolations = join listOfListOfViolations
            reportViolations fileNames allViolations 
            case allViolations of
                (_:_) -> return False
                []    -> return True
    in
        executionContainer fileNames rules operation

autoCorrectViolationsInFiles :: [String] -> [Rule] -> IO ()
autoCorrectViolationsInFiles fileNames rules =
    let
        operation = do
            printInfo $ "Autocorrecting code violations in " ++ show (length fileNames) ++ " files..."
            sequence_ [autoCorrectViolationsOfRulesInFile f rules | f <- fileNames]
            return True
    in
        executionContainer fileNames rules operation
    
autoCorrectViolationsOfRulesInFile :: String -> [Rule] -> IO ()
autoCorrectViolationsOfRulesInFile fileName rules = 
    let
        rulesToApply = filter (\r -> (fileNameRestrictionForRule r) (takeFileName fileName)) rules
        autoCorrectors = catMaybes $ map violationCorrectorForRule rules
        compose = foldr (.) id
    in 
        case autoCorrectors of
            [] -> return ()
            _  -> do
                oldFileContents <- deepReadFile fileName
                let newFileContents = (compose autoCorrectors) oldFileContents
                writeFile fileName newFileContents

searchFileForViolationsOfRules :: String -> [Rule] -> IO [Violation]
searchFileForViolationsOfRules file rules = do
    let baseName = takeFileName file
    let rulesToApply = filter (\r -> fileNameRestrictionForRule r baseName) rules
    fileContents <- deepReadFile file
    join <$> forM rulesToApply (\Rule{violationExtractorForRule=e,nameOfRule=n} -> 
        case e (file, fileContents) of
            Right xs  ->  return xs
            Left  err -> (do
                printError $ "An error occurred while searching for violations of rule '" ++ n ++ "' in file '"
                            ++ file ++ "'.\nThis is what we know:\n" ++ (indent err)
                return [])
        )

deepReadFile :: String -> IO String
deepReadFile file = (withFile file ReadMode (\handle -> do
        contents <- hGetContents handle
        contents `deepseq` return () -- this is some bull shit needed for forcing eval of lazy IO
        return contents))
 
reportViolations :: [String] -> [Violation] -> IO ()
reportViolations fileNames violations = do
    mapM_ (printInfo . show) violations
    putStrLn ""
    printInfo $ "      Files searched: " ++ show (length fileNames)
    printInfo $ "    Violations found: " ++ show (length violations)

die :: String -> IO ()
die msg = do
    printError msg
    exitFailure

printError :: String -> IO ()
printError x  = do
    progName <- getProgName
    hPutStrLn stderr $ (progName ++ ": ") ++ x

printInfo :: String -> IO ()
printInfo x  =  putStrLn x

expandTilde :: String -> String -> String
expandTilde homeDir path = case (splitPath path) of
    "~/" : t -> joinPath $ [homeDir] ++ t
    _        -> path


-- test runner

runTestsOnRules :: [Rule] -> IO ()
runTestsOnRules rules = 
    let
        testFailures = join $ map testRule rules
    in
        case testFailures of
            [] -> printInfo "All tests pass."
            _  -> do 
                mapM_ printError testFailures
                exitFailure

