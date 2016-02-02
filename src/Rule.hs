module Rule
( makeSingleLineRegexRule
, makeMultiLineRegexRule
, testRule
, File
, FileName
, FileContents
, Priority, kPriorityDisabled, kPriorityLow, kPriorityMedium, kPriorityHigh, kPriorityHighest
, RuleType(..)
, Rule(..)
, ViolationsOrError
) where

import Control.Monad
import RegexUtils
import Utils
import Violation

data RuleType = SingleLineRegex | MultiLineRegex | WholeFileMap

type FileName = String
type FileContents = String
type File = (FileName, FileContents)
type ErrorMessage = String
type ViolationsOrError = Either ErrorMessage [Violation]

type Priority = Int
kPriorityDisabled = 0 :: Priority
kPriorityLow = 25 :: Priority
kPriorityMedium = 50 :: Priority
kPriorityHigh = 75 :: Priority
kPriorityHighest = 100 :: Priority

data Rule = Rule { typeOfRule :: RuleType
                 , nameOfRule :: String
                 , priorityOfRule :: Priority
                 , longDescriptionOfRule :: String
                 , fileNameRestrictionForRule :: FileName -> Bool -- True iff the file should be considered
                 , violationDescriptionOfRule :: String
                 , violationExtractorForRule :: File -> ViolationsOrError
                 , violationCorrectorForRule :: Maybe (FileContents -> FileContents)
                 , testPositivesForRule :: [String]
                 , testNegativesForRule :: [String]
                 , testPositivesAutoCorrectedForRule :: [String]
                 }

type RuleMaker = String -> Regex -> Priority -> String -> (FileName -> Bool) -> String -> [String] -> [String] -> Maybe (RegexMatch -> String) -> [String] -> Rule

makeSingleLineRegexRule :: RuleMaker
makeSingleLineRegexRule = makeRegexRule SingleLineRegex

makeMultiLineRegexRule :: RuleMaker
makeMultiLineRegexRule = makeRegexRule MultiLineRegex

makeRegexRule :: RuleType -> RuleMaker
makeRegexRule ruleType name regex priority longDescription fileNameRestriction violationDescription testPositives testNegatives autoCorrecter testRulePositivesAutoCorrected =
    let
        violationExtractor =
            case ruleType of
                SingleLineRegex -> getViolationsForSingleLineRegex regex violationDescription
                MultiLineRegex  -> getViolationsForMultiLineRegex regex violationDescription
                _               -> \_ -> Right []
        violationCorrector =
            case (ruleType, autoCorrecter) of
                (_, Nothing)              -> Nothing
                (SingleLineRegex, Just f) -> Just (singleLineRegexReplacer regex f)
                (MultiLineRegex, Just f)  -> Just (multiLineRegexReplacer regex f)
                (_, Just f)               -> Nothing
    in
        Rule { typeOfRule=ruleType
             , nameOfRule=name
             , priorityOfRule=priority
             , longDescriptionOfRule=longDescription
             , fileNameRestrictionForRule=fileNameRestriction
             , violationDescriptionOfRule=violationDescription
             , violationExtractorForRule=violationExtractor
             , violationCorrectorForRule=violationCorrector
             , testPositivesForRule=testPositives
             , testNegativesForRule=testNegatives
             , testPositivesAutoCorrectedForRule=testRulePositivesAutoCorrected
             }

getViolationsForSingleLineRegex :: Regex -> String -> File -> ViolationsOrError
getViolationsForSingleLineRegex regex summary (fileName, fileContents)  =  
    let
        getViolationsOnLine = \(line, text) -> 
            (map (makeViolationFromRegexMatch summary line fileName) (matchAllText regex text))
    in 
        Right $ join $ map getViolationsOnLine (zip [1..] (lines fileContents))

getViolationsForMultiLineRegex :: Regex -> String -> File -> ViolationsOrError
getViolationsForMultiLineRegex regex summary (fileName, fileContents)  =  
    let
        violationFromMatch = \match -> makeViolationFromRegexMatch summary (lineFromRegexMatch fileContents match) fileName match
        allMatchesInFile = matchAllText regex fileContents
    in 
        Right $ map violationFromMatch allMatchesInFile

-- returns empty list on success
testRule :: Rule -> [String]
testRule Rule { nameOfRule=ruleName
              , violationExtractorForRule=v
              , violationCorrectorForRule=vc
              , testPositivesForRule=ps
              , testNegativesForRule=ns
              , testPositivesAutoCorrectedForRule=pc
              } = 
    let
        positiveTester codeSample = 
            case v ("", codeSample) of
                Right [] -> ["TEST FAILED: Failed to find violation of " ++ ruleName ++  " in code:" ++ codeSample]
                Left err -> ["TEST FAILED: Violation extraction erred with message "
                            ++ err ++ "\nwhile extracting violations of rule " ++ ruleName ++ " in code:" ++ codeSample]
                _  -> []
        negativeTester codeSample = 
            case v ("", codeSample) of
                Right []  -> []
                Left err  -> ["TEST FAILED: Violation extraction erred with message "
                            ++ err ++ "\nwhile extracting violations of rule " ++ ruleName ++ " in code:" ++ codeSample]
                _   -> ["TEST FAILED: Found unexpected violation of " ++ ruleName ++  " in code: " ++ codeSample]
        autoCorrectTester (codeSample, expectedCorrected) =
            case vc of
                Nothing -> []
                Just correcter ->
                    let
                        actualCorrected = stripTrailingNewLine $ correcter codeSample
                    in
                        if (actualCorrected == expectedCorrected)
                            then []
                            else [  "TEST FAILED: Autocorrect for " ++ ruleName ++  " did not produce the expected corrected code.\n"
                                 ++ "    Expected:\n" ++ (indentWithLevel 1 ("'" ++ expectedCorrected ++ "'")) ++ "\n"
                                 ++ "    Actual:\n" ++ (indentWithLevel 1 ("'" ++ actualCorrected ++ "'"))]

    in
        join $ (map positiveTester ps) ++ (map negativeTester ns) ++ (map autoCorrectTester (zip ps pc))


