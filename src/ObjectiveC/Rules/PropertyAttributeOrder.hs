{-# LANGUAGE QuasiQuotes #-}

module ObjectiveC.Rules.PropertyAttributeOrder
( rule ) where

import Data.Char
import Data.List
import Data.List.Split
import Text.RawString.QQ -- allows long strings with [r| ... |]

-- local
import RegexUtils
import Rule
import Utils
import Violation

name :: String
name = "PropertyAttributeOrder"

ruleType :: RuleType
ruleType = WholeFileMap

priority :: Priority
priority = kPriorityHigh

fileNameRestriction :: FileName -> Bool
fileNameRestriction = \_ -> True

longDescription :: String
longDescription = [r|
Property attributes if specified should occur in the following order
(nonatomic|atomic), (weak|strong|assign|copy), (readonly|readwrite)

Sample bad code:
@property (weak, nonatomic) IBOutlet UIView* foo;

Corrected code:
@property (nonatomic, weak) IBOutlet UIView* foo;
|]

violationDescription :: String
violationDescription = "Property attributes have bad order or spacing."

testRulePositives :: [String]
testRulePositives =
    [ [r|
@property (weak, nonatomic) IBOutlet UIView* foo;
      |]
    , [r|
@property (nonatomic, readonly, weak) IBOutlet UIView* foo;
      |]
    , [r|
@property (nonatomic, getter=isFoo, weak) IBOutlet UIView* foo;
      |]
    , [r|
@property (nonatomic,weak) IBOutlet UIView* foo;
      |]
    ]

testRulePositivesAutoCorrected :: [String]
testRulePositivesAutoCorrected = 
    [ [r|
@property (nonatomic, weak) IBOutlet UIView* foo;
      |]
    , [r|
@property (nonatomic, weak, readonly) IBOutlet UIView* foo;
      |]
    , [r|
@property (nonatomic, weak, getter=isFoo) IBOutlet UIView* foo;
      |]
    , [r|
@property (nonatomic, weak) IBOutlet UIView* foo;
      |]
    ]

testRuleNegatives :: [String]
testRuleNegatives = 
    [ [r|
@property (nonatomic, weak, readonly, getter=isFoo) IBOutlet UIView* foo;
@property (nonatomic, strong, readonly) IBOutlet UIView* foo;
      |]
    , [r|
@property (atomic, readwrite) IBOutlet UIView* foo;
@property (nonatomic, readonly) IBOutlet UIView* foo;
      |]
    ]

propertyAttributesRegex :: Regex
propertyAttributesRegex = makeRegex "^@property[[:space:]]*\\(([^)]+)\\)" :: Regex

violationExtractor :: File -> ViolationsOrError
violationExtractor (fileName, fileContents) = 
    let
        startLineFromMatch = (lineFromRegexMatch fileContents)
        attributesString = firstGroupOfRegexMatch
        combine = \r -> ((startLineFromMatch r), (attributesString r), (attributesSortedString r))
        allInfo = map combine $ matchAllText propertyAttributesRegex fileContents
        badGuys = filter (\(x, unsorted, sorted) -> (unsorted /= sorted)) allInfo
        excerpt = (take 60) . compactWhiteSpace
    in
        Right $ map (\(line,unsorted,sorted) -> (makeViolation violationDescription line fileName 1 (excerpt unsorted))) badGuys

attributesFromMatch :: RegexMatch -> [String]
attributesFromMatch = (splitOn ",") .  removeWhiteSpace . firstGroupOfRegexMatch

correctOrderOfAttributes :: [String]
correctOrderOfAttributes = ["atomic", "nonatomic",
                            "strong", "weak", "copy", "assign",
                            "readonly", "readwrite"]

attributesSortedString :: RegexMatch -> String
attributesSortedString r = 
    let 
        attrs = attributesFromMatch r
        unknownAttrs = filter (`notElem` correctOrderOfAttributes) attrs
        sortedKnownAttrs = filter (`elem` attrs) correctOrderOfAttributes
    in
        intercalate ", " (sortedKnownAttrs ++ unknownAttrs)


violationCorrector :: Maybe (FileContents -> FileContents)
violationCorrector = Just (\contents ->
    regexReplace propertyAttributesRegex ((\s -> "@property (" ++ s ++ ")") . attributesSortedString) contents
    )

rule :: Rule
rule = Rule { typeOfRule=ruleType
            , nameOfRule=name
            , priorityOfRule=priority
            , longDescriptionOfRule=longDescription
            , fileNameRestrictionForRule=fileNameRestriction
            , violationDescriptionOfRule=violationDescription
            , violationExtractorForRule=violationExtractor
            , violationCorrectorForRule=violationCorrector
            , testPositivesForRule=testRulePositives
            , testNegativesForRule=testRuleNegatives
            , testPositivesAutoCorrectedForRule=testRulePositivesAutoCorrected
            }

