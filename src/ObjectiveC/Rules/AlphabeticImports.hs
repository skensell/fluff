{-# LANGUAGE QuasiQuotes #-}

module ObjectiveC.Rules.AlphabeticImports
( rule ) where

import Data.Char
import Data.List
import Text.RawString.QQ -- allows long strings with [r| ... |]

-- local
import RegexUtils
import Rule
import Utils
import Violation

name :: String
name = "AlphabeticImports"

ruleType :: RuleType
ruleType = WholeFileMap

priority :: Priority
priority = kPriorityHigh

fileNameRestriction :: FileName -> Bool
fileNameRestriction = \_ -> True

longDescription :: String
longDescription = [r|
Do you know your ABC's?
This rule takes any group of imports which is separated by <=1 newline and puts them in ABC order.

Sample bad code:
#import "SKBobsClass.h"
#import "SKAlicesClass.h"

Corrected code:
#import "SKAlicesClass.h"
#import "SKBobsClass.h"
|]

violationDescription :: String
violationDescription = "Imports are not in alphabetical order."

testRulePositives :: [String]
testRulePositives =
    [ [r|

#import "SKAClass.h"
#import "SKCClass.h"
#import "SKBClass.h"
      |]
    , [r|

#import "SKAClass.h"
#import "SKBClass.h"
#import "SKCClass.h"

#import "SKCClass.h"
#import "SKAClass.h"
      |]
    , [r|
// Unicode test: By Máté

#import "SKFooViewController.h"
#import "SKAFooViewController.h"
// common-cocoapods-ios
      |]
    ]

testRulePositivesAutoCorrected :: [String]
testRulePositivesAutoCorrected = 
    [ [r|

#import "SKAClass.h"
#import "SKBClass.h"
#import "SKCClass.h"
      |]
    , [r|

#import "SKAClass.h"
#import "SKBClass.h"
#import "SKCClass.h"

#import "SKAClass.h"
#import "SKCClass.h"
      |]
    , [r|
// Unicode test: By Máté

#import "SKAFooViewController.h"
#import "SKFooViewController.h"
// common-cocoapods-ios
      |]
    ]

testRuleNegatives :: [String]
testRuleNegatives = 
    [ [r|

#import "SKAClass.h"
#import "SKBClass.h"
#import "SKCClass.h"
      |]
    , [r|

#import "SKAClass.h"
#import "SKBClass.h"
#import "SKCClass.h"

#import "SKDClass.h"
      |]
    , [r|

#import <GLKit/GLKit.h>

#import "SKViewController.h"

@protocol SKFooViewerModelProtocol;
      |]
    ]

-- Note: newline is $[[:space:]]^
blockOfImportsRegex :: Regex
blockOfImportsRegex = makeRegex "$[[:space:]]^(#import [^[:space:]]+$[[:space:]]^)+" :: Regex

importBlockFromMatch :: RegexMatch -> String
importBlockFromMatch = stripNewLines . entireMatchedStringFromMatchText 

sortedBlockOfImports :: RegexMatch -> String
sortedBlockOfImports = 
    let
        pairUp = \x -> (zip (lines x) ((map (map toLower)) (lines x)))
        sortPairs = sortBy (\a b -> (compare (snd a) (snd b))) -- sortOn snd
        sortedBlockFromPairs = stripNewLines . unlines . fst . unzip 
    in
        sortedBlockFromPairs . sortPairs . pairUp . importBlockFromMatch

violationExtractor :: File -> ViolationsOrError
violationExtractor (fileName, fileContents) = 
    let
        startLineFromMatch = (lineFromRegexMatch fileContents)
        combine = \x -> ((startLineFromMatch x), (importBlockFromMatch x), (sortedBlockOfImports x))
        allInfo = map combine $ matchAllText blockOfImportsRegex fileContents
        badBlocks = filter (\(x, unsorted, sorted) -> (unsorted /= sorted)) allInfo
        excerpt = \block -> (head . lines) block
    in
        Right $ map (\(line,unsorted,sorted) -> (makeViolation violationDescription line fileName 1 (excerpt unsorted))) badBlocks

violationCorrector :: Maybe (FileContents -> FileContents)
violationCorrector = Just (\contents ->
    regexReplace blockOfImportsRegex ((\s -> "\n" ++ s ++ "\n") . sortedBlockOfImports) contents
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

