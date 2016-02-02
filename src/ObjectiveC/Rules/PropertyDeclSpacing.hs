{-# LANGUAGE QuasiQuotes #-}

module ObjectiveC.Rules.PropertyDeclSpacing
( rule ) where

-- 3rd party
import Text.RawString.QQ -- allows long strings with [r| ... |]

-- local
import RegexUtils
import Rule

name :: String
name = "PropertyDeclSpacing"

ruleType :: RuleType
ruleType = SingleLineRegex

priority :: Priority
priority = kPriorityHigh

fileNameRestriction :: FileName -> Bool
fileNameRestriction = \_ -> True

longDescription :: String
longDescription = [r|
This rule enforces exactly one space between @property and (.

Sample bad code:
@property(nonatomic) NSString* foo;

Corrected code:
@property (nonatomic) NSString* foo;
|]

violationDescription :: String
violationDescription = "Missing or bad spacing after @property keyword."

testRulePositives :: [String]
testRulePositives =
    [ "@property(nonatomic) NSString* foo"
    , "@property   (nonatomic) NSString* foo"
    ]

testRulePositivesAutoCorrected :: [String]
testRulePositivesAutoCorrected =
    [ "@property (nonatomic) NSString* foo"
    , "@property (nonatomic) NSString* foo"
    ]

testRuleNegatives :: [String]
testRuleNegatives = 
    [ "@property (nonatomic) NSString* foo"
    ]

regex :: Regex
regex = makeRegex "@property(\\(|[[:space:]]{2,}\\()" :: Regex

autoCorrect :: Maybe (RegexMatch -> String)
autoCorrect = Just (\r -> "@property (") 

rule :: Rule
rule = makeSingleLineRegexRule name regex priority longDescription fileNameRestriction violationDescription testRulePositives testRuleNegatives autoCorrect testRulePositivesAutoCorrected

