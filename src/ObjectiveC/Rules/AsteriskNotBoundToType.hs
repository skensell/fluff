{-# LANGUAGE QuasiQuotes #-}

module ObjectiveC.Rules.AsteriskNotBoundToType 
( rule ) where

-- 3rd party
import Text.RawString.QQ -- allows long strings with [r| ... |]

-- local
import RegexUtils
import Rule

name :: String
name = "AsteriskNotBoundToType"

ruleType :: RuleType
ruleType = SingleLineRegex

priority :: Priority
priority = kPriorityHigh

fileNameRestriction :: FileName -> Bool
fileNameRestriction = \_ -> True

longDescription :: String
longDescription = [r|
This rule enforces the asterisk be bound to the type in variable
and property declarations, casts, and parameter declarations.

Sample bad code:
NSString *myVar = @"blue";

Corrected code:
NSString* myVar = @"blue";
|]

violationDescription :: String
violationDescription = "Asterisk not bound to type"

testRulePositives :: [String]
testRulePositives =
    [ "           NSString *blah "
    , "@property (nonatomic) NSObject *a"
    , "          NSObject *a = nil"
    , "- (instancetype)initWithFoo:(SKFoo*)aFoo previewManager:(SKIOSPreviewManager *)aPreviewManager"
    , "- (instancetype)initWithFoo:(SKFoo *)aFoo previewManager:(SKIOSPreviewManager *)aPreviewManager"
    , "a = (NSString *)b;"
    ]

testRulePositivesAutoCorrected :: [String]
testRulePositivesAutoCorrected =
    [ "           NSString* blah "
    , "@property (nonatomic) NSObject* a"
    , "          NSObject* a = nil"
    , "- (instancetype)initWithFoo:(SKFoo*)aFoo previewManager:(SKIOSPreviewManager*)aPreviewManager"
    , "- (instancetype)initWithFoo:(SKFoo*)aFoo previewManager:(SKIOSPreviewManager*)aPreviewManager"
    , "a = (NSString*)b;"
    ]

testRuleNegatives :: [String]
testRuleNegatives = 
    [ "       *error = foo;"
    , " foo = *error;"
    , " foo = **error"
    , " j = *a + *b "
    , "a * b"
    , "20 *2"
    , "someVar * anotherVar"
    ]

regex :: Regex
regex = makeRegex "([A-Z][[:alnum:]]+) \\*([^ ])" :: Regex

autoCorrect :: Maybe (RegexMatch -> String)
autoCorrect = Just (\r ->
    let
        group1 = fst $ r ! 1
        group2 = fst $ r ! 2
    in
        case group2 of
            ")" -> group1 ++ "*" ++ group2
            "(" -> group1 ++ "*" ++ group2
            _   -> group1 ++ "* " ++ group2
    )

rule :: Rule
rule = makeSingleLineRegexRule name regex priority longDescription fileNameRestriction violationDescription testRulePositives testRuleNegatives autoCorrect testRulePositivesAutoCorrected

