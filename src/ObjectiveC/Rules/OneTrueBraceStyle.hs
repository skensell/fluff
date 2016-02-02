{-# LANGUAGE QuasiQuotes #-}

module ObjectiveC.Rules.OneTrueBraceStyle 
( rule ) where

-- 3rd party
import Text.RawString.QQ -- allows long strings with [r| ... |]

-- local
import RegexUtils
import Rule

name :: String
name = "OneTrueBraceStyle"

ruleType :: RuleType
ruleType = MultiLineRegex

priority :: Priority
priority = kPriorityHigh

fileNameRestriction :: FileName -> Bool
fileNameRestriction = \_ -> True

longDescription :: String
longDescription = [r|
There is only one true way to use braces. See http://en.wikipedia.org/wiki/Indent_style#cite_note-3

Sample bad code:
if (thisIsBadCode)
{
    die();
}
else 
{
...

Corrected code:
if (thisIsBadCode) {
    die();
} else {
...
|]

violationDescription :: String
violationDescription = "Brace style is not the One-True-Brace-Style"

testRulePositives :: [String]
testRulePositives =
    [ [r|
         if (thisIsBadCode)
         {
             die();
         }
      |]
    , [r|
         if (thisIsBadCode) {
             die();
         }
         else {
             i++;
         }
      |]
    ]

testRulePositivesAutoCorrected :: [String]
testRulePositivesAutoCorrected = 
    [ [r|
         if (thisIsBadCode) {
             die();
         }
      |]
    , [r|
         if (thisIsBadCode) {
             die();
         } else {
             i++;
         }
      |]
    ]

testRuleNegatives :: [String]
testRuleNegatives = 
    [ [r|
         if (thisIsBadCode) {
             die();
         }
      |]
    , [r|
         if (thisIsBadCode) {
             die();
         } else {
             i++;
         }
      |]
    , [r|
glPushMatrix();
{
    glTranslatef(0,195,-245);
    glCallList(x);
}
glPopMatrix();
      |]
    ]

regex1String = "^(.*[^;][[:space:]]*$)[[:space:]]*\\{[[:space:]]*$"
regex2String = "\\}[[:space:]]*$[[:space:]]*else"
regex1 = makeRegex regex1String :: Regex
regex2 = makeRegex regex2String :: Regex

regex :: Regex
regex = makeRegex (regex1String ++ "|" ++ regex2String) :: Regex

autoCorrect :: Maybe (RegexMatch -> String)
autoCorrect = Just (\r ->
    let
        wholeMatch = fst $ r ! 0
        group1 = \r -> fst $ r ! 1
    in
        case (matchTest regex1 wholeMatch, matchTest regex2 wholeMatch) of
            (True,_) -> (group1 r ++ " {")
            (_,True) -> "} else"
            (_,_) -> "AUTOCORRECTERROR"
    )

rule :: Rule
rule = makeMultiLineRegexRule name regex priority longDescription fileNameRestriction violationDescription testRulePositives testRuleNegatives autoCorrect testRulePositivesAutoCorrected

