{-# LANGUAGE QuasiQuotes #-}

module ObjectiveC.Rules.MissingStrongify 
( rule ) where

-- 3rd party
import Control.Applicative
import Data.Char
import Data.List
import Text.RawString.QQ -- allows long strings with [r| ... |]
import System.FilePath

-- local
import ObjectiveC.ParsingUtils
import RegexUtils
import Rule
import Utils
import Violation

name :: String
name = "MissingStrongify"

ruleType :: RuleType
ruleType = WholeFileMap

priority :: Priority
priority = kPriorityHigh

fileNameRestriction :: FileName -> Bool
fileNameRestriction file = let e=(takeExtension file) in (e==".m" || e==".mm")

longDescription :: String
longDescription = [r|
When referring to self in a block, you might accidentally create a retain cycle.
One way this happens is by forgetting to specify @strongify in a block.
See http://stackoverflow.com/questions/21716982/explanation-of-how-weakify-and-strongify-work-in-reactivecocoa-libextobjc

Sample bad code:
@weakify(self);
_logoutCommand = [[RACCommand alloc] initWithSignalBlock:^RACSignal*(id input) {
    return [self.loginManager logOut];
}];

Corrected code:
@weakify(self);
_logoutCommand = [[RACCommand alloc] initWithSignalBlock:^RACSignal*(id input) {
    @strongify(self);
    return [self.loginManager logOut];
}];
|]

violationDescription :: String
violationDescription = "Missing strongify may lead to retain cycle."

testRulePositives :: [String]
testRulePositives =
    [ [r|
@weakify(self);
_logoutCommand = [[RACCommand alloc] initWithSignalBlock:^RACSignal*(id input) {
    return [self.loginManager logOut];
}];
      |] ]

testRulePositivesAutoCorrected :: [String]
testRulePositivesAutoCorrected = 
    [ [r|
@weakify(self);
_logoutCommand = [[RACCommand alloc] initWithSignalBlock:^RACSignal*(id input) {
    @strongify(self);
    return [self.loginManager logOut];
}];
      |] ]

testRuleNegatives :: [String]
testRuleNegatives = 
    [ [r|
@weakify(self);
_logoutCommand = [[RACCommand alloc] initWithSignalBlock:^RACSignal*(id input) {
    @strongify(self);
    return [self.loginManager logOut];
}];
      |]
    , [r|
[UIView pz_view:self.overlay fadeIn:NO duration:self.toggleAnimationDuration delay:0 options:0 completion:^(BOOL finished) {
    [self doSomething];
}];
      |]
    , [r|
if (_descriptor) {
    doSomething();
    dispatch_once(&onceToken, ^{
        [self bindPropertiesUsingDescriptor:self.descriptor];
    });
}
|]
   , [r|
[viewAttributes enumerateKeysAndObjectsUsingBlock:^(id key, id obj, BOOL *stop) {
    id attribute = [self valueForKeyPath:key];
    if([attribute isMemberOfClass:[NSLayoutConstraint class]]) {
        ((NSLayoutConstraint*)attribute).constant = [obj doubleValue];
    } else {
        [self setValue:obj forKey:key];
    }
}];
   |]]

allBlocks :: String -> [String]
allBlocks contents = []

violationExtractor :: File -> ViolationsOrError
violationExtractor (fileName, fileContents) = 
    let
        hasSelf = filter (\b -> b =~ ".*self.*" :: Bool)
        lacksStrongify = filter (\b -> not (b =~ ".*@strongify.*" :: Bool))
        correctedByUser = filter (\b -> not (b =~ ".*fluffNotRetained.*" :: Bool))
        howToCorrectTip = "TIP -- You can suppress this warning by inserting 'fluffNotRetained' somewhere inside the block."
        violationMaker = makeViolationWithTip howToCorrectTip violationDescription 1 fileName 1 
    in
        (map violationMaker) . correctedByUser. hasSelf . lacksStrongify <$> getAllRetainedObjcBlocks fileName fileContents 

violationCorrector :: Maybe (FileContents -> FileContents)
violationCorrector = Nothing

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

