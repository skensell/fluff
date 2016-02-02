-- The Violation module provides the Violation data type which is meant
-- for pretty reporting of rule violations.

module Violation 
( makeViolation
, makeViolationFromRegexMatch
, makeViolationWithTip
, Violation)
where

import RegexUtils
import Utils

data Violation = Violation { violationSummary :: String
                           , codeExcerpt :: String
                           , lineNumber :: Int
                           , columnNumber :: Int
                           , fileName :: String
                           , howToCorrectTip :: String
                           } 

instance Show Violation where
    show v =  (fileName v) ++
              ":" ++ show (lineNumber v) ++ 
              ":" ++ show (columnNumber v) ++
              " " ++ (violationSummary v) ++
              ": '" ++ (codeExcerpt v) ++ "'" ++
              (let s=(howToCorrectTip v) in (if (length s > 0) then ("\n" ++ (indent s)) else []))

              

makeViolationFromRegexMatch :: String -> Int -> String -> RegexMatch -> Violation
makeViolationFromRegexMatch summary lineNumber fileName matchText =
    let
        unwrap = (\(excerpt,(column,_)) -> 
            Violation { violationSummary=summary
                      , lineNumber=lineNumber
                      , codeExcerpt=(take 20 $ compactWhiteSpace excerpt)
                      , columnNumber=(if column > 80 then 0 else column)
                      , fileName=fileName
                      , howToCorrectTip=[]
                      }) . head . elems
    in
        unwrap matchText

makeViolation :: String -> Int -> String -> Int -> String -> Violation
makeViolation = makeViolationWithTip []

makeViolationWithTip :: String -> String -> Int -> String -> Int -> String -> Violation
makeViolationWithTip tip summary lineNumber fileName column excerpt =
    Violation { violationSummary=summary
              , codeExcerpt=excerpt
              , lineNumber=lineNumber
              , columnNumber=column
              , fileName=fileName
              , howToCorrectTip=tip
              } 


