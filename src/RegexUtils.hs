module RegexUtils
( RegexMatch
, RegexMatches
, singleLineRegexReplacer
, multiLineRegexReplacer
, identityReplacer
, lineFromRegexMatch
, regexReplace
, entireMatchedStringFromMatchText
, nthGroupOfRegexMatch
, firstGroupOfRegexMatch
, secondGroupOfRegexMatch
, module Data.Array.IArray
, module Text.Regex.TDFA
, module Text.Regex.Base
) where

import Data.Array.IArray
import Text.Regex.TDFA
import Text.Regex.Base

type RegexMatch = MatchText String -- which is Array Int (String, (MatchOffset, MatchLength))
type RegexMatches = [RegexMatch]

regexReplaceHelper :: (RegexMatch -> String) -> String -> RegexMatches -> String
regexReplaceHelper replacer source matches =
    let
        rangeOfRegexMatch match = (\(excerpt, range) -> range) $ (head . elems) match
        helper sourceRemainder matches takenSoFar newString =
            case matches of
                x:xs     -> let
                                (xoffset, xlength) = rangeOfRegexMatch x 
                                upToXLength = xoffset - takenSoFar
                                nextNewString = newString ++ (take upToXLength sourceRemainder) ++ (replacer x)
                                incr = (upToXLength + xlength)
                                nextSourceRemainder = drop incr sourceRemainder
                                nextTakenSoFar = takenSoFar + incr
                            in
                                helper nextSourceRemainder xs nextTakenSoFar nextNewString
                []       -> newString ++ sourceRemainder
    in
        helper source matches 0 ""

regexReplace :: Regex -> (RegexMatch -> String) -> String -> String
regexReplace regex replacer source = 
    regexReplaceHelper replacer source $ matchAllText regex source

singleLineRegexReplacer :: Regex -> (RegexMatch -> String) -> String -> String
singleLineRegexReplacer regex replacer source = unlines $ map (regexReplace regex replacer) $ lines source

multiLineRegexReplacer :: Regex -> (RegexMatch -> String) -> String -> String
multiLineRegexReplacer = regexReplace 

entireMatchedStringFromMatchText :: RegexMatch -> String
entireMatchedStringFromMatchText r = fst $ r ! 0 -- This returns the entire match

nthGroupOfRegexMatch :: Int -> RegexMatch -> String
nthGroupOfRegexMatch n r = fst $ r ! n

firstGroupOfRegexMatch :: RegexMatch -> String
firstGroupOfRegexMatch = nthGroupOfRegexMatch 1

secondGroupOfRegexMatch :: RegexMatch -> String
secondGroupOfRegexMatch = nthGroupOfRegexMatch 2

identityReplacer :: RegexMatch -> String
identityReplacer = entireMatchedStringFromMatchText

lineFromRegexMatch :: String -> RegexMatch  -> Int
lineFromRegexMatch fileContents =
    let
        positionOfMatch = \m -> (fst . snd $ m ! 0)
        lineFromPosition = \p -> (+1) $ length $ filter (== '\n') (take p fileContents)
    in
        lineFromPosition . positionOfMatch


