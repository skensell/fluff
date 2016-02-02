module Utils where

import RegexUtils

indentWithLevel :: Int -> String -> String
indentWithLevel i = stripTrailingNewLine . unlines . (map ((++) (take (i*4) $ repeat ' '))) . lines

indent :: String -> String
indent = indentWithLevel 1

stripTrailingNewLine :: String -> String
stripTrailingNewLine [] = []
stripTrailingNewLine (s@(x:xs)) = 
    case splitAt (length xs) s of
        (a, "\n") -> a
        _ -> s

stripTrailingNewLines :: String -> String
stripTrailingNewLines [] = []
stripTrailingNewLines (s@(x:xs)) = 
    case splitAt (length xs) s of
        (a, "\n") -> stripTrailingNewLines a
        _ -> s

stripLeadingNewLines :: String -> String
stripLeadingNewLines s =
    case s of
        []      -> []
        '\n':xs -> stripLeadingNewLines xs
        _:xs    -> s

stripNewLines :: String -> String 
stripNewLines = stripTrailingNewLines . stripLeadingNewLines

whiteSpaceRegex :: Regex
whiteSpaceRegex = makeRegex "[[:space:]]+" :: Regex

compactWhiteSpace :: String -> String
compactWhiteSpace = regexReplace whiteSpaceRegex (\_ -> " ") 

removeWhiteSpace :: String -> String
removeWhiteSpace = regexReplace whiteSpaceRegex (\_ -> "")

-- Good to Know
-- ------------
--
-- *Main> lines "MyString\n"
-- ["MyString"]
-- *Main> lines "MyString"
-- ["MyString"]
-- *Main> lines "\nMyString\n"
-- ["","MyString"]
--
-- *Main> unlines . lines $ "\nMyString"
-- "\nMyString\n"
--

