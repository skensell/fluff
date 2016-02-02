module ObjectiveC.ParsingUtils
(getAllTopLevelObjcBlocks
,getAllRetainedObjcBlocks)
where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import qualified Text.ParserCombinators.Parsec as P
import qualified Text.Parsec as P2

import ParsingUtils

getAllRetainedObjcBlocks :: String -> String -> Either String [String]
getAllRetainedObjcBlocks fileName fileContents = 
    let
        allBlocks = getAllTopLevelObjcBlocks fileName fileContents
        allNonRetainedObjcBlocks = getAllNonRetainedObjcBlocks fileName fileContents
        -- the following "isNestedBlock" check is necessary because the getAllRetainedObjcBlocks function
        -- does not guarantee that the blocks returned are top level blocks
        isANestedBlockOf b c = b `isInfixOf` c && (length b < length c)
        doesNotContainNestedBlockFrom xs b = not $ any (\c -> c `isANestedBlockOf` b) xs
        isIn xs b = any (\a -> a == b) xs
    in
        case (allBlocks, allNonRetainedObjcBlocks) of
            (Left err, _) -> Left $ show err
            (_, Left err) -> Left $ show err
            (Right allOfEm, Right notRetained) -> Right $ (filter (doesNotContainNestedBlockFrom notRetained)) 
                                                  $ (filter (not . (isIn notRetained)) allOfEm)

getAllTopLevelObjcBlocks :: String -> String -> Either P.ParseError [String]
getAllTopLevelObjcBlocks fileName fileContents =
    let
        objcFile = P.many1 (objcBlock <|> outsideObjcBlock)
        result = P.parse objcFile fileName fileContents
    in
        fmap (filter (\s -> s /= "")) result
        
getAllNonRetainedObjcBlocks :: String -> String -> Either P.ParseError [String]
getAllNonRetainedObjcBlocks fileName fileContents =
    let
        objcFile = P.many1 (objcBlock <|> outsideNonRetainedObjcBlock)
        result = P.parse objcFile fileName fileContents
    in
        fmap (filter (\s -> s /= "")) result
        
--
-- Tokens
--

outsideObjcBlock = (P.try parseToNextObjcBlock) <|> parseToEnd
outsideNonRetainedObjcBlock = P.try parseToNextNonRetainedObjcBlock <|> parseToEnd
objcBlock = do
    isBlockImplementation
    carat <- P.string "^"
    upToOpeningBrace <- parseUntilChar '{'
    bracesWithContents <- braces
    return (carat ++ upToOpeningBrace ++ bracesWithContents)

--
-- Advancers
--

parseToNextObjcBlock = (parseUntilChar '^') >> ifThenElse isBlockImplementation (return []) (P.string "^" >> parseToNextObjcBlock)
parseToNextNonRetainedObjcBlock = parseUntilChar '\n'
                                      >> P.spaces
                                      >> ifThenElse 
                                          (P.try (P.char '[' >> P.upper >> parseToBlockBeforeSemicolon)
                                           <|> P.try (P.string "dispatch" >> parseToBlockBeforeSemicolon)
                                           <|> P.try (P.char '[' >> isNotFollowedByString "self">> P.letter >> parseUntilSpaces >> P.spaces 
                                                >> P.string "enumerate" >> parseToBlockBeforeSemicolon)
                                           <|> P.try (P.string "return [" >> P.upper >> parseToBlockBeforeSemicolon))
                                          (return [])
                                          parseToNextNonRetainedObjcBlock
parseToBlockBeforeSemicolon = parseUntilOneOf ";^" >> isBlockImplementation

--
-- Truth Testers
--

isBlockImplementation = P.lookAhead (P.try (P.string "^" >> P.notFollowedBy (P.char ')') >> parseUntilOneOf "\n{=" >> P.string "{"))

-- for testing
-- main :: IO ()
-- main = runCommand $ \opts args -> do 
--     let ruleSet = optRuleSet opts
--     rules <- parseRules opts
--     searchDirFileNames <- getFilenamesInSearchDir (optSearchDir opts) (optFileExtensions opts) (optExcludeDirs opts)
--     let fileNames = args ++ searchDirFileNames
--     let file = head args
--     input <- readFile file
--     let result = getAllObjcBlocks file input
--     case result of
--         Left err -> print err
--         -- Right xs -> print (filter (\s -> s /= "") xs)
--         Right xs -> mapM_ (\x -> putStrLn (x ++ "\n\n===SEP===\n")) xs

