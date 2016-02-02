-- Parsing tokens, advancers, and truth-testers applicable to multiple languages.
module ParsingUtils where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import qualified Text.ParserCombinators.Parsec as P
import qualified Text.Parsec as P2

type Parser = P2.Parsec String () String

--
-- Tokens
--

nonEmptyString :: Parser
nonEmptyString = P.many1 P.anyChar

braces = P.try atomBraces <|> do
    startBrace <- P.string "{"
    upToNextBrace <- parseUntilOneOf "{}"
    theRest <- endNestedBraces
    return (startBrace ++ upToNextBrace ++ theRest)

endNestedBraces = P.string "}" <|> do
    innerBraces <- braces
    upToNextBrace <- parseUntilOneOf "{}"
    theRest <- endNestedBraces
    return (innerBraces ++ upToNextBrace ++ theRest)

atomBraces = do
    s <- P.string "{"
    m <- parseUntilOneOf "{}"
    e <- P.string "}"
    return (s ++ m ++ e)

--
-- Advancers
--

parseUntilChar :: Char -> Parser
parseUntilChar c = P.manyTill (P.noneOf [c]) (P.lookAhead (P.char c))
parseUntilOneOf s = P.manyTill (P.noneOf s) (P.lookAhead (P.oneOf s))
parseUntilSpaces :: Parser
parseUntilSpaces = P.manyTill (P.satisfy (not . isSpace)) (P.lookAhead (P.space))
parseToEnd = (nonEmptyString <|> (P.anyToken >> return [])) >> P2.eof >> return []

--
-- Truth Testers
--

-- this is only a real if-then-else if b never fails
ifThenElse a b c = P.try (a >> b) <|> c
isFollowedByString s = P.lookAhead (P.try (P.string s))
isNotFollowedByString s = P.notFollowedBy (isFollowedByString s)

