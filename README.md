# fluff

fluff is a heuristics-based, lightweight linter meant to help you keep your code in
shape. It's basically a wrapper for text search and replace.

This is *not* a tool for heavy/accurate analysis of your code's AST. Think of it
instead as a convenient interface for text analysis which your editor forgot to do.

## Usage

There are a variety of options you can pass fluff, but the most basic usage is simply
`fluff myFile` which will show a list of code violations in the given file. Here's an
example using some of the options:

```bash
fluff --auto-correct --search-dir=. --exclude-dirs=ThirdParty
```

which will recursively search the current directory and attempt to autocorrect any
violations found.

## Installation / Development

Run `./setup.sh` from the project root.

If all you want is to use fluff, you are done. Just add bin/ to your PATH.

If you'd like to develop fluff, you must know that it was developed with ghc 7.8.3 and cabal-install 1.22.0.1. If you have a different version of ghc and cabal, the setup script should install a container for you (which is actually ghc 7.8.4).  You should add the installed ghc distribution's bin to your PATH. Then you can use cabal normally, e.g. `cabal build && fluff --test` to run unit tests.

## Writing custom rules

It's easiest to copy-paste an existing rule and then modify it.
For reference, here are 4 rules with 4 distinct strategies:

- `AsteriskNotBoundToType.hs` is a simple `SingleLineRegex` rule
- `OneTrueBraceStyle.hs` is a `MultiLineRegex` rule
- `AlphabeticImports.hs` is a `WholeFileMap` which uses regular expressions
- `MissingStrongify.hs` is a `WholeFileMap` which uses parser combinators

Here's an example of a minimal rule file:

```haskell
module Rules.ObjectiveC.AsteriskNotBoundToType 
( rule ) where

import RegexUtils
import Rule

name = "AsteriskNotBoundToType"
ruleType = SingleLineRegex
fileNameRestriction = \_ -> True
longDescription = (" This rule enforces the asterisk be bound to the type in variable"
                ++ "and property declarations, casts, and parameter declarations.")
violationDescription = "Asterisk not bound to type"
testRulePositives = ["NSString *blah ", "@property NSObject *a"]
testRulePositivesAutoCorrected = []
testRuleNegatives = [" foo = *error;", "a * b" ]
regex = makeRegex "([A-Z][[:alnum:]]+) \\*([^ ])" :: Regex
autoCorrect = Nothing
rule = makeSingleLineRegexRule name regex longDescription fileNameRestriction violationDescription testRulePositives testRuleNegatives autoCorrect testRulePositivesAutoCorrected
```

While the example uses a regular expression, you can solve slightly more complex
issues using Haskell's Parsec library and parser-combinators.

## Resources

For a good introduction to using Parsec, see http://book.realworldhaskell.org/read/using-parsec.html or the video http://book.realworldhaskell.org/read/using-parsec.html

Otherwise, the hackage documentation for all the libraries is the next best resource.
