module RuleSets
(ruleSets)
where

import qualified Data.Map as Map

import Rule
import qualified ObjectiveC.Rules.AlphabeticImports
import qualified ObjectiveC.Rules.AsteriskNotBoundToType 
import qualified ObjectiveC.Rules.MissingStrongify
import qualified ObjectiveC.Rules.OneTrueBraceStyle
import qualified ObjectiveC.Rules.PropertyAttributeOrder
import qualified ObjectiveC.Rules.PropertyDeclSpacing

ruleSets :: Map.Map String [Rule]
ruleSets = Map.fromList $
    [("pn-objc", [ ObjectiveC.Rules.AlphabeticImports.rule
                 , ObjectiveC.Rules.AsteriskNotBoundToType.rule
                 , ObjectiveC.Rules.MissingStrongify.rule                
                 , ObjectiveC.Rules.OneTrueBraceStyle.rule
                 , ObjectiveC.Rules.PropertyAttributeOrder.rule
                 , ObjectiveC.Rules.PropertyDeclSpacing.rule
                 ]
     )
    ]
                
