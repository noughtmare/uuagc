

{-# LANGUAGE DeriveLift #-}
-- UUAGC 0.9.53.1 (src-ag/Macro.ag)
module Macro where
{-# LINE 9 "src-ag/Macro.ag" #-}

import CommonTypes
import Language.Haskell.TH.Syntax (Lift)
import LiftOrphans ()
{-# LINE 12 "src-generated/Macro.hs" #-}
-- Macro -------------------------------------------------------
{-
   alternatives:
      alternative Macro:
         child con            : {ConstructorIdent}
         child children       : MacroChildren 
      alternative None:
-}
data Macro = Macro (ConstructorIdent) (MacroChildren)
           | None
           deriving ( Lift,Show)
-- MacroChild --------------------------------------------------
{-
   alternatives:
      alternative RuleChild:
         child name           : {Identifier}
         child macro          : Macro 
      alternative ChildChild:
         child name           : {Identifier}
         child child          : {Identifier}
      alternative ValueChild:
         child name           : {Identifier}
         child value          : {String}
-}
data MacroChild = RuleChild (Identifier) (Macro)
                | ChildChild (Identifier) (Identifier)
                | ValueChild (Identifier) (String)
                deriving ( Lift,Show)
-- MacroChildren -----------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : MacroChild 
         child tl             : MacroChildren 
      alternative Nil:
-}
type MacroChildren = [MacroChild]
-- MaybeMacro --------------------------------------------------
{-
   alternatives:
      alternative Just:
         child just           : Macro 
      alternative Nothing:
-}
type MaybeMacro = Maybe (Macro)