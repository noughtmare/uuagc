

{-# LANGUAGE DeriveLift #-}
-- UUAGC 0.9.53.1 (src-ag/Expression.ag)
module Expression where
{-# LINE 6 "src-ag/Expression.ag" #-}

import UU.Scanner.Position(Pos)
import HsToken
import Language.Haskell.TH.Syntax (Lift)
import LiftOrphans ()
{-# LINE 13 "src-generated/Expression.hs" #-}
-- Expression --------------------------------------------------
{-
   alternatives:
      alternative Expression:
         child pos            : {Pos}
         child tks            : {[HsToken]}
-}
data Expression = Expression (Pos) (([HsToken]))
                deriving ( Lift)