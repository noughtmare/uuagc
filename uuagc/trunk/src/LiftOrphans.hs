{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveLift, StandaloneDeriving #-}

module LiftOrphans where

import Language.Haskell.TH.Syntax (Lift)

import UU.Scanner.Position

deriving instance Lift Pos
