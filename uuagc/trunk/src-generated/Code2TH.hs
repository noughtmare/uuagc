{-# LANGUAGE Rank2Types, GADTs #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE DeriveLift #-}
module Code2TH where
{-# LINE 6 "src-ag/Patterns.ag" #-}

-- Patterns.ag imports
import UU.Scanner.Position(Pos)
import CommonTypes (ConstructorIdent,Identifier)
import Language.Haskell.TH.Syntax (Lift)
import LiftOrphans ()
{-# LINE 15 "src-generated/Code2TH.hs" #-}

{-# LINE 2 "src-ag/Code.ag" #-}

import Patterns
import Data.Set(Set)
import qualified Data.Set as Set
import Data.Map(Map)
import qualified Data.Map as Map
{-# LINE 24 "src-generated/Code2TH.hs" #-}

{-# LINE 10 "src-ag/Code2TH.ag" #-}

import Data.Char (isAlphaNum)
-- import Pretty
import Code
import Options
import CommonTypes (attrname, _LOC, nullIdent)
import Data.List(intersperse)
import System.IO
import System.Directory
import System.FilePath
import CommonTypes(BlockInfo, BlockKind(..))
import Data.Char (isUpper)

import Control.Applicative (Alternative (..))

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH
import qualified Language.Haskell.Meta as Meta

import Debug.Trace
{-# LINE 47 "src-generated/Code2TH.hs" #-}
import Control.Monad.Identity (Identity)
import qualified Control.Monad.Identity
{-# LINE 146 "src-ag/Code.ag" #-}

-- Unboxed tuples
--   unbox  Whether unboxed tuples are wanted or not
--   inh    The inherited attributes.
--          If there are none, no unboxing can take place,
--          because in that case the semantic function (a top-level identifier) would have an unboxed type.
-- Of course we can't have an unboxed 1-tuple
mkTupleExpr :: Bool -> Bool -> Exprs -> Expr
mkTupleExpr unbox' noInh exprs | not unbox' || noInh || length exprs == 1 = TupleExpr exprs
                               | otherwise                                = UnboxedTupleExpr exprs
mkTupleType :: Bool -> Bool -> Types -> Type
mkTupleType unbox' noInh tps | not unbox' || noInh || length tps == 1 = TupleType tps
                             | otherwise                              = UnboxedTupleType tps
mkTupleLhs :: Bool -> Bool -> [String] -> Lhs
mkTupleLhs  unbox' noInh comps | not unbox' || noInh || length comps == 1 = TupleLhs comps
                               | otherwise                                = UnboxedTupleLhs comps
{-# LINE 67 "src-generated/Code2TH.hs" #-}

{-# LINE 32 "src-ag/Code2TH.ag" #-}

tupleT :: Bool -> Bool -> [TH.Type] -> TH.Type
tupleT _       _      [tp] = tp
tupleT unboxed nested tps
  | nested = foldl1 (\xs x -> con 2 `TH.AppT` xs `TH.AppT` x) tps
  | otherwise = foldl TH.AppT (con (length tps)) tps
  where con = if unboxed then TH.UnboxedTupleT else TH.TupleT

tupleP :: Bool -> Bool -> [TH.Pat] -> TH.Pat
tupleP _       _      [tp] = tp
tupleP unboxed nested tps
  | nested = foldl1 (\xs x -> con [xs, x]) tps
  | otherwise = con tps
  where con = if unboxed then TH.UnboxedTupP else TH.TupP

tupleE :: Bool -> Bool -> [TH.Exp] -> TH.Exp
tupleE _       _      [tp] = tp
tupleE unboxed nested tps
  | nested = foldl1 (\xs x -> con [Just xs, Just x]) tps
  | otherwise = con (map Just tps)
  where con = if unboxed then TH.UnboxedTupE else TH.TupE

-- This is a bit of a hack because the UUAG intermediate representation Code 
-- doesn't group function declaration clauses. This functions recovers that structure.
dedupFunDecls :: [TH.Dec] -> [TH.Dec]
dedupFunDecls (x1@(TH.FunD n1 cl1) : x2@(TH.FunD n2 cl2) : xs)
  | n1 == n2 = dedupFunDecls (TH.FunD n1 (cl1 ++ cl2) : xs)
dedupFunDecls (x1:x2:xs) = x1 : dedupFunDecls (x2 : xs)
dedupFunDecls xs = xs

varConE :: String -> TH.Exp
varConE xs@(x:_)
  | isUpper x = TH.ConE (TH.mkName xs)
  | otherwise = TH.VarE (TH.mkName xs)

seqsE :: [TH.Exp] -> TH.Exp -> TH.Exp
seqsE xs x = foldr (TH.AppE . TH.AppE (TH.VarE (TH.mkName "seq"))) x xs
{-# LINE 107 "src-generated/Code2TH.hs" #-}
-- CaseAlt -----------------------------------------------------
-- wrapper
data Inh_CaseAlt  = Inh_CaseAlt { nested_Inh_CaseAlt :: !(Bool), options_Inh_CaseAlt :: !(Options) }
data Syn_CaseAlt  = Syn_CaseAlt { th_Syn_CaseAlt :: !(TH.Match) }
{-# INLINABLE wrap_CaseAlt #-}
wrap_CaseAlt :: T_CaseAlt  -> Inh_CaseAlt  -> (Syn_CaseAlt )
wrap_CaseAlt !(T_CaseAlt act) !(Inh_CaseAlt _lhsInested _lhsIoptions) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg1 = T_CaseAlt_vIn1 _lhsInested _lhsIoptions
        !(T_CaseAlt_vOut1 _lhsOth) <- return (inv_CaseAlt_s2 sem arg1)
        return (Syn_CaseAlt _lhsOth)
   )

-- cata
{-# NOINLINE sem_CaseAlt #-}
sem_CaseAlt :: CaseAlt  -> T_CaseAlt 
sem_CaseAlt ( CaseAlt left_ expr_ ) = sem_CaseAlt_CaseAlt ( sem_Lhs left_ ) ( sem_Expr expr_ )

-- semantic domain
newtype T_CaseAlt  = T_CaseAlt {
                               attach_T_CaseAlt :: Identity (T_CaseAlt_s2 )
                               }
newtype T_CaseAlt_s2  = C_CaseAlt_s2 {
                                     inv_CaseAlt_s2 :: (T_CaseAlt_v1 )
                                     }
data T_CaseAlt_s3  = C_CaseAlt_s3
type T_CaseAlt_v1  = (T_CaseAlt_vIn1 ) -> (T_CaseAlt_vOut1 )
data T_CaseAlt_vIn1  = T_CaseAlt_vIn1 (Bool) (Options)
data T_CaseAlt_vOut1  = T_CaseAlt_vOut1 (TH.Match)
{-# NOINLINE sem_CaseAlt_CaseAlt #-}
sem_CaseAlt_CaseAlt :: T_Lhs  -> T_Expr  -> T_CaseAlt 
sem_CaseAlt_CaseAlt arg_left_ arg_expr_ = T_CaseAlt (return st2) where
   {-# NOINLINE st2 #-}
   !st2 = let
      v1 :: T_CaseAlt_v1 
      v1 = \ !(T_CaseAlt_vIn1 _lhsInested _lhsIoptions) -> ( let
         _leftX32 = Control.Monad.Identity.runIdentity (attach_T_Lhs (arg_left_))
         _exprX26 = Control.Monad.Identity.runIdentity (attach_T_Expr (arg_expr_))
         (T_Lhs_vOut31 _leftIpat _leftIth) = inv_Lhs_s32 _leftX32 (T_Lhs_vIn31 _leftOisDeclOfLet _leftOnested _leftOoptions)
         (T_Expr_vOut25 _exprIpat _exprIth) = inv_Expr_s26 _exprX26 (T_Expr_vIn25 _exprOnested _exprOoptions)
         _lhsOth :: TH.Match
         _lhsOth = rule0 _exprIth _leftIpat
         _leftOisDeclOfLet = rule1  ()
         _leftOnested = rule2 _lhsInested
         _leftOoptions = rule3 _lhsIoptions
         _exprOnested = rule4 _lhsInested
         _exprOoptions = rule5 _lhsIoptions
         !__result_ = T_CaseAlt_vOut1 _lhsOth
         in __result_ )
     in C_CaseAlt_s2 v1
   {-# INLINE rule0 #-}
   {-# LINE 239 "src-ag/Code2TH.ag" #-}
   rule0 = \ ((_exprIth) :: TH.Exp) ((_leftIpat) :: TH.Pat) ->
                              {-# LINE 239 "src-ag/Code2TH.ag" #-}
                              TH.Match _leftIpat (TH.NormalB _exprIth) []
                              {-# LINE 164 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule1 #-}
   {-# LINE 425 "src-ag/Code2TH.ag" #-}
   rule1 = \  (_ :: ()) ->
                           {-# LINE 425 "src-ag/Code2TH.ag" #-}
                           False
                           {-# LINE 170 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule2 #-}
   rule2 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule3 #-}
   rule3 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule4 #-}
   rule4 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule5 #-}
   rule5 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions

-- CaseAlts ----------------------------------------------------
-- wrapper
data Inh_CaseAlts  = Inh_CaseAlts { nested_Inh_CaseAlts :: !(Bool), options_Inh_CaseAlts :: !(Options) }
data Syn_CaseAlts  = Syn_CaseAlts { th_Syn_CaseAlts :: !([TH.Match]) }
{-# INLINABLE wrap_CaseAlts #-}
wrap_CaseAlts :: T_CaseAlts  -> Inh_CaseAlts  -> (Syn_CaseAlts )
wrap_CaseAlts !(T_CaseAlts act) !(Inh_CaseAlts _lhsInested _lhsIoptions) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg4 = T_CaseAlts_vIn4 _lhsInested _lhsIoptions
        !(T_CaseAlts_vOut4 _lhsOth) <- return (inv_CaseAlts_s5 sem arg4)
        return (Syn_CaseAlts _lhsOth)
   )

-- cata
{-# NOINLINE sem_CaseAlts #-}
sem_CaseAlts :: CaseAlts  -> T_CaseAlts 
sem_CaseAlts list = Prelude.foldr sem_CaseAlts_Cons sem_CaseAlts_Nil (Prelude.map sem_CaseAlt list)

-- semantic domain
newtype T_CaseAlts  = T_CaseAlts {
                                 attach_T_CaseAlts :: Identity (T_CaseAlts_s5 )
                                 }
newtype T_CaseAlts_s5  = C_CaseAlts_s5 {
                                       inv_CaseAlts_s5 :: (T_CaseAlts_v4 )
                                       }
data T_CaseAlts_s6  = C_CaseAlts_s6
type T_CaseAlts_v4  = (T_CaseAlts_vIn4 ) -> (T_CaseAlts_vOut4 )
data T_CaseAlts_vIn4  = T_CaseAlts_vIn4 (Bool) (Options)
data T_CaseAlts_vOut4  = T_CaseAlts_vOut4 ([TH.Match])
{-# NOINLINE sem_CaseAlts_Cons #-}
sem_CaseAlts_Cons :: T_CaseAlt  -> T_CaseAlts  -> T_CaseAlts 
sem_CaseAlts_Cons arg_hd_ arg_tl_ = T_CaseAlts (return st5) where
   {-# NOINLINE st5 #-}
   !st5 = let
      v4 :: T_CaseAlts_v4 
      v4 = \ !(T_CaseAlts_vIn4 _lhsInested _lhsIoptions) -> ( let
         _hdX2 = Control.Monad.Identity.runIdentity (attach_T_CaseAlt (arg_hd_))
         _tlX5 = Control.Monad.Identity.runIdentity (attach_T_CaseAlts (arg_tl_))
         (T_CaseAlt_vOut1 _hdIth) = inv_CaseAlt_s2 _hdX2 (T_CaseAlt_vIn1 _hdOnested _hdOoptions)
         (T_CaseAlts_vOut4 _tlIth) = inv_CaseAlts_s5 _tlX5 (T_CaseAlts_vIn4 _tlOnested _tlOoptions)
         _lhsOth :: [TH.Match]
         _lhsOth = rule6 _hdIth _tlIth
         _hdOnested = rule7 _lhsInested
         _hdOoptions = rule8 _lhsIoptions
         _tlOnested = rule9 _lhsInested
         _tlOoptions = rule10 _lhsIoptions
         !__result_ = T_CaseAlts_vOut4 _lhsOth
         in __result_ )
     in C_CaseAlts_s5 v4
   {-# INLINE rule6 #-}
   rule6 = \ ((_hdIth) :: TH.Match) ((_tlIth) :: [TH.Match]) ->
     _hdIth : _tlIth
   {-# INLINE rule7 #-}
   rule7 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule8 #-}
   rule8 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule9 #-}
   rule9 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule10 #-}
   rule10 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_CaseAlts_Nil #-}
sem_CaseAlts_Nil ::  T_CaseAlts 
sem_CaseAlts_Nil  = T_CaseAlts (return st5) where
   {-# NOINLINE st5 #-}
   !st5 = let
      v4 :: T_CaseAlts_v4 
      v4 = \ !(T_CaseAlts_vIn4 _lhsInested _lhsIoptions) -> ( let
         _lhsOth :: [TH.Match]
         _lhsOth = rule11  ()
         !__result_ = T_CaseAlts_vOut4 _lhsOth
         in __result_ )
     in C_CaseAlts_s5 v4
   {-# INLINE rule11 #-}
   rule11 = \  (_ :: ()) ->
     []

-- Chunk -------------------------------------------------------
-- wrapper
data Inh_Chunk  = Inh_Chunk { isDeclOfLet_Inh_Chunk :: !(Bool), nested_Inh_Chunk :: !(Bool), options_Inh_Chunk :: !(Options) }
data Syn_Chunk  = Syn_Chunk { th_Syn_Chunk :: !([TH.Dec]) }
{-# INLINABLE wrap_Chunk #-}
wrap_Chunk :: T_Chunk  -> Inh_Chunk  -> (Syn_Chunk )
wrap_Chunk !(T_Chunk act) !(Inh_Chunk _lhsIisDeclOfLet _lhsInested _lhsIoptions) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg7 = T_Chunk_vIn7 _lhsIisDeclOfLet _lhsInested _lhsIoptions
        !(T_Chunk_vOut7 _lhsOth) <- return (inv_Chunk_s8 sem arg7)
        return (Syn_Chunk _lhsOth)
   )

-- cata
{-# INLINE sem_Chunk #-}
sem_Chunk :: Chunk  -> T_Chunk 
sem_Chunk ( Chunk !name_ comment_ info_ dataDef_ cataFun_ semDom_ semWrapper_ semFunctions_ !semNames_ ) = sem_Chunk_Chunk name_ ( sem_Decl comment_ ) ( sem_Decls info_ ) ( sem_Decls dataDef_ ) ( sem_Decls cataFun_ ) ( sem_Decls semDom_ ) ( sem_Decls semWrapper_ ) ( sem_Decls semFunctions_ ) semNames_

-- semantic domain
newtype T_Chunk  = T_Chunk {
                           attach_T_Chunk :: Identity (T_Chunk_s8 )
                           }
newtype T_Chunk_s8  = C_Chunk_s8 {
                                 inv_Chunk_s8 :: (T_Chunk_v7 )
                                 }
data T_Chunk_s9  = C_Chunk_s9
type T_Chunk_v7  = (T_Chunk_vIn7 ) -> (T_Chunk_vOut7 )
data T_Chunk_vIn7  = T_Chunk_vIn7 (Bool) (Bool) (Options)
data T_Chunk_vOut7  = T_Chunk_vOut7 ([TH.Dec])
{-# NOINLINE sem_Chunk_Chunk #-}
sem_Chunk_Chunk :: (String) -> T_Decl  -> T_Decls  -> T_Decls  -> T_Decls  -> T_Decls  -> T_Decls  -> T_Decls  -> ([String]) -> T_Chunk 
sem_Chunk_Chunk _ arg_comment_ arg_info_ arg_dataDef_ arg_cataFun_ arg_semDom_ arg_semWrapper_ arg_semFunctions_ _ = T_Chunk (return st8) where
   {-# NOINLINE st8 #-}
   !st8 = let
      v7 :: T_Chunk_v7 
      v7 = \ !(T_Chunk_vIn7 _lhsIisDeclOfLet _lhsInested _lhsIoptions) -> ( let
         _commentX20 = Control.Monad.Identity.runIdentity (attach_T_Decl (arg_comment_))
         _infoX23 = Control.Monad.Identity.runIdentity (attach_T_Decls (arg_info_))
         _dataDefX23 = Control.Monad.Identity.runIdentity (attach_T_Decls (arg_dataDef_))
         _cataFunX23 = Control.Monad.Identity.runIdentity (attach_T_Decls (arg_cataFun_))
         _semDomX23 = Control.Monad.Identity.runIdentity (attach_T_Decls (arg_semDom_))
         _semWrapperX23 = Control.Monad.Identity.runIdentity (attach_T_Decls (arg_semWrapper_))
         _semFunctionsX23 = Control.Monad.Identity.runIdentity (attach_T_Decls (arg_semFunctions_))
         (T_Decl_vOut19 _commentIstmt _commentIth) = inv_Decl_s20 _commentX20 (T_Decl_vIn19 _commentOisDeclOfLet _commentOnested _commentOoptions)
         (T_Decls_vOut22 _infoIstmt _infoIth) = inv_Decls_s23 _infoX23 (T_Decls_vIn22 _infoOisDeclOfLet _infoOnested _infoOoptions)
         (T_Decls_vOut22 _dataDefIstmt _dataDefIth) = inv_Decls_s23 _dataDefX23 (T_Decls_vIn22 _dataDefOisDeclOfLet _dataDefOnested _dataDefOoptions)
         (T_Decls_vOut22 _cataFunIstmt _cataFunIth) = inv_Decls_s23 _cataFunX23 (T_Decls_vIn22 _cataFunOisDeclOfLet _cataFunOnested _cataFunOoptions)
         (T_Decls_vOut22 _semDomIstmt _semDomIth) = inv_Decls_s23 _semDomX23 (T_Decls_vIn22 _semDomOisDeclOfLet _semDomOnested _semDomOoptions)
         (T_Decls_vOut22 _semWrapperIstmt _semWrapperIth) = inv_Decls_s23 _semWrapperX23 (T_Decls_vIn22 _semWrapperOisDeclOfLet _semWrapperOnested _semWrapperOoptions)
         (T_Decls_vOut22 _semFunctionsIstmt _semFunctionsIth) = inv_Decls_s23 _semFunctionsX23 (T_Decls_vIn22 _semFunctionsOisDeclOfLet _semFunctionsOnested _semFunctionsOoptions)
         _th = rule12 _cataFunIth _commentIth _dataDefIth _infoIth _semDomIth _semFunctionsIth _semWrapperIth
         _lhsOth :: [TH.Dec]
         _lhsOth = rule13 _th
         _commentOisDeclOfLet = rule14 _lhsIisDeclOfLet
         _commentOnested = rule15 _lhsInested
         _commentOoptions = rule16 _lhsIoptions
         _infoOisDeclOfLet = rule17 _lhsIisDeclOfLet
         _infoOnested = rule18 _lhsInested
         _infoOoptions = rule19 _lhsIoptions
         _dataDefOisDeclOfLet = rule20 _lhsIisDeclOfLet
         _dataDefOnested = rule21 _lhsInested
         _dataDefOoptions = rule22 _lhsIoptions
         _cataFunOisDeclOfLet = rule23 _lhsIisDeclOfLet
         _cataFunOnested = rule24 _lhsInested
         _cataFunOoptions = rule25 _lhsIoptions
         _semDomOisDeclOfLet = rule26 _lhsIisDeclOfLet
         _semDomOnested = rule27 _lhsInested
         _semDomOoptions = rule28 _lhsIoptions
         _semWrapperOisDeclOfLet = rule29 _lhsIisDeclOfLet
         _semWrapperOnested = rule30 _lhsInested
         _semWrapperOoptions = rule31 _lhsIoptions
         _semFunctionsOisDeclOfLet = rule32 _lhsIisDeclOfLet
         _semFunctionsOnested = rule33 _lhsInested
         _semFunctionsOoptions = rule34 _lhsIoptions
         !__result_ = T_Chunk_vOut7 _lhsOth
         in __result_ )
     in C_Chunk_s8 v7
   {-# INLINE rule12 #-}
   {-# LINE 74 "src-ag/Code2TH.ag" #-}
   rule12 = \ ((_cataFunIth) :: [TH.Dec]) ((_commentIth) :: Maybe TH.Dec) ((_dataDefIth) :: [TH.Dec]) ((_infoIth) :: [TH.Dec]) ((_semDomIth) :: [TH.Dec]) ((_semFunctionsIth) :: [TH.Dec]) ((_semWrapperIth) :: [TH.Dec]) ->
                     {-# LINE 74 "src-ag/Code2TH.ag" #-}
                     maybe id (:) _commentIth $ concat
                       [ _infoIth
                       , _dataDefIth
                       , _cataFunIth
                       , _semDomIth
                       , _semWrapperIth
                       , _semFunctionsIth
                       ]
                     {-# LINE 355 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule13 #-}
   rule13 = \ _th ->
     _th
   {-# INLINE rule14 #-}
   rule14 = \ ((_lhsIisDeclOfLet) :: Bool) ->
     _lhsIisDeclOfLet
   {-# INLINE rule15 #-}
   rule15 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule16 #-}
   rule16 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule17 #-}
   rule17 = \ ((_lhsIisDeclOfLet) :: Bool) ->
     _lhsIisDeclOfLet
   {-# INLINE rule18 #-}
   rule18 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule19 #-}
   rule19 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule20 #-}
   rule20 = \ ((_lhsIisDeclOfLet) :: Bool) ->
     _lhsIisDeclOfLet
   {-# INLINE rule21 #-}
   rule21 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule22 #-}
   rule22 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule23 #-}
   rule23 = \ ((_lhsIisDeclOfLet) :: Bool) ->
     _lhsIisDeclOfLet
   {-# INLINE rule24 #-}
   rule24 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule25 #-}
   rule25 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule26 #-}
   rule26 = \ ((_lhsIisDeclOfLet) :: Bool) ->
     _lhsIisDeclOfLet
   {-# INLINE rule27 #-}
   rule27 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule28 #-}
   rule28 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule29 #-}
   rule29 = \ ((_lhsIisDeclOfLet) :: Bool) ->
     _lhsIisDeclOfLet
   {-# INLINE rule30 #-}
   rule30 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule31 #-}
   rule31 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule32 #-}
   rule32 = \ ((_lhsIisDeclOfLet) :: Bool) ->
     _lhsIisDeclOfLet
   {-# INLINE rule33 #-}
   rule33 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule34 #-}
   rule34 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions

-- Chunks ------------------------------------------------------
-- wrapper
data Inh_Chunks  = Inh_Chunks { isDeclOfLet_Inh_Chunks :: !(Bool), nested_Inh_Chunks :: !(Bool), options_Inh_Chunks :: !(Options) }
data Syn_Chunks  = Syn_Chunks { th_Syn_Chunks :: !([TH.Dec]) }
{-# INLINABLE wrap_Chunks #-}
wrap_Chunks :: T_Chunks  -> Inh_Chunks  -> (Syn_Chunks )
wrap_Chunks !(T_Chunks act) !(Inh_Chunks _lhsIisDeclOfLet _lhsInested _lhsIoptions) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg10 = T_Chunks_vIn10 _lhsIisDeclOfLet _lhsInested _lhsIoptions
        !(T_Chunks_vOut10 _lhsOth) <- return (inv_Chunks_s11 sem arg10)
        return (Syn_Chunks _lhsOth)
   )

-- cata
{-# NOINLINE sem_Chunks #-}
sem_Chunks :: Chunks  -> T_Chunks 
sem_Chunks list = Prelude.foldr sem_Chunks_Cons sem_Chunks_Nil (Prelude.map sem_Chunk list)

-- semantic domain
newtype T_Chunks  = T_Chunks {
                             attach_T_Chunks :: Identity (T_Chunks_s11 )
                             }
newtype T_Chunks_s11  = C_Chunks_s11 {
                                     inv_Chunks_s11 :: (T_Chunks_v10 )
                                     }
data T_Chunks_s12  = C_Chunks_s12
type T_Chunks_v10  = (T_Chunks_vIn10 ) -> (T_Chunks_vOut10 )
data T_Chunks_vIn10  = T_Chunks_vIn10 (Bool) (Bool) (Options)
data T_Chunks_vOut10  = T_Chunks_vOut10 ([TH.Dec])
{-# NOINLINE sem_Chunks_Cons #-}
sem_Chunks_Cons :: T_Chunk  -> T_Chunks  -> T_Chunks 
sem_Chunks_Cons arg_hd_ arg_tl_ = T_Chunks (return st11) where
   {-# NOINLINE st11 #-}
   !st11 = let
      v10 :: T_Chunks_v10 
      v10 = \ !(T_Chunks_vIn10 _lhsIisDeclOfLet _lhsInested _lhsIoptions) -> ( let
         _hdX8 = Control.Monad.Identity.runIdentity (attach_T_Chunk (arg_hd_))
         _tlX11 = Control.Monad.Identity.runIdentity (attach_T_Chunks (arg_tl_))
         (T_Chunk_vOut7 _hdIth) = inv_Chunk_s8 _hdX8 (T_Chunk_vIn7 _hdOisDeclOfLet _hdOnested _hdOoptions)
         (T_Chunks_vOut10 _tlIth) = inv_Chunks_s11 _tlX11 (T_Chunks_vIn10 _tlOisDeclOfLet _tlOnested _tlOoptions)
         _lhsOth :: [TH.Dec]
         _lhsOth = rule35 _hdIth _tlIth
         _hdOisDeclOfLet = rule36 _lhsIisDeclOfLet
         _hdOnested = rule37 _lhsInested
         _hdOoptions = rule38 _lhsIoptions
         _tlOisDeclOfLet = rule39 _lhsIisDeclOfLet
         _tlOnested = rule40 _lhsInested
         _tlOoptions = rule41 _lhsIoptions
         !__result_ = T_Chunks_vOut10 _lhsOth
         in __result_ )
     in C_Chunks_s11 v10
   {-# INLINE rule35 #-}
   rule35 = \ ((_hdIth) :: [TH.Dec]) ((_tlIth) :: [TH.Dec]) ->
     _hdIth ++ _tlIth
   {-# INLINE rule36 #-}
   rule36 = \ ((_lhsIisDeclOfLet) :: Bool) ->
     _lhsIisDeclOfLet
   {-# INLINE rule37 #-}
   rule37 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule38 #-}
   rule38 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule39 #-}
   rule39 = \ ((_lhsIisDeclOfLet) :: Bool) ->
     _lhsIisDeclOfLet
   {-# INLINE rule40 #-}
   rule40 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule41 #-}
   rule41 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Chunks_Nil #-}
sem_Chunks_Nil ::  T_Chunks 
sem_Chunks_Nil  = T_Chunks (return st11) where
   {-# NOINLINE st11 #-}
   !st11 = let
      v10 :: T_Chunks_v10 
      v10 = \ !(T_Chunks_vIn10 _lhsIisDeclOfLet _lhsInested _lhsIoptions) -> ( let
         _lhsOth :: [TH.Dec]
         _lhsOth = rule42  ()
         !__result_ = T_Chunks_vOut10 _lhsOth
         in __result_ )
     in C_Chunks_s11 v10
   {-# INLINE rule42 #-}
   rule42 = \  (_ :: ()) ->
     []

-- DataAlt -----------------------------------------------------
-- wrapper
data Inh_DataAlt  = Inh_DataAlt { nested_Inh_DataAlt :: !(Bool), strictPre_Inh_DataAlt :: !(TH.Bang) }
data Syn_DataAlt  = Syn_DataAlt { th_Syn_DataAlt :: !(TH.Con) }
{-# INLINABLE wrap_DataAlt #-}
wrap_DataAlt :: T_DataAlt  -> Inh_DataAlt  -> (Syn_DataAlt )
wrap_DataAlt !(T_DataAlt act) !(Inh_DataAlt _lhsInested _lhsIstrictPre) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg13 = T_DataAlt_vIn13 _lhsInested _lhsIstrictPre
        !(T_DataAlt_vOut13 _lhsOth) <- return (inv_DataAlt_s14 sem arg13)
        return (Syn_DataAlt _lhsOth)
   )

-- cata
{-# NOINLINE sem_DataAlt #-}
sem_DataAlt :: DataAlt  -> T_DataAlt 
sem_DataAlt ( DataAlt !name_ args_ ) = sem_DataAlt_DataAlt name_ ( sem_Types args_ )
sem_DataAlt ( Record !name_ args_ ) = sem_DataAlt_Record name_ ( sem_NamedTypes args_ )

-- semantic domain
newtype T_DataAlt  = T_DataAlt {
                               attach_T_DataAlt :: Identity (T_DataAlt_s14 )
                               }
newtype T_DataAlt_s14  = C_DataAlt_s14 {
                                       inv_DataAlt_s14 :: (T_DataAlt_v13 )
                                       }
data T_DataAlt_s15  = C_DataAlt_s15
type T_DataAlt_v13  = (T_DataAlt_vIn13 ) -> (T_DataAlt_vOut13 )
data T_DataAlt_vIn13  = T_DataAlt_vIn13 (Bool) (TH.Bang)
data T_DataAlt_vOut13  = T_DataAlt_vOut13 (TH.Con)
{-# NOINLINE sem_DataAlt_DataAlt #-}
sem_DataAlt_DataAlt :: (String) -> T_Types  -> T_DataAlt 
sem_DataAlt_DataAlt !arg_name_ arg_args_ = T_DataAlt (return st14) where
   {-# NOINLINE st14 #-}
   !st14 = let
      v13 :: T_DataAlt_v13 
      v13 = \ !(T_DataAlt_vIn13 _lhsInested _lhsIstrictPre) -> ( let
         _argsX53 = Control.Monad.Identity.runIdentity (attach_T_Types (arg_args_))
         (T_Types_vOut52 _argsIth) = inv_Types_s53 _argsX53 (T_Types_vIn52 _argsOnested)
         _lhsOth :: TH.Con
         _lhsOth = rule43 _argsIth _lhsIstrictPre arg_name_
         _argsOnested = rule44 _lhsInested
         !__result_ = T_DataAlt_vOut13 _lhsOth
         in __result_ )
     in C_DataAlt_s14 v13
   {-# INLINE rule43 #-}
   {-# LINE 245 "src-ag/Code2TH.ag" #-}
   rule43 = \ ((_argsIth) :: [TH.Type]) ((_lhsIstrictPre) :: TH.Bang) name_ ->
                               {-# LINE 245 "src-ag/Code2TH.ag" #-}
                               TH.NormalC (TH.mkName name_) (map (\x -> (_lhsIstrictPre, x)) _argsIth)
                               {-# LINE 563 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule44 #-}
   rule44 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
{-# NOINLINE sem_DataAlt_Record #-}
sem_DataAlt_Record :: (String) -> T_NamedTypes  -> T_DataAlt 
sem_DataAlt_Record !arg_name_ arg_args_ = T_DataAlt (return st14) where
   {-# NOINLINE st14 #-}
   !st14 = let
      v13 :: T_DataAlt_v13 
      v13 = \ !(T_DataAlt_vIn13 _lhsInested _lhsIstrictPre) -> ( let
         _argsX38 = Control.Monad.Identity.runIdentity (attach_T_NamedTypes (arg_args_))
         (T_NamedTypes_vOut37 _argsIth) = inv_NamedTypes_s38 _argsX38 (T_NamedTypes_vIn37 _argsOnested)
         _lhsOth :: TH.Con
         _lhsOth = rule45 _argsIth arg_name_
         _argsOnested = rule46 _lhsInested
         !__result_ = T_DataAlt_vOut13 _lhsOth
         in __result_ )
     in C_DataAlt_s14 v13
   {-# INLINE rule45 #-}
   {-# LINE 246 "src-ag/Code2TH.ag" #-}
   rule45 = \ ((_argsIth) :: [TH.VarBangType]) name_ ->
                               {-# LINE 246 "src-ag/Code2TH.ag" #-}
                               TH.RecC (TH.mkName name_) _argsIth
                               {-# LINE 587 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule46 #-}
   rule46 = \ ((_lhsInested) :: Bool) ->
     _lhsInested

-- DataAlts ----------------------------------------------------
-- wrapper
data Inh_DataAlts  = Inh_DataAlts { nested_Inh_DataAlts :: !(Bool), strictPre_Inh_DataAlts :: !(TH.Bang) }
data Syn_DataAlts  = Syn_DataAlts { th_Syn_DataAlts :: !([TH.Con]) }
{-# INLINABLE wrap_DataAlts #-}
wrap_DataAlts :: T_DataAlts  -> Inh_DataAlts  -> (Syn_DataAlts )
wrap_DataAlts !(T_DataAlts act) !(Inh_DataAlts _lhsInested _lhsIstrictPre) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg16 = T_DataAlts_vIn16 _lhsInested _lhsIstrictPre
        !(T_DataAlts_vOut16 _lhsOth) <- return (inv_DataAlts_s17 sem arg16)
        return (Syn_DataAlts _lhsOth)
   )

-- cata
{-# NOINLINE sem_DataAlts #-}
sem_DataAlts :: DataAlts  -> T_DataAlts 
sem_DataAlts list = Prelude.foldr sem_DataAlts_Cons sem_DataAlts_Nil (Prelude.map sem_DataAlt list)

-- semantic domain
newtype T_DataAlts  = T_DataAlts {
                                 attach_T_DataAlts :: Identity (T_DataAlts_s17 )
                                 }
newtype T_DataAlts_s17  = C_DataAlts_s17 {
                                         inv_DataAlts_s17 :: (T_DataAlts_v16 )
                                         }
data T_DataAlts_s18  = C_DataAlts_s18
type T_DataAlts_v16  = (T_DataAlts_vIn16 ) -> (T_DataAlts_vOut16 )
data T_DataAlts_vIn16  = T_DataAlts_vIn16 (Bool) (TH.Bang)
data T_DataAlts_vOut16  = T_DataAlts_vOut16 ([TH.Con])
{-# NOINLINE sem_DataAlts_Cons #-}
sem_DataAlts_Cons :: T_DataAlt  -> T_DataAlts  -> T_DataAlts 
sem_DataAlts_Cons arg_hd_ arg_tl_ = T_DataAlts (return st17) where
   {-# NOINLINE st17 #-}
   !st17 = let
      v16 :: T_DataAlts_v16 
      v16 = \ !(T_DataAlts_vIn16 _lhsInested _lhsIstrictPre) -> ( let
         _hdX14 = Control.Monad.Identity.runIdentity (attach_T_DataAlt (arg_hd_))
         _tlX17 = Control.Monad.Identity.runIdentity (attach_T_DataAlts (arg_tl_))
         (T_DataAlt_vOut13 _hdIth) = inv_DataAlt_s14 _hdX14 (T_DataAlt_vIn13 _hdOnested _hdOstrictPre)
         (T_DataAlts_vOut16 _tlIth) = inv_DataAlts_s17 _tlX17 (T_DataAlts_vIn16 _tlOnested _tlOstrictPre)
         _lhsOth :: [TH.Con]
         _lhsOth = rule47 _hdIth _tlIth
         _hdOnested = rule48 _lhsInested
         _hdOstrictPre = rule49 _lhsIstrictPre
         _tlOnested = rule50 _lhsInested
         _tlOstrictPre = rule51 _lhsIstrictPre
         !__result_ = T_DataAlts_vOut16 _lhsOth
         in __result_ )
     in C_DataAlts_s17 v16
   {-# INLINE rule47 #-}
   rule47 = \ ((_hdIth) :: TH.Con) ((_tlIth) :: [TH.Con]) ->
     _hdIth : _tlIth
   {-# INLINE rule48 #-}
   rule48 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule49 #-}
   rule49 = \ ((_lhsIstrictPre) :: TH.Bang) ->
     _lhsIstrictPre
   {-# INLINE rule50 #-}
   rule50 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule51 #-}
   rule51 = \ ((_lhsIstrictPre) :: TH.Bang) ->
     _lhsIstrictPre
{-# NOINLINE sem_DataAlts_Nil #-}
sem_DataAlts_Nil ::  T_DataAlts 
sem_DataAlts_Nil  = T_DataAlts (return st17) where
   {-# NOINLINE st17 #-}
   !st17 = let
      v16 :: T_DataAlts_v16 
      v16 = \ !(T_DataAlts_vIn16 _lhsInested _lhsIstrictPre) -> ( let
         _lhsOth :: [TH.Con]
         _lhsOth = rule52  ()
         !__result_ = T_DataAlts_vOut16 _lhsOth
         in __result_ )
     in C_DataAlts_s17 v16
   {-# INLINE rule52 #-}
   rule52 = \  (_ :: ()) ->
     []

-- Decl --------------------------------------------------------
-- wrapper
data Inh_Decl  = Inh_Decl { isDeclOfLet_Inh_Decl :: !(Bool), nested_Inh_Decl :: !(Bool), options_Inh_Decl :: !(Options) }
data Syn_Decl  = Syn_Decl { stmt_Syn_Decl :: !(Maybe TH.Stmt), th_Syn_Decl :: !(Maybe TH.Dec) }
{-# INLINABLE wrap_Decl #-}
wrap_Decl :: T_Decl  -> Inh_Decl  -> (Syn_Decl )
wrap_Decl !(T_Decl act) !(Inh_Decl _lhsIisDeclOfLet _lhsInested _lhsIoptions) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg19 = T_Decl_vIn19 _lhsIisDeclOfLet _lhsInested _lhsIoptions
        !(T_Decl_vOut19 _lhsOstmt _lhsOth) <- return (inv_Decl_s20 sem arg19)
        return (Syn_Decl _lhsOstmt _lhsOth)
   )

-- cata
{-# NOINLINE sem_Decl #-}
sem_Decl :: Decl  -> T_Decl 
sem_Decl ( Decl left_ rhs_ !binds_ !uses_ ) = sem_Decl_Decl ( sem_Lhs left_ ) ( sem_Expr rhs_ ) binds_ uses_
sem_Decl ( Bind left_ rhs_ ) = sem_Decl_Bind ( sem_Lhs left_ ) ( sem_Expr rhs_ )
sem_Decl ( BindLet left_ rhs_ ) = sem_Decl_BindLet ( sem_Lhs left_ ) ( sem_Expr rhs_ )
sem_Decl ( Data !name_ !params_ alts_ !strict_ !derivings_ ) = sem_Decl_Data name_ params_ ( sem_DataAlts alts_ ) strict_ derivings_
sem_Decl ( NewType !name_ !params_ !con_ tp_ ) = sem_Decl_NewType name_ params_ con_ ( sem_Type tp_ )
sem_Decl ( Type !name_ !params_ tp_ ) = sem_Decl_Type name_ params_ ( sem_Type tp_ )
sem_Decl ( TSig !name_ tp_ ) = sem_Decl_TSig name_ ( sem_Type tp_ )
sem_Decl ( Comment !txt_ ) = sem_Decl_Comment txt_
sem_Decl ( PragmaDecl !txt_ ) = sem_Decl_PragmaDecl txt_
sem_Decl ( Resume !monadic_ !nt_ left_ rhs_ ) = sem_Decl_Resume monadic_ nt_ ( sem_Lhs left_ ) ( sem_Expr rhs_ )
sem_Decl ( EvalDecl !nt_ left_ rhs_ ) = sem_Decl_EvalDecl nt_ ( sem_Lhs left_ ) ( sem_Expr rhs_ )

-- semantic domain
newtype T_Decl  = T_Decl {
                         attach_T_Decl :: Identity (T_Decl_s20 )
                         }
newtype T_Decl_s20  = C_Decl_s20 {
                                 inv_Decl_s20 :: (T_Decl_v19 )
                                 }
data T_Decl_s21  = C_Decl_s21
type T_Decl_v19  = (T_Decl_vIn19 ) -> (T_Decl_vOut19 )
data T_Decl_vIn19  = T_Decl_vIn19 (Bool) (Bool) (Options)
data T_Decl_vOut19  = T_Decl_vOut19 (Maybe TH.Stmt) (Maybe TH.Dec)
{-# NOINLINE sem_Decl_Decl #-}
sem_Decl_Decl :: T_Lhs  -> T_Expr  -> (Set String) -> (Set String) -> T_Decl 
sem_Decl_Decl arg_left_ arg_rhs_ _ _ = T_Decl (return st20) where
   {-# NOINLINE st20 #-}
   !st20 = let
      v19 :: T_Decl_v19 
      v19 = \ !(T_Decl_vIn19 _lhsIisDeclOfLet _lhsInested _lhsIoptions) -> ( let
         _leftX32 = Control.Monad.Identity.runIdentity (attach_T_Lhs (arg_left_))
         _rhsX26 = Control.Monad.Identity.runIdentity (attach_T_Expr (arg_rhs_))
         (T_Lhs_vOut31 _leftIpat _leftIth) = inv_Lhs_s32 _leftX32 (T_Lhs_vIn31 _leftOisDeclOfLet _leftOnested _leftOoptions)
         (T_Expr_vOut25 _rhsIpat _rhsIth) = inv_Expr_s26 _rhsX26 (T_Expr_vIn25 _rhsOnested _rhsOoptions)
         _lhsOth :: Maybe TH.Dec
         _lhsOth = rule53 _leftIth _rhsIth
         _lhsOstmt :: Maybe TH.Stmt
         _lhsOstmt = rule54  ()
         _leftOisDeclOfLet = rule55 _lhsIisDeclOfLet
         _leftOnested = rule56 _lhsInested
         _leftOoptions = rule57 _lhsIoptions
         _rhsOnested = rule58 _lhsInested
         _rhsOoptions = rule59 _lhsIoptions
         !__result_ = T_Decl_vOut19 _lhsOstmt _lhsOth
         in __result_ )
     in C_Decl_s20 v19
   {-# INLINE rule53 #-}
   {-# LINE 124 "src-ag/Code2TH.ag" #-}
   rule53 = \ ((_leftIth) :: TH.Exp -> TH.Dec) ((_rhsIth) :: TH.Exp) ->
                               {-# LINE 124 "src-ag/Code2TH.ag" #-}
                               pure $ _leftIth _rhsIth
                               {-# LINE 741 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule54 #-}
   rule54 = \  (_ :: ()) ->
     Nothing
   {-# INLINE rule55 #-}
   rule55 = \ ((_lhsIisDeclOfLet) :: Bool) ->
     _lhsIisDeclOfLet
   {-# INLINE rule56 #-}
   rule56 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule57 #-}
   rule57 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule58 #-}
   rule58 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule59 #-}
   rule59 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Decl_Bind #-}
sem_Decl_Bind :: T_Lhs  -> T_Expr  -> T_Decl 
sem_Decl_Bind arg_left_ arg_rhs_ = T_Decl (return st20) where
   {-# NOINLINE st20 #-}
   !st20 = let
      v19 :: T_Decl_v19 
      v19 = \ !(T_Decl_vIn19 _lhsIisDeclOfLet _lhsInested _lhsIoptions) -> ( let
         _leftX32 = Control.Monad.Identity.runIdentity (attach_T_Lhs (arg_left_))
         _rhsX26 = Control.Monad.Identity.runIdentity (attach_T_Expr (arg_rhs_))
         (T_Lhs_vOut31 _leftIpat _leftIth) = inv_Lhs_s32 _leftX32 (T_Lhs_vIn31 _leftOisDeclOfLet _leftOnested _leftOoptions)
         (T_Expr_vOut25 _rhsIpat _rhsIth) = inv_Expr_s26 _rhsX26 (T_Expr_vIn25 _rhsOnested _rhsOoptions)
         _lhsOstmt :: Maybe TH.Stmt
         _lhsOstmt = rule60 _leftIpat _rhsIth
         _lhsOth :: Maybe TH.Dec
         _lhsOth = rule61 _leftIth _rhsIth
         _leftOisDeclOfLet = rule62 _lhsIisDeclOfLet
         _leftOnested = rule63 _lhsInested
         _leftOoptions = rule64 _lhsIoptions
         _rhsOnested = rule65 _lhsInested
         _rhsOoptions = rule66 _lhsIoptions
         !__result_ = T_Decl_vOut19 _lhsOstmt _lhsOth
         in __result_ )
     in C_Decl_s20 v19
   {-# INLINE rule60 #-}
   {-# LINE 125 "src-ag/Code2TH.ag" #-}
   rule60 = \ ((_leftIpat) :: TH.Pat) ((_rhsIth) :: TH.Exp) ->
                               {-# LINE 125 "src-ag/Code2TH.ag" #-}
                               pure $ TH.BindS _leftIpat _rhsIth
                               {-# LINE 788 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule61 #-}
   rule61 = \ ((_leftIth) :: TH.Exp -> TH.Dec) ((_rhsIth) :: TH.Exp) ->
     (error "can't combine decl th" _leftIth _rhsIth)
   {-# INLINE rule62 #-}
   rule62 = \ ((_lhsIisDeclOfLet) :: Bool) ->
     _lhsIisDeclOfLet
   {-# INLINE rule63 #-}
   rule63 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule64 #-}
   rule64 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule65 #-}
   rule65 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule66 #-}
   rule66 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Decl_BindLet #-}
sem_Decl_BindLet :: T_Lhs  -> T_Expr  -> T_Decl 
sem_Decl_BindLet arg_left_ arg_rhs_ = T_Decl (return st20) where
   {-# NOINLINE st20 #-}
   !st20 = let
      v19 :: T_Decl_v19 
      v19 = \ !(T_Decl_vIn19 _lhsIisDeclOfLet _lhsInested _lhsIoptions) -> ( let
         _leftX32 = Control.Monad.Identity.runIdentity (attach_T_Lhs (arg_left_))
         _rhsX26 = Control.Monad.Identity.runIdentity (attach_T_Expr (arg_rhs_))
         (T_Lhs_vOut31 _leftIpat _leftIth) = inv_Lhs_s32 _leftX32 (T_Lhs_vIn31 _leftOisDeclOfLet _leftOnested _leftOoptions)
         (T_Expr_vOut25 _rhsIpat _rhsIth) = inv_Expr_s26 _rhsX26 (T_Expr_vIn25 _rhsOnested _rhsOoptions)
         _lhsOstmt :: Maybe TH.Stmt
         _lhsOstmt = rule67 _leftIth _rhsIth
         _lhsOth :: Maybe TH.Dec
         _lhsOth = rule68 _leftIth _rhsIth
         _leftOisDeclOfLet = rule69 _lhsIisDeclOfLet
         _leftOnested = rule70 _lhsInested
         _leftOoptions = rule71 _lhsIoptions
         _rhsOnested = rule72 _lhsInested
         _rhsOoptions = rule73 _lhsIoptions
         !__result_ = T_Decl_vOut19 _lhsOstmt _lhsOth
         in __result_ )
     in C_Decl_s20 v19
   {-# INLINE rule67 #-}
   {-# LINE 126 "src-ag/Code2TH.ag" #-}
   rule67 = \ ((_leftIth) :: TH.Exp -> TH.Dec) ((_rhsIth) :: TH.Exp) ->
                               {-# LINE 126 "src-ag/Code2TH.ag" #-}
                               pure $ TH.LetS [_leftIth _rhsIth]
                               {-# LINE 835 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule68 #-}
   rule68 = \ ((_leftIth) :: TH.Exp -> TH.Dec) ((_rhsIth) :: TH.Exp) ->
     (error "can't combine decl th" _leftIth _rhsIth)
   {-# INLINE rule69 #-}
   rule69 = \ ((_lhsIisDeclOfLet) :: Bool) ->
     _lhsIisDeclOfLet
   {-# INLINE rule70 #-}
   rule70 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule71 #-}
   rule71 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule72 #-}
   rule72 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule73 #-}
   rule73 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Decl_Data #-}
sem_Decl_Data :: (String) -> ([String]) -> T_DataAlts  -> (Bool) -> ([String]) -> T_Decl 
sem_Decl_Data !arg_name_ !arg_params_ arg_alts_ !arg_strict_ !arg_derivings_ = T_Decl (return st20) where
   {-# NOINLINE st20 #-}
   !st20 = let
      v19 :: T_Decl_v19 
      v19 = \ !(T_Decl_vIn19 _lhsIisDeclOfLet _lhsInested _lhsIoptions) -> ( let
         _altsX17 = Control.Monad.Identity.runIdentity (attach_T_DataAlts (arg_alts_))
         (T_DataAlts_vOut16 _altsIth) = inv_DataAlts_s17 _altsX17 (T_DataAlts_vIn16 _altsOnested _altsOstrictPre)
         _lhsOth :: Maybe TH.Dec
         _lhsOth = rule74 _altsIth arg_derivings_ arg_name_ arg_params_
         _altsOstrictPre = rule75 arg_strict_
         _lhsOstmt :: Maybe TH.Stmt
         _lhsOstmt = rule76  ()
         _altsOnested = rule77 _lhsInested
         !__result_ = T_Decl_vOut19 _lhsOstmt _lhsOth
         in __result_ )
     in C_Decl_s20 v19
   {-# INLINE rule74 #-}
   {-# LINE 127 "src-ag/Code2TH.ag" #-}
   rule74 = \ ((_altsIth) :: [TH.Con]) derivings_ name_ params_ ->
                               {-# LINE 127 "src-ag/Code2TH.ag" #-}
                               pure $ TH.DataD []
                                               (TH.mkName name_)
                                               (map (TH.PlainTV . TH.mkName) params_)
                                               Nothing
                                               _altsIth
                                               [TH.DerivClause Nothing (map (TH.ConT . TH.mkName) derivings_)]
                               {-# LINE 882 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule75 #-}
   {-# LINE 326 "src-ag/Code2TH.ag" #-}
   rule75 = \ strict_ ->
                            {-# LINE 326 "src-ag/Code2TH.ag" #-}
                            if strict_ then TH.Bang TH.NoSourceUnpackedness TH.SourceStrict
                                       else TH.Bang TH.NoSourceUnpackedness TH.NoSourceStrictness
                            {-# LINE 889 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule76 #-}
   rule76 = \  (_ :: ()) ->
     Nothing
   {-# INLINE rule77 #-}
   rule77 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
{-# NOINLINE sem_Decl_NewType #-}
sem_Decl_NewType :: (String) -> ([String]) -> (String) -> T_Type  -> T_Decl 
sem_Decl_NewType !arg_name_ !arg_params_ !arg_con_ arg_tp_ = T_Decl (return st20) where
   {-# NOINLINE st20 #-}
   !st20 = let
      v19 :: T_Decl_v19 
      v19 = \ !(T_Decl_vIn19 _lhsIisDeclOfLet _lhsInested _lhsIoptions) -> ( let
         _tpX50 = Control.Monad.Identity.runIdentity (attach_T_Type (arg_tp_))
         (T_Type_vOut49 _tpIth) = inv_Type_s50 _tpX50 (T_Type_vIn49 _tpOnested)
         _lhsOth :: Maybe TH.Dec
         _lhsOth = rule78 _tpIth arg_con_ arg_name_ arg_params_
         _lhsOstmt :: Maybe TH.Stmt
         _lhsOstmt = rule79  ()
         _tpOnested = rule80 _lhsInested
         !__result_ = T_Decl_vOut19 _lhsOstmt _lhsOth
         in __result_ )
     in C_Decl_s20 v19
   {-# INLINE rule78 #-}
   {-# LINE 133 "src-ag/Code2TH.ag" #-}
   rule78 = \ ((_tpIth) :: TH.Type) con_ name_ params_ ->
                               {-# LINE 133 "src-ag/Code2TH.ag" #-}
                               pure $ TH.NewtypeD []
                                                  (TH.mkName name_)
                                                  (map (TH.PlainTV . TH.mkName) params_)
                                                  Nothing
                                                  (TH.NormalC (TH.mkName con_)
                                                    [(TH.Bang TH.NoSourceUnpackedness TH.NoSourceStrictness, _tpIth)])
                                                  []
                               {-# LINE 924 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule79 #-}
   rule79 = \  (_ :: ()) ->
     Nothing
   {-# INLINE rule80 #-}
   rule80 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
{-# NOINLINE sem_Decl_Type #-}
sem_Decl_Type :: (String) -> ([String]) -> T_Type  -> T_Decl 
sem_Decl_Type !arg_name_ !arg_params_ arg_tp_ = T_Decl (return st20) where
   {-# NOINLINE st20 #-}
   !st20 = let
      v19 :: T_Decl_v19 
      v19 = \ !(T_Decl_vIn19 _lhsIisDeclOfLet _lhsInested _lhsIoptions) -> ( let
         _tpX50 = Control.Monad.Identity.runIdentity (attach_T_Type (arg_tp_))
         (T_Type_vOut49 _tpIth) = inv_Type_s50 _tpX50 (T_Type_vIn49 _tpOnested)
         _lhsOth :: Maybe TH.Dec
         _lhsOth = rule81 _tpIth arg_name_ arg_params_
         _lhsOstmt :: Maybe TH.Stmt
         _lhsOstmt = rule82  ()
         _tpOnested = rule83 _lhsInested
         !__result_ = T_Decl_vOut19 _lhsOstmt _lhsOth
         in __result_ )
     in C_Decl_s20 v19
   {-# INLINE rule81 #-}
   {-# LINE 140 "src-ag/Code2TH.ag" #-}
   rule81 = \ ((_tpIth) :: TH.Type) name_ params_ ->
                               {-# LINE 140 "src-ag/Code2TH.ag" #-}
                               pure $ TH.TySynD (TH.mkName name_)
                                                (map (TH.PlainTV . TH.mkName) params_)
                                                _tpIth
                               {-# LINE 955 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule82 #-}
   rule82 = \  (_ :: ()) ->
     Nothing
   {-# INLINE rule83 #-}
   rule83 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
{-# NOINLINE sem_Decl_TSig #-}
sem_Decl_TSig :: (String) -> T_Type  -> T_Decl 
sem_Decl_TSig !arg_name_ arg_tp_ = T_Decl (return st20) where
   {-# NOINLINE st20 #-}
   !st20 = let
      v19 :: T_Decl_v19 
      v19 = \ !(T_Decl_vIn19 _lhsIisDeclOfLet _lhsInested _lhsIoptions) -> ( let
         _tpX50 = Control.Monad.Identity.runIdentity (attach_T_Type (arg_tp_))
         (T_Type_vOut49 _tpIth) = inv_Type_s50 _tpX50 (T_Type_vIn49 _tpOnested)
         _lhsOth :: Maybe TH.Dec
         _lhsOth = rule84 _lhsIisDeclOfLet _tpIth arg_name_
         _lhsOstmt :: Maybe TH.Stmt
         _lhsOstmt = rule85  ()
         _tpOnested = rule86 _lhsInested
         !__result_ = T_Decl_vOut19 _lhsOstmt _lhsOth
         in __result_ )
     in C_Decl_s20 v19
   {-# INLINE rule84 #-}
   {-# LINE 143 "src-ag/Code2TH.ag" #-}
   rule84 = \ ((_lhsIisDeclOfLet) :: Bool) ((_tpIth) :: TH.Type) name_ ->
                               {-# LINE 143 "src-ag/Code2TH.ag" #-}
                               if _lhsIisDeclOfLet then empty else pure $ TH.SigD (TH.mkName name_) _tpIth
                               {-# LINE 984 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule85 #-}
   rule85 = \  (_ :: ()) ->
     Nothing
   {-# INLINE rule86 #-}
   rule86 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
{-# NOINLINE sem_Decl_Comment #-}
sem_Decl_Comment :: (String) -> T_Decl 
sem_Decl_Comment _ = T_Decl (return st20) where
   {-# NOINLINE st20 #-}
   !st20 = let
      v19 :: T_Decl_v19 
      v19 = \ !(T_Decl_vIn19 _lhsIisDeclOfLet _lhsInested _lhsIoptions) -> ( let
         _lhsOstmt :: Maybe TH.Stmt
         _lhsOstmt = rule87  ()
         _lhsOth :: Maybe TH.Dec
         _lhsOth = rule88  ()
         !__result_ = T_Decl_vOut19 _lhsOstmt _lhsOth
         in __result_ )
     in C_Decl_s20 v19
   {-# INLINE rule87 #-}
   rule87 = \  (_ :: ()) ->
     Nothing
   {-# INLINE rule88 #-}
   rule88 = \  (_ :: ()) ->
     Nothing
{-# NOINLINE sem_Decl_PragmaDecl #-}
sem_Decl_PragmaDecl :: (String) -> T_Decl 
sem_Decl_PragmaDecl _ = T_Decl (return st20) where
   {-# NOINLINE st20 #-}
   !st20 = let
      v19 :: T_Decl_v19 
      v19 = \ !(T_Decl_vIn19 _lhsIisDeclOfLet _lhsInested _lhsIoptions) -> ( let
         _lhsOstmt :: Maybe TH.Stmt
         _lhsOstmt = rule89  ()
         _lhsOth :: Maybe TH.Dec
         _lhsOth = rule90  ()
         !__result_ = T_Decl_vOut19 _lhsOstmt _lhsOth
         in __result_ )
     in C_Decl_s20 v19
   {-# INLINE rule89 #-}
   rule89 = \  (_ :: ()) ->
     Nothing
   {-# INLINE rule90 #-}
   rule90 = \  (_ :: ()) ->
     Nothing
{-# NOINLINE sem_Decl_Resume #-}
sem_Decl_Resume :: (Bool) -> (String) -> T_Lhs  -> T_Expr  -> T_Decl 
sem_Decl_Resume !arg_monadic_ _ arg_left_ arg_rhs_ = T_Decl (return st20) where
   {-# NOINLINE st20 #-}
   !st20 = let
      v19 :: T_Decl_v19 
      v19 = \ !(T_Decl_vIn19 _lhsIisDeclOfLet _lhsInested _lhsIoptions) -> ( let
         _leftX32 = Control.Monad.Identity.runIdentity (attach_T_Lhs (arg_left_))
         _rhsX26 = Control.Monad.Identity.runIdentity (attach_T_Expr (arg_rhs_))
         (T_Lhs_vOut31 _leftIpat _leftIth) = inv_Lhs_s32 _leftX32 (T_Lhs_vIn31 _leftOisDeclOfLet _leftOnested _leftOoptions)
         (T_Expr_vOut25 _rhsIpat _rhsIth) = inv_Expr_s26 _rhsX26 (T_Expr_vIn25 _rhsOnested _rhsOoptions)
         _lhsOth :: Maybe TH.Dec
         _lhsOstmt :: Maybe TH.Stmt
         (_lhsOth,_lhsOstmt) = rule91 _leftIpat _leftIth _rhsIth arg_monadic_
         _leftOisDeclOfLet = rule92 _lhsIisDeclOfLet
         _leftOnested = rule93 _lhsInested
         _leftOoptions = rule94 _lhsIoptions
         _rhsOnested = rule95 _lhsInested
         _rhsOoptions = rule96 _lhsIoptions
         !__result_ = T_Decl_vOut19 _lhsOstmt _lhsOth
         in __result_ )
     in C_Decl_s20 v19
   {-# INLINE rule91 #-}
   {-# LINE 146 "src-ag/Code2TH.ag" #-}
   rule91 = \ ((_leftIpat) :: TH.Pat) ((_leftIth) :: TH.Exp -> TH.Dec) ((_rhsIth) :: TH.Exp) monadic_ ->
                               {-# LINE 146 "src-ag/Code2TH.ag" #-}
                               if monadic_
                               then (Nothing, Just (TH.BindS _leftIpat _rhsIth))
                               else (Just (_leftIth _rhsIth), Nothing)
                               {-# LINE 1060 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule92 #-}
   rule92 = \ ((_lhsIisDeclOfLet) :: Bool) ->
     _lhsIisDeclOfLet
   {-# INLINE rule93 #-}
   rule93 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule94 #-}
   rule94 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule95 #-}
   rule95 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule96 #-}
   rule96 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Decl_EvalDecl #-}
sem_Decl_EvalDecl :: (String) -> T_Lhs  -> T_Expr  -> T_Decl 
sem_Decl_EvalDecl !arg_nt_ arg_left_ arg_rhs_ = T_Decl (return st20) where
   {-# NOINLINE st20 #-}
   !st20 = let
      v19 :: T_Decl_v19 
      v19 = \ !(T_Decl_vIn19 _lhsIisDeclOfLet _lhsInested _lhsIoptions) -> ( let
         _leftX32 = Control.Monad.Identity.runIdentity (attach_T_Lhs (arg_left_))
         _rhsX26 = Control.Monad.Identity.runIdentity (attach_T_Expr (arg_rhs_))
         (T_Lhs_vOut31 _leftIpat _leftIth) = inv_Lhs_s32 _leftX32 (T_Lhs_vIn31 _leftOisDeclOfLet _leftOnested _leftOoptions)
         (T_Expr_vOut25 _rhsIpat _rhsIth) = inv_Expr_s26 _rhsX26 (T_Expr_vIn25 _rhsOnested _rhsOoptions)
         _strat = rule97 _lhsIoptions
         _lhsOth :: Maybe TH.Dec
         _lhsOth = rule98 _leftIth _lhsIoptions _rhsIth _strat arg_nt_
         _lhsOstmt :: Maybe TH.Stmt
         _lhsOstmt = rule99  ()
         _leftOisDeclOfLet = rule100 _lhsIisDeclOfLet
         _leftOnested = rule101 _lhsInested
         _leftOoptions = rule102 _lhsIoptions
         _rhsOnested = rule103 _lhsInested
         _rhsOoptions = rule104 _lhsIoptions
         !__result_ = T_Decl_vOut19 _lhsOstmt _lhsOth
         in __result_ )
     in C_Decl_s20 v19
   {-# INLINE rule97 #-}
   {-# LINE 149 "src-ag/Code2TH.ag" #-}
   rule97 = \ ((_lhsIoptions) :: Options) ->
                               {-# LINE 149 "src-ag/Code2TH.ag" #-}
                               if breadthFirstStrict _lhsIoptions
                               then TH.VarE (TH.mkName "stepwiseEval")
                               else TH.VarE (TH.mkName "lazyEval")
                               {-# LINE 1107 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule98 #-}
   {-# LINE 152 "src-ag/Code2TH.ag" #-}
   rule98 = \ ((_leftIth) :: TH.Exp -> TH.Dec) ((_lhsIoptions) :: Options) ((_rhsIth) :: TH.Exp) _strat nt_ ->
                               {-# LINE 152 "src-ag/Code2TH.ag" #-}
                               pure $ if breadthFirst _lhsIoptions
                               then _leftIth $ TH.CaseE (TH.AppE _strat     _rhsIth)
                                    [ TH.Match (TH.ConP (TH.mkName (nt_ ++ "_Syn"))
                                                 [TH.VarP (TH.mkName "_val")])
                                               (TH.NormalB (TH.VarE (TH.mkName "_val")))
                                               []
                                    ]
                               else _leftIth _rhsIth
                               {-# LINE 1120 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule99 #-}
   rule99 = \  (_ :: ()) ->
     Nothing
   {-# INLINE rule100 #-}
   rule100 = \ ((_lhsIisDeclOfLet) :: Bool) ->
     _lhsIisDeclOfLet
   {-# INLINE rule101 #-}
   rule101 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule102 #-}
   rule102 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule103 #-}
   rule103 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule104 #-}
   rule104 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions

-- Decls -------------------------------------------------------
-- wrapper
data Inh_Decls  = Inh_Decls { isDeclOfLet_Inh_Decls :: !(Bool), nested_Inh_Decls :: !(Bool), options_Inh_Decls :: !(Options) }
data Syn_Decls  = Syn_Decls { stmt_Syn_Decls :: !([TH.Stmt]), th_Syn_Decls :: !([TH.Dec]) }
{-# INLINABLE wrap_Decls #-}
wrap_Decls :: T_Decls  -> Inh_Decls  -> (Syn_Decls )
wrap_Decls !(T_Decls act) !(Inh_Decls _lhsIisDeclOfLet _lhsInested _lhsIoptions) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg22 = T_Decls_vIn22 _lhsIisDeclOfLet _lhsInested _lhsIoptions
        !(T_Decls_vOut22 _lhsOstmt _lhsOth) <- return (inv_Decls_s23 sem arg22)
        return (Syn_Decls _lhsOstmt _lhsOth)
   )

-- cata
{-# NOINLINE sem_Decls #-}
sem_Decls :: Decls  -> T_Decls 
sem_Decls list = Prelude.foldr sem_Decls_Cons sem_Decls_Nil (Prelude.map sem_Decl list)

-- semantic domain
newtype T_Decls  = T_Decls {
                           attach_T_Decls :: Identity (T_Decls_s23 )
                           }
newtype T_Decls_s23  = C_Decls_s23 {
                                   inv_Decls_s23 :: (T_Decls_v22 )
                                   }
data T_Decls_s24  = C_Decls_s24
type T_Decls_v22  = (T_Decls_vIn22 ) -> (T_Decls_vOut22 )
data T_Decls_vIn22  = T_Decls_vIn22 (Bool) (Bool) (Options)
data T_Decls_vOut22  = T_Decls_vOut22 ([TH.Stmt]) ([TH.Dec])
{-# NOINLINE sem_Decls_Cons #-}
sem_Decls_Cons :: T_Decl  -> T_Decls  -> T_Decls 
sem_Decls_Cons arg_hd_ arg_tl_ = T_Decls (return st23) where
   {-# NOINLINE st23 #-}
   !st23 = let
      v22 :: T_Decls_v22 
      v22 = \ !(T_Decls_vIn22 _lhsIisDeclOfLet _lhsInested _lhsIoptions) -> ( let
         _hdX20 = Control.Monad.Identity.runIdentity (attach_T_Decl (arg_hd_))
         _tlX23 = Control.Monad.Identity.runIdentity (attach_T_Decls (arg_tl_))
         (T_Decl_vOut19 _hdIstmt _hdIth) = inv_Decl_s20 _hdX20 (T_Decl_vIn19 _hdOisDeclOfLet _hdOnested _hdOoptions)
         (T_Decls_vOut22 _tlIstmt _tlIth) = inv_Decls_s23 _tlX23 (T_Decls_vIn22 _tlOisDeclOfLet _tlOnested _tlOoptions)
         _lhsOstmt :: [TH.Stmt]
         _lhsOstmt = rule105 _hdIstmt _tlIstmt
         _lhsOth :: [TH.Dec]
         _lhsOth = rule106 _hdIth _tlIth
         _hdOisDeclOfLet = rule107 _lhsIisDeclOfLet
         _hdOnested = rule108 _lhsInested
         _hdOoptions = rule109 _lhsIoptions
         _tlOisDeclOfLet = rule110 _lhsIisDeclOfLet
         _tlOnested = rule111 _lhsInested
         _tlOoptions = rule112 _lhsIoptions
         !__result_ = T_Decls_vOut22 _lhsOstmt _lhsOth
         in __result_ )
     in C_Decls_s23 v22
   {-# INLINE rule105 #-}
   rule105 = \ ((_hdIstmt) :: Maybe TH.Stmt) ((_tlIstmt) :: [TH.Stmt]) ->
     (maybe id (:) _hdIstmt _tlIstmt)
   {-# INLINE rule106 #-}
   rule106 = \ ((_hdIth) :: Maybe TH.Dec) ((_tlIth) :: [TH.Dec]) ->
     (maybe id (:) _hdIth _tlIth)
   {-# INLINE rule107 #-}
   rule107 = \ ((_lhsIisDeclOfLet) :: Bool) ->
     _lhsIisDeclOfLet
   {-# INLINE rule108 #-}
   rule108 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule109 #-}
   rule109 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule110 #-}
   rule110 = \ ((_lhsIisDeclOfLet) :: Bool) ->
     _lhsIisDeclOfLet
   {-# INLINE rule111 #-}
   rule111 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule112 #-}
   rule112 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Decls_Nil #-}
sem_Decls_Nil ::  T_Decls 
sem_Decls_Nil  = T_Decls (return st23) where
   {-# NOINLINE st23 #-}
   !st23 = let
      v22 :: T_Decls_v22 
      v22 = \ !(T_Decls_vIn22 _lhsIisDeclOfLet _lhsInested _lhsIoptions) -> ( let
         _lhsOstmt :: [TH.Stmt]
         _lhsOstmt = rule113  ()
         _lhsOth :: [TH.Dec]
         _lhsOth = rule114  ()
         !__result_ = T_Decls_vOut22 _lhsOstmt _lhsOth
         in __result_ )
     in C_Decls_s23 v22
   {-# INLINE rule113 #-}
   rule113 = \  (_ :: ()) ->
     []
   {-# INLINE rule114 #-}
   rule114 = \  (_ :: ()) ->
     []

-- Expr --------------------------------------------------------
-- wrapper
data Inh_Expr  = Inh_Expr { nested_Inh_Expr :: !(Bool), options_Inh_Expr :: !(Options) }
data Syn_Expr  = Syn_Expr { pat_Syn_Expr :: !(Maybe TH.Pat), th_Syn_Expr :: !(TH.Exp) }
{-# INLINABLE wrap_Expr #-}
wrap_Expr :: T_Expr  -> Inh_Expr  -> (Syn_Expr )
wrap_Expr !(T_Expr act) !(Inh_Expr _lhsInested _lhsIoptions) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg25 = T_Expr_vIn25 _lhsInested _lhsIoptions
        !(T_Expr_vOut25 _lhsOpat _lhsOth) <- return (inv_Expr_s26 sem arg25)
        return (Syn_Expr _lhsOpat _lhsOth)
   )

-- cata
{-# NOINLINE sem_Expr #-}
sem_Expr :: Expr  -> T_Expr 
sem_Expr ( Let decls_ body_ ) = sem_Expr_Let ( sem_Decls decls_ ) ( sem_Expr body_ )
sem_Expr ( Case expr_ alts_ ) = sem_Expr_Case ( sem_Expr expr_ ) ( sem_CaseAlts alts_ )
sem_Expr ( Do stmts_ body_ ) = sem_Expr_Do ( sem_Decls stmts_ ) ( sem_Expr body_ )
sem_Expr ( Lambda args_ body_ ) = sem_Expr_Lambda ( sem_Exprs args_ ) ( sem_Expr body_ )
sem_Expr ( TupleExpr exprs_ ) = sem_Expr_TupleExpr ( sem_Exprs exprs_ )
sem_Expr ( UnboxedTupleExpr exprs_ ) = sem_Expr_UnboxedTupleExpr ( sem_Exprs exprs_ )
sem_Expr ( App !name_ args_ ) = sem_Expr_App name_ ( sem_Exprs args_ )
sem_Expr ( SimpleExpr !txt_ ) = sem_Expr_SimpleExpr txt_
sem_Expr ( TextExpr !lns_ ) = sem_Expr_TextExpr lns_
sem_Expr ( Trace !txt_ expr_ ) = sem_Expr_Trace txt_ ( sem_Expr expr_ )
sem_Expr ( PragmaExpr !onLeftSide_ !onNewLine_ !txt_ expr_ ) = sem_Expr_PragmaExpr onLeftSide_ onNewLine_ txt_ ( sem_Expr expr_ )
sem_Expr ( LineExpr expr_ ) = sem_Expr_LineExpr ( sem_Expr expr_ )
sem_Expr ( TypedExpr expr_ tp_ ) = sem_Expr_TypedExpr ( sem_Expr expr_ ) ( sem_Type tp_ )
sem_Expr ( ResultExpr !nt_ expr_ ) = sem_Expr_ResultExpr nt_ ( sem_Expr expr_ )
sem_Expr ( InvokeExpr !nt_ expr_ args_ ) = sem_Expr_InvokeExpr nt_ ( sem_Expr expr_ ) ( sem_Exprs args_ )
sem_Expr ( ResumeExpr !nt_ expr_ left_ rhs_ ) = sem_Expr_ResumeExpr nt_ ( sem_Expr expr_ ) ( sem_Lhs left_ ) ( sem_Expr rhs_ )
sem_Expr ( SemFun !nt_ args_ body_ ) = sem_Expr_SemFun nt_ ( sem_Exprs args_ ) ( sem_Expr body_ )

-- semantic domain
newtype T_Expr  = T_Expr {
                         attach_T_Expr :: Identity (T_Expr_s26 )
                         }
newtype T_Expr_s26  = C_Expr_s26 {
                                 inv_Expr_s26 :: (T_Expr_v25 )
                                 }
data T_Expr_s27  = C_Expr_s27
type T_Expr_v25  = (T_Expr_vIn25 ) -> (T_Expr_vOut25 )
data T_Expr_vIn25  = T_Expr_vIn25 (Bool) (Options)
data T_Expr_vOut25  = T_Expr_vOut25 (Maybe TH.Pat) (TH.Exp)
{-# NOINLINE sem_Expr_Let #-}
sem_Expr_Let :: T_Decls  -> T_Expr  -> T_Expr 
sem_Expr_Let arg_decls_ arg_body_ = T_Expr (return st26) where
   {-# NOINLINE st26 #-}
   !st26 = let
      v25 :: T_Expr_v25 
      v25 = \ !(T_Expr_vIn25 _lhsInested _lhsIoptions) -> ( let
         _declsX23 = Control.Monad.Identity.runIdentity (attach_T_Decls (arg_decls_))
         _bodyX26 = Control.Monad.Identity.runIdentity (attach_T_Expr (arg_body_))
         (T_Decls_vOut22 _declsIstmt _declsIth) = inv_Decls_s23 _declsX23 (T_Decls_vIn22 _declsOisDeclOfLet _declsOnested _declsOoptions)
         (T_Expr_vOut25 _bodyIpat _bodyIth) = inv_Expr_s26 _bodyX26 (T_Expr_vIn25 _bodyOnested _bodyOoptions)
         _lhsOth :: TH.Exp
         _lhsOth = rule115 _bodyIth _declsIth
         _lhsOpat :: Maybe TH.Pat
         _lhsOpat = rule116  ()
         _declsOisDeclOfLet = rule117  ()
         _declsOnested = rule118 _lhsInested
         _declsOoptions = rule119 _lhsIoptions
         _bodyOnested = rule120 _lhsInested
         _bodyOoptions = rule121 _lhsIoptions
         !__result_ = T_Expr_vOut25 _lhsOpat _lhsOth
         in __result_ )
     in C_Expr_s26 v25
   {-# INLINE rule115 #-}
   {-# LINE 162 "src-ag/Code2TH.ag" #-}
   rule115 = \ ((_bodyIth) :: TH.Exp) ((_declsIth) :: [TH.Dec]) ->
                               {-# LINE 162 "src-ag/Code2TH.ag" #-}
                               TH.LetE _declsIth _bodyIth
                               {-# LINE 1313 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule116 #-}
   {-# LINE 163 "src-ag/Code2TH.ag" #-}
   rule116 = \  (_ :: ()) ->
                               {-# LINE 163 "src-ag/Code2TH.ag" #-}
                               error "Cannot use let expression in pattern"
                               {-# LINE 1319 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule117 #-}
   {-# LINE 417 "src-ag/Code2TH.ag" #-}
   rule117 = \  (_ :: ()) ->
                            {-# LINE 417 "src-ag/Code2TH.ag" #-}
                            True
                            {-# LINE 1325 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule118 #-}
   rule118 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule119 #-}
   rule119 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule120 #-}
   rule120 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule121 #-}
   rule121 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Expr_Case #-}
sem_Expr_Case :: T_Expr  -> T_CaseAlts  -> T_Expr 
sem_Expr_Case arg_expr_ arg_alts_ = T_Expr (return st26) where
   {-# NOINLINE st26 #-}
   !st26 = let
      v25 :: T_Expr_v25 
      v25 = \ !(T_Expr_vIn25 _lhsInested _lhsIoptions) -> ( let
         _exprX26 = Control.Monad.Identity.runIdentity (attach_T_Expr (arg_expr_))
         _altsX5 = Control.Monad.Identity.runIdentity (attach_T_CaseAlts (arg_alts_))
         (T_Expr_vOut25 _exprIpat _exprIth) = inv_Expr_s26 _exprX26 (T_Expr_vIn25 _exprOnested _exprOoptions)
         (T_CaseAlts_vOut4 _altsIth) = inv_CaseAlts_s5 _altsX5 (T_CaseAlts_vIn4 _altsOnested _altsOoptions)
         _lhsOth :: TH.Exp
         _lhsOth = rule122 _altsIth _exprIth
         _lhsOpat :: Maybe TH.Pat
         _lhsOpat = rule123  ()
         _exprOnested = rule124 _lhsInested
         _exprOoptions = rule125 _lhsIoptions
         _altsOnested = rule126 _lhsInested
         _altsOoptions = rule127 _lhsIoptions
         !__result_ = T_Expr_vOut25 _lhsOpat _lhsOth
         in __result_ )
     in C_Expr_s26 v25
   {-# INLINE rule122 #-}
   {-# LINE 164 "src-ag/Code2TH.ag" #-}
   rule122 = \ ((_altsIth) :: [TH.Match]) ((_exprIth) :: TH.Exp) ->
                               {-# LINE 164 "src-ag/Code2TH.ag" #-}
                               TH.CaseE _exprIth _altsIth
                               {-# LINE 1365 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule123 #-}
   {-# LINE 165 "src-ag/Code2TH.ag" #-}
   rule123 = \  (_ :: ()) ->
                               {-# LINE 165 "src-ag/Code2TH.ag" #-}
                               error "Cannot use case expression in pattern"
                               {-# LINE 1371 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule124 #-}
   rule124 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule125 #-}
   rule125 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule126 #-}
   rule126 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule127 #-}
   rule127 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Expr_Do #-}
sem_Expr_Do :: T_Decls  -> T_Expr  -> T_Expr 
sem_Expr_Do arg_stmts_ arg_body_ = T_Expr (return st26) where
   {-# NOINLINE st26 #-}
   !st26 = let
      v25 :: T_Expr_v25 
      v25 = \ !(T_Expr_vIn25 _lhsInested _lhsIoptions) -> ( let
         _stmtsX23 = Control.Monad.Identity.runIdentity (attach_T_Decls (arg_stmts_))
         _bodyX26 = Control.Monad.Identity.runIdentity (attach_T_Expr (arg_body_))
         (T_Decls_vOut22 _stmtsIstmt _stmtsIth) = inv_Decls_s23 _stmtsX23 (T_Decls_vIn22 _stmtsOisDeclOfLet _stmtsOnested _stmtsOoptions)
         (T_Expr_vOut25 _bodyIpat _bodyIth) = inv_Expr_s26 _bodyX26 (T_Expr_vIn25 _bodyOnested _bodyOoptions)
         _lhsOth :: TH.Exp
         _lhsOth = rule128 _bodyIth _stmtsIstmt
         _lhsOpat :: Maybe TH.Pat
         _lhsOpat = rule129  ()
         _stmtsOisDeclOfLet = rule130  ()
         _stmtsOnested = rule131 _lhsInested
         _stmtsOoptions = rule132 _lhsIoptions
         _bodyOnested = rule133 _lhsInested
         _bodyOoptions = rule134 _lhsIoptions
         !__result_ = T_Expr_vOut25 _lhsOpat _lhsOth
         in __result_ )
     in C_Expr_s26 v25
   {-# INLINE rule128 #-}
   {-# LINE 166 "src-ag/Code2TH.ag" #-}
   rule128 = \ ((_bodyIth) :: TH.Exp) ((_stmtsIstmt) :: [TH.Stmt]) ->
                               {-# LINE 166 "src-ag/Code2TH.ag" #-}
                               TH.DoE (_stmtsIstmt ++ [TH.NoBindS (TH.VarE (TH.mkName "return") `TH.AppE` _bodyIth)])
                               {-# LINE 1412 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule129 #-}
   {-# LINE 167 "src-ag/Code2TH.ag" #-}
   rule129 = \  (_ :: ()) ->
                               {-# LINE 167 "src-ag/Code2TH.ag" #-}
                               error "Cannot use do expression in pattern"
                               {-# LINE 1418 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule130 #-}
   {-# LINE 419 "src-ag/Code2TH.ag" #-}
   rule130 = \  (_ :: ()) ->
                            {-# LINE 419 "src-ag/Code2TH.ag" #-}
                            False
                            {-# LINE 1424 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule131 #-}
   rule131 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule132 #-}
   rule132 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule133 #-}
   rule133 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule134 #-}
   rule134 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Expr_Lambda #-}
sem_Expr_Lambda :: T_Exprs  -> T_Expr  -> T_Expr 
sem_Expr_Lambda arg_args_ arg_body_ = T_Expr (return st26) where
   {-# NOINLINE st26 #-}
   !st26 = let
      v25 :: T_Expr_v25 
      v25 = \ !(T_Expr_vIn25 _lhsInested _lhsIoptions) -> ( let
         _argsX29 = Control.Monad.Identity.runIdentity (attach_T_Exprs (arg_args_))
         _bodyX26 = Control.Monad.Identity.runIdentity (attach_T_Expr (arg_body_))
         (T_Exprs_vOut28 _argsIpat _argsIth) = inv_Exprs_s29 _argsX29 (T_Exprs_vIn28 _argsOnested _argsOoptions)
         (T_Expr_vOut25 _bodyIpat _bodyIth) = inv_Expr_s26 _bodyX26 (T_Expr_vIn25 _bodyOnested _bodyOoptions)
         _strictParams = rule135 _argsIth _lhsIoptions
         _addBang = rule136 _lhsIoptions
         _lhsOth :: TH.Exp
         _lhsOth = rule137 _addBang _argsIpat _bodyIth _strictParams
         _lhsOpat :: Maybe TH.Pat
         _lhsOpat = rule138  ()
         _argsOnested = rule139 _lhsInested
         _argsOoptions = rule140 _lhsIoptions
         _bodyOnested = rule141 _lhsInested
         _bodyOoptions = rule142 _lhsIoptions
         !__result_ = T_Expr_vOut25 _lhsOpat _lhsOth
         in __result_ )
     in C_Expr_s26 v25
   {-# INLINE rule135 #-}
   {-# LINE 168 "src-ag/Code2TH.ag" #-}
   rule135 = \ ((_argsIth) :: [TH.Exp]) ((_lhsIoptions) :: Options) ->
                                    {-# LINE 168 "src-ag/Code2TH.ag" #-}
                                    if strictSems _lhsIoptions
                                    then _argsIth
                                    else []
                                    {-# LINE 1468 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule136 #-}
   {-# LINE 171 "src-ag/Code2TH.ag" #-}
   rule136 = \ ((_lhsIoptions) :: Options) ->
                               {-# LINE 171 "src-ag/Code2TH.ag" #-}
                               if bangpats _lhsIoptions
                               then TH.BangP
                               else id
                               {-# LINE 1476 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule137 #-}
   {-# LINE 174 "src-ag/Code2TH.ag" #-}
   rule137 = \ _addBang ((_argsIpat) :: [TH.Pat]) ((_bodyIth) :: TH.Exp) _strictParams ->
                               {-# LINE 174 "src-ag/Code2TH.ag" #-}
                               TH.LamE (map _addBang     _argsIpat) (seqsE _strictParams     _bodyIth)
                               {-# LINE 1482 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule138 #-}
   {-# LINE 175 "src-ag/Code2TH.ag" #-}
   rule138 = \  (_ :: ()) ->
                               {-# LINE 175 "src-ag/Code2TH.ag" #-}
                               error "Cannot use lambda expression in pattern"
                               {-# LINE 1488 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule139 #-}
   rule139 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule140 #-}
   rule140 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule141 #-}
   rule141 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule142 #-}
   rule142 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Expr_TupleExpr #-}
sem_Expr_TupleExpr :: T_Exprs  -> T_Expr 
sem_Expr_TupleExpr arg_exprs_ = T_Expr (return st26) where
   {-# NOINLINE st26 #-}
   !st26 = let
      v25 :: T_Expr_v25 
      v25 = \ !(T_Expr_vIn25 _lhsInested _lhsIoptions) -> ( let
         _exprsX29 = Control.Monad.Identity.runIdentity (attach_T_Exprs (arg_exprs_))
         (T_Exprs_vOut28 _exprsIpat _exprsIth) = inv_Exprs_s29 _exprsX29 (T_Exprs_vIn28 _exprsOnested _exprsOoptions)
         _lhsOth :: TH.Exp
         _lhsOth = rule143 _exprsIth _lhsInested
         _lhsOpat :: Maybe TH.Pat
         _lhsOpat = rule144 _exprsIpat _lhsInested
         _exprsOnested = rule145 _lhsInested
         _exprsOoptions = rule146 _lhsIoptions
         !__result_ = T_Expr_vOut25 _lhsOpat _lhsOth
         in __result_ )
     in C_Expr_s26 v25
   {-# INLINE rule143 #-}
   {-# LINE 176 "src-ag/Code2TH.ag" #-}
   rule143 = \ ((_exprsIth) :: [TH.Exp]) ((_lhsInested) :: Bool) ->
                               {-# LINE 176 "src-ag/Code2TH.ag" #-}
                               tupleE False _lhsInested _exprsIth
                               {-# LINE 1524 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule144 #-}
   {-# LINE 177 "src-ag/Code2TH.ag" #-}
   rule144 = \ ((_exprsIpat) :: [TH.Pat]) ((_lhsInested) :: Bool) ->
                               {-# LINE 177 "src-ag/Code2TH.ag" #-}
                               pure $ tupleP False _lhsInested _exprsIpat
                               {-# LINE 1530 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule145 #-}
   rule145 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule146 #-}
   rule146 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Expr_UnboxedTupleExpr #-}
sem_Expr_UnboxedTupleExpr :: T_Exprs  -> T_Expr 
sem_Expr_UnboxedTupleExpr arg_exprs_ = T_Expr (return st26) where
   {-# NOINLINE st26 #-}
   !st26 = let
      v25 :: T_Expr_v25 
      v25 = \ !(T_Expr_vIn25 _lhsInested _lhsIoptions) -> ( let
         _exprsX29 = Control.Monad.Identity.runIdentity (attach_T_Exprs (arg_exprs_))
         (T_Exprs_vOut28 _exprsIpat _exprsIth) = inv_Exprs_s29 _exprsX29 (T_Exprs_vIn28 _exprsOnested _exprsOoptions)
         _lhsOth :: TH.Exp
         _lhsOth = rule147 _exprsIth _lhsInested
         _lhsOpat :: Maybe TH.Pat
         _lhsOpat = rule148 _exprsIpat _lhsInested
         _exprsOnested = rule149 _lhsInested
         _exprsOoptions = rule150 _lhsIoptions
         !__result_ = T_Expr_vOut25 _lhsOpat _lhsOth
         in __result_ )
     in C_Expr_s26 v25
   {-# INLINE rule147 #-}
   {-# LINE 179 "src-ag/Code2TH.ag" #-}
   rule147 = \ ((_exprsIth) :: [TH.Exp]) ((_lhsInested) :: Bool) ->
                               {-# LINE 179 "src-ag/Code2TH.ag" #-}
                               tupleE True _lhsInested _exprsIth
                               {-# LINE 1560 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule148 #-}
   {-# LINE 180 "src-ag/Code2TH.ag" #-}
   rule148 = \ ((_exprsIpat) :: [TH.Pat]) ((_lhsInested) :: Bool) ->
                               {-# LINE 180 "src-ag/Code2TH.ag" #-}
                               pure $ tupleP True _lhsInested _exprsIpat
                               {-# LINE 1566 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule149 #-}
   rule149 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule150 #-}
   rule150 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Expr_App #-}
sem_Expr_App :: (String) -> T_Exprs  -> T_Expr 
sem_Expr_App !arg_name_ arg_args_ = T_Expr (return st26) where
   {-# NOINLINE st26 #-}
   !st26 = let
      v25 :: T_Expr_v25 
      v25 = \ !(T_Expr_vIn25 _lhsInested _lhsIoptions) -> ( let
         _argsX29 = Control.Monad.Identity.runIdentity (attach_T_Exprs (arg_args_))
         (T_Exprs_vOut28 _argsIpat _argsIth) = inv_Exprs_s29 _argsX29 (T_Exprs_vIn28 _argsOnested _argsOoptions)
         _lhsOth :: TH.Exp
         _lhsOth = rule151 _argsIth arg_name_
         _lhsOpat :: Maybe TH.Pat
         _lhsOpat = rule152 _argsIpat arg_name_
         _argsOnested = rule153 _lhsInested
         _argsOoptions = rule154 _lhsIoptions
         !__result_ = T_Expr_vOut25 _lhsOpat _lhsOth
         in __result_ )
     in C_Expr_s26 v25
   {-# INLINE rule151 #-}
   {-# LINE 181 "src-ag/Code2TH.ag" #-}
   rule151 = \ ((_argsIth) :: [TH.Exp]) name_ ->
                               {-# LINE 181 "src-ag/Code2TH.ag" #-}
                               foldl TH.AppE (varConE name_) _argsIth
                               {-# LINE 1596 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule152 #-}
   {-# LINE 182 "src-ag/Code2TH.ag" #-}
   rule152 = \ ((_argsIpat) :: [TH.Pat]) name_ ->
                               {-# LINE 182 "src-ag/Code2TH.ag" #-}
                               pure $ TH.ConP (TH.mkName name_) _argsIpat
                               {-# LINE 1602 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule153 #-}
   rule153 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule154 #-}
   rule154 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Expr_SimpleExpr #-}
sem_Expr_SimpleExpr :: (String) -> T_Expr 
sem_Expr_SimpleExpr !arg_txt_ = T_Expr (return st26) where
   {-# NOINLINE st26 #-}
   !st26 = let
      v25 :: T_Expr_v25 
      v25 = \ !(T_Expr_vIn25 _lhsInested _lhsIoptions) -> ( let
         _lhsOth :: TH.Exp
         _lhsOth = rule155 arg_txt_
         _lhsOpat :: Maybe TH.Pat
         _lhsOpat = rule156 arg_txt_
         !__result_ = T_Expr_vOut25 _lhsOpat _lhsOth
         in __result_ )
     in C_Expr_s26 v25
   {-# INLINE rule155 #-}
   {-# LINE 183 "src-ag/Code2TH.ag" #-}
   rule155 = \ txt_ ->
                               {-# LINE 183 "src-ag/Code2TH.ag" #-}
                               varConE txt_
                               {-# LINE 1628 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule156 #-}
   {-# LINE 184 "src-ag/Code2TH.ag" #-}
   rule156 = \ txt_ ->
                               {-# LINE 184 "src-ag/Code2TH.ag" #-}
                               pure $ TH.VarP (TH.mkName txt_)
                               {-# LINE 1634 "src-generated/Code2TH.hs" #-}
{-# NOINLINE sem_Expr_TextExpr #-}
sem_Expr_TextExpr :: ([String]) -> T_Expr 
sem_Expr_TextExpr !arg_lns_ = T_Expr (return st26) where
   {-# NOINLINE st26 #-}
   !st26 = let
      v25 :: T_Expr_v25 
      v25 = \ !(T_Expr_vIn25 _lhsInested _lhsIoptions) -> ( let
         _lhsOth :: TH.Exp
         _lhsOth = rule157 arg_lns_
         _lhsOpat :: Maybe TH.Pat
         _lhsOpat = rule158  ()
         !__result_ = T_Expr_vOut25 _lhsOpat _lhsOth
         in __result_ )
     in C_Expr_s26 v25
   {-# INLINE rule157 #-}
   {-# LINE 185 "src-ag/Code2TH.ag" #-}
   rule157 = \ lns_ ->
                               {-# LINE 185 "src-ag/Code2TH.ag" #-}
                               either error id $ Meta.parseExp (unlines lns_)
                               {-# LINE 1654 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule158 #-}
   {-# LINE 186 "src-ag/Code2TH.ag" #-}
   rule158 = \  (_ :: ()) ->
                               {-# LINE 186 "src-ag/Code2TH.ag" #-}
                               error "Figure out what to do with TextExpr pat"
                               {-# LINE 1660 "src-generated/Code2TH.hs" #-}
{-# NOINLINE sem_Expr_Trace #-}
sem_Expr_Trace :: (String) -> T_Expr  -> T_Expr 
sem_Expr_Trace !arg_txt_ arg_expr_ = T_Expr (return st26) where
   {-# NOINLINE st26 #-}
   !st26 = let
      v25 :: T_Expr_v25 
      v25 = \ !(T_Expr_vIn25 _lhsInested _lhsIoptions) -> ( let
         _exprX26 = Control.Monad.Identity.runIdentity (attach_T_Expr (arg_expr_))
         (T_Expr_vOut25 _exprIpat _exprIth) = inv_Expr_s26 _exprX26 (T_Expr_vIn25 _exprOnested _exprOoptions)
         _lhsOth :: TH.Exp
         _lhsOth = rule159 _exprIth arg_txt_
         _lhsOpat :: Maybe TH.Pat
         _lhsOpat = rule160  ()
         _exprOnested = rule161 _lhsInested
         _exprOoptions = rule162 _lhsIoptions
         !__result_ = T_Expr_vOut25 _lhsOpat _lhsOth
         in __result_ )
     in C_Expr_s26 v25
   {-# INLINE rule159 #-}
   {-# LINE 187 "src-ag/Code2TH.ag" #-}
   rule159 = \ ((_exprIth) :: TH.Exp) txt_ ->
                               {-# LINE 187 "src-ag/Code2TH.ag" #-}
                               TH.VarE (TH.mkName "trace") `TH.AppE` (TH.LitE (TH.StringL txt_)) `TH.AppE` _exprIth
                               {-# LINE 1684 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule160 #-}
   {-# LINE 188 "src-ag/Code2TH.ag" #-}
   rule160 = \  (_ :: ()) ->
                               {-# LINE 188 "src-ag/Code2TH.ag" #-}
                               error "Cannot use trace expression in pattern"
                               {-# LINE 1690 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule161 #-}
   rule161 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule162 #-}
   rule162 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Expr_PragmaExpr #-}
sem_Expr_PragmaExpr :: (Bool) -> (Bool) -> (String) -> T_Expr  -> T_Expr 
sem_Expr_PragmaExpr _ _ _ arg_expr_ = T_Expr (return st26) where
   {-# NOINLINE st26 #-}
   !st26 = let
      v25 :: T_Expr_v25 
      v25 = \ !(T_Expr_vIn25 _lhsInested _lhsIoptions) -> ( let
         _exprX26 = Control.Monad.Identity.runIdentity (attach_T_Expr (arg_expr_))
         (T_Expr_vOut25 _exprIpat _exprIth) = inv_Expr_s26 _exprX26 (T_Expr_vIn25 _exprOnested _exprOoptions)
         _lhsOpat :: Maybe TH.Pat
         _lhsOpat = rule163 _exprIpat
         _lhsOth :: TH.Exp
         _lhsOth = rule164 _exprIth
         _exprOnested = rule165 _lhsInested
         _exprOoptions = rule166 _lhsIoptions
         !__result_ = T_Expr_vOut25 _lhsOpat _lhsOth
         in __result_ )
     in C_Expr_s26 v25
   {-# INLINE rule163 #-}
   rule163 = \ ((_exprIpat) :: Maybe TH.Pat) ->
     _exprIpat
   {-# INLINE rule164 #-}
   rule164 = \ ((_exprIth) :: TH.Exp) ->
     _exprIth
   {-# INLINE rule165 #-}
   rule165 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule166 #-}
   rule166 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Expr_LineExpr #-}
sem_Expr_LineExpr :: T_Expr  -> T_Expr 
sem_Expr_LineExpr arg_expr_ = T_Expr (return st26) where
   {-# NOINLINE st26 #-}
   !st26 = let
      v25 :: T_Expr_v25 
      v25 = \ !(T_Expr_vIn25 _lhsInested _lhsIoptions) -> ( let
         _exprX26 = Control.Monad.Identity.runIdentity (attach_T_Expr (arg_expr_))
         (T_Expr_vOut25 _exprIpat _exprIth) = inv_Expr_s26 _exprX26 (T_Expr_vIn25 _exprOnested _exprOoptions)
         _lhsOth :: TH.Exp
         _lhsOth = rule167 _exprIth
         _lhsOpat :: Maybe TH.Pat
         _lhsOpat = rule168  ()
         _exprOnested = rule169 _lhsInested
         _exprOoptions = rule170 _lhsIoptions
         !__result_ = T_Expr_vOut25 _lhsOpat _lhsOth
         in __result_ )
     in C_Expr_s26 v25
   {-# INLINE rule167 #-}
   {-# LINE 201 "src-ag/Code2TH.ag" #-}
   rule167 = \ ((_exprIth) :: TH.Exp) ->
                               {-# LINE 201 "src-ag/Code2TH.ag" #-}
                               _exprIth
                               {-# LINE 1750 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule168 #-}
   {-# LINE 204 "src-ag/Code2TH.ag" #-}
   rule168 = \  (_ :: ()) ->
                               {-# LINE 204 "src-ag/Code2TH.ag" #-}
                               error "Cannot use line pragma expression in pattern"
                               {-# LINE 1756 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule169 #-}
   rule169 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule170 #-}
   rule170 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Expr_TypedExpr #-}
sem_Expr_TypedExpr :: T_Expr  -> T_Type  -> T_Expr 
sem_Expr_TypedExpr arg_expr_ arg_tp_ = T_Expr (return st26) where
   {-# NOINLINE st26 #-}
   !st26 = let
      v25 :: T_Expr_v25 
      v25 = \ !(T_Expr_vIn25 _lhsInested _lhsIoptions) -> ( let
         _exprX26 = Control.Monad.Identity.runIdentity (attach_T_Expr (arg_expr_))
         _tpX50 = Control.Monad.Identity.runIdentity (attach_T_Type (arg_tp_))
         (T_Expr_vOut25 _exprIpat _exprIth) = inv_Expr_s26 _exprX26 (T_Expr_vIn25 _exprOnested _exprOoptions)
         (T_Type_vOut49 _tpIth) = inv_Type_s50 _tpX50 (T_Type_vIn49 _tpOnested)
         _lhsOth :: TH.Exp
         _lhsOth = rule171 _exprIth _tpIth
         _lhsOpat :: Maybe TH.Pat
         _lhsOpat = rule172 _exprIpat
         _exprOnested = rule173 _lhsInested
         _exprOoptions = rule174 _lhsIoptions
         _tpOnested = rule175 _lhsInested
         !__result_ = T_Expr_vOut25 _lhsOpat _lhsOth
         in __result_ )
     in C_Expr_s26 v25
   {-# INLINE rule171 #-}
   {-# LINE 205 "src-ag/Code2TH.ag" #-}
   rule171 = \ ((_exprIth) :: TH.Exp) ((_tpIth) :: TH.Type) ->
                               {-# LINE 205 "src-ag/Code2TH.ag" #-}
                               TH.SigE _exprIth _tpIth
                               {-# LINE 1789 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule172 #-}
   {-# LINE 206 "src-ag/Code2TH.ag" #-}
   rule172 = \ ((_exprIpat) :: Maybe TH.Pat) ->
                               {-# LINE 206 "src-ag/Code2TH.ag" #-}
                               _exprIpat
                               {-# LINE 1795 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule173 #-}
   rule173 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule174 #-}
   rule174 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule175 #-}
   rule175 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
{-# NOINLINE sem_Expr_ResultExpr #-}
sem_Expr_ResultExpr :: (String) -> T_Expr  -> T_Expr 
sem_Expr_ResultExpr !arg_nt_ arg_expr_ = T_Expr (return st26) where
   {-# NOINLINE st26 #-}
   !st26 = let
      v25 :: T_Expr_v25 
      v25 = \ !(T_Expr_vIn25 _lhsInested _lhsIoptions) -> ( let
         _exprX26 = Control.Monad.Identity.runIdentity (attach_T_Expr (arg_expr_))
         (T_Expr_vOut25 _exprIpat _exprIth) = inv_Expr_s26 _exprX26 (T_Expr_vIn25 _exprOnested _exprOoptions)
         _lhsOth :: TH.Exp
         _lhsOth = rule176 _exprIth _lhsIoptions arg_nt_
         _lhsOpat :: Maybe TH.Pat
         _lhsOpat = rule177  ()
         _exprOnested = rule178 _lhsInested
         _exprOoptions = rule179 _lhsIoptions
         !__result_ = T_Expr_vOut25 _lhsOpat _lhsOth
         in __result_ )
     in C_Expr_s26 v25
   {-# INLINE rule176 #-}
   {-# LINE 207 "src-ag/Code2TH.ag" #-}
   rule176 = \ ((_exprIth) :: TH.Exp) ((_lhsIoptions) :: Options) nt_ ->
                               {-# LINE 207 "src-ag/Code2TH.ag" #-}
                               if breadthFirst _lhsIoptions
                               then TH.VarE (TH.mkName "final") `TH.AppE` (TH.ConE (TH.mkName (nt_ ++ "_Syn")) `TH.AppE` _exprIth)
                               else _exprIth
                               {-# LINE 1830 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule177 #-}
   {-# LINE 210 "src-ag/Code2TH.ag" #-}
   rule177 = \  (_ :: ()) ->
                               {-# LINE 210 "src-ag/Code2TH.ag" #-}
                               error "Cannot use result expression in pattern"
                               {-# LINE 1836 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule178 #-}
   rule178 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule179 #-}
   rule179 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Expr_InvokeExpr #-}
sem_Expr_InvokeExpr :: (String) -> T_Expr  -> T_Exprs  -> T_Expr 
sem_Expr_InvokeExpr !arg_nt_ arg_expr_ arg_args_ = T_Expr (return st26) where
   {-# NOINLINE st26 #-}
   !st26 = let
      v25 :: T_Expr_v25 
      v25 = \ !(T_Expr_vIn25 _lhsInested _lhsIoptions) -> ( let
         _exprX26 = Control.Monad.Identity.runIdentity (attach_T_Expr (arg_expr_))
         _argsX29 = Control.Monad.Identity.runIdentity (attach_T_Exprs (arg_args_))
         (T_Expr_vOut25 _exprIpat _exprIth) = inv_Expr_s26 _exprX26 (T_Expr_vIn25 _exprOnested _exprOoptions)
         (T_Exprs_vOut28 _argsIpat _argsIth) = inv_Exprs_s29 _argsX29 (T_Exprs_vIn28 _argsOnested _argsOoptions)
         _lhsOth :: TH.Exp
         _lhsOth = rule180 _argsIth _exprIth _lhsIoptions arg_nt_
         _lhsOpat :: Maybe TH.Pat
         _lhsOpat = rule181  ()
         _exprOnested = rule182 _lhsInested
         _exprOoptions = rule183 _lhsIoptions
         _argsOnested = rule184 _lhsInested
         _argsOoptions = rule185 _lhsIoptions
         !__result_ = T_Expr_vOut25 _lhsOpat _lhsOth
         in __result_ )
     in C_Expr_s26 v25
   {-# INLINE rule180 #-}
   {-# LINE 211 "src-ag/Code2TH.ag" #-}
   rule180 = \ ((_argsIth) :: [TH.Exp]) ((_exprIth) :: TH.Exp) ((_lhsIoptions) :: Options) nt_ ->
                               {-# LINE 211 "src-ag/Code2TH.ag" #-}
                               if breadthFirst _lhsIoptions
                               then TH.VarE (TH.mkName "invoke")
                                   `TH.AppE` _exprIth
                                   `TH.AppE` (TH.ConE (TH.mkName (nt_ ++ "_Inh")) `TH.AppE` (TH.TupE (map Just _argsIth)))
                               else foldl TH.AppE _exprIth _argsIth
                               {-# LINE 1874 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule181 #-}
   {-# LINE 216 "src-ag/Code2TH.ag" #-}
   rule181 = \  (_ :: ()) ->
                               {-# LINE 216 "src-ag/Code2TH.ag" #-}
                               error "Cannot use invoke expression in pattern"
                               {-# LINE 1880 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule182 #-}
   rule182 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule183 #-}
   rule183 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule184 #-}
   rule184 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule185 #-}
   rule185 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Expr_ResumeExpr #-}
sem_Expr_ResumeExpr :: (String) -> T_Expr  -> T_Lhs  -> T_Expr  -> T_Expr 
sem_Expr_ResumeExpr !arg_nt_ arg_expr_ arg_left_ arg_rhs_ = T_Expr (return st26) where
   {-# NOINLINE st26 #-}
   !st26 = let
      v25 :: T_Expr_v25 
      v25 = \ !(T_Expr_vIn25 _lhsInested _lhsIoptions) -> ( let
         _exprX26 = Control.Monad.Identity.runIdentity (attach_T_Expr (arg_expr_))
         _leftX32 = Control.Monad.Identity.runIdentity (attach_T_Lhs (arg_left_))
         _rhsX26 = Control.Monad.Identity.runIdentity (attach_T_Expr (arg_rhs_))
         (T_Expr_vOut25 _exprIpat _exprIth) = inv_Expr_s26 _exprX26 (T_Expr_vIn25 _exprOnested _exprOoptions)
         (T_Lhs_vOut31 _leftIpat _leftIth) = inv_Lhs_s32 _leftX32 (T_Lhs_vIn31 _leftOisDeclOfLet _leftOnested _leftOoptions)
         (T_Expr_vOut25 _rhsIpat _rhsIth) = inv_Expr_s26 _rhsX26 (T_Expr_vIn25 _rhsOnested _rhsOoptions)
         _lhsOth :: TH.Exp
         _lhsOth = rule186 _exprIth _leftIpat _leftIth _lhsIoptions _rhsIth arg_nt_
         _lhsOpat :: Maybe TH.Pat
         _lhsOpat = rule187  ()
         _leftOisDeclOfLet = rule188  ()
         _exprOnested = rule189 _lhsInested
         _exprOoptions = rule190 _lhsIoptions
         _leftOnested = rule191 _lhsInested
         _leftOoptions = rule192 _lhsIoptions
         _rhsOnested = rule193 _lhsInested
         _rhsOoptions = rule194 _lhsIoptions
         !__result_ = T_Expr_vOut25 _lhsOpat _lhsOth
         in __result_ )
     in C_Expr_s26 v25
   {-# INLINE rule186 #-}
   {-# LINE 217 "src-ag/Code2TH.ag" #-}
   rule186 = \ ((_exprIth) :: TH.Exp) ((_leftIpat) :: TH.Pat) ((_leftIth) :: TH.Exp -> TH.Dec) ((_lhsIoptions) :: Options) ((_rhsIth) :: TH.Exp) nt_ ->
                               {-# LINE 217 "src-ag/Code2TH.ag" #-}
                               if breadthFirst _lhsIoptions
                               then TH.VarE (TH.mkName "resume")
                                   `TH.AppE` _exprIth
                                   `TH.AppE` TH.LamE [TH.TildeP (TH.ConP (TH.mkName (nt_ ++ "_Syn")) [TH.VarP (TH.mkName "_inh_arg")])]
                                                     (TH.LetE [_leftIth (TH.VarE (TH.mkName "_inh_arg"))] _rhsIth)
                               else TH.CaseE _exprIth [TH.Match _leftIpat (TH.NormalB _rhsIth) []]
                               {-# LINE 1930 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule187 #-}
   {-# LINE 223 "src-ag/Code2TH.ag" #-}
   rule187 = \  (_ :: ()) ->
                               {-# LINE 223 "src-ag/Code2TH.ag" #-}
                               error "Cannot use resume expression in pattern"
                               {-# LINE 1936 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule188 #-}
   {-# LINE 421 "src-ag/Code2TH.ag" #-}
   rule188 = \  (_ :: ()) ->
                           {-# LINE 421 "src-ag/Code2TH.ag" #-}
                           False
                           {-# LINE 1942 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule189 #-}
   rule189 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule190 #-}
   rule190 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule191 #-}
   rule191 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule192 #-}
   rule192 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule193 #-}
   rule193 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule194 #-}
   rule194 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Expr_SemFun #-}
sem_Expr_SemFun :: (String) -> T_Exprs  -> T_Expr  -> T_Expr 
sem_Expr_SemFun !arg_nt_ arg_args_ arg_body_ = T_Expr (return st26) where
   {-# NOINLINE st26 #-}
   !st26 = let
      v25 :: T_Expr_v25 
      v25 = \ !(T_Expr_vIn25 _lhsInested _lhsIoptions) -> ( let
         _argsX29 = Control.Monad.Identity.runIdentity (attach_T_Exprs (arg_args_))
         _bodyX26 = Control.Monad.Identity.runIdentity (attach_T_Expr (arg_body_))
         (T_Exprs_vOut28 _argsIpat _argsIth) = inv_Exprs_s29 _argsX29 (T_Exprs_vIn28 _argsOnested _argsOoptions)
         (T_Expr_vOut25 _bodyIpat _bodyIth) = inv_Expr_s26 _bodyX26 (T_Expr_vIn25 _bodyOnested _bodyOoptions)
         _strictParams = rule195 _argsIth _lhsIoptions
         _addBang = rule196 _lhsIoptions
         _lhsOth :: TH.Exp
         _lhsOth = rule197 _addBang _argsIpat _bodyIth _lhsIoptions _strictParams arg_nt_
         _lhsOpat :: Maybe TH.Pat
         _lhsOpat = rule198  ()
         _argsOnested = rule199 _lhsInested
         _argsOoptions = rule200 _lhsIoptions
         _bodyOnested = rule201 _lhsInested
         _bodyOoptions = rule202 _lhsIoptions
         !__result_ = T_Expr_vOut25 _lhsOpat _lhsOth
         in __result_ )
     in C_Expr_s26 v25
   {-# INLINE rule195 #-}
   {-# LINE 224 "src-ag/Code2TH.ag" #-}
   rule195 = \ ((_argsIth) :: [TH.Exp]) ((_lhsIoptions) :: Options) ->
                                    {-# LINE 224 "src-ag/Code2TH.ag" #-}
                                    if strictSems _lhsIoptions
                                    then _argsIth
                                    else []
                                    {-# LINE 1992 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule196 #-}
   {-# LINE 227 "src-ag/Code2TH.ag" #-}
   rule196 = \ ((_lhsIoptions) :: Options) ->
                               {-# LINE 227 "src-ag/Code2TH.ag" #-}
                               if bangpats _lhsIoptions then TH.BangP else id
                               {-# LINE 1998 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule197 #-}
   {-# LINE 228 "src-ag/Code2TH.ag" #-}
   rule197 = \ _addBang ((_argsIpat) :: [TH.Pat]) ((_bodyIth) :: TH.Exp) ((_lhsIoptions) :: Options) _strictParams nt_ ->
                               {-# LINE 228 "src-ag/Code2TH.ag" #-}
                               if breadthFirst _lhsIoptions
                               then TH.ConE (TH.mkName "Child")
                                   `TH.AppE` TH.LamE [TH.ConP (TH.mkName (nt_ ++ "_Inh")) [TH.TupP (map _addBang     _argsIpat)]]
                                                     (seqsE _strictParams     _bodyIth)
                               else if null _argsIpat
                                    then _bodyIth
                                    else TH.LamE (map _addBang     _argsIpat)
                                                 (seqsE _strictParams     _bodyIth)
                               {-# LINE 2011 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule198 #-}
   {-# LINE 236 "src-ag/Code2TH.ag" #-}
   rule198 = \  (_ :: ()) ->
                               {-# LINE 236 "src-ag/Code2TH.ag" #-}
                               error "Cannot use semfun expression in pattern"
                               {-# LINE 2017 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule199 #-}
   rule199 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule200 #-}
   rule200 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule201 #-}
   rule201 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule202 #-}
   rule202 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions

-- Exprs -------------------------------------------------------
-- wrapper
data Inh_Exprs  = Inh_Exprs { nested_Inh_Exprs :: !(Bool), options_Inh_Exprs :: !(Options) }
data Syn_Exprs  = Syn_Exprs { pat_Syn_Exprs :: !([TH.Pat]), th_Syn_Exprs :: !([TH.Exp]) }
{-# INLINABLE wrap_Exprs #-}
wrap_Exprs :: T_Exprs  -> Inh_Exprs  -> (Syn_Exprs )
wrap_Exprs !(T_Exprs act) !(Inh_Exprs _lhsInested _lhsIoptions) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg28 = T_Exprs_vIn28 _lhsInested _lhsIoptions
        !(T_Exprs_vOut28 _lhsOpat _lhsOth) <- return (inv_Exprs_s29 sem arg28)
        return (Syn_Exprs _lhsOpat _lhsOth)
   )

-- cata
{-# NOINLINE sem_Exprs #-}
sem_Exprs :: Exprs  -> T_Exprs 
sem_Exprs list = Prelude.foldr sem_Exprs_Cons sem_Exprs_Nil (Prelude.map sem_Expr list)

-- semantic domain
newtype T_Exprs  = T_Exprs {
                           attach_T_Exprs :: Identity (T_Exprs_s29 )
                           }
newtype T_Exprs_s29  = C_Exprs_s29 {
                                   inv_Exprs_s29 :: (T_Exprs_v28 )
                                   }
data T_Exprs_s30  = C_Exprs_s30
type T_Exprs_v28  = (T_Exprs_vIn28 ) -> (T_Exprs_vOut28 )
data T_Exprs_vIn28  = T_Exprs_vIn28 (Bool) (Options)
data T_Exprs_vOut28  = T_Exprs_vOut28 ([TH.Pat]) ([TH.Exp])
{-# NOINLINE sem_Exprs_Cons #-}
sem_Exprs_Cons :: T_Expr  -> T_Exprs  -> T_Exprs 
sem_Exprs_Cons arg_hd_ arg_tl_ = T_Exprs (return st29) where
   {-# NOINLINE st29 #-}
   !st29 = let
      v28 :: T_Exprs_v28 
      v28 = \ !(T_Exprs_vIn28 _lhsInested _lhsIoptions) -> ( let
         _hdX26 = Control.Monad.Identity.runIdentity (attach_T_Expr (arg_hd_))
         _tlX29 = Control.Monad.Identity.runIdentity (attach_T_Exprs (arg_tl_))
         (T_Expr_vOut25 _hdIpat _hdIth) = inv_Expr_s26 _hdX26 (T_Expr_vIn25 _hdOnested _hdOoptions)
         (T_Exprs_vOut28 _tlIpat _tlIth) = inv_Exprs_s29 _tlX29 (T_Exprs_vIn28 _tlOnested _tlOoptions)
         _lhsOpat :: [TH.Pat]
         _lhsOpat = rule203 _hdIpat _tlIpat
         _lhsOth :: [TH.Exp]
         _lhsOth = rule204 _hdIth _tlIth
         _hdOnested = rule205 _lhsInested
         _hdOoptions = rule206 _lhsIoptions
         _tlOnested = rule207 _lhsInested
         _tlOoptions = rule208 _lhsIoptions
         !__result_ = T_Exprs_vOut28 _lhsOpat _lhsOth
         in __result_ )
     in C_Exprs_s29 v28
   {-# INLINE rule203 #-}
   rule203 = \ ((_hdIpat) :: Maybe TH.Pat) ((_tlIpat) :: [TH.Pat]) ->
     (maybe id (:) _hdIpat _tlIpat)
   {-# INLINE rule204 #-}
   rule204 = \ ((_hdIth) :: TH.Exp) ((_tlIth) :: [TH.Exp]) ->
     _hdIth : _tlIth
   {-# INLINE rule205 #-}
   rule205 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule206 #-}
   rule206 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule207 #-}
   rule207 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule208 #-}
   rule208 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Exprs_Nil #-}
sem_Exprs_Nil ::  T_Exprs 
sem_Exprs_Nil  = T_Exprs (return st29) where
   {-# NOINLINE st29 #-}
   !st29 = let
      v28 :: T_Exprs_v28 
      v28 = \ !(T_Exprs_vIn28 _lhsInested _lhsIoptions) -> ( let
         _lhsOpat :: [TH.Pat]
         _lhsOpat = rule209  ()
         _lhsOth :: [TH.Exp]
         _lhsOth = rule210  ()
         !__result_ = T_Exprs_vOut28 _lhsOpat _lhsOth
         in __result_ )
     in C_Exprs_s29 v28
   {-# INLINE rule209 #-}
   rule209 = \  (_ :: ()) ->
     []
   {-# INLINE rule210 #-}
   rule210 = \  (_ :: ()) ->
     []

-- Lhs ---------------------------------------------------------
-- wrapper
data Inh_Lhs  = Inh_Lhs { isDeclOfLet_Inh_Lhs :: !(Bool), nested_Inh_Lhs :: !(Bool), options_Inh_Lhs :: !(Options) }
data Syn_Lhs  = Syn_Lhs { pat_Syn_Lhs :: !(TH.Pat), th_Syn_Lhs :: !(TH.Exp -> TH.Dec) }
{-# INLINABLE wrap_Lhs #-}
wrap_Lhs :: T_Lhs  -> Inh_Lhs  -> (Syn_Lhs )
wrap_Lhs !(T_Lhs act) !(Inh_Lhs _lhsIisDeclOfLet _lhsInested _lhsIoptions) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg31 = T_Lhs_vIn31 _lhsIisDeclOfLet _lhsInested _lhsIoptions
        !(T_Lhs_vOut31 _lhsOpat _lhsOth) <- return (inv_Lhs_s32 sem arg31)
        return (Syn_Lhs _lhsOpat _lhsOth)
   )

-- cata
{-# NOINLINE sem_Lhs #-}
sem_Lhs :: Lhs  -> T_Lhs 
sem_Lhs ( Pattern3 pat3_ ) = sem_Lhs_Pattern3 ( sem_Pattern pat3_ )
sem_Lhs ( Pattern3SM pat3_ ) = sem_Lhs_Pattern3SM ( sem_Pattern pat3_ )
sem_Lhs ( TupleLhs !comps_ ) = sem_Lhs_TupleLhs comps_
sem_Lhs ( UnboxedTupleLhs !comps_ ) = sem_Lhs_UnboxedTupleLhs comps_
sem_Lhs ( Fun !name_ args_ ) = sem_Lhs_Fun name_ ( sem_Exprs args_ )
sem_Lhs ( Unwrap !name_ sub_ ) = sem_Lhs_Unwrap name_ ( sem_Lhs sub_ )

-- semantic domain
newtype T_Lhs  = T_Lhs {
                       attach_T_Lhs :: Identity (T_Lhs_s32 )
                       }
newtype T_Lhs_s32  = C_Lhs_s32 {
                               inv_Lhs_s32 :: (T_Lhs_v31 )
                               }
data T_Lhs_s33  = C_Lhs_s33
type T_Lhs_v31  = (T_Lhs_vIn31 ) -> (T_Lhs_vOut31 )
data T_Lhs_vIn31  = T_Lhs_vIn31 (Bool) (Bool) (Options)
data T_Lhs_vOut31  = T_Lhs_vOut31 (TH.Pat) (TH.Exp -> TH.Dec)
{-# NOINLINE sem_Lhs_Pattern3 #-}
sem_Lhs_Pattern3 :: T_Pattern  -> T_Lhs 
sem_Lhs_Pattern3 arg_pat3_ = T_Lhs (return st32) where
   {-# NOINLINE st32 #-}
   !st32 = let
      v31 :: T_Lhs_v31 
      v31 = \ !(T_Lhs_vIn31 _lhsIisDeclOfLet _lhsInested _lhsIoptions) -> ( let
         _pat3X41 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat3_))
         (T_Pattern_vOut40 _pat3Icopy _pat3IisUnderscore _pat3IstrictVars _pat3Ith) = inv_Pattern_s41 _pat3X41 (T_Pattern_vIn40 _pat3ObelowIrrefutable _pat3OisDeclOfLet _pat3Ooptions)
         _lhsOpat :: TH.Pat
         _lhsOpat = rule211  ()
         _lhsOth :: TH.Exp -> TH.Dec
         _lhsOth = rule212 _pat3IstrictVars _pat3Ith
         _pat3ObelowIrrefutable = rule213  ()
         _pat3OisDeclOfLet = rule214 _lhsIisDeclOfLet
         _pat3Ooptions = rule215 _lhsIoptions
         !__result_ = T_Lhs_vOut31 _lhsOpat _lhsOth
         in __result_ )
     in C_Lhs_s32 v31
   {-# INLINE rule211 #-}
   {-# LINE 275 "src-ag/Code2TH.ag" #-}
   rule211 = \  (_ :: ()) ->
                                {-# LINE 275 "src-ag/Code2TH.ag" #-}
                                error "pat Deal with Pattern3"
                                {-# LINE 2181 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule212 #-}
   {-# LINE 276 "src-ag/Code2TH.ag" #-}
   rule212 = \ ((_pat3IstrictVars) :: [TH.Name]) ((_pat3Ith) :: TH.Pat) ->
                                {-# LINE 276 "src-ag/Code2TH.ag" #-}
                                \x -> TH.ValD _pat3Ith (TH.NormalB (seqsE (map TH.VarE _pat3IstrictVars) x)) []
                                {-# LINE 2187 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule213 #-}
   {-# LINE 382 "src-ag/Code2TH.ag" #-}
   rule213 = \  (_ :: ()) ->
                                {-# LINE 382 "src-ag/Code2TH.ag" #-}
                                False
                                {-# LINE 2193 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule214 #-}
   rule214 = \ ((_lhsIisDeclOfLet) :: Bool) ->
     _lhsIisDeclOfLet
   {-# INLINE rule215 #-}
   rule215 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Lhs_Pattern3SM #-}
sem_Lhs_Pattern3SM :: T_Pattern  -> T_Lhs 
sem_Lhs_Pattern3SM arg_pat3_ = T_Lhs (return st32) where
   {-# NOINLINE st32 #-}
   !st32 = let
      v31 :: T_Lhs_v31 
      v31 = \ !(T_Lhs_vIn31 _lhsIisDeclOfLet _lhsInested _lhsIoptions) -> ( let
         _pat3X41 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat3_))
         (T_Pattern_vOut40 _pat3Icopy _pat3IisUnderscore _pat3IstrictVars _pat3Ith) = inv_Pattern_s41 _pat3X41 (T_Pattern_vIn40 _pat3ObelowIrrefutable _pat3OisDeclOfLet _pat3Ooptions)
         _lhsOpat :: TH.Pat
         _lhsOpat = rule216  ()
         _lhsOth :: TH.Exp -> TH.Dec
         _lhsOth = rule217  ()
         _pat3ObelowIrrefutable = rule218  ()
         _pat3OisDeclOfLet = rule219 _lhsIisDeclOfLet
         _pat3Ooptions = rule220 _lhsIoptions
         !__result_ = T_Lhs_vOut31 _lhsOpat _lhsOth
         in __result_ )
     in C_Lhs_s32 v31
   {-# INLINE rule216 #-}
   {-# LINE 277 "src-ag/Code2TH.ag" #-}
   rule216 = \  (_ :: ()) ->
                                {-# LINE 277 "src-ag/Code2TH.ag" #-}
                                error "pat Deal with Pattern3SM"
                                {-# LINE 2224 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule217 #-}
   {-# LINE 278 "src-ag/Code2TH.ag" #-}
   rule217 = \  (_ :: ()) ->
                                {-# LINE 278 "src-ag/Code2TH.ag" #-}
                                error "th Deal with Pattern3SM"
                                {-# LINE 2230 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule218 #-}
   {-# LINE 382 "src-ag/Code2TH.ag" #-}
   rule218 = \  (_ :: ()) ->
                                {-# LINE 382 "src-ag/Code2TH.ag" #-}
                                False
                                {-# LINE 2236 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule219 #-}
   rule219 = \ ((_lhsIisDeclOfLet) :: Bool) ->
     _lhsIisDeclOfLet
   {-# INLINE rule220 #-}
   rule220 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Lhs_TupleLhs #-}
sem_Lhs_TupleLhs :: ([String]) -> T_Lhs 
sem_Lhs_TupleLhs !arg_comps_ = T_Lhs (return st32) where
   {-# NOINLINE st32 #-}
   !st32 = let
      v31 :: T_Lhs_v31 
      v31 = \ !(T_Lhs_vIn31 _lhsIisDeclOfLet _lhsInested _lhsIoptions) -> ( let
         _addBang = rule221 _lhsIoptions
         _pat = rule222 _addBang _lhsInested arg_comps_
         _lhsOth :: TH.Exp -> TH.Dec
         _lhsOth = rule223 _pat
         _lhsOpat :: TH.Pat
         _lhsOpat = rule224 _pat
         !__result_ = T_Lhs_vOut31 _lhsOpat _lhsOth
         in __result_ )
     in C_Lhs_s32 v31
   {-# INLINE rule221 #-}
   {-# LINE 273 "src-ag/Code2TH.ag" #-}
   rule221 = \ ((_lhsIoptions) :: Options) ->
                      {-# LINE 273 "src-ag/Code2TH.ag" #-}
                      if bangpats _lhsIoptions then TH.BangP else id
                      {-# LINE 2264 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule222 #-}
   {-# LINE 279 "src-ag/Code2TH.ag" #-}
   rule222 = \ _addBang ((_lhsInested) :: Bool) comps_ ->
                                {-# LINE 279 "src-ag/Code2TH.ag" #-}
                                tupleP False _lhsInested (map (_addBang     . TH.VarP . TH.mkName) comps_)
                                {-# LINE 2270 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule223 #-}
   {-# LINE 280 "src-ag/Code2TH.ag" #-}
   rule223 = \ _pat ->
                                {-# LINE 280 "src-ag/Code2TH.ag" #-}
                                \x -> TH.ValD _pat     (TH.NormalB x) []
                                {-# LINE 2276 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule224 #-}
   rule224 = \ _pat ->
     _pat
{-# NOINLINE sem_Lhs_UnboxedTupleLhs #-}
sem_Lhs_UnboxedTupleLhs :: ([String]) -> T_Lhs 
sem_Lhs_UnboxedTupleLhs _ = T_Lhs (return st32) where
   {-# NOINLINE st32 #-}
   !st32 = let
      v31 :: T_Lhs_v31 
      v31 = \ !(T_Lhs_vIn31 _lhsIisDeclOfLet _lhsInested _lhsIoptions) -> ( let
         _addBang = rule225 _lhsIoptions
         _lhsOpat :: TH.Pat
         _lhsOpat = rule226  ()
         _lhsOth :: TH.Exp -> TH.Dec
         _lhsOth = rule227  ()
         !__result_ = T_Lhs_vOut31 _lhsOpat _lhsOth
         in __result_ )
     in C_Lhs_s32 v31
   {-# INLINE rule225 #-}
   {-# LINE 273 "src-ag/Code2TH.ag" #-}
   rule225 = \ ((_lhsIoptions) :: Options) ->
                      {-# LINE 273 "src-ag/Code2TH.ag" #-}
                      if bangpats _lhsIoptions then TH.BangP else id
                      {-# LINE 2300 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule226 #-}
   {-# LINE 281 "src-ag/Code2TH.ag" #-}
   rule226 = \  (_ :: ()) ->
                                       {-# LINE 281 "src-ag/Code2TH.ag" #-}
                                       error "pat Deal with UnboxedTupleLhs"
                                       {-# LINE 2306 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule227 #-}
   {-# LINE 282 "src-ag/Code2TH.ag" #-}
   rule227 = \  (_ :: ()) ->
                                       {-# LINE 282 "src-ag/Code2TH.ag" #-}
                                       error "th Deal with UnboxedTupleLhs"
                                       {-# LINE 2312 "src-generated/Code2TH.hs" #-}
{-# NOINLINE sem_Lhs_Fun #-}
sem_Lhs_Fun :: (String) -> T_Exprs  -> T_Lhs 
sem_Lhs_Fun !arg_name_ arg_args_ = T_Lhs (return st32) where
   {-# NOINLINE st32 #-}
   !st32 = let
      v31 :: T_Lhs_v31 
      v31 = \ !(T_Lhs_vIn31 _lhsIisDeclOfLet _lhsInested _lhsIoptions) -> ( let
         _argsX29 = Control.Monad.Identity.runIdentity (attach_T_Exprs (arg_args_))
         (T_Exprs_vOut28 _argsIpat _argsIth) = inv_Exprs_s29 _argsX29 (T_Exprs_vIn28 _argsOnested _argsOoptions)
         _addBang = rule228 _lhsIoptions
         _lhsOpat :: TH.Pat
         _lhsOpat = rule229  ()
         _lhsOth :: TH.Exp -> TH.Dec
         _lhsOth = rule230 _addBang _argsIpat arg_name_
         _argsOnested = rule231 _lhsInested
         _argsOoptions = rule232 _lhsIoptions
         !__result_ = T_Lhs_vOut31 _lhsOpat _lhsOth
         in __result_ )
     in C_Lhs_s32 v31
   {-# INLINE rule228 #-}
   {-# LINE 273 "src-ag/Code2TH.ag" #-}
   rule228 = \ ((_lhsIoptions) :: Options) ->
                      {-# LINE 273 "src-ag/Code2TH.ag" #-}
                      if bangpats _lhsIoptions then TH.BangP else id
                      {-# LINE 2337 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule229 #-}
   {-# LINE 283 "src-ag/Code2TH.ag" #-}
   rule229 = \  (_ :: ()) ->
                                {-# LINE 283 "src-ag/Code2TH.ag" #-}
                                error "pat Deal with Fun"
                                {-# LINE 2343 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule230 #-}
   {-# LINE 284 "src-ag/Code2TH.ag" #-}
   rule230 = \ _addBang ((_argsIpat) :: [TH.Pat]) name_ ->
                                {-# LINE 284 "src-ag/Code2TH.ag" #-}
                                \x -> TH.FunD (TH.mkName name_) [TH.Clause (map _addBang     _argsIpat) (TH.NormalB x) []]
                                {-# LINE 2349 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule231 #-}
   rule231 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule232 #-}
   rule232 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Lhs_Unwrap #-}
sem_Lhs_Unwrap :: (String) -> T_Lhs  -> T_Lhs 
sem_Lhs_Unwrap _ arg_sub_ = T_Lhs (return st32) where
   {-# NOINLINE st32 #-}
   !st32 = let
      v31 :: T_Lhs_v31 
      v31 = \ !(T_Lhs_vIn31 _lhsIisDeclOfLet _lhsInested _lhsIoptions) -> ( let
         _subX32 = Control.Monad.Identity.runIdentity (attach_T_Lhs (arg_sub_))
         (T_Lhs_vOut31 _subIpat _subIth) = inv_Lhs_s32 _subX32 (T_Lhs_vIn31 _subOisDeclOfLet _subOnested _subOoptions)
         _lhsOpat :: TH.Pat
         _lhsOpat = rule233  ()
         _lhsOth :: TH.Exp -> TH.Dec
         _lhsOth = rule234  ()
         _subOisDeclOfLet = rule235 _lhsIisDeclOfLet
         _subOnested = rule236 _lhsInested
         _subOoptions = rule237 _lhsIoptions
         !__result_ = T_Lhs_vOut31 _lhsOpat _lhsOth
         in __result_ )
     in C_Lhs_s32 v31
   {-# INLINE rule233 #-}
   {-# LINE 285 "src-ag/Code2TH.ag" #-}
   rule233 = \  (_ :: ()) ->
                                {-# LINE 285 "src-ag/Code2TH.ag" #-}
                                error "pat Deal with Unwrap"
                                {-# LINE 2380 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule234 #-}
   {-# LINE 286 "src-ag/Code2TH.ag" #-}
   rule234 = \  (_ :: ()) ->
                                {-# LINE 286 "src-ag/Code2TH.ag" #-}
                                error "th Deal with Unwrap"
                                {-# LINE 2386 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule235 #-}
   rule235 = \ ((_lhsIisDeclOfLet) :: Bool) ->
     _lhsIisDeclOfLet
   {-# INLINE rule236 #-}
   rule236 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule237 #-}
   rule237 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions

-- NamedType ---------------------------------------------------
-- wrapper
data Inh_NamedType  = Inh_NamedType { nested_Inh_NamedType :: !(Bool) }
data Syn_NamedType  = Syn_NamedType { th_Syn_NamedType :: !(TH.VarBangType) }
{-# INLINABLE wrap_NamedType #-}
wrap_NamedType :: T_NamedType  -> Inh_NamedType  -> (Syn_NamedType )
wrap_NamedType !(T_NamedType act) !(Inh_NamedType _lhsInested) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg34 = T_NamedType_vIn34 _lhsInested
        !(T_NamedType_vOut34 _lhsOth) <- return (inv_NamedType_s35 sem arg34)
        return (Syn_NamedType _lhsOth)
   )

-- cata
{-# INLINE sem_NamedType #-}
sem_NamedType :: NamedType  -> T_NamedType 
sem_NamedType ( Named !strict_ !name_ tp_ ) = sem_NamedType_Named strict_ name_ ( sem_Type tp_ )

-- semantic domain
newtype T_NamedType  = T_NamedType {
                                   attach_T_NamedType :: Identity (T_NamedType_s35 )
                                   }
newtype T_NamedType_s35  = C_NamedType_s35 {
                                           inv_NamedType_s35 :: (T_NamedType_v34 )
                                           }
data T_NamedType_s36  = C_NamedType_s36
type T_NamedType_v34  = (T_NamedType_vIn34 ) -> (T_NamedType_vOut34 )
data T_NamedType_vIn34  = T_NamedType_vIn34 (Bool)
data T_NamedType_vOut34  = T_NamedType_vOut34 (TH.VarBangType)
{-# NOINLINE sem_NamedType_Named #-}
sem_NamedType_Named :: (Bool) -> (String) -> T_Type  -> T_NamedType 
sem_NamedType_Named !arg_strict_ !arg_name_ arg_tp_ = T_NamedType (return st35) where
   {-# NOINLINE st35 #-}
   !st35 = let
      v34 :: T_NamedType_v34 
      v34 = \ !(T_NamedType_vIn34 _lhsInested) -> ( let
         _tpX50 = Control.Monad.Identity.runIdentity (attach_T_Type (arg_tp_))
         (T_Type_vOut49 _tpIth) = inv_Type_s50 _tpX50 (T_Type_vIn49 _tpOnested)
         _lhsOth :: TH.VarBangType
         _lhsOth = rule238 _tpIth arg_name_ arg_strict_
         _tpOnested = rule239 _lhsInested
         !__result_ = T_NamedType_vOut34 _lhsOth
         in __result_ )
     in C_NamedType_s35 v34
   {-# INLINE rule238 #-}
   {-# LINE 249 "src-ag/Code2TH.ag" #-}
   rule238 = \ ((_tpIth) :: TH.Type) name_ strict_ ->
                               {-# LINE 249 "src-ag/Code2TH.ag" #-}
                               if strict_
                               then (TH.mkName name_, TH.Bang TH.NoSourceUnpackedness TH.SourceStrict, _tpIth)
                               else (TH.mkName name_, TH.Bang TH.NoSourceUnpackedness TH.NoSourceStrictness, _tpIth)
                               {-# LINE 2449 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule239 #-}
   rule239 = \ ((_lhsInested) :: Bool) ->
     _lhsInested

-- NamedTypes --------------------------------------------------
-- wrapper
data Inh_NamedTypes  = Inh_NamedTypes { nested_Inh_NamedTypes :: !(Bool) }
data Syn_NamedTypes  = Syn_NamedTypes { th_Syn_NamedTypes :: !([TH.VarBangType]) }
{-# INLINABLE wrap_NamedTypes #-}
wrap_NamedTypes :: T_NamedTypes  -> Inh_NamedTypes  -> (Syn_NamedTypes )
wrap_NamedTypes !(T_NamedTypes act) !(Inh_NamedTypes _lhsInested) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg37 = T_NamedTypes_vIn37 _lhsInested
        !(T_NamedTypes_vOut37 _lhsOth) <- return (inv_NamedTypes_s38 sem arg37)
        return (Syn_NamedTypes _lhsOth)
   )

-- cata
{-# NOINLINE sem_NamedTypes #-}
sem_NamedTypes :: NamedTypes  -> T_NamedTypes 
sem_NamedTypes list = Prelude.foldr sem_NamedTypes_Cons sem_NamedTypes_Nil (Prelude.map sem_NamedType list)

-- semantic domain
newtype T_NamedTypes  = T_NamedTypes {
                                     attach_T_NamedTypes :: Identity (T_NamedTypes_s38 )
                                     }
newtype T_NamedTypes_s38  = C_NamedTypes_s38 {
                                             inv_NamedTypes_s38 :: (T_NamedTypes_v37 )
                                             }
data T_NamedTypes_s39  = C_NamedTypes_s39
type T_NamedTypes_v37  = (T_NamedTypes_vIn37 ) -> (T_NamedTypes_vOut37 )
data T_NamedTypes_vIn37  = T_NamedTypes_vIn37 (Bool)
data T_NamedTypes_vOut37  = T_NamedTypes_vOut37 ([TH.VarBangType])
{-# NOINLINE sem_NamedTypes_Cons #-}
sem_NamedTypes_Cons :: T_NamedType  -> T_NamedTypes  -> T_NamedTypes 
sem_NamedTypes_Cons arg_hd_ arg_tl_ = T_NamedTypes (return st38) where
   {-# NOINLINE st38 #-}
   !st38 = let
      v37 :: T_NamedTypes_v37 
      v37 = \ !(T_NamedTypes_vIn37 _lhsInested) -> ( let
         _hdX35 = Control.Monad.Identity.runIdentity (attach_T_NamedType (arg_hd_))
         _tlX38 = Control.Monad.Identity.runIdentity (attach_T_NamedTypes (arg_tl_))
         (T_NamedType_vOut34 _hdIth) = inv_NamedType_s35 _hdX35 (T_NamedType_vIn34 _hdOnested)
         (T_NamedTypes_vOut37 _tlIth) = inv_NamedTypes_s38 _tlX38 (T_NamedTypes_vIn37 _tlOnested)
         _lhsOth :: [TH.VarBangType]
         _lhsOth = rule240 _hdIth _tlIth
         _hdOnested = rule241 _lhsInested
         _tlOnested = rule242 _lhsInested
         !__result_ = T_NamedTypes_vOut37 _lhsOth
         in __result_ )
     in C_NamedTypes_s38 v37
   {-# INLINE rule240 #-}
   rule240 = \ ((_hdIth) :: TH.VarBangType) ((_tlIth) :: [TH.VarBangType]) ->
     _hdIth : _tlIth
   {-# INLINE rule241 #-}
   rule241 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule242 #-}
   rule242 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
{-# NOINLINE sem_NamedTypes_Nil #-}
sem_NamedTypes_Nil ::  T_NamedTypes 
sem_NamedTypes_Nil  = T_NamedTypes (return st38) where
   {-# NOINLINE st38 #-}
   !st38 = let
      v37 :: T_NamedTypes_v37 
      v37 = \ !(T_NamedTypes_vIn37 _lhsInested) -> ( let
         _lhsOth :: [TH.VarBangType]
         _lhsOth = rule243  ()
         !__result_ = T_NamedTypes_vOut37 _lhsOth
         in __result_ )
     in C_NamedTypes_s38 v37
   {-# INLINE rule243 #-}
   rule243 = \  (_ :: ()) ->
     []

-- Pattern -----------------------------------------------------
-- wrapper
data Inh_Pattern  = Inh_Pattern { belowIrrefutable_Inh_Pattern :: !(Bool), isDeclOfLet_Inh_Pattern :: !(Bool), options_Inh_Pattern :: !(Options) }
data Syn_Pattern  = Syn_Pattern { copy_Syn_Pattern :: !(Pattern), isUnderscore_Syn_Pattern :: !(Bool), strictVars_Syn_Pattern :: !([TH.Name]), th_Syn_Pattern :: !(TH.Pat) }
{-# INLINABLE wrap_Pattern #-}
wrap_Pattern :: T_Pattern  -> Inh_Pattern  -> (Syn_Pattern )
wrap_Pattern !(T_Pattern act) !(Inh_Pattern _lhsIbelowIrrefutable _lhsIisDeclOfLet _lhsIoptions) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg40 = T_Pattern_vIn40 _lhsIbelowIrrefutable _lhsIisDeclOfLet _lhsIoptions
        !(T_Pattern_vOut40 _lhsOcopy _lhsOisUnderscore _lhsOstrictVars _lhsOth) <- return (inv_Pattern_s41 sem arg40)
        return (Syn_Pattern _lhsOcopy _lhsOisUnderscore _lhsOstrictVars _lhsOth)
   )

-- cata
{-# NOINLINE sem_Pattern #-}
sem_Pattern :: Pattern  -> T_Pattern 
sem_Pattern ( Constr !name_ pats_ ) = sem_Pattern_Constr name_ ( sem_Patterns pats_ )
sem_Pattern ( Product !pos_ pats_ ) = sem_Pattern_Product pos_ ( sem_Patterns pats_ )
sem_Pattern ( Alias !field_ !attr_ pat_ ) = sem_Pattern_Alias field_ attr_ ( sem_Pattern pat_ )
sem_Pattern ( Irrefutable pat_ ) = sem_Pattern_Irrefutable ( sem_Pattern pat_ )
sem_Pattern ( Underscore !pos_ ) = sem_Pattern_Underscore pos_

-- semantic domain
newtype T_Pattern  = T_Pattern {
                               attach_T_Pattern :: Identity (T_Pattern_s41 )
                               }
newtype T_Pattern_s41  = C_Pattern_s41 {
                                       inv_Pattern_s41 :: (T_Pattern_v40 )
                                       }
data T_Pattern_s42  = C_Pattern_s42
type T_Pattern_v40  = (T_Pattern_vIn40 ) -> (T_Pattern_vOut40 )
data T_Pattern_vIn40  = T_Pattern_vIn40 (Bool) (Bool) (Options)
data T_Pattern_vOut40  = T_Pattern_vOut40 (Pattern) (Bool) ([TH.Name]) (TH.Pat)
{-# NOINLINE sem_Pattern_Constr #-}
sem_Pattern_Constr :: (ConstructorIdent) -> T_Patterns  -> T_Pattern 
sem_Pattern_Constr !arg_name_ arg_pats_ = T_Pattern (return st41) where
   {-# NOINLINE st41 #-}
   !st41 = let
      v40 :: T_Pattern_v40 
      v40 = \ !(T_Pattern_vIn40 _lhsIbelowIrrefutable _lhsIisDeclOfLet _lhsIoptions) -> ( let
         _patsX44 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_pats_))
         (T_Patterns_vOut43 _patsIcopy _patsIstrictVars _patsIth) = inv_Patterns_s44 _patsX44 (T_Patterns_vIn43 _patsObelowIrrefutable _patsOisDeclOfLet _patsOoptions)
         _addBang = rule244 _lhsIbelowIrrefutable _lhsIisDeclOfLet _lhsIoptions
         _lhsOth :: TH.Pat
         _lhsOth = rule245 _addBang _patsIth arg_name_
         _lhsOisUnderscore :: Bool
         _lhsOisUnderscore = rule246  ()
         _lhsOstrictVars :: [TH.Name]
         _lhsOstrictVars = rule247 _patsIstrictVars
         _copy = rule248 _patsIcopy arg_name_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule249 _copy
         _patsObelowIrrefutable = rule250 _lhsIbelowIrrefutable
         _patsOisDeclOfLet = rule251 _lhsIisDeclOfLet
         _patsOoptions = rule252 _lhsIoptions
         !__result_ = T_Pattern_vOut40 _lhsOcopy _lhsOisUnderscore _lhsOstrictVars _lhsOth
         in __result_ )
     in C_Pattern_s41 v40
   {-# INLINE rule244 #-}
   {-# LINE 357 "src-ag/Code2TH.ag" #-}
   rule244 = \ ((_lhsIbelowIrrefutable) :: Bool) ((_lhsIisDeclOfLet) :: Bool) ((_lhsIoptions) :: Options) ->
                      {-# LINE 357 "src-ag/Code2TH.ag" #-}
                      if bangpats _lhsIoptions && not _lhsIisDeclOfLet && not _lhsIbelowIrrefutable then TH.BangP else id
                      {-# LINE 2591 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule245 #-}
   {-# LINE 360 "src-ag/Code2TH.ag" #-}
   rule245 = \ _addBang ((_patsIth) :: [TH.Pat]) name_ ->
                           {-# LINE 360 "src-ag/Code2TH.ag" #-}
                           _addBang     $ TH.ConP (TH.mkName (getName name_)) _patsIth
                           {-# LINE 2597 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule246 #-}
   {-# LINE 370 "src-ag/Code2TH.ag" #-}
   rule246 = \  (_ :: ()) ->
                                    {-# LINE 370 "src-ag/Code2TH.ag" #-}
                                    False
                                    {-# LINE 2603 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule247 #-}
   rule247 = \ ((_patsIstrictVars) :: [TH.Name]) ->
     _patsIstrictVars
   {-# INLINE rule248 #-}
   rule248 = \ ((_patsIcopy) :: Patterns) name_ ->
     Constr name_ _patsIcopy
   {-# INLINE rule249 #-}
   rule249 = \ _copy ->
     _copy
   {-# INLINE rule250 #-}
   rule250 = \ ((_lhsIbelowIrrefutable) :: Bool) ->
     _lhsIbelowIrrefutable
   {-# INLINE rule251 #-}
   rule251 = \ ((_lhsIisDeclOfLet) :: Bool) ->
     _lhsIisDeclOfLet
   {-# INLINE rule252 #-}
   rule252 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Pattern_Product #-}
sem_Pattern_Product :: (Pos) -> T_Patterns  -> T_Pattern 
sem_Pattern_Product !arg_pos_ arg_pats_ = T_Pattern (return st41) where
   {-# NOINLINE st41 #-}
   !st41 = let
      v40 :: T_Pattern_v40 
      v40 = \ !(T_Pattern_vIn40 _lhsIbelowIrrefutable _lhsIisDeclOfLet _lhsIoptions) -> ( let
         _patsX44 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_pats_))
         (T_Patterns_vOut43 _patsIcopy _patsIstrictVars _patsIth) = inv_Patterns_s44 _patsX44 (T_Patterns_vIn43 _patsObelowIrrefutable _patsOisDeclOfLet _patsOoptions)
         _addBang = rule253 _lhsIbelowIrrefutable _lhsIisDeclOfLet _lhsIoptions
         _lhsOth :: TH.Pat
         _lhsOth = rule254 _addBang _patsIth
         _lhsOisUnderscore :: Bool
         _lhsOisUnderscore = rule255  ()
         _lhsOstrictVars :: [TH.Name]
         _lhsOstrictVars = rule256 _patsIstrictVars
         _copy = rule257 _patsIcopy arg_pos_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule258 _copy
         _patsObelowIrrefutable = rule259 _lhsIbelowIrrefutable
         _patsOisDeclOfLet = rule260 _lhsIisDeclOfLet
         _patsOoptions = rule261 _lhsIoptions
         !__result_ = T_Pattern_vOut40 _lhsOcopy _lhsOisUnderscore _lhsOstrictVars _lhsOth
         in __result_ )
     in C_Pattern_s41 v40
   {-# INLINE rule253 #-}
   {-# LINE 357 "src-ag/Code2TH.ag" #-}
   rule253 = \ ((_lhsIbelowIrrefutable) :: Bool) ((_lhsIisDeclOfLet) :: Bool) ((_lhsIoptions) :: Options) ->
                      {-# LINE 357 "src-ag/Code2TH.ag" #-}
                      if bangpats _lhsIoptions && not _lhsIisDeclOfLet && not _lhsIbelowIrrefutable then TH.BangP else id
                      {-# LINE 2652 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule254 #-}
   {-# LINE 361 "src-ag/Code2TH.ag" #-}
   rule254 = \ _addBang ((_patsIth) :: [TH.Pat]) ->
                           {-# LINE 361 "src-ag/Code2TH.ag" #-}
                           _addBang     $ TH.TupP _patsIth
                           {-# LINE 2658 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule255 #-}
   {-# LINE 371 "src-ag/Code2TH.ag" #-}
   rule255 = \  (_ :: ()) ->
                                    {-# LINE 371 "src-ag/Code2TH.ag" #-}
                                    False
                                    {-# LINE 2664 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule256 #-}
   rule256 = \ ((_patsIstrictVars) :: [TH.Name]) ->
     _patsIstrictVars
   {-# INLINE rule257 #-}
   rule257 = \ ((_patsIcopy) :: Patterns) pos_ ->
     Product pos_ _patsIcopy
   {-# INLINE rule258 #-}
   rule258 = \ _copy ->
     _copy
   {-# INLINE rule259 #-}
   rule259 = \ ((_lhsIbelowIrrefutable) :: Bool) ->
     _lhsIbelowIrrefutable
   {-# INLINE rule260 #-}
   rule260 = \ ((_lhsIisDeclOfLet) :: Bool) ->
     _lhsIisDeclOfLet
   {-# INLINE rule261 #-}
   rule261 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Pattern_Alias #-}
sem_Pattern_Alias :: (Identifier) -> (Identifier) -> T_Pattern  -> T_Pattern 
sem_Pattern_Alias !arg_field_ !arg_attr_ arg_pat_ = T_Pattern (return st41) where
   {-# NOINLINE st41 #-}
   !st41 = let
      v40 :: T_Pattern_v40 
      v40 = \ !(T_Pattern_vIn40 _lhsIbelowIrrefutable _lhsIisDeclOfLet _lhsIoptions) -> ( let
         _patX41 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat_))
         (T_Pattern_vOut40 _patIcopy _patIisUnderscore _patIstrictVars _patIth) = inv_Pattern_s41 _patX41 (T_Pattern_vIn40 _patObelowIrrefutable _patOisDeclOfLet _patOoptions)
         _strictVar = rule262 _lhsIisDeclOfLet _lhsIoptions _varName
         _strictPatVars = rule263 _lhsIisDeclOfLet _lhsIoptions _patIstrictVars
         _lhsOstrictVars :: [TH.Name]
         _lhsOstrictVars = rule264 _strictPatVars _strictVar
         _addBang = rule265 _lhsIbelowIrrefutable _lhsIisDeclOfLet _lhsIoptions
         _varName = rule266 _lhsIoptions arg_attr_ arg_field_
         _lhsOth :: TH.Pat
         _lhsOth = rule267 _addBang _patIisUnderscore _patIth _varName
         _lhsOisUnderscore :: Bool
         _lhsOisUnderscore = rule268  ()
         _copy = rule269 _patIcopy arg_attr_ arg_field_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule270 _copy
         _patObelowIrrefutable = rule271 _lhsIbelowIrrefutable
         _patOisDeclOfLet = rule272 _lhsIisDeclOfLet
         _patOoptions = rule273 _lhsIoptions
         !__result_ = T_Pattern_vOut40 _lhsOcopy _lhsOisUnderscore _lhsOstrictVars _lhsOth
         in __result_ )
     in C_Pattern_s41 v40
   {-# INLINE rule262 #-}
   {-# LINE 337 "src-ag/Code2TH.ag" #-}
   rule262 = \ ((_lhsIisDeclOfLet) :: Bool) ((_lhsIoptions) :: Options) _varName ->
            {-# LINE 337 "src-ag/Code2TH.ag" #-}
            if strictCases _lhsIoptions && not _lhsIisDeclOfLet
            then [_varName    ]
            else []
            {-# LINE 2718 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule263 #-}
   {-# LINE 341 "src-ag/Code2TH.ag" #-}
   rule263 = \ ((_lhsIisDeclOfLet) :: Bool) ((_lhsIoptions) :: Options) ((_patIstrictVars) :: [TH.Name]) ->
            {-# LINE 341 "src-ag/Code2TH.ag" #-}
            if stricterCases _lhsIoptions && not _lhsIisDeclOfLet
            then _patIstrictVars
            else []
            {-# LINE 2726 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule264 #-}
   {-# LINE 345 "src-ag/Code2TH.ag" #-}
   rule264 = \ _strictPatVars _strictVar ->
            {-# LINE 345 "src-ag/Code2TH.ag" #-}
            _strictVar     ++ _strictPatVars
            {-# LINE 2732 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule265 #-}
   {-# LINE 357 "src-ag/Code2TH.ag" #-}
   rule265 = \ ((_lhsIbelowIrrefutable) :: Bool) ((_lhsIisDeclOfLet) :: Bool) ((_lhsIoptions) :: Options) ->
                      {-# LINE 357 "src-ag/Code2TH.ag" #-}
                      if bangpats _lhsIoptions && not _lhsIisDeclOfLet && not _lhsIbelowIrrefutable then TH.BangP else id
                      {-# LINE 2738 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule266 #-}
   {-# LINE 362 "src-ag/Code2TH.ag" #-}
   rule266 = \ ((_lhsIoptions) :: Options) attr_ field_ ->
                             {-# LINE 362 "src-ag/Code2TH.ag" #-}
                             TH.mkName (attrname _lhsIoptions False field_ attr_)
                             {-# LINE 2744 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule267 #-}
   {-# LINE 363 "src-ag/Code2TH.ag" #-}
   rule267 = \ _addBang ((_patIisUnderscore) :: Bool) ((_patIth) :: TH.Pat) _varName ->
                           {-# LINE 363 "src-ag/Code2TH.ag" #-}
                           _addBang     $ if _patIisUnderscore
                            then TH.VarP _varName
                            else TH.AsP _varName     _patIth
                           {-# LINE 2752 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule268 #-}
   {-# LINE 372 "src-ag/Code2TH.ag" #-}
   rule268 = \  (_ :: ()) ->
                                    {-# LINE 372 "src-ag/Code2TH.ag" #-}
                                    False
                                    {-# LINE 2758 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule269 #-}
   rule269 = \ ((_patIcopy) :: Pattern) attr_ field_ ->
     Alias field_ attr_ _patIcopy
   {-# INLINE rule270 #-}
   rule270 = \ _copy ->
     _copy
   {-# INLINE rule271 #-}
   rule271 = \ ((_lhsIbelowIrrefutable) :: Bool) ->
     _lhsIbelowIrrefutable
   {-# INLINE rule272 #-}
   rule272 = \ ((_lhsIisDeclOfLet) :: Bool) ->
     _lhsIisDeclOfLet
   {-# INLINE rule273 #-}
   rule273 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Pattern_Irrefutable #-}
sem_Pattern_Irrefutable :: T_Pattern  -> T_Pattern 
sem_Pattern_Irrefutable arg_pat_ = T_Pattern (return st41) where
   {-# NOINLINE st41 #-}
   !st41 = let
      v40 :: T_Pattern_v40 
      v40 = \ !(T_Pattern_vIn40 _lhsIbelowIrrefutable _lhsIisDeclOfLet _lhsIoptions) -> ( let
         _patX41 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat_))
         (T_Pattern_vOut40 _patIcopy _patIisUnderscore _patIstrictVars _patIth) = inv_Pattern_s41 _patX41 (T_Pattern_vIn40 _patObelowIrrefutable _patOisDeclOfLet _patOoptions)
         _lhsOstrictVars :: [TH.Name]
         _lhsOstrictVars = rule274  ()
         _lhsOth :: TH.Pat
         _lhsOth = rule275 _patIth
         _patObelowIrrefutable = rule276  ()
         _copy = rule277 _patIcopy
         _lhsOcopy :: Pattern
         _lhsOcopy = rule278 _copy
         _lhsOisUnderscore :: Bool
         _lhsOisUnderscore = rule279 _patIisUnderscore
         _patOisDeclOfLet = rule280 _lhsIisDeclOfLet
         _patOoptions = rule281 _lhsIoptions
         !__result_ = T_Pattern_vOut40 _lhsOcopy _lhsOisUnderscore _lhsOstrictVars _lhsOth
         in __result_ )
     in C_Pattern_s41 v40
   {-# INLINE rule274 #-}
   {-# LINE 347 "src-ag/Code2TH.ag" #-}
   rule274 = \  (_ :: ()) ->
                         {-# LINE 347 "src-ag/Code2TH.ag" #-}
                         []
                         {-# LINE 2803 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule275 #-}
   {-# LINE 366 "src-ag/Code2TH.ag" #-}
   rule275 = \ ((_patIth) :: TH.Pat) ->
                           {-# LINE 366 "src-ag/Code2TH.ag" #-}
                           TH.TildeP _patIth
                           {-# LINE 2809 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule276 #-}
   {-# LINE 378 "src-ag/Code2TH.ag" #-}
   rule276 = \  (_ :: ()) ->
                               {-# LINE 378 "src-ag/Code2TH.ag" #-}
                               True
                               {-# LINE 2815 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule277 #-}
   rule277 = \ ((_patIcopy) :: Pattern) ->
     Irrefutable _patIcopy
   {-# INLINE rule278 #-}
   rule278 = \ _copy ->
     _copy
   {-# INLINE rule279 #-}
   rule279 = \ ((_patIisUnderscore) :: Bool) ->
     _patIisUnderscore
   {-# INLINE rule280 #-}
   rule280 = \ ((_lhsIisDeclOfLet) :: Bool) ->
     _lhsIisDeclOfLet
   {-# INLINE rule281 #-}
   rule281 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Pattern_Underscore #-}
sem_Pattern_Underscore :: (Pos) -> T_Pattern 
sem_Pattern_Underscore !arg_pos_ = T_Pattern (return st41) where
   {-# NOINLINE st41 #-}
   !st41 = let
      v40 :: T_Pattern_v40 
      v40 = \ !(T_Pattern_vIn40 _lhsIbelowIrrefutable _lhsIisDeclOfLet _lhsIoptions) -> ( let
         _lhsOth :: TH.Pat
         _lhsOth = rule282  ()
         _lhsOisUnderscore :: Bool
         _lhsOisUnderscore = rule283  ()
         _lhsOstrictVars :: [TH.Name]
         _lhsOstrictVars = rule284  ()
         _copy = rule285 arg_pos_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule286 _copy
         !__result_ = T_Pattern_vOut40 _lhsOcopy _lhsOisUnderscore _lhsOstrictVars _lhsOth
         in __result_ )
     in C_Pattern_s41 v40
   {-# INLINE rule282 #-}
   {-# LINE 367 "src-ag/Code2TH.ag" #-}
   rule282 = \  (_ :: ()) ->
                           {-# LINE 367 "src-ag/Code2TH.ag" #-}
                           TH.WildP
                           {-# LINE 2855 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule283 #-}
   {-# LINE 373 "src-ag/Code2TH.ag" #-}
   rule283 = \  (_ :: ()) ->
                                    {-# LINE 373 "src-ag/Code2TH.ag" #-}
                                    True
                                    {-# LINE 2861 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule284 #-}
   rule284 = \  (_ :: ()) ->
     []
   {-# INLINE rule285 #-}
   rule285 = \ pos_ ->
     Underscore pos_
   {-# INLINE rule286 #-}
   rule286 = \ _copy ->
     _copy

-- Patterns ----------------------------------------------------
-- wrapper
data Inh_Patterns  = Inh_Patterns { belowIrrefutable_Inh_Patterns :: !(Bool), isDeclOfLet_Inh_Patterns :: !(Bool), options_Inh_Patterns :: !(Options) }
data Syn_Patterns  = Syn_Patterns { copy_Syn_Patterns :: !(Patterns), strictVars_Syn_Patterns :: !([TH.Name]), th_Syn_Patterns :: !([TH.Pat]) }
{-# INLINABLE wrap_Patterns #-}
wrap_Patterns :: T_Patterns  -> Inh_Patterns  -> (Syn_Patterns )
wrap_Patterns !(T_Patterns act) !(Inh_Patterns _lhsIbelowIrrefutable _lhsIisDeclOfLet _lhsIoptions) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg43 = T_Patterns_vIn43 _lhsIbelowIrrefutable _lhsIisDeclOfLet _lhsIoptions
        !(T_Patterns_vOut43 _lhsOcopy _lhsOstrictVars _lhsOth) <- return (inv_Patterns_s44 sem arg43)
        return (Syn_Patterns _lhsOcopy _lhsOstrictVars _lhsOth)
   )

-- cata
{-# NOINLINE sem_Patterns #-}
sem_Patterns :: Patterns  -> T_Patterns 
sem_Patterns list = Prelude.foldr sem_Patterns_Cons sem_Patterns_Nil (Prelude.map sem_Pattern list)

-- semantic domain
newtype T_Patterns  = T_Patterns {
                                 attach_T_Patterns :: Identity (T_Patterns_s44 )
                                 }
newtype T_Patterns_s44  = C_Patterns_s44 {
                                         inv_Patterns_s44 :: (T_Patterns_v43 )
                                         }
data T_Patterns_s45  = C_Patterns_s45
type T_Patterns_v43  = (T_Patterns_vIn43 ) -> (T_Patterns_vOut43 )
data T_Patterns_vIn43  = T_Patterns_vIn43 (Bool) (Bool) (Options)
data T_Patterns_vOut43  = T_Patterns_vOut43 (Patterns) ([TH.Name]) ([TH.Pat])
{-# NOINLINE sem_Patterns_Cons #-}
sem_Patterns_Cons :: T_Pattern  -> T_Patterns  -> T_Patterns 
sem_Patterns_Cons arg_hd_ arg_tl_ = T_Patterns (return st44) where
   {-# NOINLINE st44 #-}
   !st44 = let
      v43 :: T_Patterns_v43 
      v43 = \ !(T_Patterns_vIn43 _lhsIbelowIrrefutable _lhsIisDeclOfLet _lhsIoptions) -> ( let
         _hdX41 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_hd_))
         _tlX44 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_tl_))
         (T_Pattern_vOut40 _hdIcopy _hdIisUnderscore _hdIstrictVars _hdIth) = inv_Pattern_s41 _hdX41 (T_Pattern_vIn40 _hdObelowIrrefutable _hdOisDeclOfLet _hdOoptions)
         (T_Patterns_vOut43 _tlIcopy _tlIstrictVars _tlIth) = inv_Patterns_s44 _tlX44 (T_Patterns_vIn43 _tlObelowIrrefutable _tlOisDeclOfLet _tlOoptions)
         _lhsOstrictVars :: [TH.Name]
         _lhsOstrictVars = rule287 _hdIstrictVars _tlIstrictVars
         _lhsOth :: [TH.Pat]
         _lhsOth = rule288 _hdIth _tlIth
         _copy = rule289 _hdIcopy _tlIcopy
         _lhsOcopy :: Patterns
         _lhsOcopy = rule290 _copy
         _hdObelowIrrefutable = rule291 _lhsIbelowIrrefutable
         _hdOisDeclOfLet = rule292 _lhsIisDeclOfLet
         _hdOoptions = rule293 _lhsIoptions
         _tlObelowIrrefutable = rule294 _lhsIbelowIrrefutable
         _tlOisDeclOfLet = rule295 _lhsIisDeclOfLet
         _tlOoptions = rule296 _lhsIoptions
         !__result_ = T_Patterns_vOut43 _lhsOcopy _lhsOstrictVars _lhsOth
         in __result_ )
     in C_Patterns_s44 v43
   {-# INLINE rule287 #-}
   rule287 = \ ((_hdIstrictVars) :: [TH.Name]) ((_tlIstrictVars) :: [TH.Name]) ->
     _hdIstrictVars ++ _tlIstrictVars
   {-# INLINE rule288 #-}
   rule288 = \ ((_hdIth) :: TH.Pat) ((_tlIth) :: [TH.Pat]) ->
     _hdIth : _tlIth
   {-# INLINE rule289 #-}
   rule289 = \ ((_hdIcopy) :: Pattern) ((_tlIcopy) :: Patterns) ->
     (:) _hdIcopy _tlIcopy
   {-# INLINE rule290 #-}
   rule290 = \ _copy ->
     _copy
   {-# INLINE rule291 #-}
   rule291 = \ ((_lhsIbelowIrrefutable) :: Bool) ->
     _lhsIbelowIrrefutable
   {-# INLINE rule292 #-}
   rule292 = \ ((_lhsIisDeclOfLet) :: Bool) ->
     _lhsIisDeclOfLet
   {-# INLINE rule293 #-}
   rule293 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule294 #-}
   rule294 = \ ((_lhsIbelowIrrefutable) :: Bool) ->
     _lhsIbelowIrrefutable
   {-# INLINE rule295 #-}
   rule295 = \ ((_lhsIisDeclOfLet) :: Bool) ->
     _lhsIisDeclOfLet
   {-# INLINE rule296 #-}
   rule296 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_Patterns_Nil #-}
sem_Patterns_Nil ::  T_Patterns 
sem_Patterns_Nil  = T_Patterns (return st44) where
   {-# NOINLINE st44 #-}
   !st44 = let
      v43 :: T_Patterns_v43 
      v43 = \ !(T_Patterns_vIn43 _lhsIbelowIrrefutable _lhsIisDeclOfLet _lhsIoptions) -> ( let
         _lhsOstrictVars :: [TH.Name]
         _lhsOstrictVars = rule297  ()
         _lhsOth :: [TH.Pat]
         _lhsOth = rule298  ()
         _copy = rule299  ()
         _lhsOcopy :: Patterns
         _lhsOcopy = rule300 _copy
         !__result_ = T_Patterns_vOut43 _lhsOcopy _lhsOstrictVars _lhsOth
         in __result_ )
     in C_Patterns_s44 v43
   {-# INLINE rule297 #-}
   rule297 = \  (_ :: ()) ->
     []
   {-# INLINE rule298 #-}
   rule298 = \  (_ :: ()) ->
     []
   {-# INLINE rule299 #-}
   rule299 = \  (_ :: ()) ->
     []
   {-# INLINE rule300 #-}
   rule300 = \ _copy ->
     _copy

-- Program -----------------------------------------------------
-- wrapper
data Inh_Program  = Inh_Program { options_Inh_Program :: !(Options) }
data Syn_Program  = Syn_Program { th_Syn_Program :: !([TH.Dec]) }
{-# INLINABLE wrap_Program #-}
wrap_Program :: T_Program  -> Inh_Program  -> (Syn_Program )
wrap_Program !(T_Program act) !(Inh_Program _lhsIoptions) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg46 = T_Program_vIn46 _lhsIoptions
        !(T_Program_vOut46 _lhsOth) <- return (inv_Program_s47 sem arg46)
        return (Syn_Program _lhsOth)
   )

-- cata
{-# INLINE sem_Program #-}
sem_Program :: Program  -> T_Program 
sem_Program ( Program chunks_ !ordered_ ) = sem_Program_Program ( sem_Chunks chunks_ ) ordered_

-- semantic domain
newtype T_Program  = T_Program {
                               attach_T_Program :: Identity (T_Program_s47 )
                               }
newtype T_Program_s47  = C_Program_s47 {
                                       inv_Program_s47 :: (T_Program_v46 )
                                       }
data T_Program_s48  = C_Program_s48
type T_Program_v46  = (T_Program_vIn46 ) -> (T_Program_vOut46 )
data T_Program_vIn46  = T_Program_vIn46 (Options)
data T_Program_vOut46  = T_Program_vOut46 ([TH.Dec])
{-# NOINLINE sem_Program_Program #-}
sem_Program_Program :: T_Chunks  -> (Bool) -> T_Program 
sem_Program_Program arg_chunks_ !arg_ordered_ = T_Program (return st47) where
   {-# NOINLINE st47 #-}
   !st47 = let
      v46 :: T_Program_v46 
      v46 = \ !(T_Program_vIn46 _lhsIoptions) -> ( let
         _chunksX11 = Control.Monad.Identity.runIdentity (attach_T_Chunks (arg_chunks_))
         (T_Chunks_vOut10 _chunksIth) = inv_Chunks_s11 _chunksX11 (T_Chunks_vIn10 _chunksOisDeclOfLet _chunksOnested _chunksOoptions)
         _options = rule301 _lhsIoptions arg_ordered_
         _chunksOnested = rule302 _lhsIoptions
         _lhsOth :: [TH.Dec]
         _lhsOth = rule303 _chunksIth
         _chunksOisDeclOfLet = rule304  ()
         _chunksOoptions = rule305 _options
         !__result_ = T_Program_vOut46 _lhsOth
         in __result_ )
     in C_Program_s47 v46
   {-# INLINE rule301 #-}
   {-# LINE 99 "src-ag/Code2TH.ag" #-}
   rule301 = \ ((_lhsIoptions) :: Options) ordered_ ->
                  {-# LINE 99 "src-ag/Code2TH.ag" #-}
                  _lhsIoptions { breadthFirst = breadthFirst _lhsIoptions && visit _lhsIoptions && cases _lhsIoptions && ordered_ }
                  {-# LINE 3042 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule302 #-}
   {-# LINE 102 "src-ag/Code2TH.ag" #-}
   rule302 = \ ((_lhsIoptions) :: Options) ->
                              {-# LINE 102 "src-ag/Code2TH.ag" #-}
                              nest _lhsIoptions
                              {-# LINE 3048 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule303 #-}
   {-# LINE 105 "src-ag/Code2TH.ag" #-}
   rule303 = \ ((_chunksIth) :: [TH.Dec]) ->
                           {-# LINE 105 "src-ag/Code2TH.ag" #-}
                           dedupFunDecls _chunksIth
                           {-# LINE 3054 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule304 #-}
   {-# LINE 413 "src-ag/Code2TH.ag" #-}
   rule304 = \  (_ :: ()) ->
                             {-# LINE 413 "src-ag/Code2TH.ag" #-}
                             False
                             {-# LINE 3060 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule305 #-}
   rule305 = \ _options ->
     _options

-- Type --------------------------------------------------------
-- wrapper
data Inh_Type  = Inh_Type { nested_Inh_Type :: !(Bool) }
data Syn_Type  = Syn_Type { th_Syn_Type :: !(TH.Type) }
{-# INLINABLE wrap_Type #-}
wrap_Type :: T_Type  -> Inh_Type  -> (Syn_Type )
wrap_Type !(T_Type act) !(Inh_Type _lhsInested) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg49 = T_Type_vIn49 _lhsInested
        !(T_Type_vOut49 _lhsOth) <- return (inv_Type_s50 sem arg49)
        return (Syn_Type _lhsOth)
   )

-- cata
{-# NOINLINE sem_Type #-}
sem_Type :: Type  -> T_Type 
sem_Type ( Arr left_ right_ ) = sem_Type_Arr ( sem_Type left_ ) ( sem_Type right_ )
sem_Type ( CtxApp !left_ right_ ) = sem_Type_CtxApp left_ ( sem_Type right_ )
sem_Type ( QuantApp !left_ right_ ) = sem_Type_QuantApp left_ ( sem_Type right_ )
sem_Type ( TypeApp func_ args_ ) = sem_Type_TypeApp ( sem_Type func_ ) ( sem_Types args_ )
sem_Type ( TupleType tps_ ) = sem_Type_TupleType ( sem_Types tps_ )
sem_Type ( UnboxedTupleType tps_ ) = sem_Type_UnboxedTupleType ( sem_Types tps_ )
sem_Type ( List tp_ ) = sem_Type_List ( sem_Type tp_ )
sem_Type ( SimpleType !txt_ ) = sem_Type_SimpleType txt_
sem_Type ( NontermType !name_ !params_ !deforested_ ) = sem_Type_NontermType name_ params_ deforested_
sem_Type ( TMaybe tp_ ) = sem_Type_TMaybe ( sem_Type tp_ )
sem_Type ( TEither left_ right_ ) = sem_Type_TEither ( sem_Type left_ ) ( sem_Type right_ )
sem_Type ( TMap key_ value_ ) = sem_Type_TMap ( sem_Type key_ ) ( sem_Type value_ )
sem_Type ( TIntMap value_ ) = sem_Type_TIntMap ( sem_Type value_ )
sem_Type ( TSet tp_ ) = sem_Type_TSet ( sem_Type tp_ )
sem_Type ( TIntSet  ) = sem_Type_TIntSet 

-- semantic domain
newtype T_Type  = T_Type {
                         attach_T_Type :: Identity (T_Type_s50 )
                         }
newtype T_Type_s50  = C_Type_s50 {
                                 inv_Type_s50 :: (T_Type_v49 )
                                 }
data T_Type_s51  = C_Type_s51
type T_Type_v49  = (T_Type_vIn49 ) -> (T_Type_vOut49 )
data T_Type_vIn49  = T_Type_vIn49 (Bool)
data T_Type_vOut49  = T_Type_vOut49 (TH.Type)
{-# NOINLINE sem_Type_Arr #-}
sem_Type_Arr :: T_Type  -> T_Type  -> T_Type 
sem_Type_Arr arg_left_ arg_right_ = T_Type (return st50) where
   {-# NOINLINE st50 #-}
   !st50 = let
      v49 :: T_Type_v49 
      v49 = \ !(T_Type_vIn49 _lhsInested) -> ( let
         _leftX50 = Control.Monad.Identity.runIdentity (attach_T_Type (arg_left_))
         _rightX50 = Control.Monad.Identity.runIdentity (attach_T_Type (arg_right_))
         (T_Type_vOut49 _leftIth) = inv_Type_s50 _leftX50 (T_Type_vIn49 _leftOnested)
         (T_Type_vOut49 _rightIth) = inv_Type_s50 _rightX50 (T_Type_vIn49 _rightOnested)
         _lhsOth :: TH.Type
         _lhsOth = rule306 _leftIth _rightIth
         _leftOnested = rule307 _lhsInested
         _rightOnested = rule308 _lhsInested
         !__result_ = T_Type_vOut49 _lhsOth
         in __result_ )
     in C_Type_s50 v49
   {-# INLINE rule306 #-}
   {-# LINE 289 "src-ag/Code2TH.ag" #-}
   rule306 = \ ((_leftIth) :: TH.Type) ((_rightIth) :: TH.Type) ->
                                {-# LINE 289 "src-ag/Code2TH.ag" #-}
                                TH.AppT (TH.AppT TH.ArrowT _leftIth) _rightIth
                                {-# LINE 3132 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule307 #-}
   rule307 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule308 #-}
   rule308 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
{-# NOINLINE sem_Type_CtxApp #-}
sem_Type_CtxApp :: ([(String, [String])]) -> T_Type  -> T_Type 
sem_Type_CtxApp !arg_left_ arg_right_ = T_Type (return st50) where
   {-# NOINLINE st50 #-}
   !st50 = let
      v49 :: T_Type_v49 
      v49 = \ !(T_Type_vIn49 _lhsInested) -> ( let
         _rightX50 = Control.Monad.Identity.runIdentity (attach_T_Type (arg_right_))
         (T_Type_vOut49 _rightIth) = inv_Type_s50 _rightX50 (T_Type_vIn49 _rightOnested)
         _lhsOth :: TH.Type
         _lhsOth = rule309 _rightIth arg_left_
         _rightOnested = rule310 _lhsInested
         !__result_ = T_Type_vOut49 _lhsOth
         in __result_ )
     in C_Type_s50 v49
   {-# INLINE rule309 #-}
   {-# LINE 295 "src-ag/Code2TH.ag" #-}
   rule309 = \ ((_rightIth) :: TH.Type) left_ ->
                 {-# LINE 295 "src-ag/Code2TH.ag" #-}
                 TH.ForallT [] (map (\(n,ns) -> foldl (\xs x -> TH.AppT xs (either error id $ Meta.parseType x)) (either error id $ Meta.parseType n) ns) left_) _rightIth
                 {-# LINE 3159 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule310 #-}
   rule310 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
{-# NOINLINE sem_Type_QuantApp #-}
sem_Type_QuantApp :: (String) -> T_Type  -> T_Type 
sem_Type_QuantApp !arg_left_ arg_right_ = T_Type (return st50) where
   {-# NOINLINE st50 #-}
   !st50 = let
      v49 :: T_Type_v49 
      v49 = \ !(T_Type_vIn49 _lhsInested) -> ( let
         _rightX50 = Control.Monad.Identity.runIdentity (attach_T_Type (arg_right_))
         (T_Type_vOut49 _rightIth) = inv_Type_s50 _rightX50 (T_Type_vIn49 _rightOnested)
         _lhsOth :: TH.Type
         _lhsOth = rule311 arg_left_
         _rightOnested = rule312 _lhsInested
         !__result_ = T_Type_vOut49 _lhsOth
         in __result_ )
     in C_Type_s50 v49
   {-# INLINE rule311 #-}
   {-# LINE 297 "src-ag/Code2TH.ag" #-}
   rule311 = \ left_ ->
                 {-# LINE 297 "src-ag/Code2TH.ag" #-}
                 error ("Deal with quant: " ++ left_)
                 {-# LINE 3183 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule312 #-}
   rule312 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
{-# NOINLINE sem_Type_TypeApp #-}
sem_Type_TypeApp :: T_Type  -> T_Types  -> T_Type 
sem_Type_TypeApp arg_func_ arg_args_ = T_Type (return st50) where
   {-# NOINLINE st50 #-}
   !st50 = let
      v49 :: T_Type_v49 
      v49 = \ !(T_Type_vIn49 _lhsInested) -> ( let
         _funcX50 = Control.Monad.Identity.runIdentity (attach_T_Type (arg_func_))
         _argsX53 = Control.Monad.Identity.runIdentity (attach_T_Types (arg_args_))
         (T_Type_vOut49 _funcIth) = inv_Type_s50 _funcX50 (T_Type_vIn49 _funcOnested)
         (T_Types_vOut52 _argsIth) = inv_Types_s53 _argsX53 (T_Types_vIn52 _argsOnested)
         _lhsOth :: TH.Type
         _lhsOth = rule313 _argsIth _funcIth
         _funcOnested = rule314 _lhsInested
         _argsOnested = rule315 _lhsInested
         !__result_ = T_Type_vOut49 _lhsOth
         in __result_ )
     in C_Type_s50 v49
   {-# INLINE rule313 #-}
   {-# LINE 292 "src-ag/Code2TH.ag" #-}
   rule313 = \ ((_argsIth) :: [TH.Type]) ((_funcIth) :: TH.Type) ->
                 {-# LINE 292 "src-ag/Code2TH.ag" #-}
                 foldl TH.AppT _funcIth _argsIth
                 {-# LINE 3210 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule314 #-}
   rule314 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule315 #-}
   rule315 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
{-# NOINLINE sem_Type_TupleType #-}
sem_Type_TupleType :: T_Types  -> T_Type 
sem_Type_TupleType arg_tps_ = T_Type (return st50) where
   {-# NOINLINE st50 #-}
   !st50 = let
      v49 :: T_Type_v49 
      v49 = \ !(T_Type_vIn49 _lhsInested) -> ( let
         _tpsX53 = Control.Monad.Identity.runIdentity (attach_T_Types (arg_tps_))
         (T_Types_vOut52 _tpsIth) = inv_Types_s53 _tpsX53 (T_Types_vIn52 _tpsOnested)
         _lhsOth :: TH.Type
         _lhsOth = rule316 _lhsInested _tpsIth
         _tpsOnested = rule317 _lhsInested
         !__result_ = T_Type_vOut49 _lhsOth
         in __result_ )
     in C_Type_s50 v49
   {-# INLINE rule316 #-}
   {-# LINE 299 "src-ag/Code2TH.ag" #-}
   rule316 = \ ((_lhsInested) :: Bool) ((_tpsIth) :: [TH.Type]) ->
                               {-# LINE 299 "src-ag/Code2TH.ag" #-}
                               tupleT False _lhsInested _tpsIth
                               {-# LINE 3237 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule317 #-}
   rule317 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
{-# NOINLINE sem_Type_UnboxedTupleType #-}
sem_Type_UnboxedTupleType :: T_Types  -> T_Type 
sem_Type_UnboxedTupleType arg_tps_ = T_Type (return st50) where
   {-# NOINLINE st50 #-}
   !st50 = let
      v49 :: T_Type_v49 
      v49 = \ !(T_Type_vIn49 _lhsInested) -> ( let
         _tpsX53 = Control.Monad.Identity.runIdentity (attach_T_Types (arg_tps_))
         (T_Types_vOut52 _tpsIth) = inv_Types_s53 _tpsX53 (T_Types_vIn52 _tpsOnested)
         _lhsOth :: TH.Type
         _lhsOth = rule318 _lhsInested _tpsIth
         _tpsOnested = rule319 _lhsInested
         !__result_ = T_Type_vOut49 _lhsOth
         in __result_ )
     in C_Type_s50 v49
   {-# INLINE rule318 #-}
   {-# LINE 301 "src-ag/Code2TH.ag" #-}
   rule318 = \ ((_lhsInested) :: Bool) ((_tpsIth) :: [TH.Type]) ->
                                      {-# LINE 301 "src-ag/Code2TH.ag" #-}
                                      tupleT True _lhsInested _tpsIth
                                      {-# LINE 3261 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule319 #-}
   rule319 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
{-# NOINLINE sem_Type_List #-}
sem_Type_List :: T_Type  -> T_Type 
sem_Type_List arg_tp_ = T_Type (return st50) where
   {-# NOINLINE st50 #-}
   !st50 = let
      v49 :: T_Type_v49 
      v49 = \ !(T_Type_vIn49 _lhsInested) -> ( let
         _tpX50 = Control.Monad.Identity.runIdentity (attach_T_Type (arg_tp_))
         (T_Type_vOut49 _tpIth) = inv_Type_s50 _tpX50 (T_Type_vIn49 _tpOnested)
         _lhsOth :: TH.Type
         _lhsOth = rule320 _tpIth
         _tpOnested = rule321 _lhsInested
         !__result_ = T_Type_vOut49 _lhsOth
         in __result_ )
     in C_Type_s50 v49
   {-# INLINE rule320 #-}
   {-# LINE 303 "src-ag/Code2TH.ag" #-}
   rule320 = \ ((_tpIth) :: TH.Type) ->
                               {-# LINE 303 "src-ag/Code2TH.ag" #-}
                               TH.ListT `TH.AppT` _tpIth
                               {-# LINE 3285 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule321 #-}
   rule321 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
{-# NOINLINE sem_Type_SimpleType #-}
sem_Type_SimpleType :: (String) -> T_Type 
sem_Type_SimpleType !arg_txt_ = T_Type (return st50) where
   {-# NOINLINE st50 #-}
   !st50 = let
      v49 :: T_Type_v49 
      v49 = \ !(T_Type_vIn49 _lhsInested) -> ( let
         _lhsOth :: TH.Type
         _lhsOth = rule322 arg_txt_
         !__result_ = T_Type_vOut49 _lhsOth
         in __result_ )
     in C_Type_s50 v49
   {-# INLINE rule322 #-}
   {-# LINE 305 "src-ag/Code2TH.ag" #-}
   rule322 = \ txt_ ->
                               {-# LINE 305 "src-ag/Code2TH.ag" #-}
                               either error id $ Meta.parseType txt_
                               {-# LINE 3306 "src-generated/Code2TH.hs" #-}
{-# NOINLINE sem_Type_NontermType #-}
sem_Type_NontermType :: (String) -> ([String]) -> (Bool) -> T_Type 
sem_Type_NontermType !arg_name_ !arg_params_ !arg_deforested_ = T_Type (return st50) where
   {-# NOINLINE st50 #-}
   !st50 = let
      v49 :: T_Type_v49 
      v49 = \ !(T_Type_vIn49 _lhsInested) -> ( let
         _lhsOth :: TH.Type
         _lhsOth = rule323 _prefix arg_name_ arg_params_
         _prefix = rule324 arg_deforested_
         !__result_ = T_Type_vOut49 _lhsOth
         in __result_ )
     in C_Type_s50 v49
   {-# INLINE rule323 #-}
   {-# LINE 307 "src-ag/Code2TH.ag" #-}
   rule323 = \ _prefix name_ params_ ->
                               {-# LINE 307 "src-ag/Code2TH.ag" #-}
                               foldl TH.AppT (TH.ConT (TH.mkName (_prefix     ++ name_))) (map (TH.VarT . TH.mkName) params_)
                               {-# LINE 3325 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule324 #-}
   {-# LINE 308 "src-ag/Code2TH.ag" #-}
   rule324 = \ deforested_ ->
                                {-# LINE 308 "src-ag/Code2TH.ag" #-}
                                if deforested_
                                then "T_"
                                else ""
                                {-# LINE 3333 "src-generated/Code2TH.hs" #-}
{-# NOINLINE sem_Type_TMaybe #-}
sem_Type_TMaybe :: T_Type  -> T_Type 
sem_Type_TMaybe arg_tp_ = T_Type (return st50) where
   {-# NOINLINE st50 #-}
   !st50 = let
      v49 :: T_Type_v49 
      v49 = \ !(T_Type_vIn49 _lhsInested) -> ( let
         _tpX50 = Control.Monad.Identity.runIdentity (attach_T_Type (arg_tp_))
         (T_Type_vOut49 _tpIth) = inv_Type_s50 _tpX50 (T_Type_vIn49 _tpOnested)
         _lhsOth :: TH.Type
         _lhsOth = rule325 _tpIth
         _tpOnested = rule326 _lhsInested
         !__result_ = T_Type_vOut49 _lhsOth
         in __result_ )
     in C_Type_s50 v49
   {-# INLINE rule325 #-}
   {-# LINE 311 "src-ag/Code2TH.ag" #-}
   rule325 = \ ((_tpIth) :: TH.Type) ->
                               {-# LINE 311 "src-ag/Code2TH.ag" #-}
                               TH.ConT (TH.mkName "Maybe") `TH.AppT` _tpIth
                               {-# LINE 3354 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule326 #-}
   rule326 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
{-# NOINLINE sem_Type_TEither #-}
sem_Type_TEither :: T_Type  -> T_Type  -> T_Type 
sem_Type_TEither arg_left_ arg_right_ = T_Type (return st50) where
   {-# NOINLINE st50 #-}
   !st50 = let
      v49 :: T_Type_v49 
      v49 = \ !(T_Type_vIn49 _lhsInested) -> ( let
         _leftX50 = Control.Monad.Identity.runIdentity (attach_T_Type (arg_left_))
         _rightX50 = Control.Monad.Identity.runIdentity (attach_T_Type (arg_right_))
         (T_Type_vOut49 _leftIth) = inv_Type_s50 _leftX50 (T_Type_vIn49 _leftOnested)
         (T_Type_vOut49 _rightIth) = inv_Type_s50 _rightX50 (T_Type_vIn49 _rightOnested)
         _lhsOth :: TH.Type
         _lhsOth = rule327 _leftIth _rightIth
         _leftOnested = rule328 _lhsInested
         _rightOnested = rule329 _lhsInested
         !__result_ = T_Type_vOut49 _lhsOth
         in __result_ )
     in C_Type_s50 v49
   {-# INLINE rule327 #-}
   {-# LINE 312 "src-ag/Code2TH.ag" #-}
   rule327 = \ ((_leftIth) :: TH.Type) ((_rightIth) :: TH.Type) ->
                               {-# LINE 312 "src-ag/Code2TH.ag" #-}
                               TH.ConT (TH.mkName "Either") `TH.AppT` _leftIth `TH.AppT` _rightIth
                               {-# LINE 3381 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule328 #-}
   rule328 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule329 #-}
   rule329 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
{-# NOINLINE sem_Type_TMap #-}
sem_Type_TMap :: T_Type  -> T_Type  -> T_Type 
sem_Type_TMap arg_key_ arg_value_ = T_Type (return st50) where
   {-# NOINLINE st50 #-}
   !st50 = let
      v49 :: T_Type_v49 
      v49 = \ !(T_Type_vIn49 _lhsInested) -> ( let
         _keyX50 = Control.Monad.Identity.runIdentity (attach_T_Type (arg_key_))
         _valueX50 = Control.Monad.Identity.runIdentity (attach_T_Type (arg_value_))
         (T_Type_vOut49 _keyIth) = inv_Type_s50 _keyX50 (T_Type_vIn49 _keyOnested)
         (T_Type_vOut49 _valueIth) = inv_Type_s50 _valueX50 (T_Type_vIn49 _valueOnested)
         _lhsOth :: TH.Type
         _lhsOth = rule330 _keyIth _valueIth
         _keyOnested = rule331 _lhsInested
         _valueOnested = rule332 _lhsInested
         !__result_ = T_Type_vOut49 _lhsOth
         in __result_ )
     in C_Type_s50 v49
   {-# INLINE rule330 #-}
   {-# LINE 313 "src-ag/Code2TH.ag" #-}
   rule330 = \ ((_keyIth) :: TH.Type) ((_valueIth) :: TH.Type) ->
                               {-# LINE 313 "src-ag/Code2TH.ag" #-}
                               TH.ConT (TH.mkName "Data.Map.Map") `TH.AppT` _keyIth `TH.AppT` _valueIth
                               {-# LINE 3411 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule331 #-}
   rule331 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule332 #-}
   rule332 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
{-# NOINLINE sem_Type_TIntMap #-}
sem_Type_TIntMap :: T_Type  -> T_Type 
sem_Type_TIntMap arg_value_ = T_Type (return st50) where
   {-# NOINLINE st50 #-}
   !st50 = let
      v49 :: T_Type_v49 
      v49 = \ !(T_Type_vIn49 _lhsInested) -> ( let
         _valueX50 = Control.Monad.Identity.runIdentity (attach_T_Type (arg_value_))
         (T_Type_vOut49 _valueIth) = inv_Type_s50 _valueX50 (T_Type_vIn49 _valueOnested)
         _lhsOth :: TH.Type
         _lhsOth = rule333 _valueIth
         _valueOnested = rule334 _lhsInested
         !__result_ = T_Type_vOut49 _lhsOth
         in __result_ )
     in C_Type_s50 v49
   {-# INLINE rule333 #-}
   {-# LINE 314 "src-ag/Code2TH.ag" #-}
   rule333 = \ ((_valueIth) :: TH.Type) ->
                               {-# LINE 314 "src-ag/Code2TH.ag" #-}
                               TH.ConT (TH.mkName "Data.IntMap.IntMap") `TH.AppT` _valueIth
                               {-# LINE 3438 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule334 #-}
   rule334 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
{-# NOINLINE sem_Type_TSet #-}
sem_Type_TSet :: T_Type  -> T_Type 
sem_Type_TSet arg_tp_ = T_Type (return st50) where
   {-# NOINLINE st50 #-}
   !st50 = let
      v49 :: T_Type_v49 
      v49 = \ !(T_Type_vIn49 _lhsInested) -> ( let
         _tpX50 = Control.Monad.Identity.runIdentity (attach_T_Type (arg_tp_))
         (T_Type_vOut49 _tpIth) = inv_Type_s50 _tpX50 (T_Type_vIn49 _tpOnested)
         _lhsOth :: TH.Type
         _lhsOth = rule335 _tpIth
         _tpOnested = rule336 _lhsInested
         !__result_ = T_Type_vOut49 _lhsOth
         in __result_ )
     in C_Type_s50 v49
   {-# INLINE rule335 #-}
   {-# LINE 315 "src-ag/Code2TH.ag" #-}
   rule335 = \ ((_tpIth) :: TH.Type) ->
                               {-# LINE 315 "src-ag/Code2TH.ag" #-}
                               TH.ConT (TH.mkName "Data.Set.Set") `TH.AppT` _tpIth
                               {-# LINE 3462 "src-generated/Code2TH.hs" #-}
   {-# INLINE rule336 #-}
   rule336 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
{-# NOINLINE sem_Type_TIntSet #-}
sem_Type_TIntSet ::  T_Type 
sem_Type_TIntSet  = T_Type (return st50) where
   {-# NOINLINE st50 #-}
   !st50 = let
      v49 :: T_Type_v49 
      v49 = \ !(T_Type_vIn49 _lhsInested) -> ( let
         _lhsOth :: TH.Type
         _lhsOth = rule337  ()
         !__result_ = T_Type_vOut49 _lhsOth
         in __result_ )
     in C_Type_s50 v49
   {-# INLINE rule337 #-}
   {-# LINE 316 "src-ag/Code2TH.ag" #-}
   rule337 = \  (_ :: ()) ->
                               {-# LINE 316 "src-ag/Code2TH.ag" #-}
                               TH.ConT (TH.mkName "Data.IntSet.IntSet")
                               {-# LINE 3483 "src-generated/Code2TH.hs" #-}

-- Types -------------------------------------------------------
-- wrapper
data Inh_Types  = Inh_Types { nested_Inh_Types :: !(Bool) }
data Syn_Types  = Syn_Types { th_Syn_Types :: !([TH.Type]) }
{-# INLINABLE wrap_Types #-}
wrap_Types :: T_Types  -> Inh_Types  -> (Syn_Types )
wrap_Types !(T_Types act) !(Inh_Types _lhsInested) =
   Control.Monad.Identity.runIdentity (
     do !sem <- act
        let arg52 = T_Types_vIn52 _lhsInested
        !(T_Types_vOut52 _lhsOth) <- return (inv_Types_s53 sem arg52)
        return (Syn_Types _lhsOth)
   )

-- cata
{-# NOINLINE sem_Types #-}
sem_Types :: Types  -> T_Types 
sem_Types list = Prelude.foldr sem_Types_Cons sem_Types_Nil (Prelude.map sem_Type list)

-- semantic domain
newtype T_Types  = T_Types {
                           attach_T_Types :: Identity (T_Types_s53 )
                           }
newtype T_Types_s53  = C_Types_s53 {
                                   inv_Types_s53 :: (T_Types_v52 )
                                   }
data T_Types_s54  = C_Types_s54
type T_Types_v52  = (T_Types_vIn52 ) -> (T_Types_vOut52 )
data T_Types_vIn52  = T_Types_vIn52 (Bool)
data T_Types_vOut52  = T_Types_vOut52 ([TH.Type])
{-# NOINLINE sem_Types_Cons #-}
sem_Types_Cons :: T_Type  -> T_Types  -> T_Types 
sem_Types_Cons arg_hd_ arg_tl_ = T_Types (return st53) where
   {-# NOINLINE st53 #-}
   !st53 = let
      v52 :: T_Types_v52 
      v52 = \ !(T_Types_vIn52 _lhsInested) -> ( let
         _hdX50 = Control.Monad.Identity.runIdentity (attach_T_Type (arg_hd_))
         _tlX53 = Control.Monad.Identity.runIdentity (attach_T_Types (arg_tl_))
         (T_Type_vOut49 _hdIth) = inv_Type_s50 _hdX50 (T_Type_vIn49 _hdOnested)
         (T_Types_vOut52 _tlIth) = inv_Types_s53 _tlX53 (T_Types_vIn52 _tlOnested)
         _lhsOth :: [TH.Type]
         _lhsOth = rule338 _hdIth _tlIth
         _hdOnested = rule339 _lhsInested
         _tlOnested = rule340 _lhsInested
         !__result_ = T_Types_vOut52 _lhsOth
         in __result_ )
     in C_Types_s53 v52
   {-# INLINE rule338 #-}
   rule338 = \ ((_hdIth) :: TH.Type) ((_tlIth) :: [TH.Type]) ->
     _hdIth : _tlIth
   {-# INLINE rule339 #-}
   rule339 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
   {-# INLINE rule340 #-}
   rule340 = \ ((_lhsInested) :: Bool) ->
     _lhsInested
{-# NOINLINE sem_Types_Nil #-}
sem_Types_Nil ::  T_Types 
sem_Types_Nil  = T_Types (return st53) where
   {-# NOINLINE st53 #-}
   !st53 = let
      v52 :: T_Types_v52 
      v52 = \ !(T_Types_vIn52 _lhsInested) -> ( let
         _lhsOth :: [TH.Type]
         _lhsOth = rule341  ()
         !__result_ = T_Types_vOut52 _lhsOth
         in __result_ )
     in C_Types_s53 v52
   {-# INLINE rule341 #-}
   rule341 = \  (_ :: ()) ->
     []
