{-# LANGUAGE Rank2Types, GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExecutionPlan2TH where
{-# LINE 2 "src-ag/HsToken.ag" #-}

import CommonTypes
import UU.Scanner.Position(Pos)
{-# LINE 10 "src-generated/ExecutionPlan2TH.hs" #-}

{-# LINE 2 "src-ag/Expression.ag" #-}

import UU.Scanner.Position(Pos)
import HsToken
{-# LINE 16 "src-generated/ExecutionPlan2TH.hs" #-}

{-# LINE 2 "src-ag/Patterns.ag" #-}

-- Patterns.ag imports
import UU.Scanner.Position(Pos)
import CommonTypes (ConstructorIdent,Identifier)
{-# LINE 23 "src-generated/ExecutionPlan2TH.hs" #-}

{-# LINE 2 "src-ag/ExecutionPlan.ag" #-}

-- VisitSyntax.ag imports
import Patterns    (Pattern(..),Patterns)
import Expression  (Expression(..))
import CommonTypes
import ErrorMessages

import qualified Data.Set as Set
import Data.Set(Set)
import qualified Data.Map as Map
import Data.Map(Map)
{-# LINE 37 "src-generated/ExecutionPlan2TH.hs" #-}

{-# LINE 7 "src-ag/ExecutionPlan2TH.ag" #-}

import ExecutionPlan
-- import Pretty
import PPUtil
import Options
import Data.Monoid(mappend,mempty)
import Data.Maybe
import Debug.Trace
import System.IO
import System.Directory
import System.FilePath
import UU.Scanner.Position

import TokenDef
import HsToken
import ErrorMessages

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence(Seq)
import qualified Data.Sequence as Seq
import Data.Foldable(toList)

import Control.Applicative (Alternative (empty))

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH
import qualified Language.Haskell.Meta as Meta
{-# LINE 70 "src-generated/ExecutionPlan2TH.hs" #-}
import Control.Monad.Identity (Identity)
import qualified Control.Monad.Identity
{-# LINE 39 "src-ag/ExecutionPlan2TH.ag" #-}

nobang :: TH.Bang
nobang = TH.Bang TH.NoSourceUnpackedness TH.NoSourceStrictness	
{-# LINE 77 "src-generated/ExecutionPlan2TH.hs" #-}

{-# LINE 156 "src-ag/ExecutionPlan2TH.ag" #-}

typeToTH :: Type -> TH.Type
typeToTH = either (error . ("Cannot convert type: " ++)) id . Meta.parseType . typeToHaskellString (error "Self types should have been resolved by now") []

identToName :: Identifier -> TH.Name
identToName = TH.mkName . getName

quantsTH :: ([TH.TyVarBndr] -> TH.Cxt -> x -> x) -> [Identifier] -> TH.Cxt -> x -> x
quantsTH foral ids ctx con = snd $ foldr (\x ~(b, xs) -> (False, foral [] (if b then ctx else []) xs)) (True, con) ids
{-# LINE 89 "src-generated/ExecutionPlan2TH.hs" #-}

{-# LINE 181 "src-ag/ExecutionPlan2TH.ag" #-}

conTH :: TH.Name -> Either [TH.VarBangType] [TH.BangType] -> TH.Con
conTH nt (Left args) = TH.RecC nt args
conTH nt (Right args) = TH.NormalC nt args
{-# LINE 96 "src-generated/ExecutionPlan2TH.hs" #-}

{-# LINE 380 "src-ag/ExecutionPlan2TH.ag" #-}
type VisitStateState = (VisitIdentifier,StateIdentifier, StateIdentifier)
{-# LINE 100 "src-generated/ExecutionPlan2TH.hs" #-}

{-# LINE 476 "src-ag/ExecutionPlan2TH.ag" #-}

-- conNmTVisit nt vId      = "T_" >|< nt >|< "_v"    >|< vId
-- conNmTVisitIn nt vId    = "T_" >|< nt >|< "_vIn"  >|< vId
-- conNmTVisitOut nt vId   = "T_" >|< nt >|< "_vOut" >|< vId
-- conNmTNextVisit nt stId = "T_" >|< nt >|< "_s"    >|< stId

monadTypeTH :: Options -> TH.Type -> TH.Type
monadTypeTH opts
  | parallelInvoke opts = TH.AppT (TH.ConT (TH.mkName "IO"))
  | otherwise           = TH.AppT (TH.ConT (TH.mkName "Identity"))
{-# LINE 113 "src-generated/ExecutionPlan2TH.hs" #-}

{-# LINE 830 "src-ag/ExecutionPlan2TH.ag" #-}

resultValName :: String
resultValName = "__result_"

nextStName :: String
nextStName = "__st_"
{-# LINE 122 "src-generated/ExecutionPlan2TH.hs" #-}

{-# LINE 901 "src-ag/ExecutionPlan2TH.ag" #-}

parResultName :: String
parResultName = "__outcome_"

-- fmtDecl :: PP a => Bool -> FormatMode -> a -> PP_Doc
-- fmtDecl declPure fmt decl = case fmt of
--   FormatLetDecl -> pp decl
--   FormatLetLine -> "let" >#< decl >#< "in"
--   FormatDo | declPure  -> "let" >#< decl
--            | otherwise -> pp decl
{-# LINE 135 "src-generated/ExecutionPlan2TH.hs" #-}

{-# LINE 1227 "src-ag/ExecutionPlan2TH.ag" #-}

data NonLocalAttr
  = AttrInh Identifier Identifier
  | AttrSyn Identifier Identifier deriving Show

mkNonLocalAttr :: Bool -> Identifier -> Identifier -> NonLocalAttr
mkNonLocalAttr True  = AttrInh  -- True: inherited attr
mkNonLocalAttr False = AttrSyn

-- lookupAttrType :: NonLocalAttr -> Map Identifier Attributes -> Map Identifier Attributes -> Map Identifier Type -> Maybe PP_Doc
-- lookupAttrType (AttrInh child name) inhs _ = lookupType child name inhs
-- lookupAttrType (AttrSyn child name) _ syns = lookupType child name syns

-- -- Note: if the child takes type parameters, the type of an attribute of this child may refer to these parameters. This means that
-- -- the actual type of the attribute needs to have its type parameters substituted with the actual type argument of the child.
-- -- However, for now we simply decide to return Nothing in this case, which skips the type annotation.
-- lookupType :: Identifier -> Identifier -> Map Identifier Attributes -> Map Identifier Type -> Maybe PP_Doc
-- lookupType child name attrMp childMp
--   | noParameters childTp = Just ppDoc
--   | otherwise            = Nothing
--   where
--     attrTp     = Map.findWithDefault (error "lookupType: the attribute is not in the attrs of the child") name childAttrs
--     childAttrs = Map.findWithDefault (error "lookupType: the attributes of the nonterm are not in the map") nonterm attrMp
--     nonterm    = extractNonterminal childTp
--     childTp    = Map.findWithDefault (error ("lookupType: the child " ++ show child ++ "is not in the appropriate map")) child childMp
--     ppDoc      = ppTp attrTp

noParameters :: Type -> Bool
noParameters (Haskell _)   = True
noParameters (NT _ args _) = null args
{-# LINE 168 "src-generated/ExecutionPlan2TH.hs" #-}

{-# LINE 1336 "src-ag/ExecutionPlan2TH.ag" #-}

unionWithSum = Map.unionWith (+)
{-# LINE 173 "src-generated/ExecutionPlan2TH.hs" #-}

{-# LINE 1359 "src-ag/ExecutionPlan2TH.ag" #-}

uwSetUnion :: (Ord a, Ord b) => Map a (Set b) -> Map a (Set b) -> Map a (Set b)
uwSetUnion = Map.unionWith Set.union

uwMapUnion :: (Ord a, Ord b) => Map a (Map b c) -> Map a (Map b c) -> Map a (Map b c)
uwMapUnion = Map.unionWith Map.union
{-# LINE 182 "src-generated/ExecutionPlan2TH.hs" #-}

{-# LINE 1576 "src-ag/ExecutionPlan2TH.ag" #-}

-- renderDocs :: [PP_Doc] -> String
-- renderDocs pps = foldr (.) id (map (\d -> (disp d 50000) . ( '\n':) ) pps) ""
-- 
-- writeModule :: FilePath -> [PP_Doc] -> IO ()
-- writeModule path docs
--   = do bExists <- doesFileExist path
--        if bExists
--         then do input <- readFile path
--                 seq (length input) (return ())
--                 if input /= output
--                  then dumpIt
--                  else return ()
--         else dumpIt
--   where
--     output = renderDocs docs
--     dumpIt = writeFile path output
{-# LINE 202 "src-generated/ExecutionPlan2TH.hs" #-}

{-# LINE 1723 "src-ag/ExecutionPlan2TH.ag" #-}

noInlineTH :: TH.Name -> TH.Dec
noInlineTH = pragmaBindingTH TH.NoInline

inlineTH :: TH.Name -> TH.Dec
inlineTH = pragmaBindingTH TH.Inline

inlinableTH :: TH.Name -> TH.Dec
inlinableTH = pragmaBindingTH TH.Inlinable

pragmaBindingTH :: TH.Inline -> TH.Name -> TH.Dec
pragmaBindingTH inl nm = TH.PragmaD (TH.InlineP nm inl TH.FunLike TH.AllPhases)

-- Cost centres are not yet supported by Template Haskell
--
-- costCentreTH :: TH.Name -> TH.Dec
-- costCentreTH nm = "{-#" >#< "SCC" >#< "\"" >|< nm >|< "\"" >#< "#-}"

-- warrenFlagsPP :: Options -> PP_Doc
-- warrenFlagsPP options = vlist
--   [ pp "{-# LANGUAGE Rank2Types, GADTs #-}"
--   , if bangpats options
--     then pp "{-# LANGUAGE BangPatterns #-}"
--     else empty
--   , if noPerRuleTypeSigs options && noPerStateTypeSigs options
--     then empty
--     else pp "{-# LANGUAGE ScopedTypeVariables #-}"
--   , if tupleAsDummyToken options
--     then empty
--     else pp "{-# LANGUAGE ScopedTypeVariables, MagicHash #-}"
--   , -- not that the meaning of "unbox" is here that strict fields in data types may be
--     -- unboxed if possible. This may affect user-defined data types declared in the module.
--     -- Unfortunately, we cannot turn it on for only the AG generated data types without
--     -- causing a zillion of warnings.
--     if unbox options && bangpats options
--         then pp $ "{-# OPTIONS_GHC -funbox-strict-fields -fstrictness #-}"
--         else empty
--   , if parallelInvoke options && not (noEagerBlackholing options)
--     then pp $ "{-# OPTIONS_GHC -feager-blackholing #-}"
--     else empty
--   ]
{-# LINE 246 "src-generated/ExecutionPlan2TH.hs" #-}
-- EChild ------------------------------------------------------
-- wrapper
data Inh_EChild  = Inh_EChild { allInitStates_Inh_EChild :: (Map NontermIdent Int), con_Inh_EChild :: (ConstructorIdent), importBlocks_Inh_EChild :: ([String]), mainFile_Inh_EChild :: (String), mainName_Inh_EChild :: (String), moduleHeader_Inh_EChild :: (String -> String -> String -> Bool -> String), nt_Inh_EChild :: (NontermIdent), options_Inh_EChild :: (Options), pragmaBlocks_Inh_EChild :: (String), textBlocks_Inh_EChild :: ([String]) }
data Syn_EChild  = Syn_EChild { argnamesw_Syn_EChild :: ( Maybe TH.Exp ), argpats_Syn_EChild :: (  Maybe TH.Pat  ), argtps_Syn_EChild :: (  Maybe (TH.Type -> TH.Type)  ), childTypes_Syn_EChild :: (Map Identifier Type), datatype_Syn_EChild :: (Maybe TH.BangType), datatypeVar_Syn_EChild :: (Maybe TH.VarBangType), terminaldefs_Syn_EChild :: (Set String), usedArgs_Syn_EChild :: (Set String) }
{-# INLINABLE wrap_EChild #-}
wrap_EChild :: T_EChild  -> Inh_EChild  -> (Syn_EChild )
wrap_EChild (T_EChild act) (Inh_EChild _lhsIallInitStates _lhsIcon _lhsIimportBlocks _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInt _lhsIoptions _lhsIpragmaBlocks _lhsItextBlocks) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg1 = T_EChild_vIn1 _lhsIallInitStates _lhsIcon _lhsIimportBlocks _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInt _lhsIoptions _lhsIpragmaBlocks _lhsItextBlocks
        (T_EChild_vOut1 _lhsOargnamesw _lhsOargpats _lhsOargtps _lhsOchildTypes _lhsOdatatype _lhsOdatatypeVar _lhsOterminaldefs _lhsOusedArgs) <- return (inv_EChild_s2 sem arg1)
        return (Syn_EChild _lhsOargnamesw _lhsOargpats _lhsOargtps _lhsOchildTypes _lhsOdatatype _lhsOdatatypeVar _lhsOterminaldefs _lhsOusedArgs)
   )

-- cata
{-# NOINLINE sem_EChild #-}
sem_EChild :: EChild  -> T_EChild 
sem_EChild ( EChild name_ tp_ kind_ hasAround_ merges_ isMerged_ ) = sem_EChild_EChild name_ tp_ kind_ hasAround_ merges_ isMerged_
sem_EChild ( ETerm name_ tp_ ) = sem_EChild_ETerm name_ tp_

-- semantic domain
newtype T_EChild  = T_EChild {
                             attach_T_EChild :: Identity (T_EChild_s2 )
                             }
newtype T_EChild_s2  = C_EChild_s2 {
                                   inv_EChild_s2 :: (T_EChild_v1 )
                                   }
data T_EChild_s3  = C_EChild_s3
type T_EChild_v1  = (T_EChild_vIn1 ) -> (T_EChild_vOut1 )
data T_EChild_vIn1  = T_EChild_vIn1 (Map NontermIdent Int) (ConstructorIdent) ([String]) (String) (String) (String -> String -> String -> Bool -> String) (NontermIdent) (Options) (String) ([String])
data T_EChild_vOut1  = T_EChild_vOut1 ( Maybe TH.Exp ) (  Maybe TH.Pat  ) (  Maybe (TH.Type -> TH.Type)  ) (Map Identifier Type) (Maybe TH.BangType) (Maybe TH.VarBangType) (Set String) (Set String)
{-# NOINLINE sem_EChild_EChild #-}
sem_EChild_EChild :: (Identifier) -> (Type) -> (ChildKind) -> (Bool) -> (Maybe [Identifier]) -> (Bool) -> T_EChild 
sem_EChild_EChild arg_name_ arg_tp_ arg_kind_ _ _ _ = T_EChild (return st2) where
   {-# NOINLINE st2 #-}
   st2 = let
      v1 :: T_EChild_v1 
      v1 = \ (T_EChild_vIn1 _lhsIallInitStates _lhsIcon _lhsIimportBlocks _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInt _lhsIoptions _lhsIpragmaBlocks _lhsItextBlocks) -> ( let
         _tpTH = rule0 arg_tp_
         _strNm = rule1 _lhsIcon _lhsInt arg_name_
         _field = rule2 _bang _tpTH
         _fieldVar = rule3 _bang _strNm _tpTH
         _bang = rule4 _lhsIoptions
         _lhsOdatatype :: Maybe TH.BangType
         _lhsOdatatype = rule5 _field arg_kind_
         _lhsOargnamesw ::  Maybe TH.Exp 
         _lhsOargnamesw = rule6 _nt arg_kind_ arg_name_
         _lhsOargtps ::   Maybe (TH.Type -> TH.Type)  
         _lhsOargtps = rule7 arg_kind_ arg_tp_
         _argpats = rule8 arg_kind_ arg_name_
         _nt = rule9 arg_tp_
         _addbang = rule10 _lhsIoptions
         _lhsOchildTypes :: Map Identifier Type
         _lhsOchildTypes = rule11 arg_name_ arg_tp_
         _initSt = rule12 _lhsIallInitStates _nt
         _lhsOterminaldefs :: Set String
         _lhsOterminaldefs = rule13  ()
         _lhsOusedArgs :: Set String
         _lhsOusedArgs = rule14  ()
         _lhsOargpats ::   Maybe TH.Pat  
         _lhsOargpats = rule15 _argpats
         _lhsOdatatypeVar :: Maybe TH.VarBangType
         _lhsOdatatypeVar = rule16  ()
         __result_ = T_EChild_vOut1 _lhsOargnamesw _lhsOargpats _lhsOargtps _lhsOchildTypes _lhsOdatatype _lhsOdatatypeVar _lhsOterminaldefs _lhsOusedArgs
         in __result_ )
     in C_EChild_s2 v1
   {-# INLINE rule0 #-}
   {-# LINE 194 "src-ag/ExecutionPlan2TH.ag" #-}
   rule0 = \ tp_ ->
                    {-# LINE 194 "src-ag/ExecutionPlan2TH.ag" #-}
                    typeToTH (removeDeforested tp_)
                    {-# LINE 318 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule1 #-}
   {-# LINE 195 "src-ag/ExecutionPlan2TH.ag" #-}
   rule1 = \ ((_lhsIcon) :: ConstructorIdent) ((_lhsInt) :: NontermIdent) name_ ->
                     {-# LINE 195 "src-ag/ExecutionPlan2TH.ag" #-}
                     recordFieldname _lhsInt _lhsIcon name_
                     {-# LINE 324 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule2 #-}
   {-# LINE 196 "src-ag/ExecutionPlan2TH.ag" #-}
   rule2 = \ _bang _tpTH ->
                    {-# LINE 196 "src-ag/ExecutionPlan2TH.ag" #-}
                    (_bang    , _tpTH    )
                    {-# LINE 330 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule3 #-}
   {-# LINE 197 "src-ag/ExecutionPlan2TH.ag" #-}
   rule3 = \ _bang _strNm _tpTH ->
                       {-# LINE 197 "src-ag/ExecutionPlan2TH.ag" #-}
                       (TH.mkName _strNm    , _bang    , _tpTH    )
                       {-# LINE 336 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule4 #-}
   {-# LINE 198 "src-ag/ExecutionPlan2TH.ag" #-}
   rule4 = \ ((_lhsIoptions) :: Options) ->
                   {-# LINE 198 "src-ag/ExecutionPlan2TH.ag" #-}
                   TH.Bang TH.NoSourceUnpackedness (if strictData _lhsIoptions then TH.SourceStrict else TH.NoSourceStrictness)
                   {-# LINE 342 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule5 #-}
   {-# LINE 199 "src-ag/ExecutionPlan2TH.ag" #-}
   rule5 = \ _field kind_ ->
                             {-# LINE 199 "src-ag/ExecutionPlan2TH.ag" #-}
                             case kind_ of
                               ChildAttr -> empty
                               _         -> pure _field
                             {-# LINE 350 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule6 #-}
   {-# LINE 346 "src-ag/ExecutionPlan2TH.ag" #-}
   rule6 = \ _nt kind_ name_ ->
                             {-# LINE 346 "src-ag/ExecutionPlan2TH.ag" #-}
                             case kind_ of
                               ChildSyntax     -> Just $ TH.VarE (TH.mkName ("sem_" ++ getName _nt    ))
                                                        `TH.AppE` TH.VarE (TH.mkName (getName name_ ++ "_"))
                               ChildAttr       -> Nothing
                               ChildReplace tp -> Just $ TH.VarE (TH.mkName ("sem_" ++ getName (extractNonterminal tp)))
                                                        `TH.AppE` TH.VarE (TH.mkName (getName name_ ++ "_"))
                             {-# LINE 361 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule7 #-}
   {-# LINE 630 "src-ag/ExecutionPlan2TH.ag" #-}
   rule7 = \ kind_ tp_ ->
                            {-# LINE 630 "src-ag/ExecutionPlan2TH.ag" #-}
                            case kind_ of
                              ChildSyntax     -> pure (typeToTH tp_ `TH.AppT` TH.ArrowT `TH.AppT`)
                              ChildReplace tp -> pure (typeToTH tp `TH.AppT` TH.ArrowT `TH.AppT`)
                              _               -> empty
                            {-# LINE 370 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule8 #-}
   {-# LINE 634 "src-ag/ExecutionPlan2TH.ag" #-}
   rule8 = \ kind_ name_ ->
                           {-# LINE 634 "src-ag/ExecutionPlan2TH.ag" #-}
                           case kind_ of
                             ChildSyntax    -> Just (TH.VarP (TH.mkName (getName name_ ++ "_")))
                             ChildReplace _ -> Just (TH.VarP (TH.mkName (getName name_ ++ "_")))
                             _              -> Nothing
                           {-# LINE 379 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule9 #-}
   {-# LINE 1025 "src-ag/ExecutionPlan2TH.ag" #-}
   rule9 = \ tp_ ->
                            {-# LINE 1025 "src-ag/ExecutionPlan2TH.ag" #-}
                            extractNonterminal tp_
                            {-# LINE 385 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule10 #-}
   {-# LINE 1604 "src-ag/ExecutionPlan2TH.ag" #-}
   rule10 = \ ((_lhsIoptions) :: Options) ->
                                                    {-# LINE 1604 "src-ag/ExecutionPlan2TH.ag" #-}
                                                    \x -> if bangpats _lhsIoptions then TH.BangP x else x
                                                    {-# LINE 391 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule11 #-}
   {-# LINE 1656 "src-ag/ExecutionPlan2TH.ag" #-}
   rule11 = \ name_ tp_ ->
                     {-# LINE 1656 "src-ag/ExecutionPlan2TH.ag" #-}
                     Map.singleton name_ tp_
                     {-# LINE 397 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule12 #-}
   {-# LINE 1700 "src-ag/ExecutionPlan2TH.ag" #-}
   rule12 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) _nt ->
                 {-# LINE 1700 "src-ag/ExecutionPlan2TH.ag" #-}
                 Map.findWithDefault (error "nonterminal not in allInitStates map") _nt     _lhsIallInitStates
                 {-# LINE 403 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule13 #-}
   rule13 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule14 #-}
   rule14 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule15 #-}
   rule15 = \ _argpats ->
     _argpats
   {-# INLINE rule16 #-}
   rule16 = \  (_ :: ()) ->
     error "missing rule: EChild.EChild.lhs.datatypeVar"
{-# NOINLINE sem_EChild_ETerm #-}
sem_EChild_ETerm :: (Identifier) -> (Type) -> T_EChild 
sem_EChild_ETerm arg_name_ arg_tp_ = T_EChild (return st2) where
   {-# NOINLINE st2 #-}
   st2 = let
      v1 :: T_EChild_v1 
      v1 = \ (T_EChild_vIn1 _lhsIallInitStates _lhsIcon _lhsIimportBlocks _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInt _lhsIoptions _lhsIpragmaBlocks _lhsItextBlocks) -> ( let
         _tpTH = rule17 arg_tp_
         _strNm = rule18 _lhsIcon _lhsInt arg_name_
         _field = rule19 _bang _tpTH
         _fieldVar = rule20 _bang _strNm _tpTH
         _bang = rule21 _lhsIoptions
         _lhsOdatatype :: Maybe TH.BangType
         _lhsOdatatype = rule22 _field
         _lhsOdatatypeVar :: Maybe TH.VarBangType
         _lhsOdatatypeVar = rule23 _fieldVar
         _lhsOargnamesw ::  Maybe TH.Exp 
         _lhsOargnamesw = rule24 arg_name_
         _lhsOargtps ::   Maybe (TH.Type -> TH.Type)  
         _lhsOargtps = rule25 arg_tp_
         _argpats = rule26 _addbang arg_name_
         _addbang = rule27 _lhsIoptions
         _lhsOchildTypes :: Map Identifier Type
         _lhsOchildTypes = rule28 arg_name_ arg_tp_
         _lhsOterminaldefs :: Set String
         _lhsOterminaldefs = rule29  ()
         _lhsOusedArgs :: Set String
         _lhsOusedArgs = rule30  ()
         _lhsOargpats ::   Maybe TH.Pat  
         _lhsOargpats = rule31 _argpats
         __result_ = T_EChild_vOut1 _lhsOargnamesw _lhsOargpats _lhsOargtps _lhsOchildTypes _lhsOdatatype _lhsOdatatypeVar _lhsOterminaldefs _lhsOusedArgs
         in __result_ )
     in C_EChild_s2 v1
   {-# INLINE rule17 #-}
   {-# LINE 194 "src-ag/ExecutionPlan2TH.ag" #-}
   rule17 = \ tp_ ->
                    {-# LINE 194 "src-ag/ExecutionPlan2TH.ag" #-}
                    typeToTH (removeDeforested tp_)
                    {-# LINE 454 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule18 #-}
   {-# LINE 195 "src-ag/ExecutionPlan2TH.ag" #-}
   rule18 = \ ((_lhsIcon) :: ConstructorIdent) ((_lhsInt) :: NontermIdent) name_ ->
                     {-# LINE 195 "src-ag/ExecutionPlan2TH.ag" #-}
                     recordFieldname _lhsInt _lhsIcon name_
                     {-# LINE 460 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule19 #-}
   {-# LINE 196 "src-ag/ExecutionPlan2TH.ag" #-}
   rule19 = \ _bang _tpTH ->
                    {-# LINE 196 "src-ag/ExecutionPlan2TH.ag" #-}
                    (_bang    , _tpTH    )
                    {-# LINE 466 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule20 #-}
   {-# LINE 197 "src-ag/ExecutionPlan2TH.ag" #-}
   rule20 = \ _bang _strNm _tpTH ->
                       {-# LINE 197 "src-ag/ExecutionPlan2TH.ag" #-}
                       (TH.mkName _strNm    , _bang    , _tpTH    )
                       {-# LINE 472 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule21 #-}
   {-# LINE 198 "src-ag/ExecutionPlan2TH.ag" #-}
   rule21 = \ ((_lhsIoptions) :: Options) ->
                   {-# LINE 198 "src-ag/ExecutionPlan2TH.ag" #-}
                   TH.Bang TH.NoSourceUnpackedness (if strictData _lhsIoptions then TH.SourceStrict else TH.NoSourceStrictness)
                   {-# LINE 478 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule22 #-}
   {-# LINE 203 "src-ag/ExecutionPlan2TH.ag" #-}
   rule22 = \ _field ->
                             {-# LINE 203 "src-ag/ExecutionPlan2TH.ag" #-}
                             Just _field
                             {-# LINE 484 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule23 #-}
   {-# LINE 204 "src-ag/ExecutionPlan2TH.ag" #-}
   rule23 = \ _fieldVar ->
                                {-# LINE 204 "src-ag/ExecutionPlan2TH.ag" #-}
                                Just _fieldVar
                                {-# LINE 490 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule24 #-}
   {-# LINE 352 "src-ag/ExecutionPlan2TH.ag" #-}
   rule24 = \ name_ ->
                             {-# LINE 352 "src-ag/ExecutionPlan2TH.ag" #-}
                             Just $ TH.VarE (TH.mkName (fieldname name_))
                             {-# LINE 496 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule25 #-}
   {-# LINE 638 "src-ag/ExecutionPlan2TH.ag" #-}
   rule25 = \ tp_ ->
                           {-# LINE 638 "src-ag/ExecutionPlan2TH.ag" #-}
                           pure $ \res -> (typeToTH tp_) `TH.AppT` TH.ArrowT `TH.AppT` res
                           {-# LINE 502 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule26 #-}
   {-# LINE 639 "src-ag/ExecutionPlan2TH.ag" #-}
   rule26 = \ _addbang name_ ->
                           {-# LINE 639 "src-ag/ExecutionPlan2TH.ag" #-}
                           pure $ _addbang     $ TH.VarP (TH.mkName (fieldname name_))
                           {-# LINE 508 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule27 #-}
   {-# LINE 1605 "src-ag/ExecutionPlan2TH.ag" #-}
   rule27 = \ ((_lhsIoptions) :: Options) ->
                                                    {-# LINE 1605 "src-ag/ExecutionPlan2TH.ag" #-}
                                                    \x -> if bangpats _lhsIoptions then TH.BangP x else x
                                                    {-# LINE 514 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule28 #-}
   {-# LINE 1656 "src-ag/ExecutionPlan2TH.ag" #-}
   rule28 = \ name_ tp_ ->
                     {-# LINE 1656 "src-ag/ExecutionPlan2TH.ag" #-}
                     Map.singleton name_ tp_
                     {-# LINE 520 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule29 #-}
   rule29 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule30 #-}
   rule30 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule31 #-}
   rule31 = \ _argpats ->
     _argpats

-- EChildren ---------------------------------------------------
-- wrapper
data Inh_EChildren  = Inh_EChildren { allInitStates_Inh_EChildren :: (Map NontermIdent Int), con_Inh_EChildren :: (ConstructorIdent), importBlocks_Inh_EChildren :: ([String]), mainFile_Inh_EChildren :: (String), mainName_Inh_EChildren :: (String), moduleHeader_Inh_EChildren :: (String -> String -> String -> Bool -> String), nt_Inh_EChildren :: (NontermIdent), options_Inh_EChildren :: (Options), pragmaBlocks_Inh_EChildren :: (String), textBlocks_Inh_EChildren :: ([String]) }
data Syn_EChildren  = Syn_EChildren { argnamesw_Syn_EChildren :: ([TH.Exp]), argpats_Syn_EChildren :: ( [TH.Pat] ), argtps_Syn_EChildren :: ( TH.Type -> TH.Type ), childTypes_Syn_EChildren :: (Map Identifier Type), datatype_Syn_EChildren :: ([TH.BangType]), datatypeVar_Syn_EChildren :: ([TH.VarBangType]), terminaldefs_Syn_EChildren :: (Set String), usedArgs_Syn_EChildren :: (Set String) }
{-# INLINABLE wrap_EChildren #-}
wrap_EChildren :: T_EChildren  -> Inh_EChildren  -> (Syn_EChildren )
wrap_EChildren (T_EChildren act) (Inh_EChildren _lhsIallInitStates _lhsIcon _lhsIimportBlocks _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInt _lhsIoptions _lhsIpragmaBlocks _lhsItextBlocks) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg4 = T_EChildren_vIn4 _lhsIallInitStates _lhsIcon _lhsIimportBlocks _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInt _lhsIoptions _lhsIpragmaBlocks _lhsItextBlocks
        (T_EChildren_vOut4 _lhsOargnamesw _lhsOargpats _lhsOargtps _lhsOchildTypes _lhsOdatatype _lhsOdatatypeVar _lhsOterminaldefs _lhsOusedArgs) <- return (inv_EChildren_s5 sem arg4)
        return (Syn_EChildren _lhsOargnamesw _lhsOargpats _lhsOargtps _lhsOchildTypes _lhsOdatatype _lhsOdatatypeVar _lhsOterminaldefs _lhsOusedArgs)
   )

-- cata
{-# NOINLINE sem_EChildren #-}
sem_EChildren :: EChildren  -> T_EChildren 
sem_EChildren list = Prelude.foldr sem_EChildren_Cons sem_EChildren_Nil (Prelude.map sem_EChild list)

-- semantic domain
newtype T_EChildren  = T_EChildren {
                                   attach_T_EChildren :: Identity (T_EChildren_s5 )
                                   }
newtype T_EChildren_s5  = C_EChildren_s5 {
                                         inv_EChildren_s5 :: (T_EChildren_v4 )
                                         }
data T_EChildren_s6  = C_EChildren_s6
type T_EChildren_v4  = (T_EChildren_vIn4 ) -> (T_EChildren_vOut4 )
data T_EChildren_vIn4  = T_EChildren_vIn4 (Map NontermIdent Int) (ConstructorIdent) ([String]) (String) (String) (String -> String -> String -> Bool -> String) (NontermIdent) (Options) (String) ([String])
data T_EChildren_vOut4  = T_EChildren_vOut4 ([TH.Exp]) ( [TH.Pat] ) ( TH.Type -> TH.Type ) (Map Identifier Type) ([TH.BangType]) ([TH.VarBangType]) (Set String) (Set String)
{-# NOINLINE sem_EChildren_Cons #-}
sem_EChildren_Cons :: T_EChild  -> T_EChildren  -> T_EChildren 
sem_EChildren_Cons arg_hd_ arg_tl_ = T_EChildren (return st5) where
   {-# NOINLINE st5 #-}
   st5 = let
      v4 :: T_EChildren_v4 
      v4 = \ (T_EChildren_vIn4 _lhsIallInitStates _lhsIcon _lhsIimportBlocks _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInt _lhsIoptions _lhsIpragmaBlocks _lhsItextBlocks) -> ( let
         _hdX2 = Control.Monad.Identity.runIdentity (attach_T_EChild (arg_hd_))
         _tlX5 = Control.Monad.Identity.runIdentity (attach_T_EChildren (arg_tl_))
         (T_EChild_vOut1 _hdIargnamesw _hdIargpats _hdIargtps _hdIchildTypes _hdIdatatype _hdIdatatypeVar _hdIterminaldefs _hdIusedArgs) = inv_EChild_s2 _hdX2 (T_EChild_vIn1 _hdOallInitStates _hdOcon _hdOimportBlocks _hdOmainFile _hdOmainName _hdOmoduleHeader _hdOnt _hdOoptions _hdOpragmaBlocks _hdOtextBlocks)
         (T_EChildren_vOut4 _tlIargnamesw _tlIargpats _tlIargtps _tlIchildTypes _tlIdatatype _tlIdatatypeVar _tlIterminaldefs _tlIusedArgs) = inv_EChildren_s5 _tlX5 (T_EChildren_vIn4 _tlOallInitStates _tlOcon _tlOimportBlocks _tlOmainFile _tlOmainName _tlOmoduleHeader _tlOnt _tlOoptions _tlOpragmaBlocks _tlOtextBlocks)
         _lhsOargnamesw :: [TH.Exp]
         _lhsOargnamesw = rule32 _hdIargnamesw _tlIargnamesw
         _lhsOargpats ::  [TH.Pat] 
         _lhsOargpats = rule33 _hdIargpats _tlIargpats
         _lhsOargtps ::  TH.Type -> TH.Type 
         _lhsOargtps = rule34 _hdIargtps _tlIargtps
         _lhsOchildTypes :: Map Identifier Type
         _lhsOchildTypes = rule35 _hdIchildTypes _tlIchildTypes
         _lhsOdatatype :: [TH.BangType]
         _lhsOdatatype = rule36 _hdIdatatype _tlIdatatype
         _lhsOterminaldefs :: Set String
         _lhsOterminaldefs = rule37 _hdIterminaldefs _tlIterminaldefs
         _lhsOusedArgs :: Set String
         _lhsOusedArgs = rule38 _hdIusedArgs _tlIusedArgs
         _lhsOdatatypeVar :: [TH.VarBangType]
         _lhsOdatatypeVar = rule39 _tlIdatatypeVar
         _hdOallInitStates = rule40 _lhsIallInitStates
         _hdOcon = rule41 _lhsIcon
         _hdOimportBlocks = rule42 _lhsIimportBlocks
         _hdOmainFile = rule43 _lhsImainFile
         _hdOmainName = rule44 _lhsImainName
         _hdOmoduleHeader = rule45 _lhsImoduleHeader
         _hdOnt = rule46 _lhsInt
         _hdOoptions = rule47 _lhsIoptions
         _hdOpragmaBlocks = rule48 _lhsIpragmaBlocks
         _hdOtextBlocks = rule49 _lhsItextBlocks
         _tlOallInitStates = rule50 _lhsIallInitStates
         _tlOcon = rule51 _lhsIcon
         _tlOimportBlocks = rule52 _lhsIimportBlocks
         _tlOmainFile = rule53 _lhsImainFile
         _tlOmainName = rule54 _lhsImainName
         _tlOmoduleHeader = rule55 _lhsImoduleHeader
         _tlOnt = rule56 _lhsInt
         _tlOoptions = rule57 _lhsIoptions
         _tlOpragmaBlocks = rule58 _lhsIpragmaBlocks
         _tlOtextBlocks = rule59 _lhsItextBlocks
         __result_ = T_EChildren_vOut4 _lhsOargnamesw _lhsOargpats _lhsOargtps _lhsOchildTypes _lhsOdatatype _lhsOdatatypeVar _lhsOterminaldefs _lhsOusedArgs
         in __result_ )
     in C_EChildren_s5 v4
   {-# INLINE rule32 #-}
   rule32 = \ ((_hdIargnamesw) ::  Maybe TH.Exp ) ((_tlIargnamesw) :: [TH.Exp]) ->
     (maybe id (:) _hdIargnamesw _tlIargnamesw)
   {-# INLINE rule33 #-}
   rule33 = \ ((_hdIargpats) ::   Maybe TH.Pat  ) ((_tlIargpats) ::  [TH.Pat] ) ->
     (maybe id (:) _hdIargpats _tlIargpats)
   {-# INLINE rule34 #-}
   rule34 = \ ((_hdIargtps) ::   Maybe (TH.Type -> TH.Type)  ) ((_tlIargtps) ::  TH.Type -> TH.Type ) ->
     (maybe id (.) _hdIargtps _tlIargtps)
   {-# INLINE rule35 #-}
   rule35 = \ ((_hdIchildTypes) :: Map Identifier Type) ((_tlIchildTypes) :: Map Identifier Type) ->
     _hdIchildTypes `mappend` _tlIchildTypes
   {-# INLINE rule36 #-}
   rule36 = \ ((_hdIdatatype) :: Maybe TH.BangType) ((_tlIdatatype) :: [TH.BangType]) ->
     (maybe id (:) _hdIdatatype _tlIdatatype)
   {-# INLINE rule37 #-}
   rule37 = \ ((_hdIterminaldefs) :: Set String) ((_tlIterminaldefs) :: Set String) ->
     _hdIterminaldefs `Set.union` _tlIterminaldefs
   {-# INLINE rule38 #-}
   rule38 = \ ((_hdIusedArgs) :: Set String) ((_tlIusedArgs) :: Set String) ->
     _hdIusedArgs `Set.union` _tlIusedArgs
   {-# INLINE rule39 #-}
   rule39 = \ ((_tlIdatatypeVar) :: [TH.VarBangType]) ->
     _tlIdatatypeVar
   {-# INLINE rule40 #-}
   rule40 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule41 #-}
   rule41 = \ ((_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# INLINE rule42 #-}
   rule42 = \ ((_lhsIimportBlocks) :: [String]) ->
     _lhsIimportBlocks
   {-# INLINE rule43 #-}
   rule43 = \ ((_lhsImainFile) :: String) ->
     _lhsImainFile
   {-# INLINE rule44 #-}
   rule44 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule45 #-}
   rule45 = \ ((_lhsImoduleHeader) :: String -> String -> String -> Bool -> String) ->
     _lhsImoduleHeader
   {-# INLINE rule46 #-}
   rule46 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule47 #-}
   rule47 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule48 #-}
   rule48 = \ ((_lhsIpragmaBlocks) :: String) ->
     _lhsIpragmaBlocks
   {-# INLINE rule49 #-}
   rule49 = \ ((_lhsItextBlocks) :: [String]) ->
     _lhsItextBlocks
   {-# INLINE rule50 #-}
   rule50 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule51 #-}
   rule51 = \ ((_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# INLINE rule52 #-}
   rule52 = \ ((_lhsIimportBlocks) :: [String]) ->
     _lhsIimportBlocks
   {-# INLINE rule53 #-}
   rule53 = \ ((_lhsImainFile) :: String) ->
     _lhsImainFile
   {-# INLINE rule54 #-}
   rule54 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule55 #-}
   rule55 = \ ((_lhsImoduleHeader) :: String -> String -> String -> Bool -> String) ->
     _lhsImoduleHeader
   {-# INLINE rule56 #-}
   rule56 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule57 #-}
   rule57 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule58 #-}
   rule58 = \ ((_lhsIpragmaBlocks) :: String) ->
     _lhsIpragmaBlocks
   {-# INLINE rule59 #-}
   rule59 = \ ((_lhsItextBlocks) :: [String]) ->
     _lhsItextBlocks
{-# NOINLINE sem_EChildren_Nil #-}
sem_EChildren_Nil ::  T_EChildren 
sem_EChildren_Nil  = T_EChildren (return st5) where
   {-# NOINLINE st5 #-}
   st5 = let
      v4 :: T_EChildren_v4 
      v4 = \ (T_EChildren_vIn4 _lhsIallInitStates _lhsIcon _lhsIimportBlocks _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInt _lhsIoptions _lhsIpragmaBlocks _lhsItextBlocks) -> ( let
         _lhsOargnamesw :: [TH.Exp]
         _lhsOargnamesw = rule60  ()
         _lhsOargpats ::  [TH.Pat] 
         _lhsOargpats = rule61  ()
         _lhsOargtps ::  TH.Type -> TH.Type 
         _lhsOargtps = rule62  ()
         _lhsOchildTypes :: Map Identifier Type
         _lhsOchildTypes = rule63  ()
         _lhsOdatatype :: [TH.BangType]
         _lhsOdatatype = rule64  ()
         _lhsOterminaldefs :: Set String
         _lhsOterminaldefs = rule65  ()
         _lhsOusedArgs :: Set String
         _lhsOusedArgs = rule66  ()
         _lhsOdatatypeVar :: [TH.VarBangType]
         _lhsOdatatypeVar = rule67  ()
         __result_ = T_EChildren_vOut4 _lhsOargnamesw _lhsOargpats _lhsOargtps _lhsOchildTypes _lhsOdatatype _lhsOdatatypeVar _lhsOterminaldefs _lhsOusedArgs
         in __result_ )
     in C_EChildren_s5 v4
   {-# INLINE rule60 #-}
   rule60 = \  (_ :: ()) ->
     []
   {-# INLINE rule61 #-}
   rule61 = \  (_ :: ()) ->
     []
   {-# INLINE rule62 #-}
   rule62 = \  (_ :: ()) ->
     id
   {-# INLINE rule63 #-}
   rule63 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule64 #-}
   rule64 = \  (_ :: ()) ->
     []
   {-# INLINE rule65 #-}
   rule65 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule66 #-}
   rule66 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule67 #-}
   rule67 = \  (_ :: ()) ->
     error "missing rule: EChildren.Nil.lhs.datatypeVar"

-- ENonterminal ------------------------------------------------
-- wrapper
data Inh_ENonterminal  = Inh_ENonterminal { allFromToStates_Inh_ENonterminal :: (Map VisitIdentifier (Int,Int)), allInitStates_Inh_ENonterminal :: (Map NontermIdent Int), allVisitKinds_Inh_ENonterminal :: (Map VisitIdentifier VisitKind), avisitdefs_Inh_ENonterminal :: (Map VisitIdentifier (Set Identifier)), avisituses_Inh_ENonterminal :: (Map VisitIdentifier (Set Identifier)), derivings_Inh_ENonterminal :: (Derivings), importBlocks_Inh_ENonterminal :: ([String]), inhmap_Inh_ENonterminal :: (Map NontermIdent Attributes), localAttrTypes_Inh_ENonterminal :: (Map NontermIdent (Map ConstructorIdent (Map Identifier Type))), mainFile_Inh_ENonterminal :: (String), mainName_Inh_ENonterminal :: (String), moduleHeader_Inh_ENonterminal :: (String -> String -> String -> Bool -> String), options_Inh_ENonterminal :: (Options), pragmaBlocks_Inh_ENonterminal :: (String), synmap_Inh_ENonterminal :: (Map NontermIdent Attributes), textBlocks_Inh_ENonterminal :: ([String]), typeSyns_Inh_ENonterminal :: (TypeSyns), wrappers_Inh_ENonterminal :: (Set NontermIdent) }
data Syn_ENonterminal  = Syn_ENonterminal { errors_Syn_ENonterminal :: (Seq Error), fromToStates_Syn_ENonterminal :: (Map VisitIdentifier (Int,Int)), initStates_Syn_ENonterminal :: (Map NontermIdent Int), output_Syn_ENonterminal :: ([TH.Dec]), semFunBndDefs_Syn_ENonterminal :: (Seq (TH.Name, TH.Exp)), semFunBndTps_Syn_ENonterminal :: (Seq (TH.VarBangType)), visitKinds_Syn_ENonterminal :: (Map VisitIdentifier VisitKind), visitdefs_Syn_ENonterminal :: (Map VisitIdentifier (Set Identifier)), visituses_Syn_ENonterminal :: (Map VisitIdentifier (Set Identifier)) }
{-# INLINABLE wrap_ENonterminal #-}
wrap_ENonterminal :: T_ENonterminal  -> Inh_ENonterminal  -> (Syn_ENonterminal )
wrap_ENonterminal (T_ENonterminal act) (Inh_ENonterminal _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIavisitdefs _lhsIavisituses _lhsIderivings _lhsIimportBlocks _lhsIinhmap _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsIoptions _lhsIpragmaBlocks _lhsIsynmap _lhsItextBlocks _lhsItypeSyns _lhsIwrappers) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg7 = T_ENonterminal_vIn7 _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIavisitdefs _lhsIavisituses _lhsIderivings _lhsIimportBlocks _lhsIinhmap _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsIoptions _lhsIpragmaBlocks _lhsIsynmap _lhsItextBlocks _lhsItypeSyns _lhsIwrappers
        (T_ENonterminal_vOut7 _lhsOerrors _lhsOfromToStates _lhsOinitStates _lhsOoutput _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses) <- return (inv_ENonterminal_s8 sem arg7)
        return (Syn_ENonterminal _lhsOerrors _lhsOfromToStates _lhsOinitStates _lhsOoutput _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses)
   )

-- cata
{-# INLINE sem_ENonterminal #-}
sem_ENonterminal :: ENonterminal  -> T_ENonterminal 
sem_ENonterminal ( ENonterminal nt_ params_ classCtxs_ initial_ initialv_ nextVisits_ prevVisits_ prods_ recursive_ hoInfo_ ) = sem_ENonterminal_ENonterminal nt_ params_ classCtxs_ initial_ initialv_ nextVisits_ prevVisits_ ( sem_EProductions prods_ ) recursive_ hoInfo_

-- semantic domain
newtype T_ENonterminal  = T_ENonterminal {
                                         attach_T_ENonterminal :: Identity (T_ENonterminal_s8 )
                                         }
newtype T_ENonterminal_s8  = C_ENonterminal_s8 {
                                               inv_ENonterminal_s8 :: (T_ENonterminal_v7 )
                                               }
data T_ENonterminal_s9  = C_ENonterminal_s9
type T_ENonterminal_v7  = (T_ENonterminal_vIn7 ) -> (T_ENonterminal_vOut7 )
data T_ENonterminal_vIn7  = T_ENonterminal_vIn7 (Map VisitIdentifier (Int,Int)) (Map NontermIdent Int) (Map VisitIdentifier VisitKind) (Map VisitIdentifier (Set Identifier)) (Map VisitIdentifier (Set Identifier)) (Derivings) ([String]) (Map NontermIdent Attributes) (Map NontermIdent (Map ConstructorIdent (Map Identifier Type))) (String) (String) (String -> String -> String -> Bool -> String) (Options) (String) (Map NontermIdent Attributes) ([String]) (TypeSyns) (Set NontermIdent)
data T_ENonterminal_vOut7  = T_ENonterminal_vOut7 (Seq Error) (Map VisitIdentifier (Int,Int)) (Map NontermIdent Int) ([TH.Dec]) (Seq (TH.Name, TH.Exp)) (Seq (TH.VarBangType)) (Map VisitIdentifier VisitKind) (Map VisitIdentifier (Set Identifier)) (Map VisitIdentifier (Set Identifier))
{-# NOINLINE sem_ENonterminal_ENonterminal #-}
sem_ENonterminal_ENonterminal :: (NontermIdent) -> ([Identifier]) -> (ClassContext) -> (StateIdentifier) -> ([VisitIdentifier]) -> (Map StateIdentifier StateCtx) -> (Map StateIdentifier StateCtx) -> T_EProductions  -> (Bool) -> (HigherOrderInfo) -> T_ENonterminal 
sem_ENonterminal_ENonterminal arg_nt_ arg_params_ _ arg_initial_ _ arg_nextVisits_ arg_prevVisits_ arg_prods_ arg_recursive_ _ = T_ENonterminal (return st8) where
   {-# NOINLINE st8 #-}
   st8 = let
      v7 :: T_ENonterminal_v7 
      v7 = \ (T_ENonterminal_vIn7 _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIavisitdefs _lhsIavisituses _lhsIderivings _lhsIimportBlocks _lhsIinhmap _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsIoptions _lhsIpragmaBlocks _lhsIsynmap _lhsItextBlocks _lhsItypeSyns _lhsIwrappers) -> ( let
         _prodsX17 = Control.Monad.Identity.runIdentity (attach_T_EProductions (arg_prods_))
         (T_EProductions_vOut16 _prodsIallvisits _prodsIcount _prodsIdatatype _prodsIerrors _prodsIfromToStates _prodsIsemFunBndDefs _prodsIsemFunBndTps _prodsIsem_nt _prodsIvisitKinds _prodsIvisitdefs _prodsIvisituses) = inv_EProductions_s17 _prodsX17 (T_EProductions_vIn16 _prodsOallFromToStates _prodsOallInhmap _prodsOallInitStates _prodsOallSynmap _prodsOallVisitKinds _prodsOallstates _prodsOavisitdefs _prodsOavisituses _prodsOimportBlocks _prodsOinhmap _prodsOinitial _prodsOlocalAttrTypes _prodsOmainFile _prodsOmainName _prodsOmoduleHeader _prodsOnextVisits _prodsOnt _prodsOntType _prodsOoptions _prodsOparams _prodsOpragmaBlocks _prodsOprevVisits _prodsOrename _prodsOsynmap _prodsOtextBlocks)
         _prodsOrename = rule68 _lhsIoptions
         _prodsOnt = rule69 arg_nt_
         _prodsOparams = rule70 arg_params_
         _lhsOoutput :: [TH.Dec]
         _lhsOoutput = rule71 _datatype _lhsIoptions
         _hasWrapper = rule72 _lhsIwrappers arg_nt_
         _aliasPre = rule73 _t_params arg_nt_
         _datatype = rule74 _aliasPre _derivings _lhsItypeSyns _prodsIdatatype _t_params arg_nt_
         _derivings = rule75 _lhsIderivings arg_nt_
         _fsemname = rule76  ()
         _semname = rule77 _fsemname arg_nt_
         _frecarg = rule78 _fsemname
         _sem_tp = rule79 _quantTH _t_params _t_type arg_nt_
         _quantTH = rule80 arg_params_
         _sem_nt = rule81 _frecarg _fsemname _lhsItypeSyns _prodsIsem_nt _semPragma _sem_tp _semname arg_nt_
         _inlineNt = rule82 _hasWrapper _lhsIoptions _prodsIcount arg_recursive_
         _semPragma = rule83 _inlineNt _lhsIoptions _semname
         (Just _prodsOinhmap) = rule84 _lhsIinhmap arg_nt_
         (Just _prodsOsynmap) = rule85 _lhsIsynmap arg_nt_
         _prodsOallInhmap = rule86 _lhsIinhmap
         _prodsOallSynmap = rule87 _lhsIsynmap
         _outedges = rule88 _prodsIallvisits
         _inedges = rule89 _prodsIallvisits
         _allstates = rule90 _inedges _outedges arg_initial_
         _stvisits = rule91 _prodsIallvisits
         _t_type = rule92 arg_nt_
         _t_params = rule93 arg_params_
         _t_init = rule94 _lhsIoptions _t_params _t_type arg_initial_
         _lhsOsemFunBndDefs :: Seq (TH.Name, TH.Exp)
         _lhsOsemFunBndDefs = rule95 _prodsIsemFunBndDefs _semFunBndDef
         _lhsOsemFunBndTps :: Seq (TH.VarBangType)
         _lhsOsemFunBndTps = rule96 _prodsIsemFunBndTps _semFunBndTp
         _semFunBndDef = rule97 _semFunBndNm _semname
         _semFunBndTp = rule98 _semFunBndNm _sem_tp
         _semFunBndNm = rule99 arg_nt_
         _prodsOinitial = rule100 arg_initial_
         _prodsOallstates = rule101 _allstates
         _addbang = rule102 _lhsIoptions
         _addbangWrap = rule103  ()
         _prodsOnextVisits = rule104 arg_nextVisits_
         _prodsOprevVisits = rule105 arg_prevVisits_
         _prodsOlocalAttrTypes = rule106 _lhsIlocalAttrTypes arg_nt_
         _lhsOinitStates :: Map NontermIdent Int
         _lhsOinitStates = rule107 arg_initial_ arg_nt_
         _ntType = rule108 arg_nt_ arg_params_
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule109 _prodsIerrors
         _lhsOfromToStates :: Map VisitIdentifier (Int,Int)
         _lhsOfromToStates = rule110 _prodsIfromToStates
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule111 _prodsIvisitKinds
         _lhsOvisitdefs :: Map VisitIdentifier (Set Identifier)
         _lhsOvisitdefs = rule112 _prodsIvisitdefs
         _lhsOvisituses :: Map VisitIdentifier (Set Identifier)
         _lhsOvisituses = rule113 _prodsIvisituses
         _prodsOallFromToStates = rule114 _lhsIallFromToStates
         _prodsOallInitStates = rule115 _lhsIallInitStates
         _prodsOallVisitKinds = rule116 _lhsIallVisitKinds
         _prodsOavisitdefs = rule117 _lhsIavisitdefs
         _prodsOavisituses = rule118 _lhsIavisituses
         _prodsOimportBlocks = rule119 _lhsIimportBlocks
         _prodsOmainFile = rule120 _lhsImainFile
         _prodsOmainName = rule121 _lhsImainName
         _prodsOmoduleHeader = rule122 _lhsImoduleHeader
         _prodsOntType = rule123 _ntType
         _prodsOoptions = rule124 _lhsIoptions
         _prodsOpragmaBlocks = rule125 _lhsIpragmaBlocks
         _prodsOtextBlocks = rule126 _lhsItextBlocks
         __result_ = T_ENonterminal_vOut7 _lhsOerrors _lhsOfromToStates _lhsOinitStates _lhsOoutput _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses
         in __result_ )
     in C_ENonterminal_s8 v7
   {-# INLINE rule68 #-}
   {-# LINE 68 "src-ag/ExecutionPlan2TH.ag" #-}
   rule68 = \ ((_lhsIoptions) :: Options) ->
                                  {-# LINE 68 "src-ag/ExecutionPlan2TH.ag" #-}
                                  rename _lhsIoptions
                                  {-# LINE 861 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule69 #-}
   {-# LINE 76 "src-ag/ExecutionPlan2TH.ag" #-}
   rule69 = \ nt_ ->
                              {-# LINE 76 "src-ag/ExecutionPlan2TH.ag" #-}
                              nt_
                              {-# LINE 867 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule70 #-}
   {-# LINE 88 "src-ag/ExecutionPlan2TH.ag" #-}
   rule70 = \ params_ ->
                   {-# LINE 88 "src-ag/ExecutionPlan2TH.ag" #-}
                   params_
                   {-# LINE 873 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule71 #-}
   {-# LINE 105 "src-ag/ExecutionPlan2TH.ag" #-}
   rule71 = \ _datatype ((_lhsIoptions) :: Options) ->
                                {-# LINE 105 "src-ag/ExecutionPlan2TH.ag" #-}
                                concat
                                [ if dataTypes _lhsIoptions
                                    then [_datatype    ]
                                    else []
                                ]
                                {-# LINE 883 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule72 #-}
   {-# LINE 127 "src-ag/ExecutionPlan2TH.ag" #-}
   rule72 = \ ((_lhsIwrappers) :: Set NontermIdent) nt_ ->
                                    {-# LINE 127 "src-ag/ExecutionPlan2TH.ag" #-}
                                    nt_ `Set.member` _lhsIwrappers
                                    {-# LINE 889 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule73 #-}
   {-# LINE 140 "src-ag/ExecutionPlan2TH.ag" #-}
   rule73 = \ _t_params nt_ ->
                                  {-# LINE 140 "src-ag/ExecutionPlan2TH.ag" #-}
                                  TH.TySynD (identToName nt_) (map TH.PlainTV _t_params    ) :: TH.Type -> TH.Dec
                                  {-# LINE 895 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule74 #-}
   {-# LINE 141 "src-ag/ExecutionPlan2TH.ag" #-}
   rule74 = \ _aliasPre _derivings ((_lhsItypeSyns) :: TypeSyns) ((_prodsIdatatype) :: [TH.Con]) _t_params nt_ ->
                                  {-# LINE 141 "src-ag/ExecutionPlan2TH.ag" #-}
                                  case lookup nt_ _lhsItypeSyns of
                                     Nothing -> TH.DataD [] (identToName nt_) (map TH.PlainTV _t_params    ) Nothing _prodsIdatatype _derivings
                                     Just (List t)     -> _aliasPre     (TH.AppT TH.ListT (typeToTH t))
                                     Just (Maybe t)    -> _aliasPre     (TH.AppT (TH.ConT (TH.mkName "Maybe")) (typeToTH t))
                                     Just (Tuple ts)   -> _aliasPre     (foldl (\xs x -> TH.AppT xs (typeToTH (snd x))) (TH.TupleT (length ts)) ts)
                                     Just (Either l r) -> _aliasPre     (TH.AppT (TH.AppT (TH.ConT (TH.mkName "Either")) (typeToTH l)) (typeToTH r))
                                     Just (Map k v)    -> _aliasPre     (TH.AppT (TH.AppT (TH.ConT (TH.mkName "Data.Map")) (typeToTH k)) (typeToTH v))
                                     Just (IntMap t)   -> _aliasPre     (TH.AppT (TH.ConT (TH.mkName "Data.IntMap.IntMap")) (typeToTH t))
                                     Just (OrdSet t)   -> _aliasPre     (TH.AppT (TH.ConT (TH.mkName "Data.Set.Set")) (typeToTH t))
                                     Just IntSet       -> _aliasPre     (TH.ConT (TH.mkName "Data.IntSet.IntSet"))
                                  {-# LINE 910 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule75 #-}
   {-# LINE 152 "src-ag/ExecutionPlan2TH.ag" #-}
   rule75 = \ ((_lhsIderivings) :: Derivings) nt_ ->
                                   {-# LINE 152 "src-ag/ExecutionPlan2TH.ag" #-}
                                   case Map.lookup nt_ _lhsIderivings of
                                      Just s -> [TH.DerivClause Nothing (map (TH.ConT . identToName) (Set.toList s))]
                                      Nothing -> []
                                   {-# LINE 918 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule76 #-}
   {-# LINE 210 "src-ag/ExecutionPlan2TH.ag" #-}
   rule76 = \  (_ :: ()) ->
                                  {-# LINE 210 "src-ag/ExecutionPlan2TH.ag" #-}
                                  \x -> "sem_" ++ show x
                                  {-# LINE 924 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule77 #-}
   {-# LINE 211 "src-ag/ExecutionPlan2TH.ag" #-}
   rule77 = \ _fsemname nt_ ->
                                 {-# LINE 211 "src-ag/ExecutionPlan2TH.ag" #-}
                                 _fsemname     nt_
                                 {-# LINE 930 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule78 #-}
   {-# LINE 212 "src-ag/ExecutionPlan2TH.ag" #-}
   rule78 = \ _fsemname ->
                                 {-# LINE 212 "src-ag/ExecutionPlan2TH.ag" #-}
                                 \t x -> case t of
                                            NT nt _ _ -> TH.VarE (TH.mkName (_fsemname nt)) `TH.AppE` x
                                            _         -> x
                                 {-# LINE 938 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule79 #-}
   {-# LINE 222 "src-ag/ExecutionPlan2TH.ag" #-}
   rule79 = \ _quantTH _t_params _t_type nt_ ->
                                 {-# LINE 222 "src-ag/ExecutionPlan2TH.ag" #-}
                                 _quantTH     [             ] (TH.AppT (TH.AppT TH.ArrowT (foldl TH.AppT (TH.ConT (identToName nt_)) (map TH.VarT _t_params    ))) (foldl TH.AppT (TH.ConT (TH.mkName _t_type    )) (map TH.VarT _t_params    )))
                                 {-# LINE 944 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule80 #-}
   {-# LINE 223 "src-ag/ExecutionPlan2TH.ag" #-}
   rule80 = \ params_ ->
                                 {-# LINE 223 "src-ag/ExecutionPlan2TH.ag" #-}
                                 quantsTH TH.ForallT params_
                                 {-# LINE 950 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule81 #-}
   {-# LINE 224 "src-ag/ExecutionPlan2TH.ag" #-}
   rule81 = \ _frecarg _fsemname ((_lhsItypeSyns) :: TypeSyns) ((_prodsIsem_nt) :: [TH.Clause]) _semPragma _sem_tp _semname nt_ ->
                                 {-# LINE 224 "src-ag/ExecutionPlan2TH.ag" #-}
                                 _semPragma
                                 ++
                                 [ TH.SigD (TH.mkName _semname    ) _sem_tp
                                 , TH.FunD (TH.mkName _semname    )
                                   $ case lookup nt_ _lhsItypeSyns of
                                       Nothing -> _prodsIsem_nt
                                       Just (List t) ->
                                         [ TH.Clause [TH.VarP (TH.mkName "list")] (TH.NormalB
                                           (TH.VarE (TH.mkName "Prelude.foldr")
                                           `TH.AppE` TH.VarE (TH.mkName (_semname     ++ "_Cons"))
                                           `TH.AppE` TH.VarE (TH.mkName (_semname     ++ "_Nil"))
                                           `TH.AppE` case t of
                                                    NT nt _ _ -> TH.VarE (TH.mkName "Prelude.map")
                                                                `TH.AppE` TH.VarE (TH.mkName (_fsemname nt))
                                                                `TH.AppE` TH.VarE (TH.mkName "list")
                                                    _         -> TH.VarE (TH.mkName "list"))) []
                                         ]
                                       Just (Maybe t) ->
                                         [ TH.Clause [TH.ConP (TH.mkName "Prelude.Nothing") []]
                                                     (TH.NormalB (TH.VarE (TH.mkName (_semname     ++ "_Nothing"))))
                                                     []
                                         , TH.Clause [TH.ConP (TH.mkName "Prelude.Just") [TH.VarP (TH.mkName "just")]]
                                                     (TH.NormalB (TH.VarE (TH.mkName (_semname     ++ "_Just"))
                                                                 `TH.AppE` (_frecarg t (TH.VarE (TH.mkName "just")))))
                                                     []
                                         ]
                                       Just (Tuple ts) ->
                                         [ TH.Clause [TH.TupP (map (TH.VarP . TH.mkName . getName . fst) ts)]
                                                     (TH.NormalB (foldl TH.AppE
                                                                        (TH.VarE (TH.mkName (_semname     ++ "_Tuple")))
                                                                        (map (\t -> _frecarg (snd t)
                                                                             (TH.VarE (TH.mkName (show $ fst t)))) ts)))
                                                     []
                                         ]
                                       Just (Either l r) ->
                                         [ TH.Clause [TH.ConP (TH.mkName "Prelude.Left") [TH.VarP (TH.mkName "left")]]
                                                     (TH.NormalB (TH.VarE (TH.mkName (_semname     ++ "_Left"))
                                                                 `TH.AppE` _frecarg l (TH.VarE (TH.mkName "left"))))
                                                     []
                                         , TH.Clause [TH.ConP (TH.mkName "Prelude.Right") [TH.VarP (TH.mkName "right")]]
                                                     (TH.NormalB (TH.VarE (TH.mkName (_semname     ++ "_Right"))
                                                                 `TH.AppE` _frecarg r (TH.VarE (TH.mkName "right"))))
                                                     []
                                         ]
                                       Just (Map k v) ->
                                         [ TH.Clause [TH.VarP (TH.mkName "m")]
                                                     (TH.NormalB (TH.VarE (TH.mkName "Data.Map.foldrWithKey")
                                                                 `TH.AppE` (TH.VarE (TH.mkName (_semname     ++ "_Entry")))
                                                                 `TH.AppE` (TH.VarE (TH.mkName (_semname     ++ "_Nil")))
                                                                 `TH.AppE` case v of
                                                                             NT nt _ _ -> TH.VarE (TH.mkName "Data.Map.map")
                                                                                         `TH.AppE` (TH.VarE (TH.mkName (_fsemname nt)))
                                                                                         `TH.AppE` (TH.VarE (TH.mkName "m"))
                                                                             _         -> TH.VarE (TH.mkName "m")))
                                                     []
                                         ]
                                       Just (IntMap v) ->
                                         [ TH.Clause [TH.VarP (TH.mkName "m")]
                                                     (TH.NormalB (TH.VarE (TH.mkName "Data.IntMap.foldWithKey")
                                                                 `TH.AppE` TH.VarE (TH.mkName (_semname     ++ "_Entry"))
                                                                 `TH.AppE` TH.VarE (TH.mkName (_semname     ++ "_Nil"))
                                                                 `TH.AppE` case v of
                                                                             NT nt _ _ -> TH.VarE (TH.mkName "Data.IntMap.map")
                                                                                         `TH.AppE` TH.VarE (TH.mkName (_fsemname nt))
                                                                                         `TH.AppE` TH.VarE (TH.mkName "m")
                                                                             _         -> TH.VarE (TH.mkName "m")))
                                                     []
                                         ]
                                       Just (OrdSet t) ->
                                         [ TH.Clause [TH.VarP (TH.mkName "s")]
                                                     (TH.NormalB (TH.VarE (TH.mkName "Prelude.foldr")
                                                                 `TH.AppE` TH.VarE (TH.mkName (_semname     ++ "_Entry"))
                                                                 `TH.AppE` TH.VarE (TH.mkName (_semname     ++ "_Nil"))
                                                                 `TH.AppE` case t of
                                                                             NT nt _ _ -> TH.VarE (TH.mkName "Prelude.map")
                                                                                         `TH.AppE` TH.VarE (TH.mkName (_fsemname nt))
                                                                                         `TH.AppE` (TH.VarE (TH.mkName "Data.IntSet.elems")
                                                                                                   `TH.AppE` TH.VarE (TH.mkName "s"))
                                                                             _         -> TH.VarE (TH.mkName "Data.IntSet.elems")
                                                                                                   `TH.AppE` TH.VarE (TH.mkName "s")))
                                                     []
                                         ]
                                       Just IntSet ->
                                         [ TH.Clause [TH.VarP (TH.mkName "s")]
                                                     (TH.NormalB (TH.VarE (TH.mkName "Prelude.foldr")
                                                                 `TH.AppE` TH.VarE (TH.mkName (_semname     ++ "_Entry"))
                                                                 `TH.AppE` TH.VarE (TH.mkName (_semname     ++ "_Nil"))
                                                                 `TH.AppE` (TH.VarE (TH.mkName "Data.IntSet.elems")
                                                                           `TH.AppE` TH.VarE (TH.mkName "s"))))
                                                     []
                                         ]
                                       Just x -> error $ "Type " ++ show x ++ " is not supported yet"
                                 ]
                                 {-# LINE 1048 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule82 #-}
   {-# LINE 319 "src-ag/ExecutionPlan2TH.ag" #-}
   rule82 = \ _hasWrapper ((_lhsIoptions) :: Options) ((_prodsIcount) :: Int) recursive_ ->
                                  {-# LINE 319 "src-ag/ExecutionPlan2TH.ag" #-}
                                  not (lateHigherOrderBinding _lhsIoptions) && not recursive_ && (_prodsIcount == 1 || (aggressiveInlinePragmas _lhsIoptions && not _hasWrapper    ))
                                  {-# LINE 1054 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule83 #-}
   {-# LINE 320 "src-ag/ExecutionPlan2TH.ag" #-}
   rule83 = \ _inlineNt ((_lhsIoptions) :: Options) _semname ->
                                  {-# LINE 320 "src-ag/ExecutionPlan2TH.ag" #-}
                                  if noInlinePragmas _lhsIoptions
                                  then empty
                                  else if _inlineNt
                                       then pure $ inlineTH (TH.mkName _semname    )
                                       else if helpInlining _lhsIoptions && not (lateHigherOrderBinding _lhsIoptions)
                                            then pure $ inlinableTH (TH.mkName _semname    )
                                            else pure $ noInlineTH (TH.mkName _semname    )
                                  {-# LINE 1066 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule84 #-}
   {-# LINE 372 "src-ag/ExecutionPlan2TH.ag" #-}
   rule84 = \ ((_lhsIinhmap) :: Map NontermIdent Attributes) nt_ ->
                                         {-# LINE 372 "src-ag/ExecutionPlan2TH.ag" #-}
                                         Map.lookup nt_ _lhsIinhmap
                                         {-# LINE 1072 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule85 #-}
   {-# LINE 373 "src-ag/ExecutionPlan2TH.ag" #-}
   rule85 = \ ((_lhsIsynmap) :: Map NontermIdent Attributes) nt_ ->
                                         {-# LINE 373 "src-ag/ExecutionPlan2TH.ag" #-}
                                         Map.lookup nt_ _lhsIsynmap
                                         {-# LINE 1078 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule86 #-}
   {-# LINE 374 "src-ag/ExecutionPlan2TH.ag" #-}
   rule86 = \ ((_lhsIinhmap) :: Map NontermIdent Attributes) ->
                                     {-# LINE 374 "src-ag/ExecutionPlan2TH.ag" #-}
                                     _lhsIinhmap
                                     {-# LINE 1084 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule87 #-}
   {-# LINE 375 "src-ag/ExecutionPlan2TH.ag" #-}
   rule87 = \ ((_lhsIsynmap) :: Map NontermIdent Attributes) ->
                                     {-# LINE 375 "src-ag/ExecutionPlan2TH.ag" #-}
                                     _lhsIsynmap
                                     {-# LINE 1090 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule88 #-}
   {-# LINE 396 "src-ag/ExecutionPlan2TH.ag" #-}
   rule88 = \ ((_prodsIallvisits) :: [VisitStateState]) ->
                                   {-# LINE 396 "src-ag/ExecutionPlan2TH.ag" #-}
                                   Set.fromList $ map (\(_,f,_) -> f) _prodsIallvisits
                                   {-# LINE 1096 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule89 #-}
   {-# LINE 397 "src-ag/ExecutionPlan2TH.ag" #-}
   rule89 = \ ((_prodsIallvisits) :: [VisitStateState]) ->
                                   {-# LINE 397 "src-ag/ExecutionPlan2TH.ag" #-}
                                   Set.fromList $ map (\(_,_,t) -> t) _prodsIallvisits
                                   {-# LINE 1102 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule90 #-}
   {-# LINE 398 "src-ag/ExecutionPlan2TH.ag" #-}
   rule90 = \ _inedges _outedges initial_ ->
                                   {-# LINE 398 "src-ag/ExecutionPlan2TH.ag" #-}
                                   Set.insert initial_ $ _inedges     `Set.union` _outedges
                                   {-# LINE 1108 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule91 #-}
   {-# LINE 399 "src-ag/ExecutionPlan2TH.ag" #-}
   rule91 = \ ((_prodsIallvisits) :: [VisitStateState]) ->
                                   {-# LINE 399 "src-ag/ExecutionPlan2TH.ag" #-}
                                   \st -> filter (\(v,f,t) -> f == st) _prodsIallvisits
                                   {-# LINE 1114 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule92 #-}
   {-# LINE 400 "src-ag/ExecutionPlan2TH.ag" #-}
   rule92 = \ nt_ ->
                                   {-# LINE 400 "src-ag/ExecutionPlan2TH.ag" #-}
                                   "T_" ++ getName nt_
                                   {-# LINE 1120 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule93 #-}
   {-# LINE 401 "src-ag/ExecutionPlan2TH.ag" #-}
   rule93 = \ params_ ->
                                   {-# LINE 401 "src-ag/ExecutionPlan2TH.ag" #-}
                                   map (TH.mkName . getName) params_
                                   {-# LINE 1126 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule94 #-}
   {-# LINE 402 "src-ag/ExecutionPlan2TH.ag" #-}
   rule94 = \ ((_lhsIoptions) :: Options) _t_params _t_type initial_ ->
                                   {-# LINE 402 "src-ag/ExecutionPlan2TH.ag" #-}
                                   TH.NewtypeD [] (TH.mkName _t_type    ) (map TH.PlainTV _t_params    ) Nothing
                                   (TH.RecC (TH.mkName _t_type    )
                                         [(TH.mkName ("attach_" ++ _t_type    ), nobang, monadTypeTH _lhsIoptions (foldl TH.AppT (TH.ConT (TH.mkName (_t_type     ++ "_s" ++ show initial_))) (map TH.VarT _t_params    )))
                                         ]) []
                                   {-# LINE 1135 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule95 #-}
   {-# LINE 575 "src-ag/ExecutionPlan2TH.ag" #-}
   rule95 = \ ((_prodsIsemFunBndDefs) :: Seq (TH.Name, TH.Exp)) _semFunBndDef ->
                        {-# LINE 575 "src-ag/ExecutionPlan2TH.ag" #-}
                        _semFunBndDef     Seq.<| _prodsIsemFunBndDefs
                        {-# LINE 1141 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule96 #-}
   {-# LINE 576 "src-ag/ExecutionPlan2TH.ag" #-}
   rule96 = \ ((_prodsIsemFunBndTps) :: Seq (TH.VarBangType)) _semFunBndTp ->
                        {-# LINE 576 "src-ag/ExecutionPlan2TH.ag" #-}
                        _semFunBndTp     Seq.<| _prodsIsemFunBndTps
                        {-# LINE 1147 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule97 #-}
   {-# LINE 577 "src-ag/ExecutionPlan2TH.ag" #-}
   rule97 = \ _semFunBndNm _semname ->
                        {-# LINE 577 "src-ag/ExecutionPlan2TH.ag" #-}
                        (_semFunBndNm    , TH.VarE (TH.mkName _semname    ))
                        {-# LINE 1153 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule98 #-}
   {-# LINE 578 "src-ag/ExecutionPlan2TH.ag" #-}
   rule98 = \ _semFunBndNm _sem_tp ->
                        {-# LINE 578 "src-ag/ExecutionPlan2TH.ag" #-}
                        (_semFunBndNm    , nobang, _sem_tp    )
                        {-# LINE 1159 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule99 #-}
   {-# LINE 579 "src-ag/ExecutionPlan2TH.ag" #-}
   rule99 = \ nt_ ->
                        {-# LINE 579 "src-ag/ExecutionPlan2TH.ag" #-}
                        TH.mkName (lateSemNtLabel nt_)
                        {-# LINE 1165 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule100 #-}
   {-# LINE 621 "src-ag/ExecutionPlan2TH.ag" #-}
   rule100 = \ initial_ ->
                                     {-# LINE 621 "src-ag/ExecutionPlan2TH.ag" #-}
                                     initial_
                                     {-# LINE 1171 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule101 #-}
   {-# LINE 622 "src-ag/ExecutionPlan2TH.ag" #-}
   rule101 = \ _allstates ->
                                     {-# LINE 622 "src-ag/ExecutionPlan2TH.ag" #-}
                                     _allstates
                                     {-# LINE 1177 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule102 #-}
   {-# LINE 1602 "src-ag/ExecutionPlan2TH.ag" #-}
   rule102 = \ ((_lhsIoptions) :: Options) ->
                                                    {-# LINE 1602 "src-ag/ExecutionPlan2TH.ag" #-}
                                                    \x -> if bangpats _lhsIoptions then TH.BangP x else x
                                                    {-# LINE 1183 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule103 #-}
   {-# LINE 1610 "src-ag/ExecutionPlan2TH.ag" #-}
   rule103 = \  (_ :: ()) ->
                                                        {-# LINE 1610 "src-ag/ExecutionPlan2TH.ag" #-}
                                                        id
                                                        {-# LINE 1189 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule104 #-}
   {-# LINE 1622 "src-ag/ExecutionPlan2TH.ag" #-}
   rule104 = \ nextVisits_ ->
                       {-# LINE 1622 "src-ag/ExecutionPlan2TH.ag" #-}
                       nextVisits_
                       {-# LINE 1195 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule105 #-}
   {-# LINE 1623 "src-ag/ExecutionPlan2TH.ag" #-}
   rule105 = \ prevVisits_ ->
                       {-# LINE 1623 "src-ag/ExecutionPlan2TH.ag" #-}
                       prevVisits_
                       {-# LINE 1201 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule106 #-}
   {-# LINE 1667 "src-ag/ExecutionPlan2TH.ag" #-}
   rule106 = \ ((_lhsIlocalAttrTypes) :: Map NontermIdent (Map ConstructorIdent (Map Identifier Type))) nt_ ->
                           {-# LINE 1667 "src-ag/ExecutionPlan2TH.ag" #-}
                           Map.findWithDefault Map.empty nt_ _lhsIlocalAttrTypes
                           {-# LINE 1207 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule107 #-}
   {-# LINE 1694 "src-ag/ExecutionPlan2TH.ag" #-}
   rule107 = \ initial_ nt_ ->
                     {-# LINE 1694 "src-ag/ExecutionPlan2TH.ag" #-}
                     Map.singleton nt_ initial_
                     {-# LINE 1213 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule108 #-}
   {-# LINE 1708 "src-ag/ExecutionPlan2TH.ag" #-}
   rule108 = \ nt_ params_ ->
                 {-# LINE 1708 "src-ag/ExecutionPlan2TH.ag" #-}
                 NT nt_ (map show params_) False
                 {-# LINE 1219 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule109 #-}
   rule109 = \ ((_prodsIerrors) :: Seq Error) ->
     _prodsIerrors
   {-# INLINE rule110 #-}
   rule110 = \ ((_prodsIfromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _prodsIfromToStates
   {-# INLINE rule111 #-}
   rule111 = \ ((_prodsIvisitKinds) :: Map VisitIdentifier VisitKind) ->
     _prodsIvisitKinds
   {-# INLINE rule112 #-}
   rule112 = \ ((_prodsIvisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _prodsIvisitdefs
   {-# INLINE rule113 #-}
   rule113 = \ ((_prodsIvisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _prodsIvisituses
   {-# INLINE rule114 #-}
   rule114 = \ ((_lhsIallFromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _lhsIallFromToStates
   {-# INLINE rule115 #-}
   rule115 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule116 #-}
   rule116 = \ ((_lhsIallVisitKinds) :: Map VisitIdentifier VisitKind) ->
     _lhsIallVisitKinds
   {-# INLINE rule117 #-}
   rule117 = \ ((_lhsIavisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisitdefs
   {-# INLINE rule118 #-}
   rule118 = \ ((_lhsIavisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisituses
   {-# INLINE rule119 #-}
   rule119 = \ ((_lhsIimportBlocks) :: [String]) ->
     _lhsIimportBlocks
   {-# INLINE rule120 #-}
   rule120 = \ ((_lhsImainFile) :: String) ->
     _lhsImainFile
   {-# INLINE rule121 #-}
   rule121 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule122 #-}
   rule122 = \ ((_lhsImoduleHeader) :: String -> String -> String -> Bool -> String) ->
     _lhsImoduleHeader
   {-# INLINE rule123 #-}
   rule123 = \ _ntType ->
     _ntType
   {-# INLINE rule124 #-}
   rule124 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule125 #-}
   rule125 = \ ((_lhsIpragmaBlocks) :: String) ->
     _lhsIpragmaBlocks
   {-# INLINE rule126 #-}
   rule126 = \ ((_lhsItextBlocks) :: [String]) ->
     _lhsItextBlocks

-- ENonterminals -----------------------------------------------
-- wrapper
data Inh_ENonterminals  = Inh_ENonterminals { allFromToStates_Inh_ENonterminals :: (Map VisitIdentifier (Int,Int)), allInitStates_Inh_ENonterminals :: (Map NontermIdent Int), allVisitKinds_Inh_ENonterminals :: (Map VisitIdentifier VisitKind), avisitdefs_Inh_ENonterminals :: (Map VisitIdentifier (Set Identifier)), avisituses_Inh_ENonterminals :: (Map VisitIdentifier (Set Identifier)), derivings_Inh_ENonterminals :: (Derivings), importBlocks_Inh_ENonterminals :: ([String]), inhmap_Inh_ENonterminals :: (Map NontermIdent Attributes), localAttrTypes_Inh_ENonterminals :: (Map NontermIdent (Map ConstructorIdent (Map Identifier Type))), mainFile_Inh_ENonterminals :: (String), mainName_Inh_ENonterminals :: (String), moduleHeader_Inh_ENonterminals :: (String -> String -> String -> Bool -> String), options_Inh_ENonterminals :: (Options), pragmaBlocks_Inh_ENonterminals :: (String), synmap_Inh_ENonterminals :: (Map NontermIdent Attributes), textBlocks_Inh_ENonterminals :: ([String]), typeSyns_Inh_ENonterminals :: (TypeSyns), wrappers_Inh_ENonterminals :: (Set NontermIdent) }
data Syn_ENonterminals  = Syn_ENonterminals { errors_Syn_ENonterminals :: (Seq Error), fromToStates_Syn_ENonterminals :: (Map VisitIdentifier (Int,Int)), initStates_Syn_ENonterminals :: (Map NontermIdent Int), output_Syn_ENonterminals :: ([TH.Dec]), semFunBndDefs_Syn_ENonterminals :: (Seq (TH.Name, TH.Exp)), semFunBndTps_Syn_ENonterminals :: (Seq (TH.VarBangType)), visitKinds_Syn_ENonterminals :: (Map VisitIdentifier VisitKind), visitdefs_Syn_ENonterminals :: (Map VisitIdentifier (Set Identifier)), visituses_Syn_ENonterminals :: (Map VisitIdentifier (Set Identifier)) }
{-# INLINABLE wrap_ENonterminals #-}
wrap_ENonterminals :: T_ENonterminals  -> Inh_ENonterminals  -> (Syn_ENonterminals )
wrap_ENonterminals (T_ENonterminals act) (Inh_ENonterminals _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIavisitdefs _lhsIavisituses _lhsIderivings _lhsIimportBlocks _lhsIinhmap _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsIoptions _lhsIpragmaBlocks _lhsIsynmap _lhsItextBlocks _lhsItypeSyns _lhsIwrappers) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg10 = T_ENonterminals_vIn10 _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIavisitdefs _lhsIavisituses _lhsIderivings _lhsIimportBlocks _lhsIinhmap _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsIoptions _lhsIpragmaBlocks _lhsIsynmap _lhsItextBlocks _lhsItypeSyns _lhsIwrappers
        (T_ENonterminals_vOut10 _lhsOerrors _lhsOfromToStates _lhsOinitStates _lhsOoutput _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses) <- return (inv_ENonterminals_s11 sem arg10)
        return (Syn_ENonterminals _lhsOerrors _lhsOfromToStates _lhsOinitStates _lhsOoutput _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses)
   )

-- cata
{-# NOINLINE sem_ENonterminals #-}
sem_ENonterminals :: ENonterminals  -> T_ENonterminals 
sem_ENonterminals list = Prelude.foldr sem_ENonterminals_Cons sem_ENonterminals_Nil (Prelude.map sem_ENonterminal list)

-- semantic domain
newtype T_ENonterminals  = T_ENonterminals {
                                           attach_T_ENonterminals :: Identity (T_ENonterminals_s11 )
                                           }
newtype T_ENonterminals_s11  = C_ENonterminals_s11 {
                                                   inv_ENonterminals_s11 :: (T_ENonterminals_v10 )
                                                   }
data T_ENonterminals_s12  = C_ENonterminals_s12
type T_ENonterminals_v10  = (T_ENonterminals_vIn10 ) -> (T_ENonterminals_vOut10 )
data T_ENonterminals_vIn10  = T_ENonterminals_vIn10 (Map VisitIdentifier (Int,Int)) (Map NontermIdent Int) (Map VisitIdentifier VisitKind) (Map VisitIdentifier (Set Identifier)) (Map VisitIdentifier (Set Identifier)) (Derivings) ([String]) (Map NontermIdent Attributes) (Map NontermIdent (Map ConstructorIdent (Map Identifier Type))) (String) (String) (String -> String -> String -> Bool -> String) (Options) (String) (Map NontermIdent Attributes) ([String]) (TypeSyns) (Set NontermIdent)
data T_ENonterminals_vOut10  = T_ENonterminals_vOut10 (Seq Error) (Map VisitIdentifier (Int,Int)) (Map NontermIdent Int) ([TH.Dec]) (Seq (TH.Name, TH.Exp)) (Seq (TH.VarBangType)) (Map VisitIdentifier VisitKind) (Map VisitIdentifier (Set Identifier)) (Map VisitIdentifier (Set Identifier))
{-# NOINLINE sem_ENonterminals_Cons #-}
sem_ENonterminals_Cons :: T_ENonterminal  -> T_ENonterminals  -> T_ENonterminals 
sem_ENonterminals_Cons arg_hd_ arg_tl_ = T_ENonterminals (return st11) where
   {-# NOINLINE st11 #-}
   st11 = let
      v10 :: T_ENonterminals_v10 
      v10 = \ (T_ENonterminals_vIn10 _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIavisitdefs _lhsIavisituses _lhsIderivings _lhsIimportBlocks _lhsIinhmap _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsIoptions _lhsIpragmaBlocks _lhsIsynmap _lhsItextBlocks _lhsItypeSyns _lhsIwrappers) -> ( let
         _hdX8 = Control.Monad.Identity.runIdentity (attach_T_ENonterminal (arg_hd_))
         _tlX11 = Control.Monad.Identity.runIdentity (attach_T_ENonterminals (arg_tl_))
         (T_ENonterminal_vOut7 _hdIerrors _hdIfromToStates _hdIinitStates _hdIoutput _hdIsemFunBndDefs _hdIsemFunBndTps _hdIvisitKinds _hdIvisitdefs _hdIvisituses) = inv_ENonterminal_s8 _hdX8 (T_ENonterminal_vIn7 _hdOallFromToStates _hdOallInitStates _hdOallVisitKinds _hdOavisitdefs _hdOavisituses _hdOderivings _hdOimportBlocks _hdOinhmap _hdOlocalAttrTypes _hdOmainFile _hdOmainName _hdOmoduleHeader _hdOoptions _hdOpragmaBlocks _hdOsynmap _hdOtextBlocks _hdOtypeSyns _hdOwrappers)
         (T_ENonterminals_vOut10 _tlIerrors _tlIfromToStates _tlIinitStates _tlIoutput _tlIsemFunBndDefs _tlIsemFunBndTps _tlIvisitKinds _tlIvisitdefs _tlIvisituses) = inv_ENonterminals_s11 _tlX11 (T_ENonterminals_vIn10 _tlOallFromToStates _tlOallInitStates _tlOallVisitKinds _tlOavisitdefs _tlOavisituses _tlOderivings _tlOimportBlocks _tlOinhmap _tlOlocalAttrTypes _tlOmainFile _tlOmainName _tlOmoduleHeader _tlOoptions _tlOpragmaBlocks _tlOsynmap _tlOtextBlocks _tlOtypeSyns _tlOwrappers)
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule127 _hdIerrors _tlIerrors
         _lhsOfromToStates :: Map VisitIdentifier (Int,Int)
         _lhsOfromToStates = rule128 _hdIfromToStates _tlIfromToStates
         _lhsOinitStates :: Map NontermIdent Int
         _lhsOinitStates = rule129 _hdIinitStates _tlIinitStates
         _lhsOoutput :: [TH.Dec]
         _lhsOoutput = rule130 _hdIoutput _tlIoutput
         _lhsOsemFunBndDefs :: Seq (TH.Name, TH.Exp)
         _lhsOsemFunBndDefs = rule131 _hdIsemFunBndDefs _tlIsemFunBndDefs
         _lhsOsemFunBndTps :: Seq (TH.VarBangType)
         _lhsOsemFunBndTps = rule132 _hdIsemFunBndTps _tlIsemFunBndTps
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule133 _hdIvisitKinds _tlIvisitKinds
         _lhsOvisitdefs :: Map VisitIdentifier (Set Identifier)
         _lhsOvisitdefs = rule134 _hdIvisitdefs _tlIvisitdefs
         _lhsOvisituses :: Map VisitIdentifier (Set Identifier)
         _lhsOvisituses = rule135 _hdIvisituses _tlIvisituses
         _hdOallFromToStates = rule136 _lhsIallFromToStates
         _hdOallInitStates = rule137 _lhsIallInitStates
         _hdOallVisitKinds = rule138 _lhsIallVisitKinds
         _hdOavisitdefs = rule139 _lhsIavisitdefs
         _hdOavisituses = rule140 _lhsIavisituses
         _hdOderivings = rule141 _lhsIderivings
         _hdOimportBlocks = rule142 _lhsIimportBlocks
         _hdOinhmap = rule143 _lhsIinhmap
         _hdOlocalAttrTypes = rule144 _lhsIlocalAttrTypes
         _hdOmainFile = rule145 _lhsImainFile
         _hdOmainName = rule146 _lhsImainName
         _hdOmoduleHeader = rule147 _lhsImoduleHeader
         _hdOoptions = rule148 _lhsIoptions
         _hdOpragmaBlocks = rule149 _lhsIpragmaBlocks
         _hdOsynmap = rule150 _lhsIsynmap
         _hdOtextBlocks = rule151 _lhsItextBlocks
         _hdOtypeSyns = rule152 _lhsItypeSyns
         _hdOwrappers = rule153 _lhsIwrappers
         _tlOallFromToStates = rule154 _lhsIallFromToStates
         _tlOallInitStates = rule155 _lhsIallInitStates
         _tlOallVisitKinds = rule156 _lhsIallVisitKinds
         _tlOavisitdefs = rule157 _lhsIavisitdefs
         _tlOavisituses = rule158 _lhsIavisituses
         _tlOderivings = rule159 _lhsIderivings
         _tlOimportBlocks = rule160 _lhsIimportBlocks
         _tlOinhmap = rule161 _lhsIinhmap
         _tlOlocalAttrTypes = rule162 _lhsIlocalAttrTypes
         _tlOmainFile = rule163 _lhsImainFile
         _tlOmainName = rule164 _lhsImainName
         _tlOmoduleHeader = rule165 _lhsImoduleHeader
         _tlOoptions = rule166 _lhsIoptions
         _tlOpragmaBlocks = rule167 _lhsIpragmaBlocks
         _tlOsynmap = rule168 _lhsIsynmap
         _tlOtextBlocks = rule169 _lhsItextBlocks
         _tlOtypeSyns = rule170 _lhsItypeSyns
         _tlOwrappers = rule171 _lhsIwrappers
         __result_ = T_ENonterminals_vOut10 _lhsOerrors _lhsOfromToStates _lhsOinitStates _lhsOoutput _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses
         in __result_ )
     in C_ENonterminals_s11 v10
   {-# INLINE rule127 #-}
   rule127 = \ ((_hdIerrors) :: Seq Error) ((_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# INLINE rule128 #-}
   rule128 = \ ((_hdIfromToStates) :: Map VisitIdentifier (Int,Int)) ((_tlIfromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _hdIfromToStates `mappend` _tlIfromToStates
   {-# INLINE rule129 #-}
   rule129 = \ ((_hdIinitStates) :: Map NontermIdent Int) ((_tlIinitStates) :: Map NontermIdent Int) ->
     _hdIinitStates `mappend` _tlIinitStates
   {-# INLINE rule130 #-}
   rule130 = \ ((_hdIoutput) :: [TH.Dec]) ((_tlIoutput) :: [TH.Dec]) ->
     _hdIoutput ++ _tlIoutput
   {-# INLINE rule131 #-}
   rule131 = \ ((_hdIsemFunBndDefs) :: Seq (TH.Name, TH.Exp)) ((_tlIsemFunBndDefs) :: Seq (TH.Name, TH.Exp)) ->
     _hdIsemFunBndDefs Seq.>< _tlIsemFunBndDefs
   {-# INLINE rule132 #-}
   rule132 = \ ((_hdIsemFunBndTps) :: Seq (TH.VarBangType)) ((_tlIsemFunBndTps) :: Seq (TH.VarBangType)) ->
     _hdIsemFunBndTps Seq.>< _tlIsemFunBndTps
   {-# INLINE rule133 #-}
   rule133 = \ ((_hdIvisitKinds) :: Map VisitIdentifier VisitKind) ((_tlIvisitKinds) :: Map VisitIdentifier VisitKind) ->
     _hdIvisitKinds `mappend` _tlIvisitKinds
   {-# INLINE rule134 #-}
   rule134 = \ ((_hdIvisitdefs) :: Map VisitIdentifier (Set Identifier)) ((_tlIvisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _hdIvisitdefs `uwSetUnion` _tlIvisitdefs
   {-# INLINE rule135 #-}
   rule135 = \ ((_hdIvisituses) :: Map VisitIdentifier (Set Identifier)) ((_tlIvisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _hdIvisituses `uwSetUnion` _tlIvisituses
   {-# INLINE rule136 #-}
   rule136 = \ ((_lhsIallFromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _lhsIallFromToStates
   {-# INLINE rule137 #-}
   rule137 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule138 #-}
   rule138 = \ ((_lhsIallVisitKinds) :: Map VisitIdentifier VisitKind) ->
     _lhsIallVisitKinds
   {-# INLINE rule139 #-}
   rule139 = \ ((_lhsIavisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisitdefs
   {-# INLINE rule140 #-}
   rule140 = \ ((_lhsIavisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisituses
   {-# INLINE rule141 #-}
   rule141 = \ ((_lhsIderivings) :: Derivings) ->
     _lhsIderivings
   {-# INLINE rule142 #-}
   rule142 = \ ((_lhsIimportBlocks) :: [String]) ->
     _lhsIimportBlocks
   {-# INLINE rule143 #-}
   rule143 = \ ((_lhsIinhmap) :: Map NontermIdent Attributes) ->
     _lhsIinhmap
   {-# INLINE rule144 #-}
   rule144 = \ ((_lhsIlocalAttrTypes) :: Map NontermIdent (Map ConstructorIdent (Map Identifier Type))) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule145 #-}
   rule145 = \ ((_lhsImainFile) :: String) ->
     _lhsImainFile
   {-# INLINE rule146 #-}
   rule146 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule147 #-}
   rule147 = \ ((_lhsImoduleHeader) :: String -> String -> String -> Bool -> String) ->
     _lhsImoduleHeader
   {-# INLINE rule148 #-}
   rule148 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule149 #-}
   rule149 = \ ((_lhsIpragmaBlocks) :: String) ->
     _lhsIpragmaBlocks
   {-# INLINE rule150 #-}
   rule150 = \ ((_lhsIsynmap) :: Map NontermIdent Attributes) ->
     _lhsIsynmap
   {-# INLINE rule151 #-}
   rule151 = \ ((_lhsItextBlocks) :: [String]) ->
     _lhsItextBlocks
   {-# INLINE rule152 #-}
   rule152 = \ ((_lhsItypeSyns) :: TypeSyns) ->
     _lhsItypeSyns
   {-# INLINE rule153 #-}
   rule153 = \ ((_lhsIwrappers) :: Set NontermIdent) ->
     _lhsIwrappers
   {-# INLINE rule154 #-}
   rule154 = \ ((_lhsIallFromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _lhsIallFromToStates
   {-# INLINE rule155 #-}
   rule155 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule156 #-}
   rule156 = \ ((_lhsIallVisitKinds) :: Map VisitIdentifier VisitKind) ->
     _lhsIallVisitKinds
   {-# INLINE rule157 #-}
   rule157 = \ ((_lhsIavisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisitdefs
   {-# INLINE rule158 #-}
   rule158 = \ ((_lhsIavisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisituses
   {-# INLINE rule159 #-}
   rule159 = \ ((_lhsIderivings) :: Derivings) ->
     _lhsIderivings
   {-# INLINE rule160 #-}
   rule160 = \ ((_lhsIimportBlocks) :: [String]) ->
     _lhsIimportBlocks
   {-# INLINE rule161 #-}
   rule161 = \ ((_lhsIinhmap) :: Map NontermIdent Attributes) ->
     _lhsIinhmap
   {-# INLINE rule162 #-}
   rule162 = \ ((_lhsIlocalAttrTypes) :: Map NontermIdent (Map ConstructorIdent (Map Identifier Type))) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule163 #-}
   rule163 = \ ((_lhsImainFile) :: String) ->
     _lhsImainFile
   {-# INLINE rule164 #-}
   rule164 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule165 #-}
   rule165 = \ ((_lhsImoduleHeader) :: String -> String -> String -> Bool -> String) ->
     _lhsImoduleHeader
   {-# INLINE rule166 #-}
   rule166 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule167 #-}
   rule167 = \ ((_lhsIpragmaBlocks) :: String) ->
     _lhsIpragmaBlocks
   {-# INLINE rule168 #-}
   rule168 = \ ((_lhsIsynmap) :: Map NontermIdent Attributes) ->
     _lhsIsynmap
   {-# INLINE rule169 #-}
   rule169 = \ ((_lhsItextBlocks) :: [String]) ->
     _lhsItextBlocks
   {-# INLINE rule170 #-}
   rule170 = \ ((_lhsItypeSyns) :: TypeSyns) ->
     _lhsItypeSyns
   {-# INLINE rule171 #-}
   rule171 = \ ((_lhsIwrappers) :: Set NontermIdent) ->
     _lhsIwrappers
{-# NOINLINE sem_ENonterminals_Nil #-}
sem_ENonterminals_Nil ::  T_ENonterminals 
sem_ENonterminals_Nil  = T_ENonterminals (return st11) where
   {-# NOINLINE st11 #-}
   st11 = let
      v10 :: T_ENonterminals_v10 
      v10 = \ (T_ENonterminals_vIn10 _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIavisitdefs _lhsIavisituses _lhsIderivings _lhsIimportBlocks _lhsIinhmap _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsIoptions _lhsIpragmaBlocks _lhsIsynmap _lhsItextBlocks _lhsItypeSyns _lhsIwrappers) -> ( let
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule172  ()
         _lhsOfromToStates :: Map VisitIdentifier (Int,Int)
         _lhsOfromToStates = rule173  ()
         _lhsOinitStates :: Map NontermIdent Int
         _lhsOinitStates = rule174  ()
         _lhsOoutput :: [TH.Dec]
         _lhsOoutput = rule175  ()
         _lhsOsemFunBndDefs :: Seq (TH.Name, TH.Exp)
         _lhsOsemFunBndDefs = rule176  ()
         _lhsOsemFunBndTps :: Seq (TH.VarBangType)
         _lhsOsemFunBndTps = rule177  ()
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule178  ()
         _lhsOvisitdefs :: Map VisitIdentifier (Set Identifier)
         _lhsOvisitdefs = rule179  ()
         _lhsOvisituses :: Map VisitIdentifier (Set Identifier)
         _lhsOvisituses = rule180  ()
         __result_ = T_ENonterminals_vOut10 _lhsOerrors _lhsOfromToStates _lhsOinitStates _lhsOoutput _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses
         in __result_ )
     in C_ENonterminals_s11 v10
   {-# INLINE rule172 #-}
   rule172 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule173 #-}
   rule173 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule174 #-}
   rule174 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule175 #-}
   rule175 = \  (_ :: ()) ->
     []
   {-# INLINE rule176 #-}
   rule176 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule177 #-}
   rule177 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule178 #-}
   rule178 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule179 #-}
   rule179 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule180 #-}
   rule180 = \  (_ :: ()) ->
     Map.empty

-- EProduction -------------------------------------------------
-- wrapper
data Inh_EProduction  = Inh_EProduction { allFromToStates_Inh_EProduction :: (Map VisitIdentifier (Int,Int)), allInhmap_Inh_EProduction :: (Map NontermIdent Attributes), allInitStates_Inh_EProduction :: (Map NontermIdent Int), allSynmap_Inh_EProduction :: (Map NontermIdent Attributes), allVisitKinds_Inh_EProduction :: (Map VisitIdentifier VisitKind), allstates_Inh_EProduction :: (Set StateIdentifier), avisitdefs_Inh_EProduction :: (Map VisitIdentifier (Set Identifier)), avisituses_Inh_EProduction :: (Map VisitIdentifier (Set Identifier)), importBlocks_Inh_EProduction :: ([String]), inhmap_Inh_EProduction :: (Attributes), initial_Inh_EProduction :: (StateIdentifier), localAttrTypes_Inh_EProduction :: (Map ConstructorIdent (Map Identifier Type)), mainFile_Inh_EProduction :: (String), mainName_Inh_EProduction :: (String), moduleHeader_Inh_EProduction :: (String -> String -> String -> Bool -> String), nextVisits_Inh_EProduction :: (Map StateIdentifier StateCtx), nt_Inh_EProduction :: (NontermIdent), ntType_Inh_EProduction :: (Type), options_Inh_EProduction :: (Options), params_Inh_EProduction :: ([Identifier]), pragmaBlocks_Inh_EProduction :: (String), prevVisits_Inh_EProduction :: (Map StateIdentifier StateCtx), rename_Inh_EProduction :: (Bool), synmap_Inh_EProduction :: (Attributes), textBlocks_Inh_EProduction :: ([String]) }
data Syn_EProduction  = Syn_EProduction { allvisits_Syn_EProduction :: ([VisitStateState]), count_Syn_EProduction :: (Int), datatype_Syn_EProduction :: (TH.Con), errors_Syn_EProduction :: (Seq Error), fromToStates_Syn_EProduction :: (Map VisitIdentifier (Int,Int)), semFunBndDefs_Syn_EProduction :: (Seq (TH.Name, TH.Exp)), semFunBndTps_Syn_EProduction :: (Seq (TH.VarBangType)), sem_nt_Syn_EProduction :: ([TH.Clause]), visitKinds_Syn_EProduction :: (Map VisitIdentifier VisitKind), visitdefs_Syn_EProduction :: (Map VisitIdentifier (Set Identifier)), visituses_Syn_EProduction :: (Map VisitIdentifier (Set Identifier)) }
{-# INLINABLE wrap_EProduction #-}
wrap_EProduction :: T_EProduction  -> Inh_EProduction  -> (Syn_EProduction )
wrap_EProduction (T_EProduction act) (Inh_EProduction _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallstates _lhsIavisitdefs _lhsIavisituses _lhsIimportBlocks _lhsIinhmap _lhsIinitial _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInextVisits _lhsInt _lhsIntType _lhsIoptions _lhsIparams _lhsIpragmaBlocks _lhsIprevVisits _lhsIrename _lhsIsynmap _lhsItextBlocks) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg13 = T_EProduction_vIn13 _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallstates _lhsIavisitdefs _lhsIavisituses _lhsIimportBlocks _lhsIinhmap _lhsIinitial _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInextVisits _lhsInt _lhsIntType _lhsIoptions _lhsIparams _lhsIpragmaBlocks _lhsIprevVisits _lhsIrename _lhsIsynmap _lhsItextBlocks
        (T_EProduction_vOut13 _lhsOallvisits _lhsOcount _lhsOdatatype _lhsOerrors _lhsOfromToStates _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOsem_nt _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses) <- return (inv_EProduction_s14 sem arg13)
        return (Syn_EProduction _lhsOallvisits _lhsOcount _lhsOdatatype _lhsOerrors _lhsOfromToStates _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOsem_nt _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses)
   )

-- cata
{-# INLINE sem_EProduction #-}
sem_EProduction :: EProduction  -> T_EProduction 
sem_EProduction ( EProduction con_ params_ constraints_ rules_ children_ visits_ ) = sem_EProduction_EProduction con_ params_ constraints_ ( sem_ERules rules_ ) ( sem_EChildren children_ ) ( sem_Visits visits_ )

-- semantic domain
newtype T_EProduction  = T_EProduction {
                                       attach_T_EProduction :: Identity (T_EProduction_s14 )
                                       }
newtype T_EProduction_s14  = C_EProduction_s14 {
                                               inv_EProduction_s14 :: (T_EProduction_v13 )
                                               }
data T_EProduction_s15  = C_EProduction_s15
type T_EProduction_v13  = (T_EProduction_vIn13 ) -> (T_EProduction_vOut13 )
data T_EProduction_vIn13  = T_EProduction_vIn13 (Map VisitIdentifier (Int,Int)) (Map NontermIdent Attributes) (Map NontermIdent Int) (Map NontermIdent Attributes) (Map VisitIdentifier VisitKind) (Set StateIdentifier) (Map VisitIdentifier (Set Identifier)) (Map VisitIdentifier (Set Identifier)) ([String]) (Attributes) (StateIdentifier) (Map ConstructorIdent (Map Identifier Type)) (String) (String) (String -> String -> String -> Bool -> String) (Map StateIdentifier StateCtx) (NontermIdent) (Type) (Options) ([Identifier]) (String) (Map StateIdentifier StateCtx) (Bool) (Attributes) ([String])
data T_EProduction_vOut13  = T_EProduction_vOut13 ([VisitStateState]) (Int) (TH.Con) (Seq Error) (Map VisitIdentifier (Int,Int)) (Seq (TH.Name, TH.Exp)) (Seq (TH.VarBangType)) ([TH.Clause]) (Map VisitIdentifier VisitKind) (Map VisitIdentifier (Set Identifier)) (Map VisitIdentifier (Set Identifier))
{-# NOINLINE sem_EProduction_EProduction #-}
sem_EProduction_EProduction :: (ConstructorIdent) -> ([Identifier]) -> ([Type]) -> T_ERules  -> T_EChildren  -> T_Visits  -> T_EProduction 
sem_EProduction_EProduction arg_con_ arg_params_ arg_constraints_ arg_rules_ arg_children_ arg_visits_ = T_EProduction (return st14) where
   {-# NOINLINE st14 #-}
   st14 = let
      v13 :: T_EProduction_v13 
      v13 = \ (T_EProduction_vIn13 _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallstates _lhsIavisitdefs _lhsIavisituses _lhsIimportBlocks _lhsIinhmap _lhsIinitial _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInextVisits _lhsInt _lhsIntType _lhsIoptions _lhsIparams _lhsIpragmaBlocks _lhsIprevVisits _lhsIrename _lhsIsynmap _lhsItextBlocks) -> ( let
         _rulesX23 = Control.Monad.Identity.runIdentity (attach_T_ERules (arg_rules_))
         _childrenX5 = Control.Monad.Identity.runIdentity (attach_T_EChildren (arg_children_))
         _visitsX56 = Control.Monad.Identity.runIdentity (attach_T_Visits (arg_visits_))
         (T_ERules_vOut22 _rulesIerrors _rulesIruledefs _rulesIruleuses _rulesIusedArgs) = inv_ERules_s23 _rulesX23 (T_ERules_vIn22 _rulesOallInhmap _rulesOallSynmap _rulesOchildTypes _rulesOcon _rulesOimportBlocks _rulesOinhmap _rulesOlazyIntras _rulesOlocalAttrTypes _rulesOmainFile _rulesOmainName _rulesOmoduleHeader _rulesOnt _rulesOoptions _rulesOpragmaBlocks _rulesOruleKinds _rulesOsynmap _rulesOtextBlocks _rulesOusageInfo)
         (T_EChildren_vOut4 _childrenIargnamesw _childrenIargpats _childrenIargtps _childrenIchildTypes _childrenIdatatype _childrenIdatatypeVar _childrenIterminaldefs _childrenIusedArgs) = inv_EChildren_s5 _childrenX5 (T_EChildren_vIn4 _childrenOallInitStates _childrenOcon _childrenOimportBlocks _childrenOmainFile _childrenOmainName _childrenOmoduleHeader _childrenOnt _childrenOoptions _childrenOpragmaBlocks _childrenOtextBlocks)
         (T_Visits_vOut55 _visitsIallvisits _visitsIerrors _visitsIfromToStates _visitsIintramap _visitsIlazyIntras _visitsIruleKinds _visitsIruleUsage _visitsIusedArgs _visitsIvisitKinds _visitsIvisitdefs _visitsIvisituses) = inv_Visits_s56 _visitsX56 (T_Visits_vIn55 _visitsOallFromToStates _visitsOallInhmap _visitsOallInitStates _visitsOallSynmap _visitsOallVisitKinds _visitsOallintramap _visitsOavisitdefs _visitsOavisituses _visitsOchildTypes _visitsOcon _visitsOinhmap _visitsOnextVisits _visitsOnt _visitsOoptions _visitsOparams _visitsOprevVisits _visitsOruledefs _visitsOruleuses _visitsOsynmap _visitsOterminaldefs)
         _childrenOcon = rule181 arg_con_
         _rulesOcon = rule182 arg_con_
         _visitsOcon = rule183 arg_con_
         _lhsOdatatype :: TH.Con
         _lhsOdatatype = rule184 _childrenIdatatype _childrenIdatatypeVar _classTH1 _lhsInt _lhsIoptions _lhsIrename _quantTH1 arg_con_
         _classTH1 = rule185 arg_constraints_
         _quantTH1 = rule186 arg_params_
         _lhsOcount :: Int
         _lhsOcount = rule187  ()
         _lhsOsem_nt :: [TH.Clause]
         _lhsOsem_nt = rule188 _childrenIargnamesw _childrenIargpats _lhsInt _lhsIrename arg_con_
         _lhsOsemFunBndDefs :: Seq (TH.Name, TH.Exp)
         _lhsOsemFunBndDefs = rule189 _semFunBndDef
         _lhsOsemFunBndTps :: Seq (TH.VarBangType)
         _lhsOsemFunBndTps = rule190 _semFunBndTp
         _semFunBndDef = rule191 _semFunBndNm _semname
         _semFunBndTp = rule192 _semFunBndNm _sem_tp
         _semFunBndNm = rule193 _lhsInt arg_con_
         _t_type = rule194 _lhsInt
         _t_params = rule195 _lhsIparams
         _semname = rule196 _lhsInt arg_con_
         _sem_tp = rule197 _childrenIargtps _classTH1 _quantTH2 _t_params _t_type
         _quantTH2 = rule198 _lhsIparams arg_params_
         _rulesOusageInfo = rule199 _visitsIruleUsage
         _lazyIntras = rule200 _visitsIlazyIntras
         _addbang = rule201 _lhsIoptions
         _childTypes = rule202 _childrenIchildTypes _lhsIntType
         _localAttrTypes = rule203 _lhsIlocalAttrTypes arg_con_
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule204 _rulesIerrors _visitsIerrors
         _lhsOfromToStates :: Map VisitIdentifier (Int,Int)
         _lhsOfromToStates = rule205 _visitsIfromToStates
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule206 _visitsIvisitKinds
         _lhsOvisitdefs :: Map VisitIdentifier (Set Identifier)
         _lhsOvisitdefs = rule207 _visitsIvisitdefs
         _lhsOvisituses :: Map VisitIdentifier (Set Identifier)
         _lhsOvisituses = rule208 _visitsIvisituses
         _lhsOallvisits :: [VisitStateState]
         _lhsOallvisits = rule209 _visitsIallvisits
         _rulesOallInhmap = rule210 _lhsIallInhmap
         _rulesOallSynmap = rule211 _lhsIallSynmap
         _rulesOchildTypes = rule212 _childTypes
         _rulesOimportBlocks = rule213 _lhsIimportBlocks
         _rulesOinhmap = rule214 _lhsIinhmap
         _rulesOlazyIntras = rule215 _lazyIntras
         _rulesOlocalAttrTypes = rule216 _localAttrTypes
         _rulesOmainFile = rule217 _lhsImainFile
         _rulesOmainName = rule218 _lhsImainName
         _rulesOmoduleHeader = rule219 _lhsImoduleHeader
         _rulesOnt = rule220 _lhsInt
         _rulesOoptions = rule221 _lhsIoptions
         _rulesOpragmaBlocks = rule222 _lhsIpragmaBlocks
         _rulesOruleKinds = rule223  ()
         _rulesOsynmap = rule224 _lhsIsynmap
         _rulesOtextBlocks = rule225 _lhsItextBlocks
         _childrenOallInitStates = rule226 _lhsIallInitStates
         _childrenOimportBlocks = rule227 _lhsIimportBlocks
         _childrenOmainFile = rule228 _lhsImainFile
         _childrenOmainName = rule229 _lhsImainName
         _childrenOmoduleHeader = rule230 _lhsImoduleHeader
         _childrenOnt = rule231 _lhsInt
         _childrenOoptions = rule232 _lhsIoptions
         _childrenOpragmaBlocks = rule233 _lhsIpragmaBlocks
         _childrenOtextBlocks = rule234 _lhsItextBlocks
         _visitsOallFromToStates = rule235 _lhsIallFromToStates
         _visitsOallInhmap = rule236 _lhsIallInhmap
         _visitsOallInitStates = rule237 _lhsIallInitStates
         _visitsOallSynmap = rule238 _lhsIallSynmap
         _visitsOallVisitKinds = rule239 _lhsIallVisitKinds
         _visitsOallintramap = rule240  ()
         _visitsOavisitdefs = rule241 _lhsIavisitdefs
         _visitsOavisituses = rule242 _lhsIavisituses
         _visitsOchildTypes = rule243 _childTypes
         _visitsOinhmap = rule244 _lhsIinhmap
         _visitsOnextVisits = rule245 _lhsInextVisits
         _visitsOnt = rule246 _lhsInt
         _visitsOoptions = rule247 _lhsIoptions
         _visitsOparams = rule248 _lhsIparams
         _visitsOprevVisits = rule249 _lhsIprevVisits
         _visitsOruledefs = rule250 _rulesIruledefs
         _visitsOruleuses = rule251 _rulesIruleuses
         _visitsOsynmap = rule252 _lhsIsynmap
         _visitsOterminaldefs = rule253 _childrenIterminaldefs
         __result_ = T_EProduction_vOut13 _lhsOallvisits _lhsOcount _lhsOdatatype _lhsOerrors _lhsOfromToStates _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOsem_nt _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses
         in __result_ )
     in C_EProduction_s14 v13
   {-# INLINE rule181 #-}
   {-# LINE 82 "src-ag/ExecutionPlan2TH.ag" #-}
   rule181 = \ con_ ->
                                 {-# LINE 82 "src-ag/ExecutionPlan2TH.ag" #-}
                                 con_
                                 {-# LINE 1699 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule182 #-}
   {-# LINE 83 "src-ag/ExecutionPlan2TH.ag" #-}
   rule182 = \ con_ ->
                                 {-# LINE 83 "src-ag/ExecutionPlan2TH.ag" #-}
                                 con_
                                 {-# LINE 1705 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule183 #-}
   {-# LINE 84 "src-ag/ExecutionPlan2TH.ag" #-}
   rule183 = \ con_ ->
                                 {-# LINE 84 "src-ag/ExecutionPlan2TH.ag" #-}
                                 con_
                                 {-# LINE 1711 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule184 #-}
   {-# LINE 173 "src-ag/ExecutionPlan2TH.ag" #-}
   rule184 = \ ((_childrenIdatatype) :: [TH.BangType]) ((_childrenIdatatypeVar) :: [TH.VarBangType]) _classTH1 ((_lhsInt) :: NontermIdent) ((_lhsIoptions) :: Options) ((_lhsIrename) :: Bool) _quantTH1 con_ ->
                                 {-# LINE 173 "src-ag/ExecutionPlan2TH.ag" #-}
                                 _quantTH1     _classTH1
                                 (conTH (TH.mkName (conname _lhsIrename _lhsInt con_))
                                        (if dataRecords _lhsIoptions
                                           then Left _childrenIdatatypeVar
                                           else Right _childrenIdatatype))
                                 {-# LINE 1721 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule185 #-}
   {-# LINE 178 "src-ag/ExecutionPlan2TH.ag" #-}
   rule185 = \ constraints_ ->
                                 {-# LINE 178 "src-ag/ExecutionPlan2TH.ag" #-}
                                 map typeToTH constraints_
                                 {-# LINE 1727 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule186 #-}
   {-# LINE 179 "src-ag/ExecutionPlan2TH.ag" #-}
   rule186 = \ params_ ->
                                 {-# LINE 179 "src-ag/ExecutionPlan2TH.ag" #-}
                                 quantsTH TH.ForallC params_
                                 {-# LINE 1733 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule187 #-}
   {-# LINE 330 "src-ag/ExecutionPlan2TH.ag" #-}
   rule187 = \  (_ :: ()) ->
                                              {-# LINE 330 "src-ag/ExecutionPlan2TH.ag" #-}
                                              1
                                              {-# LINE 1739 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule188 #-}
   {-# LINE 335 "src-ag/ExecutionPlan2TH.ag" #-}
   rule188 = \ ((_childrenIargnamesw) :: [TH.Exp]) ((_childrenIargpats) ::  [TH.Pat] ) ((_lhsInt) :: NontermIdent) ((_lhsIrename) :: Bool) con_ ->
                               {-# LINE 335 "src-ag/ExecutionPlan2TH.ag" #-}
                               [ TH.Clause [TH.ConP (TH.mkName (conname _lhsIrename _lhsInt con_)) _childrenIargpats]
                                           (TH.NormalB (foldl TH.AppE
                                                              (TH.VarE (TH.mkName ("sem_" ++ getName _lhsInt ++ "_" ++ getName con_)))
                                                              _childrenIargnamesw))
                                           []
                               ]
                               {-# LINE 1750 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule189 #-}
   {-# LINE 582 "src-ag/ExecutionPlan2TH.ag" #-}
   rule189 = \ _semFunBndDef ->
                        {-# LINE 582 "src-ag/ExecutionPlan2TH.ag" #-}
                        Seq.singleton _semFunBndDef
                        {-# LINE 1756 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule190 #-}
   {-# LINE 583 "src-ag/ExecutionPlan2TH.ag" #-}
   rule190 = \ _semFunBndTp ->
                        {-# LINE 583 "src-ag/ExecutionPlan2TH.ag" #-}
                        Seq.singleton _semFunBndTp
                        {-# LINE 1762 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule191 #-}
   {-# LINE 584 "src-ag/ExecutionPlan2TH.ag" #-}
   rule191 = \ _semFunBndNm _semname ->
                        {-# LINE 584 "src-ag/ExecutionPlan2TH.ag" #-}
                        (_semFunBndNm    , TH.VarE _semname    )
                        {-# LINE 1768 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule192 #-}
   {-# LINE 585 "src-ag/ExecutionPlan2TH.ag" #-}
   rule192 = \ _semFunBndNm _sem_tp ->
                        {-# LINE 585 "src-ag/ExecutionPlan2TH.ag" #-}
                        (_semFunBndNm    , nobang, _sem_tp    )
                        {-# LINE 1774 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule193 #-}
   {-# LINE 586 "src-ag/ExecutionPlan2TH.ag" #-}
   rule193 = \ ((_lhsInt) :: NontermIdent) con_ ->
                        {-# LINE 586 "src-ag/ExecutionPlan2TH.ag" #-}
                        TH.mkName (lateSemConLabel _lhsInt con_)
                        {-# LINE 1780 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule194 #-}
   {-# LINE 648 "src-ag/ExecutionPlan2TH.ag" #-}
   rule194 = \ ((_lhsInt) :: NontermIdent) ->
                                 {-# LINE 648 "src-ag/ExecutionPlan2TH.ag" #-}
                                 TH.ConT (TH.mkName ("T_" ++ getName _lhsInt))
                                 {-# LINE 1786 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule195 #-}
   {-# LINE 649 "src-ag/ExecutionPlan2TH.ag" #-}
   rule195 = \ ((_lhsIparams) :: [Identifier]) ->
                                 {-# LINE 649 "src-ag/ExecutionPlan2TH.ag" #-}
                                 map (TH.VarT . TH.mkName . getName) _lhsIparams
                                 {-# LINE 1792 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule196 #-}
   {-# LINE 660 "src-ag/ExecutionPlan2TH.ag" #-}
   rule196 = \ ((_lhsInt) :: NontermIdent) con_ ->
                                 {-# LINE 660 "src-ag/ExecutionPlan2TH.ag" #-}
                                 TH.mkName ("sem_" ++ show _lhsInt ++ "_" ++ show con_)
                                 {-# LINE 1798 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule197 #-}
   {-# LINE 661 "src-ag/ExecutionPlan2TH.ag" #-}
   rule197 = \ ((_childrenIargtps) ::  TH.Type -> TH.Type ) _classTH1 _quantTH2 _t_params _t_type ->
                                 {-# LINE 661 "src-ag/ExecutionPlan2TH.ag" #-}
                                 _quantTH2     _classTH1     (_childrenIargtps (foldl TH.AppT _t_type     _t_params    ))
                                 {-# LINE 1804 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule198 #-}
   {-# LINE 662 "src-ag/ExecutionPlan2TH.ag" #-}
   rule198 = \ ((_lhsIparams) :: [Identifier]) params_ ->
                                 {-# LINE 662 "src-ag/ExecutionPlan2TH.ag" #-}
                                 quantsTH TH.ForallT (_lhsIparams ++ params_)
                                 {-# LINE 1810 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule199 #-}
   {-# LINE 1332 "src-ag/ExecutionPlan2TH.ag" #-}
   rule199 = \ ((_visitsIruleUsage) :: Map Identifier Int) ->
                                                   {-# LINE 1332 "src-ag/ExecutionPlan2TH.ag" #-}
                                                   _visitsIruleUsage
                                                   {-# LINE 1816 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule200 #-}
   {-# LINE 1456 "src-ag/ExecutionPlan2TH.ag" #-}
   rule200 = \ ((_visitsIlazyIntras) :: Set String) ->
                     {-# LINE 1456 "src-ag/ExecutionPlan2TH.ag" #-}
                     _visitsIlazyIntras
                     {-# LINE 1822 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule201 #-}
   {-# LINE 1603 "src-ag/ExecutionPlan2TH.ag" #-}
   rule201 = \ ((_lhsIoptions) :: Options) ->
                                                    {-# LINE 1603 "src-ag/ExecutionPlan2TH.ag" #-}
                                                    \x -> if bangpats _lhsIoptions then TH.BangP x else x
                                                    {-# LINE 1828 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule202 #-}
   {-# LINE 1653 "src-ag/ExecutionPlan2TH.ag" #-}
   rule202 = \ ((_childrenIchildTypes) :: Map Identifier Type) ((_lhsIntType) :: Type) ->
                     {-# LINE 1653 "src-ag/ExecutionPlan2TH.ag" #-}
                     Map.singleton _LHS _lhsIntType `Map.union` _childrenIchildTypes
                     {-# LINE 1834 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule203 #-}
   {-# LINE 1670 "src-ag/ExecutionPlan2TH.ag" #-}
   rule203 = \ ((_lhsIlocalAttrTypes) :: Map ConstructorIdent (Map Identifier Type)) con_ ->
                           {-# LINE 1670 "src-ag/ExecutionPlan2TH.ag" #-}
                           Map.findWithDefault Map.empty con_ _lhsIlocalAttrTypes
                           {-# LINE 1840 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule204 #-}
   rule204 = \ ((_rulesIerrors) :: Seq Error) ((_visitsIerrors) :: Seq Error) ->
     _rulesIerrors Seq.>< _visitsIerrors
   {-# INLINE rule205 #-}
   rule205 = \ ((_visitsIfromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _visitsIfromToStates
   {-# INLINE rule206 #-}
   rule206 = \ ((_visitsIvisitKinds) :: Map VisitIdentifier VisitKind) ->
     _visitsIvisitKinds
   {-# INLINE rule207 #-}
   rule207 = \ ((_visitsIvisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _visitsIvisitdefs
   {-# INLINE rule208 #-}
   rule208 = \ ((_visitsIvisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _visitsIvisituses
   {-# INLINE rule209 #-}
   rule209 = \ ((_visitsIallvisits) :: [VisitStateState]) ->
     _visitsIallvisits
   {-# INLINE rule210 #-}
   rule210 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule211 #-}
   rule211 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule212 #-}
   rule212 = \ _childTypes ->
     _childTypes
   {-# INLINE rule213 #-}
   rule213 = \ ((_lhsIimportBlocks) :: [String]) ->
     _lhsIimportBlocks
   {-# INLINE rule214 #-}
   rule214 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule215 #-}
   rule215 = \ _lazyIntras ->
     _lazyIntras
   {-# INLINE rule216 #-}
   rule216 = \ _localAttrTypes ->
     _localAttrTypes
   {-# INLINE rule217 #-}
   rule217 = \ ((_lhsImainFile) :: String) ->
     _lhsImainFile
   {-# INLINE rule218 #-}
   rule218 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule219 #-}
   rule219 = \ ((_lhsImoduleHeader) :: String -> String -> String -> Bool -> String) ->
     _lhsImoduleHeader
   {-# INLINE rule220 #-}
   rule220 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule221 #-}
   rule221 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule222 #-}
   rule222 = \ ((_lhsIpragmaBlocks) :: String) ->
     _lhsIpragmaBlocks
   {-# INLINE rule223 #-}
   rule223 = \  (_ :: ()) ->
     error "missing rule: EProduction.EProduction.rules.ruleKinds"
   {-# INLINE rule224 #-}
   rule224 = \ ((_lhsIsynmap) :: Attributes) ->
     _lhsIsynmap
   {-# INLINE rule225 #-}
   rule225 = \ ((_lhsItextBlocks) :: [String]) ->
     _lhsItextBlocks
   {-# INLINE rule226 #-}
   rule226 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule227 #-}
   rule227 = \ ((_lhsIimportBlocks) :: [String]) ->
     _lhsIimportBlocks
   {-# INLINE rule228 #-}
   rule228 = \ ((_lhsImainFile) :: String) ->
     _lhsImainFile
   {-# INLINE rule229 #-}
   rule229 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule230 #-}
   rule230 = \ ((_lhsImoduleHeader) :: String -> String -> String -> Bool -> String) ->
     _lhsImoduleHeader
   {-# INLINE rule231 #-}
   rule231 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule232 #-}
   rule232 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule233 #-}
   rule233 = \ ((_lhsIpragmaBlocks) :: String) ->
     _lhsIpragmaBlocks
   {-# INLINE rule234 #-}
   rule234 = \ ((_lhsItextBlocks) :: [String]) ->
     _lhsItextBlocks
   {-# INLINE rule235 #-}
   rule235 = \ ((_lhsIallFromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _lhsIallFromToStates
   {-# INLINE rule236 #-}
   rule236 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule237 #-}
   rule237 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule238 #-}
   rule238 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule239 #-}
   rule239 = \ ((_lhsIallVisitKinds) :: Map VisitIdentifier VisitKind) ->
     _lhsIallVisitKinds
   {-# INLINE rule240 #-}
   rule240 = \  (_ :: ()) ->
     error "missing rule: EProduction.EProduction.visits.allintramap"
   {-# INLINE rule241 #-}
   rule241 = \ ((_lhsIavisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisitdefs
   {-# INLINE rule242 #-}
   rule242 = \ ((_lhsIavisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisituses
   {-# INLINE rule243 #-}
   rule243 = \ _childTypes ->
     _childTypes
   {-# INLINE rule244 #-}
   rule244 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule245 #-}
   rule245 = \ ((_lhsInextVisits) :: Map StateIdentifier StateCtx) ->
     _lhsInextVisits
   {-# INLINE rule246 #-}
   rule246 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule247 #-}
   rule247 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule248 #-}
   rule248 = \ ((_lhsIparams) :: [Identifier]) ->
     _lhsIparams
   {-# INLINE rule249 #-}
   rule249 = \ ((_lhsIprevVisits) :: Map StateIdentifier StateCtx) ->
     _lhsIprevVisits
   {-# INLINE rule250 #-}
   rule250 = \ ((_rulesIruledefs) :: Map Identifier (Set String)) ->
     _rulesIruledefs
   {-# INLINE rule251 #-}
   rule251 = \ ((_rulesIruleuses) :: Map Identifier (Map String (Maybe NonLocalAttr))) ->
     _rulesIruleuses
   {-# INLINE rule252 #-}
   rule252 = \ ((_lhsIsynmap) :: Attributes) ->
     _lhsIsynmap
   {-# INLINE rule253 #-}
   rule253 = \ ((_childrenIterminaldefs) :: Set String) ->
     _childrenIterminaldefs

-- EProductions ------------------------------------------------
-- wrapper
data Inh_EProductions  = Inh_EProductions { allFromToStates_Inh_EProductions :: (Map VisitIdentifier (Int,Int)), allInhmap_Inh_EProductions :: (Map NontermIdent Attributes), allInitStates_Inh_EProductions :: (Map NontermIdent Int), allSynmap_Inh_EProductions :: (Map NontermIdent Attributes), allVisitKinds_Inh_EProductions :: (Map VisitIdentifier VisitKind), allstates_Inh_EProductions :: (Set StateIdentifier), avisitdefs_Inh_EProductions :: (Map VisitIdentifier (Set Identifier)), avisituses_Inh_EProductions :: (Map VisitIdentifier (Set Identifier)), importBlocks_Inh_EProductions :: ([String]), inhmap_Inh_EProductions :: (Attributes), initial_Inh_EProductions :: (StateIdentifier), localAttrTypes_Inh_EProductions :: (Map ConstructorIdent (Map Identifier Type)), mainFile_Inh_EProductions :: (String), mainName_Inh_EProductions :: (String), moduleHeader_Inh_EProductions :: (String -> String -> String -> Bool -> String), nextVisits_Inh_EProductions :: (Map StateIdentifier StateCtx), nt_Inh_EProductions :: (NontermIdent), ntType_Inh_EProductions :: (Type), options_Inh_EProductions :: (Options), params_Inh_EProductions :: ([Identifier]), pragmaBlocks_Inh_EProductions :: (String), prevVisits_Inh_EProductions :: (Map StateIdentifier StateCtx), rename_Inh_EProductions :: (Bool), synmap_Inh_EProductions :: (Attributes), textBlocks_Inh_EProductions :: ([String]) }
data Syn_EProductions  = Syn_EProductions { allvisits_Syn_EProductions :: ([VisitStateState]), count_Syn_EProductions :: (Int), datatype_Syn_EProductions :: ([TH.Con]), errors_Syn_EProductions :: (Seq Error), fromToStates_Syn_EProductions :: (Map VisitIdentifier (Int,Int)), semFunBndDefs_Syn_EProductions :: (Seq (TH.Name, TH.Exp)), semFunBndTps_Syn_EProductions :: (Seq (TH.VarBangType)), sem_nt_Syn_EProductions :: ([TH.Clause]), visitKinds_Syn_EProductions :: (Map VisitIdentifier VisitKind), visitdefs_Syn_EProductions :: (Map VisitIdentifier (Set Identifier)), visituses_Syn_EProductions :: (Map VisitIdentifier (Set Identifier)) }
{-# INLINABLE wrap_EProductions #-}
wrap_EProductions :: T_EProductions  -> Inh_EProductions  -> (Syn_EProductions )
wrap_EProductions (T_EProductions act) (Inh_EProductions _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallstates _lhsIavisitdefs _lhsIavisituses _lhsIimportBlocks _lhsIinhmap _lhsIinitial _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInextVisits _lhsInt _lhsIntType _lhsIoptions _lhsIparams _lhsIpragmaBlocks _lhsIprevVisits _lhsIrename _lhsIsynmap _lhsItextBlocks) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg16 = T_EProductions_vIn16 _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallstates _lhsIavisitdefs _lhsIavisituses _lhsIimportBlocks _lhsIinhmap _lhsIinitial _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInextVisits _lhsInt _lhsIntType _lhsIoptions _lhsIparams _lhsIpragmaBlocks _lhsIprevVisits _lhsIrename _lhsIsynmap _lhsItextBlocks
        (T_EProductions_vOut16 _lhsOallvisits _lhsOcount _lhsOdatatype _lhsOerrors _lhsOfromToStates _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOsem_nt _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses) <- return (inv_EProductions_s17 sem arg16)
        return (Syn_EProductions _lhsOallvisits _lhsOcount _lhsOdatatype _lhsOerrors _lhsOfromToStates _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOsem_nt _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses)
   )

-- cata
{-# NOINLINE sem_EProductions #-}
sem_EProductions :: EProductions  -> T_EProductions 
sem_EProductions list = Prelude.foldr sem_EProductions_Cons sem_EProductions_Nil (Prelude.map sem_EProduction list)

-- semantic domain
newtype T_EProductions  = T_EProductions {
                                         attach_T_EProductions :: Identity (T_EProductions_s17 )
                                         }
newtype T_EProductions_s17  = C_EProductions_s17 {
                                                 inv_EProductions_s17 :: (T_EProductions_v16 )
                                                 }
data T_EProductions_s18  = C_EProductions_s18
type T_EProductions_v16  = (T_EProductions_vIn16 ) -> (T_EProductions_vOut16 )
data T_EProductions_vIn16  = T_EProductions_vIn16 (Map VisitIdentifier (Int,Int)) (Map NontermIdent Attributes) (Map NontermIdent Int) (Map NontermIdent Attributes) (Map VisitIdentifier VisitKind) (Set StateIdentifier) (Map VisitIdentifier (Set Identifier)) (Map VisitIdentifier (Set Identifier)) ([String]) (Attributes) (StateIdentifier) (Map ConstructorIdent (Map Identifier Type)) (String) (String) (String -> String -> String -> Bool -> String) (Map StateIdentifier StateCtx) (NontermIdent) (Type) (Options) ([Identifier]) (String) (Map StateIdentifier StateCtx) (Bool) (Attributes) ([String])
data T_EProductions_vOut16  = T_EProductions_vOut16 ([VisitStateState]) (Int) ([TH.Con]) (Seq Error) (Map VisitIdentifier (Int,Int)) (Seq (TH.Name, TH.Exp)) (Seq (TH.VarBangType)) ([TH.Clause]) (Map VisitIdentifier VisitKind) (Map VisitIdentifier (Set Identifier)) (Map VisitIdentifier (Set Identifier))
{-# NOINLINE sem_EProductions_Cons #-}
sem_EProductions_Cons :: T_EProduction  -> T_EProductions  -> T_EProductions 
sem_EProductions_Cons arg_hd_ arg_tl_ = T_EProductions (return st17) where
   {-# NOINLINE st17 #-}
   st17 = let
      v16 :: T_EProductions_v16 
      v16 = \ (T_EProductions_vIn16 _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallstates _lhsIavisitdefs _lhsIavisituses _lhsIimportBlocks _lhsIinhmap _lhsIinitial _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInextVisits _lhsInt _lhsIntType _lhsIoptions _lhsIparams _lhsIpragmaBlocks _lhsIprevVisits _lhsIrename _lhsIsynmap _lhsItextBlocks) -> ( let
         _hdX14 = Control.Monad.Identity.runIdentity (attach_T_EProduction (arg_hd_))
         _tlX17 = Control.Monad.Identity.runIdentity (attach_T_EProductions (arg_tl_))
         (T_EProduction_vOut13 _hdIallvisits _hdIcount _hdIdatatype _hdIerrors _hdIfromToStates _hdIsemFunBndDefs _hdIsemFunBndTps _hdIsem_nt _hdIvisitKinds _hdIvisitdefs _hdIvisituses) = inv_EProduction_s14 _hdX14 (T_EProduction_vIn13 _hdOallFromToStates _hdOallInhmap _hdOallInitStates _hdOallSynmap _hdOallVisitKinds _hdOallstates _hdOavisitdefs _hdOavisituses _hdOimportBlocks _hdOinhmap _hdOinitial _hdOlocalAttrTypes _hdOmainFile _hdOmainName _hdOmoduleHeader _hdOnextVisits _hdOnt _hdOntType _hdOoptions _hdOparams _hdOpragmaBlocks _hdOprevVisits _hdOrename _hdOsynmap _hdOtextBlocks)
         (T_EProductions_vOut16 _tlIallvisits _tlIcount _tlIdatatype _tlIerrors _tlIfromToStates _tlIsemFunBndDefs _tlIsemFunBndTps _tlIsem_nt _tlIvisitKinds _tlIvisitdefs _tlIvisituses) = inv_EProductions_s17 _tlX17 (T_EProductions_vIn16 _tlOallFromToStates _tlOallInhmap _tlOallInitStates _tlOallSynmap _tlOallVisitKinds _tlOallstates _tlOavisitdefs _tlOavisituses _tlOimportBlocks _tlOinhmap _tlOinitial _tlOlocalAttrTypes _tlOmainFile _tlOmainName _tlOmoduleHeader _tlOnextVisits _tlOnt _tlOntType _tlOoptions _tlOparams _tlOpragmaBlocks _tlOprevVisits _tlOrename _tlOsynmap _tlOtextBlocks)
         _lhsOallvisits :: [VisitStateState]
         _lhsOallvisits = rule254 _hdIallvisits
         _lhsOcount :: Int
         _lhsOcount = rule255 _hdIcount _tlIcount
         _lhsOdatatype :: [TH.Con]
         _lhsOdatatype = rule256 _hdIdatatype _tlIdatatype
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule257 _hdIerrors _tlIerrors
         _lhsOfromToStates :: Map VisitIdentifier (Int,Int)
         _lhsOfromToStates = rule258 _hdIfromToStates _tlIfromToStates
         _lhsOsemFunBndDefs :: Seq (TH.Name, TH.Exp)
         _lhsOsemFunBndDefs = rule259 _hdIsemFunBndDefs _tlIsemFunBndDefs
         _lhsOsemFunBndTps :: Seq (TH.VarBangType)
         _lhsOsemFunBndTps = rule260 _hdIsemFunBndTps _tlIsemFunBndTps
         _lhsOsem_nt :: [TH.Clause]
         _lhsOsem_nt = rule261 _hdIsem_nt _tlIsem_nt
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule262 _hdIvisitKinds _tlIvisitKinds
         _lhsOvisitdefs :: Map VisitIdentifier (Set Identifier)
         _lhsOvisitdefs = rule263 _hdIvisitdefs _tlIvisitdefs
         _lhsOvisituses :: Map VisitIdentifier (Set Identifier)
         _lhsOvisituses = rule264 _hdIvisituses _tlIvisituses
         _hdOallFromToStates = rule265 _lhsIallFromToStates
         _hdOallInhmap = rule266 _lhsIallInhmap
         _hdOallInitStates = rule267 _lhsIallInitStates
         _hdOallSynmap = rule268 _lhsIallSynmap
         _hdOallVisitKinds = rule269 _lhsIallVisitKinds
         _hdOallstates = rule270 _lhsIallstates
         _hdOavisitdefs = rule271 _lhsIavisitdefs
         _hdOavisituses = rule272 _lhsIavisituses
         _hdOimportBlocks = rule273 _lhsIimportBlocks
         _hdOinhmap = rule274 _lhsIinhmap
         _hdOinitial = rule275 _lhsIinitial
         _hdOlocalAttrTypes = rule276 _lhsIlocalAttrTypes
         _hdOmainFile = rule277 _lhsImainFile
         _hdOmainName = rule278 _lhsImainName
         _hdOmoduleHeader = rule279 _lhsImoduleHeader
         _hdOnextVisits = rule280 _lhsInextVisits
         _hdOnt = rule281 _lhsInt
         _hdOntType = rule282 _lhsIntType
         _hdOoptions = rule283 _lhsIoptions
         _hdOparams = rule284 _lhsIparams
         _hdOpragmaBlocks = rule285 _lhsIpragmaBlocks
         _hdOprevVisits = rule286 _lhsIprevVisits
         _hdOrename = rule287 _lhsIrename
         _hdOsynmap = rule288 _lhsIsynmap
         _hdOtextBlocks = rule289 _lhsItextBlocks
         _tlOallFromToStates = rule290 _lhsIallFromToStates
         _tlOallInhmap = rule291 _lhsIallInhmap
         _tlOallInitStates = rule292 _lhsIallInitStates
         _tlOallSynmap = rule293 _lhsIallSynmap
         _tlOallVisitKinds = rule294 _lhsIallVisitKinds
         _tlOallstates = rule295 _lhsIallstates
         _tlOavisitdefs = rule296 _lhsIavisitdefs
         _tlOavisituses = rule297 _lhsIavisituses
         _tlOimportBlocks = rule298 _lhsIimportBlocks
         _tlOinhmap = rule299 _lhsIinhmap
         _tlOinitial = rule300 _lhsIinitial
         _tlOlocalAttrTypes = rule301 _lhsIlocalAttrTypes
         _tlOmainFile = rule302 _lhsImainFile
         _tlOmainName = rule303 _lhsImainName
         _tlOmoduleHeader = rule304 _lhsImoduleHeader
         _tlOnextVisits = rule305 _lhsInextVisits
         _tlOnt = rule306 _lhsInt
         _tlOntType = rule307 _lhsIntType
         _tlOoptions = rule308 _lhsIoptions
         _tlOparams = rule309 _lhsIparams
         _tlOpragmaBlocks = rule310 _lhsIpragmaBlocks
         _tlOprevVisits = rule311 _lhsIprevVisits
         _tlOrename = rule312 _lhsIrename
         _tlOsynmap = rule313 _lhsIsynmap
         _tlOtextBlocks = rule314 _lhsItextBlocks
         __result_ = T_EProductions_vOut16 _lhsOallvisits _lhsOcount _lhsOdatatype _lhsOerrors _lhsOfromToStates _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOsem_nt _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses
         in __result_ )
     in C_EProductions_s17 v16
   {-# INLINE rule254 #-}
   {-# LINE 391 "src-ag/ExecutionPlan2TH.ag" #-}
   rule254 = \ ((_hdIallvisits) :: [VisitStateState]) ->
                           {-# LINE 391 "src-ag/ExecutionPlan2TH.ag" #-}
                           _hdIallvisits
                           {-# LINE 2113 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule255 #-}
   rule255 = \ ((_hdIcount) :: Int) ((_tlIcount) :: Int) ->
     _hdIcount + _tlIcount
   {-# INLINE rule256 #-}
   rule256 = \ ((_hdIdatatype) :: TH.Con) ((_tlIdatatype) :: [TH.Con]) ->
     _hdIdatatype : _tlIdatatype
   {-# INLINE rule257 #-}
   rule257 = \ ((_hdIerrors) :: Seq Error) ((_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# INLINE rule258 #-}
   rule258 = \ ((_hdIfromToStates) :: Map VisitIdentifier (Int,Int)) ((_tlIfromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _hdIfromToStates `mappend` _tlIfromToStates
   {-# INLINE rule259 #-}
   rule259 = \ ((_hdIsemFunBndDefs) :: Seq (TH.Name, TH.Exp)) ((_tlIsemFunBndDefs) :: Seq (TH.Name, TH.Exp)) ->
     _hdIsemFunBndDefs Seq.>< _tlIsemFunBndDefs
   {-# INLINE rule260 #-}
   rule260 = \ ((_hdIsemFunBndTps) :: Seq (TH.VarBangType)) ((_tlIsemFunBndTps) :: Seq (TH.VarBangType)) ->
     _hdIsemFunBndTps Seq.>< _tlIsemFunBndTps
   {-# INLINE rule261 #-}
   rule261 = \ ((_hdIsem_nt) :: [TH.Clause]) ((_tlIsem_nt) :: [TH.Clause]) ->
     _hdIsem_nt ++ _tlIsem_nt
   {-# INLINE rule262 #-}
   rule262 = \ ((_hdIvisitKinds) :: Map VisitIdentifier VisitKind) ((_tlIvisitKinds) :: Map VisitIdentifier VisitKind) ->
     _hdIvisitKinds `mappend` _tlIvisitKinds
   {-# INLINE rule263 #-}
   rule263 = \ ((_hdIvisitdefs) :: Map VisitIdentifier (Set Identifier)) ((_tlIvisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _hdIvisitdefs `uwSetUnion` _tlIvisitdefs
   {-# INLINE rule264 #-}
   rule264 = \ ((_hdIvisituses) :: Map VisitIdentifier (Set Identifier)) ((_tlIvisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _hdIvisituses `uwSetUnion` _tlIvisituses
   {-# INLINE rule265 #-}
   rule265 = \ ((_lhsIallFromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _lhsIallFromToStates
   {-# INLINE rule266 #-}
   rule266 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule267 #-}
   rule267 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule268 #-}
   rule268 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule269 #-}
   rule269 = \ ((_lhsIallVisitKinds) :: Map VisitIdentifier VisitKind) ->
     _lhsIallVisitKinds
   {-# INLINE rule270 #-}
   rule270 = \ ((_lhsIallstates) :: Set StateIdentifier) ->
     _lhsIallstates
   {-# INLINE rule271 #-}
   rule271 = \ ((_lhsIavisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisitdefs
   {-# INLINE rule272 #-}
   rule272 = \ ((_lhsIavisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisituses
   {-# INLINE rule273 #-}
   rule273 = \ ((_lhsIimportBlocks) :: [String]) ->
     _lhsIimportBlocks
   {-# INLINE rule274 #-}
   rule274 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule275 #-}
   rule275 = \ ((_lhsIinitial) :: StateIdentifier) ->
     _lhsIinitial
   {-# INLINE rule276 #-}
   rule276 = \ ((_lhsIlocalAttrTypes) :: Map ConstructorIdent (Map Identifier Type)) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule277 #-}
   rule277 = \ ((_lhsImainFile) :: String) ->
     _lhsImainFile
   {-# INLINE rule278 #-}
   rule278 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule279 #-}
   rule279 = \ ((_lhsImoduleHeader) :: String -> String -> String -> Bool -> String) ->
     _lhsImoduleHeader
   {-# INLINE rule280 #-}
   rule280 = \ ((_lhsInextVisits) :: Map StateIdentifier StateCtx) ->
     _lhsInextVisits
   {-# INLINE rule281 #-}
   rule281 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule282 #-}
   rule282 = \ ((_lhsIntType) :: Type) ->
     _lhsIntType
   {-# INLINE rule283 #-}
   rule283 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule284 #-}
   rule284 = \ ((_lhsIparams) :: [Identifier]) ->
     _lhsIparams
   {-# INLINE rule285 #-}
   rule285 = \ ((_lhsIpragmaBlocks) :: String) ->
     _lhsIpragmaBlocks
   {-# INLINE rule286 #-}
   rule286 = \ ((_lhsIprevVisits) :: Map StateIdentifier StateCtx) ->
     _lhsIprevVisits
   {-# INLINE rule287 #-}
   rule287 = \ ((_lhsIrename) :: Bool) ->
     _lhsIrename
   {-# INLINE rule288 #-}
   rule288 = \ ((_lhsIsynmap) :: Attributes) ->
     _lhsIsynmap
   {-# INLINE rule289 #-}
   rule289 = \ ((_lhsItextBlocks) :: [String]) ->
     _lhsItextBlocks
   {-# INLINE rule290 #-}
   rule290 = \ ((_lhsIallFromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _lhsIallFromToStates
   {-# INLINE rule291 #-}
   rule291 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule292 #-}
   rule292 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule293 #-}
   rule293 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule294 #-}
   rule294 = \ ((_lhsIallVisitKinds) :: Map VisitIdentifier VisitKind) ->
     _lhsIallVisitKinds
   {-# INLINE rule295 #-}
   rule295 = \ ((_lhsIallstates) :: Set StateIdentifier) ->
     _lhsIallstates
   {-# INLINE rule296 #-}
   rule296 = \ ((_lhsIavisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisitdefs
   {-# INLINE rule297 #-}
   rule297 = \ ((_lhsIavisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisituses
   {-# INLINE rule298 #-}
   rule298 = \ ((_lhsIimportBlocks) :: [String]) ->
     _lhsIimportBlocks
   {-# INLINE rule299 #-}
   rule299 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule300 #-}
   rule300 = \ ((_lhsIinitial) :: StateIdentifier) ->
     _lhsIinitial
   {-# INLINE rule301 #-}
   rule301 = \ ((_lhsIlocalAttrTypes) :: Map ConstructorIdent (Map Identifier Type)) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule302 #-}
   rule302 = \ ((_lhsImainFile) :: String) ->
     _lhsImainFile
   {-# INLINE rule303 #-}
   rule303 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule304 #-}
   rule304 = \ ((_lhsImoduleHeader) :: String -> String -> String -> Bool -> String) ->
     _lhsImoduleHeader
   {-# INLINE rule305 #-}
   rule305 = \ ((_lhsInextVisits) :: Map StateIdentifier StateCtx) ->
     _lhsInextVisits
   {-# INLINE rule306 #-}
   rule306 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule307 #-}
   rule307 = \ ((_lhsIntType) :: Type) ->
     _lhsIntType
   {-# INLINE rule308 #-}
   rule308 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule309 #-}
   rule309 = \ ((_lhsIparams) :: [Identifier]) ->
     _lhsIparams
   {-# INLINE rule310 #-}
   rule310 = \ ((_lhsIpragmaBlocks) :: String) ->
     _lhsIpragmaBlocks
   {-# INLINE rule311 #-}
   rule311 = \ ((_lhsIprevVisits) :: Map StateIdentifier StateCtx) ->
     _lhsIprevVisits
   {-# INLINE rule312 #-}
   rule312 = \ ((_lhsIrename) :: Bool) ->
     _lhsIrename
   {-# INLINE rule313 #-}
   rule313 = \ ((_lhsIsynmap) :: Attributes) ->
     _lhsIsynmap
   {-# INLINE rule314 #-}
   rule314 = \ ((_lhsItextBlocks) :: [String]) ->
     _lhsItextBlocks
{-# NOINLINE sem_EProductions_Nil #-}
sem_EProductions_Nil ::  T_EProductions 
sem_EProductions_Nil  = T_EProductions (return st17) where
   {-# NOINLINE st17 #-}
   st17 = let
      v16 :: T_EProductions_v16 
      v16 = \ (T_EProductions_vIn16 _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallstates _lhsIavisitdefs _lhsIavisituses _lhsIimportBlocks _lhsIinhmap _lhsIinitial _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInextVisits _lhsInt _lhsIntType _lhsIoptions _lhsIparams _lhsIpragmaBlocks _lhsIprevVisits _lhsIrename _lhsIsynmap _lhsItextBlocks) -> ( let
         _lhsOallvisits :: [VisitStateState]
         _lhsOallvisits = rule315  ()
         _lhsOcount :: Int
         _lhsOcount = rule316  ()
         _lhsOdatatype :: [TH.Con]
         _lhsOdatatype = rule317  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule318  ()
         _lhsOfromToStates :: Map VisitIdentifier (Int,Int)
         _lhsOfromToStates = rule319  ()
         _lhsOsemFunBndDefs :: Seq (TH.Name, TH.Exp)
         _lhsOsemFunBndDefs = rule320  ()
         _lhsOsemFunBndTps :: Seq (TH.VarBangType)
         _lhsOsemFunBndTps = rule321  ()
         _lhsOsem_nt :: [TH.Clause]
         _lhsOsem_nt = rule322  ()
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule323  ()
         _lhsOvisitdefs :: Map VisitIdentifier (Set Identifier)
         _lhsOvisitdefs = rule324  ()
         _lhsOvisituses :: Map VisitIdentifier (Set Identifier)
         _lhsOvisituses = rule325  ()
         __result_ = T_EProductions_vOut16 _lhsOallvisits _lhsOcount _lhsOdatatype _lhsOerrors _lhsOfromToStates _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOsem_nt _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses
         in __result_ )
     in C_EProductions_s17 v16
   {-# INLINE rule315 #-}
   {-# LINE 392 "src-ag/ExecutionPlan2TH.ag" #-}
   rule315 = \  (_ :: ()) ->
                           {-# LINE 392 "src-ag/ExecutionPlan2TH.ag" #-}
                           error "Every nonterminal should have at least 1 production"
                           {-# LINE 2331 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule316 #-}
   rule316 = \  (_ :: ()) ->
     0
   {-# INLINE rule317 #-}
   rule317 = \  (_ :: ()) ->
     []
   {-# INLINE rule318 #-}
   rule318 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule319 #-}
   rule319 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule320 #-}
   rule320 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule321 #-}
   rule321 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule322 #-}
   rule322 = \  (_ :: ()) ->
     []
   {-# INLINE rule323 #-}
   rule323 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule324 #-}
   rule324 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule325 #-}
   rule325 = \  (_ :: ()) ->
     Map.empty

-- ERule -------------------------------------------------------
-- wrapper
data Inh_ERule  = Inh_ERule { allInhmap_Inh_ERule :: (Map NontermIdent Attributes), allSynmap_Inh_ERule :: (Map NontermIdent Attributes), childTypes_Inh_ERule :: (Map Identifier Type), con_Inh_ERule :: (ConstructorIdent), importBlocks_Inh_ERule :: ([String]), inhmap_Inh_ERule :: (Attributes), lazyIntras_Inh_ERule :: (Set String), localAttrTypes_Inh_ERule :: (Map Identifier Type), mainFile_Inh_ERule :: (String), mainName_Inh_ERule :: (String), moduleHeader_Inh_ERule :: (String -> String -> String -> Bool -> String), nt_Inh_ERule :: (NontermIdent), options_Inh_ERule :: (Options), pragmaBlocks_Inh_ERule :: (String), ruleKinds_Inh_ERule :: (Map Identifier (Set VisitKind)), synmap_Inh_ERule :: (Attributes), textBlocks_Inh_ERule :: ([String]), usageInfo_Inh_ERule :: (Map Identifier Int) }
data Syn_ERule  = Syn_ERule { errors_Syn_ERule :: (Seq Error), ruledefs_Syn_ERule :: (Map Identifier (Set String)), ruleuses_Syn_ERule :: (Map Identifier (Map String (Maybe NonLocalAttr))), usedArgs_Syn_ERule :: (Set String) }
{-# INLINABLE wrap_ERule #-}
wrap_ERule :: T_ERule  -> Inh_ERule  -> (Syn_ERule )
wrap_ERule (T_ERule act) (Inh_ERule _lhsIallInhmap _lhsIallSynmap _lhsIchildTypes _lhsIcon _lhsIimportBlocks _lhsIinhmap _lhsIlazyIntras _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInt _lhsIoptions _lhsIpragmaBlocks _lhsIruleKinds _lhsIsynmap _lhsItextBlocks _lhsIusageInfo) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg19 = T_ERule_vIn19 _lhsIallInhmap _lhsIallSynmap _lhsIchildTypes _lhsIcon _lhsIimportBlocks _lhsIinhmap _lhsIlazyIntras _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInt _lhsIoptions _lhsIpragmaBlocks _lhsIruleKinds _lhsIsynmap _lhsItextBlocks _lhsIusageInfo
        (T_ERule_vOut19 _lhsOerrors _lhsOruledefs _lhsOruleuses _lhsOusedArgs) <- return (inv_ERule_s20 sem arg19)
        return (Syn_ERule _lhsOerrors _lhsOruledefs _lhsOruleuses _lhsOusedArgs)
   )

-- cata
{-# INLINE sem_ERule #-}
sem_ERule :: ERule  -> T_ERule 
sem_ERule ( ERule name_ pattern_ rhs_ owrt_ origin_ explicit_ pure_ mbError_ ) = sem_ERule_ERule name_ ( sem_Pattern pattern_ ) ( sem_Expression rhs_ ) owrt_ origin_ explicit_ pure_ mbError_

-- semantic domain
newtype T_ERule  = T_ERule {
                           attach_T_ERule :: Identity (T_ERule_s20 )
                           }
newtype T_ERule_s20  = C_ERule_s20 {
                                   inv_ERule_s20 :: (T_ERule_v19 )
                                   }
data T_ERule_s21  = C_ERule_s21
type T_ERule_v19  = (T_ERule_vIn19 ) -> (T_ERule_vOut19 )
data T_ERule_vIn19  = T_ERule_vIn19 (Map NontermIdent Attributes) (Map NontermIdent Attributes) (Map Identifier Type) (ConstructorIdent) ([String]) (Attributes) (Set String) (Map Identifier Type) (String) (String) (String -> String -> String -> Bool -> String) (NontermIdent) (Options) (String) (Map Identifier (Set VisitKind)) (Attributes) ([String]) (Map Identifier Int)
data T_ERule_vOut19  = T_ERule_vOut19 (Seq Error) (Map Identifier (Set String)) (Map Identifier (Map String (Maybe NonLocalAttr))) (Set String)
{-# NOINLINE sem_ERule_ERule #-}
sem_ERule_ERule :: (Identifier) -> T_Pattern  -> T_Expression  -> (Bool) -> (String) -> (Bool) -> (Bool) -> (Maybe Error) -> T_ERule 
sem_ERule_ERule arg_name_ arg_pattern_ arg_rhs_ _ _ _ _ arg_mbError_ = T_ERule (return st20) where
   {-# NOINLINE st20 #-}
   st20 = let
      v19 :: T_ERule_v19 
      v19 = \ (T_ERule_vIn19 _lhsIallInhmap _lhsIallSynmap _lhsIchildTypes _lhsIcon _lhsIimportBlocks _lhsIinhmap _lhsIlazyIntras _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInt _lhsIoptions _lhsIpragmaBlocks _lhsIruleKinds _lhsIsynmap _lhsItextBlocks _lhsIusageInfo) -> ( let
         _patternX41 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pattern_))
         _rhsX29 = Control.Monad.Identity.runIdentity (attach_T_Expression (arg_rhs_))
         (T_Pattern_vOut40 _patternIattrs _patternIcopy _patternIisUnderscore) = inv_Pattern_s41 _patternX41 (T_Pattern_vIn40 _patternOallInhmap _patternOallSynmap _patternOanyLazyKind _patternOinhmap _patternOlocalAttrTypes _patternOoptions _patternOsynmap)
         (T_Expression_vOut28 _rhsIattrs _rhsIpos _rhsItks) = inv_Expression_s29 _rhsX29 (T_Expression_vIn28 _rhsOoptions)
         _used = rule326 _lhsIusageInfo arg_name_
         _kinds = rule327 _lhsIruleKinds arg_name_
         _anyLazyKind = rule328 _kinds
         _addbang = rule329 _lhsIoptions
         _addbang1 = rule330 _addbang _anyLazyKind
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule331 _used arg_mbError_
         _lhsOruledefs :: Map Identifier (Set String)
         _lhsOruledefs = rule332  ()
         _lhsOruleuses :: Map Identifier (Map String (Maybe NonLocalAttr))
         _lhsOruleuses = rule333  ()
         _lhsOusedArgs :: Set String
         _lhsOusedArgs = rule334  ()
         _patternOallInhmap = rule335 _lhsIallInhmap
         _patternOallSynmap = rule336 _lhsIallSynmap
         _patternOanyLazyKind = rule337 _anyLazyKind
         _patternOinhmap = rule338 _lhsIinhmap
         _patternOlocalAttrTypes = rule339 _lhsIlocalAttrTypes
         _patternOoptions = rule340 _lhsIoptions
         _patternOsynmap = rule341 _lhsIsynmap
         _rhsOoptions = rule342 _lhsIoptions
         __result_ = T_ERule_vOut19 _lhsOerrors _lhsOruledefs _lhsOruleuses _lhsOusedArgs
         in __result_ )
     in C_ERule_s20 v19
   {-# INLINE rule326 #-}
   {-# LINE 1334 "src-ag/ExecutionPlan2TH.ag" #-}
   rule326 = \ ((_lhsIusageInfo) :: Map Identifier Int) name_ ->
                                                 {-# LINE 1334 "src-ag/ExecutionPlan2TH.ag" #-}
                                                 Map.findWithDefault 0 name_ _lhsIusageInfo
                                                 {-# LINE 2433 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule327 #-}
   {-# LINE 1350 "src-ag/ExecutionPlan2TH.ag" #-}
   rule327 = \ ((_lhsIruleKinds) :: Map Identifier (Set VisitKind)) name_ ->
                {-# LINE 1350 "src-ag/ExecutionPlan2TH.ag" #-}
                Map.findWithDefault Set.empty name_ _lhsIruleKinds
                {-# LINE 2439 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule328 #-}
   {-# LINE 1351 "src-ag/ExecutionPlan2TH.ag" #-}
   rule328 = \ _kinds ->
                      {-# LINE 1351 "src-ag/ExecutionPlan2TH.ag" #-}
                      Set.fold (\k r -> isLazyKind k || r) False _kinds
                      {-# LINE 2445 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule329 #-}
   {-# LINE 1600 "src-ag/ExecutionPlan2TH.ag" #-}
   rule329 = \ ((_lhsIoptions) :: Options) ->
                                                    {-# LINE 1600 "src-ag/ExecutionPlan2TH.ag" #-}
                                                    \x -> if bangpats _lhsIoptions then TH.BangP x else x
                                                    {-# LINE 2451 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule330 #-}
   {-# LINE 1611 "src-ag/ExecutionPlan2TH.ag" #-}
   rule330 = \ _addbang _anyLazyKind ->
                                                     {-# LINE 1611 "src-ag/ExecutionPlan2TH.ag" #-}
                                                     if _anyLazyKind     then id else _addbang
                                                     {-# LINE 2457 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule331 #-}
   {-# LINE 1717 "src-ag/ExecutionPlan2TH.ag" #-}
   rule331 = \ _used mbError_ ->
                 {-# LINE 1717 "src-ag/ExecutionPlan2TH.ag" #-}
                 case mbError_ of
                   Just e | _used     > 0 -> Seq.singleton e
                   _                      -> Seq.empty
                 {-# LINE 2465 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule332 #-}
   rule332 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule333 #-}
   rule333 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule334 #-}
   rule334 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule335 #-}
   rule335 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule336 #-}
   rule336 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule337 #-}
   rule337 = \ _anyLazyKind ->
     _anyLazyKind
   {-# INLINE rule338 #-}
   rule338 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule339 #-}
   rule339 = \ ((_lhsIlocalAttrTypes) :: Map Identifier Type) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule340 #-}
   rule340 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule341 #-}
   rule341 = \ ((_lhsIsynmap) :: Attributes) ->
     _lhsIsynmap
   {-# INLINE rule342 #-}
   rule342 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions

-- ERules ------------------------------------------------------
-- wrapper
data Inh_ERules  = Inh_ERules { allInhmap_Inh_ERules :: (Map NontermIdent Attributes), allSynmap_Inh_ERules :: (Map NontermIdent Attributes), childTypes_Inh_ERules :: (Map Identifier Type), con_Inh_ERules :: (ConstructorIdent), importBlocks_Inh_ERules :: ([String]), inhmap_Inh_ERules :: (Attributes), lazyIntras_Inh_ERules :: (Set String), localAttrTypes_Inh_ERules :: (Map Identifier Type), mainFile_Inh_ERules :: (String), mainName_Inh_ERules :: (String), moduleHeader_Inh_ERules :: (String -> String -> String -> Bool -> String), nt_Inh_ERules :: (NontermIdent), options_Inh_ERules :: (Options), pragmaBlocks_Inh_ERules :: (String), ruleKinds_Inh_ERules :: (Map Identifier (Set VisitKind)), synmap_Inh_ERules :: (Attributes), textBlocks_Inh_ERules :: ([String]), usageInfo_Inh_ERules :: (Map Identifier Int) }
data Syn_ERules  = Syn_ERules { errors_Syn_ERules :: (Seq Error), ruledefs_Syn_ERules :: (Map Identifier (Set String)), ruleuses_Syn_ERules :: (Map Identifier (Map String (Maybe NonLocalAttr))), usedArgs_Syn_ERules :: (Set String) }
{-# INLINABLE wrap_ERules #-}
wrap_ERules :: T_ERules  -> Inh_ERules  -> (Syn_ERules )
wrap_ERules (T_ERules act) (Inh_ERules _lhsIallInhmap _lhsIallSynmap _lhsIchildTypes _lhsIcon _lhsIimportBlocks _lhsIinhmap _lhsIlazyIntras _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInt _lhsIoptions _lhsIpragmaBlocks _lhsIruleKinds _lhsIsynmap _lhsItextBlocks _lhsIusageInfo) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg22 = T_ERules_vIn22 _lhsIallInhmap _lhsIallSynmap _lhsIchildTypes _lhsIcon _lhsIimportBlocks _lhsIinhmap _lhsIlazyIntras _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInt _lhsIoptions _lhsIpragmaBlocks _lhsIruleKinds _lhsIsynmap _lhsItextBlocks _lhsIusageInfo
        (T_ERules_vOut22 _lhsOerrors _lhsOruledefs _lhsOruleuses _lhsOusedArgs) <- return (inv_ERules_s23 sem arg22)
        return (Syn_ERules _lhsOerrors _lhsOruledefs _lhsOruleuses _lhsOusedArgs)
   )

-- cata
{-# NOINLINE sem_ERules #-}
sem_ERules :: ERules  -> T_ERules 
sem_ERules list = Prelude.foldr sem_ERules_Cons sem_ERules_Nil (Prelude.map sem_ERule list)

-- semantic domain
newtype T_ERules  = T_ERules {
                             attach_T_ERules :: Identity (T_ERules_s23 )
                             }
newtype T_ERules_s23  = C_ERules_s23 {
                                     inv_ERules_s23 :: (T_ERules_v22 )
                                     }
data T_ERules_s24  = C_ERules_s24
type T_ERules_v22  = (T_ERules_vIn22 ) -> (T_ERules_vOut22 )
data T_ERules_vIn22  = T_ERules_vIn22 (Map NontermIdent Attributes) (Map NontermIdent Attributes) (Map Identifier Type) (ConstructorIdent) ([String]) (Attributes) (Set String) (Map Identifier Type) (String) (String) (String -> String -> String -> Bool -> String) (NontermIdent) (Options) (String) (Map Identifier (Set VisitKind)) (Attributes) ([String]) (Map Identifier Int)
data T_ERules_vOut22  = T_ERules_vOut22 (Seq Error) (Map Identifier (Set String)) (Map Identifier (Map String (Maybe NonLocalAttr))) (Set String)
{-# NOINLINE sem_ERules_Cons #-}
sem_ERules_Cons :: T_ERule  -> T_ERules  -> T_ERules 
sem_ERules_Cons arg_hd_ arg_tl_ = T_ERules (return st23) where
   {-# NOINLINE st23 #-}
   st23 = let
      v22 :: T_ERules_v22 
      v22 = \ (T_ERules_vIn22 _lhsIallInhmap _lhsIallSynmap _lhsIchildTypes _lhsIcon _lhsIimportBlocks _lhsIinhmap _lhsIlazyIntras _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInt _lhsIoptions _lhsIpragmaBlocks _lhsIruleKinds _lhsIsynmap _lhsItextBlocks _lhsIusageInfo) -> ( let
         _hdX20 = Control.Monad.Identity.runIdentity (attach_T_ERule (arg_hd_))
         _tlX23 = Control.Monad.Identity.runIdentity (attach_T_ERules (arg_tl_))
         (T_ERule_vOut19 _hdIerrors _hdIruledefs _hdIruleuses _hdIusedArgs) = inv_ERule_s20 _hdX20 (T_ERule_vIn19 _hdOallInhmap _hdOallSynmap _hdOchildTypes _hdOcon _hdOimportBlocks _hdOinhmap _hdOlazyIntras _hdOlocalAttrTypes _hdOmainFile _hdOmainName _hdOmoduleHeader _hdOnt _hdOoptions _hdOpragmaBlocks _hdOruleKinds _hdOsynmap _hdOtextBlocks _hdOusageInfo)
         (T_ERules_vOut22 _tlIerrors _tlIruledefs _tlIruleuses _tlIusedArgs) = inv_ERules_s23 _tlX23 (T_ERules_vIn22 _tlOallInhmap _tlOallSynmap _tlOchildTypes _tlOcon _tlOimportBlocks _tlOinhmap _tlOlazyIntras _tlOlocalAttrTypes _tlOmainFile _tlOmainName _tlOmoduleHeader _tlOnt _tlOoptions _tlOpragmaBlocks _tlOruleKinds _tlOsynmap _tlOtextBlocks _tlOusageInfo)
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule343 _hdIerrors _tlIerrors
         _lhsOruledefs :: Map Identifier (Set String)
         _lhsOruledefs = rule344 _hdIruledefs _tlIruledefs
         _lhsOruleuses :: Map Identifier (Map String (Maybe NonLocalAttr))
         _lhsOruleuses = rule345 _hdIruleuses _tlIruleuses
         _lhsOusedArgs :: Set String
         _lhsOusedArgs = rule346 _hdIusedArgs _tlIusedArgs
         _hdOallInhmap = rule347 _lhsIallInhmap
         _hdOallSynmap = rule348 _lhsIallSynmap
         _hdOchildTypes = rule349 _lhsIchildTypes
         _hdOcon = rule350 _lhsIcon
         _hdOimportBlocks = rule351 _lhsIimportBlocks
         _hdOinhmap = rule352 _lhsIinhmap
         _hdOlazyIntras = rule353 _lhsIlazyIntras
         _hdOlocalAttrTypes = rule354 _lhsIlocalAttrTypes
         _hdOmainFile = rule355 _lhsImainFile
         _hdOmainName = rule356 _lhsImainName
         _hdOmoduleHeader = rule357 _lhsImoduleHeader
         _hdOnt = rule358 _lhsInt
         _hdOoptions = rule359 _lhsIoptions
         _hdOpragmaBlocks = rule360 _lhsIpragmaBlocks
         _hdOruleKinds = rule361 _lhsIruleKinds
         _hdOsynmap = rule362 _lhsIsynmap
         _hdOtextBlocks = rule363 _lhsItextBlocks
         _hdOusageInfo = rule364 _lhsIusageInfo
         _tlOallInhmap = rule365 _lhsIallInhmap
         _tlOallSynmap = rule366 _lhsIallSynmap
         _tlOchildTypes = rule367 _lhsIchildTypes
         _tlOcon = rule368 _lhsIcon
         _tlOimportBlocks = rule369 _lhsIimportBlocks
         _tlOinhmap = rule370 _lhsIinhmap
         _tlOlazyIntras = rule371 _lhsIlazyIntras
         _tlOlocalAttrTypes = rule372 _lhsIlocalAttrTypes
         _tlOmainFile = rule373 _lhsImainFile
         _tlOmainName = rule374 _lhsImainName
         _tlOmoduleHeader = rule375 _lhsImoduleHeader
         _tlOnt = rule376 _lhsInt
         _tlOoptions = rule377 _lhsIoptions
         _tlOpragmaBlocks = rule378 _lhsIpragmaBlocks
         _tlOruleKinds = rule379 _lhsIruleKinds
         _tlOsynmap = rule380 _lhsIsynmap
         _tlOtextBlocks = rule381 _lhsItextBlocks
         _tlOusageInfo = rule382 _lhsIusageInfo
         __result_ = T_ERules_vOut22 _lhsOerrors _lhsOruledefs _lhsOruleuses _lhsOusedArgs
         in __result_ )
     in C_ERules_s23 v22
   {-# INLINE rule343 #-}
   rule343 = \ ((_hdIerrors) :: Seq Error) ((_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# INLINE rule344 #-}
   rule344 = \ ((_hdIruledefs) :: Map Identifier (Set String)) ((_tlIruledefs) :: Map Identifier (Set String)) ->
     _hdIruledefs `uwSetUnion` _tlIruledefs
   {-# INLINE rule345 #-}
   rule345 = \ ((_hdIruleuses) :: Map Identifier (Map String (Maybe NonLocalAttr))) ((_tlIruleuses) :: Map Identifier (Map String (Maybe NonLocalAttr))) ->
     _hdIruleuses `uwMapUnion` _tlIruleuses
   {-# INLINE rule346 #-}
   rule346 = \ ((_hdIusedArgs) :: Set String) ((_tlIusedArgs) :: Set String) ->
     _hdIusedArgs `Set.union` _tlIusedArgs
   {-# INLINE rule347 #-}
   rule347 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule348 #-}
   rule348 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule349 #-}
   rule349 = \ ((_lhsIchildTypes) :: Map Identifier Type) ->
     _lhsIchildTypes
   {-# INLINE rule350 #-}
   rule350 = \ ((_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# INLINE rule351 #-}
   rule351 = \ ((_lhsIimportBlocks) :: [String]) ->
     _lhsIimportBlocks
   {-# INLINE rule352 #-}
   rule352 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule353 #-}
   rule353 = \ ((_lhsIlazyIntras) :: Set String) ->
     _lhsIlazyIntras
   {-# INLINE rule354 #-}
   rule354 = \ ((_lhsIlocalAttrTypes) :: Map Identifier Type) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule355 #-}
   rule355 = \ ((_lhsImainFile) :: String) ->
     _lhsImainFile
   {-# INLINE rule356 #-}
   rule356 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule357 #-}
   rule357 = \ ((_lhsImoduleHeader) :: String -> String -> String -> Bool -> String) ->
     _lhsImoduleHeader
   {-# INLINE rule358 #-}
   rule358 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule359 #-}
   rule359 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule360 #-}
   rule360 = \ ((_lhsIpragmaBlocks) :: String) ->
     _lhsIpragmaBlocks
   {-# INLINE rule361 #-}
   rule361 = \ ((_lhsIruleKinds) :: Map Identifier (Set VisitKind)) ->
     _lhsIruleKinds
   {-# INLINE rule362 #-}
   rule362 = \ ((_lhsIsynmap) :: Attributes) ->
     _lhsIsynmap
   {-# INLINE rule363 #-}
   rule363 = \ ((_lhsItextBlocks) :: [String]) ->
     _lhsItextBlocks
   {-# INLINE rule364 #-}
   rule364 = \ ((_lhsIusageInfo) :: Map Identifier Int) ->
     _lhsIusageInfo
   {-# INLINE rule365 #-}
   rule365 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule366 #-}
   rule366 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule367 #-}
   rule367 = \ ((_lhsIchildTypes) :: Map Identifier Type) ->
     _lhsIchildTypes
   {-# INLINE rule368 #-}
   rule368 = \ ((_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# INLINE rule369 #-}
   rule369 = \ ((_lhsIimportBlocks) :: [String]) ->
     _lhsIimportBlocks
   {-# INLINE rule370 #-}
   rule370 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule371 #-}
   rule371 = \ ((_lhsIlazyIntras) :: Set String) ->
     _lhsIlazyIntras
   {-# INLINE rule372 #-}
   rule372 = \ ((_lhsIlocalAttrTypes) :: Map Identifier Type) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule373 #-}
   rule373 = \ ((_lhsImainFile) :: String) ->
     _lhsImainFile
   {-# INLINE rule374 #-}
   rule374 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule375 #-}
   rule375 = \ ((_lhsImoduleHeader) :: String -> String -> String -> Bool -> String) ->
     _lhsImoduleHeader
   {-# INLINE rule376 #-}
   rule376 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule377 #-}
   rule377 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule378 #-}
   rule378 = \ ((_lhsIpragmaBlocks) :: String) ->
     _lhsIpragmaBlocks
   {-# INLINE rule379 #-}
   rule379 = \ ((_lhsIruleKinds) :: Map Identifier (Set VisitKind)) ->
     _lhsIruleKinds
   {-# INLINE rule380 #-}
   rule380 = \ ((_lhsIsynmap) :: Attributes) ->
     _lhsIsynmap
   {-# INLINE rule381 #-}
   rule381 = \ ((_lhsItextBlocks) :: [String]) ->
     _lhsItextBlocks
   {-# INLINE rule382 #-}
   rule382 = \ ((_lhsIusageInfo) :: Map Identifier Int) ->
     _lhsIusageInfo
{-# NOINLINE sem_ERules_Nil #-}
sem_ERules_Nil ::  T_ERules 
sem_ERules_Nil  = T_ERules (return st23) where
   {-# NOINLINE st23 #-}
   st23 = let
      v22 :: T_ERules_v22 
      v22 = \ (T_ERules_vIn22 _lhsIallInhmap _lhsIallSynmap _lhsIchildTypes _lhsIcon _lhsIimportBlocks _lhsIinhmap _lhsIlazyIntras _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInt _lhsIoptions _lhsIpragmaBlocks _lhsIruleKinds _lhsIsynmap _lhsItextBlocks _lhsIusageInfo) -> ( let
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule383  ()
         _lhsOruledefs :: Map Identifier (Set String)
         _lhsOruledefs = rule384  ()
         _lhsOruleuses :: Map Identifier (Map String (Maybe NonLocalAttr))
         _lhsOruleuses = rule385  ()
         _lhsOusedArgs :: Set String
         _lhsOusedArgs = rule386  ()
         __result_ = T_ERules_vOut22 _lhsOerrors _lhsOruledefs _lhsOruleuses _lhsOusedArgs
         in __result_ )
     in C_ERules_s23 v22
   {-# INLINE rule383 #-}
   rule383 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule384 #-}
   rule384 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule385 #-}
   rule385 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule386 #-}
   rule386 = \  (_ :: ()) ->
     Set.empty

-- ExecutionPlan -----------------------------------------------
-- wrapper
data Inh_ExecutionPlan  = Inh_ExecutionPlan { importBlocks_Inh_ExecutionPlan :: ([String]), inhmap_Inh_ExecutionPlan :: (Map NontermIdent Attributes), localAttrTypes_Inh_ExecutionPlan :: (Map NontermIdent (Map ConstructorIdent (Map Identifier Type))), mainFile_Inh_ExecutionPlan :: (String), mainName_Inh_ExecutionPlan :: (String), moduleHeader_Inh_ExecutionPlan :: (String -> String -> String -> Bool -> String), options_Inh_ExecutionPlan :: (Options), pragmaBlocks_Inh_ExecutionPlan :: (String), synmap_Inh_ExecutionPlan :: (Map NontermIdent Attributes), textBlocks_Inh_ExecutionPlan :: ([String]) }
data Syn_ExecutionPlan  = Syn_ExecutionPlan { errors_Syn_ExecutionPlan :: (Seq Error), output_Syn_ExecutionPlan :: ([TH.Dec]) }
{-# INLINABLE wrap_ExecutionPlan #-}
wrap_ExecutionPlan :: T_ExecutionPlan  -> Inh_ExecutionPlan  -> (Syn_ExecutionPlan )
wrap_ExecutionPlan (T_ExecutionPlan act) (Inh_ExecutionPlan _lhsIimportBlocks _lhsIinhmap _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsIoptions _lhsIpragmaBlocks _lhsIsynmap _lhsItextBlocks) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg25 = T_ExecutionPlan_vIn25 _lhsIimportBlocks _lhsIinhmap _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsIoptions _lhsIpragmaBlocks _lhsIsynmap _lhsItextBlocks
        (T_ExecutionPlan_vOut25 _lhsOerrors _lhsOoutput) <- return (inv_ExecutionPlan_s26 sem arg25)
        return (Syn_ExecutionPlan _lhsOerrors _lhsOoutput)
   )

-- cata
{-# INLINE sem_ExecutionPlan #-}
sem_ExecutionPlan :: ExecutionPlan  -> T_ExecutionPlan 
sem_ExecutionPlan ( ExecutionPlan nonts_ typeSyns_ wrappers_ derivings_ ) = sem_ExecutionPlan_ExecutionPlan ( sem_ENonterminals nonts_ ) typeSyns_ wrappers_ derivings_

-- semantic domain
newtype T_ExecutionPlan  = T_ExecutionPlan {
                                           attach_T_ExecutionPlan :: Identity (T_ExecutionPlan_s26 )
                                           }
newtype T_ExecutionPlan_s26  = C_ExecutionPlan_s26 {
                                                   inv_ExecutionPlan_s26 :: (T_ExecutionPlan_v25 )
                                                   }
data T_ExecutionPlan_s27  = C_ExecutionPlan_s27
type T_ExecutionPlan_v25  = (T_ExecutionPlan_vIn25 ) -> (T_ExecutionPlan_vOut25 )
data T_ExecutionPlan_vIn25  = T_ExecutionPlan_vIn25 ([String]) (Map NontermIdent Attributes) (Map NontermIdent (Map ConstructorIdent (Map Identifier Type))) (String) (String) (String -> String -> String -> Bool -> String) (Options) (String) (Map NontermIdent Attributes) ([String])
data T_ExecutionPlan_vOut25  = T_ExecutionPlan_vOut25 (Seq Error) ([TH.Dec])
{-# NOINLINE sem_ExecutionPlan_ExecutionPlan #-}
sem_ExecutionPlan_ExecutionPlan :: T_ENonterminals  -> (TypeSyns) -> (Set NontermIdent) -> (Derivings) -> T_ExecutionPlan 
sem_ExecutionPlan_ExecutionPlan arg_nonts_ arg_typeSyns_ arg_wrappers_ arg_derivings_ = T_ExecutionPlan (return st26) where
   {-# NOINLINE st26 #-}
   st26 = let
      v25 :: T_ExecutionPlan_v25 
      v25 = \ (T_ExecutionPlan_vIn25 _lhsIimportBlocks _lhsIinhmap _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsIoptions _lhsIpragmaBlocks _lhsIsynmap _lhsItextBlocks) -> ( let
         _nontsX11 = Control.Monad.Identity.runIdentity (attach_T_ENonterminals (arg_nonts_))
         (T_ENonterminals_vOut10 _nontsIerrors _nontsIfromToStates _nontsIinitStates _nontsIoutput _nontsIsemFunBndDefs _nontsIsemFunBndTps _nontsIvisitKinds _nontsIvisitdefs _nontsIvisituses) = inv_ENonterminals_s11 _nontsX11 (T_ENonterminals_vIn10 _nontsOallFromToStates _nontsOallInitStates _nontsOallVisitKinds _nontsOavisitdefs _nontsOavisituses _nontsOderivings _nontsOimportBlocks _nontsOinhmap _nontsOlocalAttrTypes _nontsOmainFile _nontsOmainName _nontsOmoduleHeader _nontsOoptions _nontsOpragmaBlocks _nontsOsynmap _nontsOtextBlocks _nontsOtypeSyns _nontsOwrappers)
         _lhsOoutput :: [TH.Dec]
         _lhsOoutput = rule387 _commonExtra _nontsIoutput _wrappersExtra
         _nontsOwrappers = rule388 arg_wrappers_
         _nontsOtypeSyns = rule389 arg_typeSyns_
         _nontsOderivings = rule390 arg_derivings_
         _wrappersExtra = rule391 _lateSemBndDef _lhsIoptions
         _commonExtra = rule392 _lateSemBndTp _lhsIoptions
         _lateSemBndTp = rule393 _lhsImainName _nontsIsemFunBndTps
         _lateSemBndDef = rule394 _lhsImainName _lhsIoptions _nontsIsemFunBndDefs arg_wrappers_
         _nontsOallFromToStates = rule395 _nontsIfromToStates
         _nontsOallVisitKinds = rule396 _nontsIvisitKinds
         _nontsOallInitStates = rule397 _nontsIinitStates
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule398 _nontsIerrors
         _nontsOavisitdefs = rule399  ()
         _nontsOavisituses = rule400  ()
         _nontsOimportBlocks = rule401 _lhsIimportBlocks
         _nontsOinhmap = rule402 _lhsIinhmap
         _nontsOlocalAttrTypes = rule403 _lhsIlocalAttrTypes
         _nontsOmainFile = rule404 _lhsImainFile
         _nontsOmainName = rule405 _lhsImainName
         _nontsOmoduleHeader = rule406 _lhsImoduleHeader
         _nontsOoptions = rule407 _lhsIoptions
         _nontsOpragmaBlocks = rule408 _lhsIpragmaBlocks
         _nontsOsynmap = rule409 _lhsIsynmap
         _nontsOtextBlocks = rule410 _lhsItextBlocks
         __result_ = T_ExecutionPlan_vOut25 _lhsOerrors _lhsOoutput
         in __result_ )
     in C_ExecutionPlan_s26 v25
   {-# INLINE rule387 #-}
   {-# LINE 96 "src-ag/ExecutionPlan2TH.ag" #-}
   rule387 = \ _commonExtra ((_nontsIoutput) :: [TH.Dec]) _wrappersExtra ->
                                 {-# LINE 96 "src-ag/ExecutionPlan2TH.ag" #-}
                                 _nontsIoutput ++ _commonExtra     ++ _wrappersExtra
                                 {-# LINE 2812 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule388 #-}
   {-# LINE 102 "src-ag/ExecutionPlan2TH.ag" #-}
   rule388 = \ wrappers_ ->
                                     {-# LINE 102 "src-ag/ExecutionPlan2TH.ag" #-}
                                     wrappers_
                                     {-# LINE 2818 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule389 #-}
   {-# LINE 136 "src-ag/ExecutionPlan2TH.ag" #-}
   rule389 = \ typeSyns_ ->
                                     {-# LINE 136 "src-ag/ExecutionPlan2TH.ag" #-}
                                     typeSyns_
                                     {-# LINE 2824 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule390 #-}
   {-# LINE 137 "src-ag/ExecutionPlan2TH.ag" #-}
   rule390 = \ derivings_ ->
                                      {-# LINE 137 "src-ag/ExecutionPlan2TH.ag" #-}
                                      derivings_
                                      {-# LINE 2830 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule391 #-}
   {-# LINE 590 "src-ag/ExecutionPlan2TH.ag" #-}
   rule391 = \ _lateSemBndDef ((_lhsIoptions) :: Options) ->
                        {-# LINE 590 "src-ag/ExecutionPlan2TH.ag" #-}
                        if lateHigherOrderBinding _lhsIoptions
                        then _lateSemBndDef
                        else []
                        {-# LINE 2838 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule392 #-}
   {-# LINE 593 "src-ag/ExecutionPlan2TH.ag" #-}
   rule392 = \ _lateSemBndTp ((_lhsIoptions) :: Options) ->
                        {-# LINE 593 "src-ag/ExecutionPlan2TH.ag" #-}
                        if lateHigherOrderBinding _lhsIoptions
                        then _lateSemBndTp
                        else []
                        {-# LINE 2846 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule393 #-}
   {-# LINE 596 "src-ag/ExecutionPlan2TH.ag" #-}
   rule393 = \ ((_lhsImainName) :: String) ((_nontsIsemFunBndTps) :: Seq (TH.VarBangType)) ->
                       {-# LINE 596 "src-ag/ExecutionPlan2TH.ag" #-}
                       [TH.DataD [] (TH.mkName (lateBindingTypeNm _lhsImainName)) [] Nothing [TH.RecC (TH.mkName (lateBindingTypeNm _lhsImainName)) (toList _nontsIsemFunBndTps)] []]
                       {-# LINE 2852 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule394 #-}
   {-# LINE 598 "src-ag/ExecutionPlan2TH.ag" #-}
   rule394 = \ ((_lhsImainName) :: String) ((_lhsIoptions) :: Options) ((_nontsIsemFunBndDefs) :: Seq (TH.Name, TH.Exp)) wrappers_ ->
        {-# LINE 598 "src-ag/ExecutionPlan2TH.ag" #-}
        ( if noInlinePragmas _lhsIoptions
          then empty
          else if helpInlining _lhsIoptions && Set.size wrappers_ == 1
            then pure $ inlineTH $ TH.mkName (lateBindingFieldNm _lhsImainName)
            else pure $ noInlineTH $ TH.mkName (lateBindingFieldNm _lhsImainName)
        )
        ++ [ TH.SigD (TH.mkName (lateBindingFieldNm _lhsImainName))
                     (TH.ConT (TH.mkName (lateBindingTypeNm _lhsImainName)))
           , TH.ValD (TH.VarP (TH.mkName (lateBindingFieldNm _lhsImainName)))
                     (TH.NormalB (TH.RecConE (TH.mkName (lateBindingTypeNm _lhsImainName))
                                             (toList _nontsIsemFunBndDefs)))
                     []
           ]
        {-# LINE 2870 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule395 #-}
   {-# LINE 1639 "src-ag/ExecutionPlan2TH.ag" #-}
   rule395 = \ ((_nontsIfromToStates) :: Map VisitIdentifier (Int,Int)) ->
                            {-# LINE 1639 "src-ag/ExecutionPlan2TH.ag" #-}
                            _nontsIfromToStates
                            {-# LINE 2876 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule396 #-}
   {-# LINE 1683 "src-ag/ExecutionPlan2TH.ag" #-}
   rule396 = \ ((_nontsIvisitKinds) :: Map VisitIdentifier VisitKind) ->
                          {-# LINE 1683 "src-ag/ExecutionPlan2TH.ag" #-}
                          _nontsIvisitKinds
                          {-# LINE 2882 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule397 #-}
   {-# LINE 1697 "src-ag/ExecutionPlan2TH.ag" #-}
   rule397 = \ ((_nontsIinitStates) :: Map NontermIdent Int) ->
                          {-# LINE 1697 "src-ag/ExecutionPlan2TH.ag" #-}
                          _nontsIinitStates
                          {-# LINE 2888 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule398 #-}
   rule398 = \ ((_nontsIerrors) :: Seq Error) ->
     _nontsIerrors
   {-# INLINE rule399 #-}
   rule399 = \  (_ :: ()) ->
     error "missing rule: ExecutionPlan.ExecutionPlan.nonts.avisitdefs"
   {-# INLINE rule400 #-}
   rule400 = \  (_ :: ()) ->
     error "missing rule: ExecutionPlan.ExecutionPlan.nonts.avisituses"
   {-# INLINE rule401 #-}
   rule401 = \ ((_lhsIimportBlocks) :: [String]) ->
     _lhsIimportBlocks
   {-# INLINE rule402 #-}
   rule402 = \ ((_lhsIinhmap) :: Map NontermIdent Attributes) ->
     _lhsIinhmap
   {-# INLINE rule403 #-}
   rule403 = \ ((_lhsIlocalAttrTypes) :: Map NontermIdent (Map ConstructorIdent (Map Identifier Type))) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule404 #-}
   rule404 = \ ((_lhsImainFile) :: String) ->
     _lhsImainFile
   {-# INLINE rule405 #-}
   rule405 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule406 #-}
   rule406 = \ ((_lhsImoduleHeader) :: String -> String -> String -> Bool -> String) ->
     _lhsImoduleHeader
   {-# INLINE rule407 #-}
   rule407 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule408 #-}
   rule408 = \ ((_lhsIpragmaBlocks) :: String) ->
     _lhsIpragmaBlocks
   {-# INLINE rule409 #-}
   rule409 = \ ((_lhsIsynmap) :: Map NontermIdent Attributes) ->
     _lhsIsynmap
   {-# INLINE rule410 #-}
   rule410 = \ ((_lhsItextBlocks) :: [String]) ->
     _lhsItextBlocks

-- Expression --------------------------------------------------
-- wrapper
data Inh_Expression  = Inh_Expression { options_Inh_Expression :: (Options) }
data Syn_Expression  = Syn_Expression { attrs_Syn_Expression :: (Map String (Maybe NonLocalAttr)), pos_Syn_Expression :: (Pos), tks_Syn_Expression :: ([HsToken]) }
{-# INLINABLE wrap_Expression #-}
wrap_Expression :: T_Expression  -> Inh_Expression  -> (Syn_Expression )
wrap_Expression (T_Expression act) (Inh_Expression _lhsIoptions) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg28 = T_Expression_vIn28 _lhsIoptions
        (T_Expression_vOut28 _lhsOattrs _lhsOpos _lhsOtks) <- return (inv_Expression_s29 sem arg28)
        return (Syn_Expression _lhsOattrs _lhsOpos _lhsOtks)
   )

-- cata
{-# INLINE sem_Expression #-}
sem_Expression :: Expression  -> T_Expression 
sem_Expression ( Expression pos_ tks_ ) = sem_Expression_Expression pos_ tks_

-- semantic domain
newtype T_Expression  = T_Expression {
                                     attach_T_Expression :: Identity (T_Expression_s29 )
                                     }
newtype T_Expression_s29  = C_Expression_s29 {
                                             inv_Expression_s29 :: (T_Expression_v28 )
                                             }
data T_Expression_s30  = C_Expression_s30
type T_Expression_v28  = (T_Expression_vIn28 ) -> (T_Expression_vOut28 )
data T_Expression_vIn28  = T_Expression_vIn28 (Options)
data T_Expression_vOut28  = T_Expression_vOut28 (Map String (Maybe NonLocalAttr)) (Pos) ([HsToken])
{-# NOINLINE sem_Expression_Expression #-}
sem_Expression_Expression :: (Pos) -> ([HsToken]) -> T_Expression 
sem_Expression_Expression arg_pos_ arg_tks_ = T_Expression (return st29) where
   {-# NOINLINE st29 #-}
   st29 = let
      v28 :: T_Expression_v28 
      v28 = \ (T_Expression_vIn28 _lhsIoptions) -> ( let
         _lhsOattrs :: Map String (Maybe NonLocalAttr)
         _lhsOattrs = rule411  ()
         _lhsOpos :: Pos
         _lhsOpos = rule412 arg_pos_
         _lhsOtks :: [HsToken]
         _lhsOtks = rule413 arg_tks_
         __result_ = T_Expression_vOut28 _lhsOattrs _lhsOpos _lhsOtks
         in __result_ )
     in C_Expression_s29 v28
   {-# INLINE rule411 #-}
   rule411 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule412 #-}
   rule412 = \ pos_ ->
     pos_
   {-# INLINE rule413 #-}
   rule413 = \ tks_ ->
     tks_

-- HsToken -----------------------------------------------------
-- wrapper
data Inh_HsToken  = Inh_HsToken { options_Inh_HsToken :: (Options) }
data Syn_HsToken  = Syn_HsToken { attrs_Syn_HsToken :: (Map String (Maybe NonLocalAttr)), tok_Syn_HsToken :: ((Pos,String)) }
{-# INLINABLE wrap_HsToken #-}
wrap_HsToken :: T_HsToken  -> Inh_HsToken  -> (Syn_HsToken )
wrap_HsToken (T_HsToken act) (Inh_HsToken _lhsIoptions) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg31 = T_HsToken_vIn31 _lhsIoptions
        (T_HsToken_vOut31 _lhsOattrs _lhsOtok) <- return (inv_HsToken_s32 sem arg31)
        return (Syn_HsToken _lhsOattrs _lhsOtok)
   )

-- cata
{-# NOINLINE sem_HsToken #-}
sem_HsToken :: HsToken  -> T_HsToken 
sem_HsToken ( AGLocal var_ pos_ rdesc_ ) = sem_HsToken_AGLocal var_ pos_ rdesc_
sem_HsToken ( AGField field_ attr_ pos_ rdesc_ ) = sem_HsToken_AGField field_ attr_ pos_ rdesc_
sem_HsToken ( HsToken value_ pos_ ) = sem_HsToken_HsToken value_ pos_
sem_HsToken ( CharToken value_ pos_ ) = sem_HsToken_CharToken value_ pos_
sem_HsToken ( StrToken value_ pos_ ) = sem_HsToken_StrToken value_ pos_
sem_HsToken ( Err mesg_ pos_ ) = sem_HsToken_Err mesg_ pos_

-- semantic domain
newtype T_HsToken  = T_HsToken {
                               attach_T_HsToken :: Identity (T_HsToken_s32 )
                               }
newtype T_HsToken_s32  = C_HsToken_s32 {
                                       inv_HsToken_s32 :: (T_HsToken_v31 )
                                       }
data T_HsToken_s33  = C_HsToken_s33
type T_HsToken_v31  = (T_HsToken_vIn31 ) -> (T_HsToken_vOut31 )
data T_HsToken_vIn31  = T_HsToken_vIn31 (Options)
data T_HsToken_vOut31  = T_HsToken_vOut31 (Map String (Maybe NonLocalAttr)) ((Pos,String))
{-# NOINLINE sem_HsToken_AGLocal #-}
sem_HsToken_AGLocal :: (Identifier) -> (Pos) -> (Maybe String) -> T_HsToken 
sem_HsToken_AGLocal arg_var_ arg_pos_ _ = T_HsToken (return st32) where
   {-# NOINLINE st32 #-}
   st32 = let
      v31 :: T_HsToken_v31 
      v31 = \ (T_HsToken_vIn31 _lhsIoptions) -> ( let
         _tok = rule414 arg_pos_ arg_var_
         _lhsOattrs :: Map String (Maybe NonLocalAttr)
         _lhsOattrs = rule415  ()
         _lhsOtok :: (Pos,String)
         _lhsOtok = rule416 _tok
         __result_ = T_HsToken_vOut31 _lhsOattrs _lhsOtok
         in __result_ )
     in C_HsToken_s32 v31
   {-# INLINE rule414 #-}
   {-# LINE 1467 "src-ag/ExecutionPlan2TH.ag" #-}
   rule414 = \ pos_ var_ ->
                          {-# LINE 1467 "src-ag/ExecutionPlan2TH.ag" #-}
                          (pos_,fieldname var_)
                          {-# LINE 3040 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule415 #-}
   rule415 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule416 #-}
   rule416 = \ _tok ->
     _tok
{-# NOINLINE sem_HsToken_AGField #-}
sem_HsToken_AGField :: (Identifier) -> (Identifier) -> (Pos) -> (Maybe String) -> T_HsToken 
sem_HsToken_AGField arg_field_ arg_attr_ arg_pos_ arg_rdesc_ = T_HsToken (return st32) where
   {-# NOINLINE st32 #-}
   st32 = let
      v31 :: T_HsToken_v31 
      v31 = \ (T_HsToken_vIn31 _lhsIoptions) -> ( let
         _addTrace = rule417 arg_attr_ arg_field_ arg_rdesc_
         _lhsOtok :: (Pos,String)
         _lhsOtok = rule418 _addTrace _lhsIoptions arg_attr_ arg_field_ arg_pos_
         _lhsOattrs :: Map String (Maybe NonLocalAttr)
         _lhsOattrs = rule419  ()
         __result_ = T_HsToken_vOut31 _lhsOattrs _lhsOtok
         in __result_ )
     in C_HsToken_s32 v31
   {-# INLINE rule417 #-}
   {-# LINE 1471 "src-ag/ExecutionPlan2TH.ag" #-}
   rule417 = \ attr_ field_ rdesc_ ->
                        {-# LINE 1471 "src-ag/ExecutionPlan2TH.ag" #-}
                        case rdesc_ of
                          Just d  -> \x -> "(trace " ++ show (d ++ " -> " ++ show field_ ++ "." ++ show attr_) ++ " (" ++ x ++ "))"
                          Nothing -> id
                        {-# LINE 3069 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule418 #-}
   {-# LINE 1474 "src-ag/ExecutionPlan2TH.ag" #-}
   rule418 = \ _addTrace ((_lhsIoptions) :: Options) attr_ field_ pos_ ->
                   {-# LINE 1474 "src-ag/ExecutionPlan2TH.ag" #-}
                   (pos_, _addTrace     $ attrname _lhsIoptions True field_ attr_)
                   {-# LINE 3075 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule419 #-}
   rule419 = \  (_ :: ()) ->
     Map.empty
{-# NOINLINE sem_HsToken_HsToken #-}
sem_HsToken_HsToken :: (String) -> (Pos) -> T_HsToken 
sem_HsToken_HsToken arg_value_ arg_pos_ = T_HsToken (return st32) where
   {-# NOINLINE st32 #-}
   st32 = let
      v31 :: T_HsToken_v31 
      v31 = \ (T_HsToken_vIn31 _lhsIoptions) -> ( let
         _lhsOtok :: (Pos,String)
         _lhsOtok = rule420 arg_pos_ arg_value_
         _lhsOattrs :: Map String (Maybe NonLocalAttr)
         _lhsOattrs = rule421  ()
         __result_ = T_HsToken_vOut31 _lhsOattrs _lhsOtok
         in __result_ )
     in C_HsToken_s32 v31
   {-# INLINE rule420 #-}
   {-# LINE 1476 "src-ag/ExecutionPlan2TH.ag" #-}
   rule420 = \ pos_ value_ ->
                         {-# LINE 1476 "src-ag/ExecutionPlan2TH.ag" #-}
                         (pos_, value_)
                         {-# LINE 3098 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule421 #-}
   rule421 = \  (_ :: ()) ->
     Map.empty
{-# NOINLINE sem_HsToken_CharToken #-}
sem_HsToken_CharToken :: (String) -> (Pos) -> T_HsToken 
sem_HsToken_CharToken arg_value_ arg_pos_ = T_HsToken (return st32) where
   {-# NOINLINE st32 #-}
   st32 = let
      v31 :: T_HsToken_v31 
      v31 = \ (T_HsToken_vIn31 _lhsIoptions) -> ( let
         _lhsOtok :: (Pos,String)
         _lhsOtok = rule422 arg_pos_ arg_value_
         _lhsOattrs :: Map String (Maybe NonLocalAttr)
         _lhsOattrs = rule423  ()
         __result_ = T_HsToken_vOut31 _lhsOattrs _lhsOtok
         in __result_ )
     in C_HsToken_s32 v31
   {-# INLINE rule422 #-}
   {-# LINE 1478 "src-ag/ExecutionPlan2TH.ag" #-}
   rule422 = \ pos_ value_ ->
                           {-# LINE 1478 "src-ag/ExecutionPlan2TH.ag" #-}
                           (pos_, if null value_
                                     then ""
                                     else showCharShort (head value_)
                           )
                           {-# LINE 3124 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule423 #-}
   rule423 = \  (_ :: ()) ->
     Map.empty
{-# NOINLINE sem_HsToken_StrToken #-}
sem_HsToken_StrToken :: (String) -> (Pos) -> T_HsToken 
sem_HsToken_StrToken arg_value_ arg_pos_ = T_HsToken (return st32) where
   {-# NOINLINE st32 #-}
   st32 = let
      v31 :: T_HsToken_v31 
      v31 = \ (T_HsToken_vIn31 _lhsIoptions) -> ( let
         _lhsOtok :: (Pos,String)
         _lhsOtok = rule424 arg_pos_ arg_value_
         _lhsOattrs :: Map String (Maybe NonLocalAttr)
         _lhsOattrs = rule425  ()
         __result_ = T_HsToken_vOut31 _lhsOattrs _lhsOtok
         in __result_ )
     in C_HsToken_s32 v31
   {-# INLINE rule424 #-}
   {-# LINE 1483 "src-ag/ExecutionPlan2TH.ag" #-}
   rule424 = \ pos_ value_ ->
                           {-# LINE 1483 "src-ag/ExecutionPlan2TH.ag" #-}
                           (pos_, showStrShort value_)
                           {-# LINE 3147 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule425 #-}
   rule425 = \  (_ :: ()) ->
     Map.empty
{-# NOINLINE sem_HsToken_Err #-}
sem_HsToken_Err :: (String) -> (Pos) -> T_HsToken 
sem_HsToken_Err _ arg_pos_ = T_HsToken (return st32) where
   {-# NOINLINE st32 #-}
   st32 = let
      v31 :: T_HsToken_v31 
      v31 = \ (T_HsToken_vIn31 _lhsIoptions) -> ( let
         _lhsOtok :: (Pos,String)
         _lhsOtok = rule426 arg_pos_
         _lhsOattrs :: Map String (Maybe NonLocalAttr)
         _lhsOattrs = rule427  ()
         __result_ = T_HsToken_vOut31 _lhsOattrs _lhsOtok
         in __result_ )
     in C_HsToken_s32 v31
   {-# INLINE rule426 #-}
   {-# LINE 1484 "src-ag/ExecutionPlan2TH.ag" #-}
   rule426 = \ pos_ ->
                           {-# LINE 1484 "src-ag/ExecutionPlan2TH.ag" #-}
                           (pos_, "")
                           {-# LINE 3170 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule427 #-}
   rule427 = \  (_ :: ()) ->
     Map.empty

-- HsTokens ----------------------------------------------------
-- wrapper
data Inh_HsTokens  = Inh_HsTokens { options_Inh_HsTokens :: (Options) }
data Syn_HsTokens  = Syn_HsTokens { tks_Syn_HsTokens :: ([(Pos,String)]) }
{-# INLINABLE wrap_HsTokens #-}
wrap_HsTokens :: T_HsTokens  -> Inh_HsTokens  -> (Syn_HsTokens )
wrap_HsTokens (T_HsTokens act) (Inh_HsTokens _lhsIoptions) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg34 = T_HsTokens_vIn34 _lhsIoptions
        (T_HsTokens_vOut34 _lhsOtks) <- return (inv_HsTokens_s35 sem arg34)
        return (Syn_HsTokens _lhsOtks)
   )

-- cata
{-# NOINLINE sem_HsTokens #-}
sem_HsTokens :: HsTokens  -> T_HsTokens 
sem_HsTokens list = Prelude.foldr sem_HsTokens_Cons sem_HsTokens_Nil (Prelude.map sem_HsToken list)

-- semantic domain
newtype T_HsTokens  = T_HsTokens {
                                 attach_T_HsTokens :: Identity (T_HsTokens_s35 )
                                 }
newtype T_HsTokens_s35  = C_HsTokens_s35 {
                                         inv_HsTokens_s35 :: (T_HsTokens_v34 )
                                         }
data T_HsTokens_s36  = C_HsTokens_s36
type T_HsTokens_v34  = (T_HsTokens_vIn34 ) -> (T_HsTokens_vOut34 )
data T_HsTokens_vIn34  = T_HsTokens_vIn34 (Options)
data T_HsTokens_vOut34  = T_HsTokens_vOut34 ([(Pos,String)])
{-# NOINLINE sem_HsTokens_Cons #-}
sem_HsTokens_Cons :: T_HsToken  -> T_HsTokens  -> T_HsTokens 
sem_HsTokens_Cons arg_hd_ arg_tl_ = T_HsTokens (return st35) where
   {-# NOINLINE st35 #-}
   st35 = let
      v34 :: T_HsTokens_v34 
      v34 = \ (T_HsTokens_vIn34 _lhsIoptions) -> ( let
         _hdX32 = Control.Monad.Identity.runIdentity (attach_T_HsToken (arg_hd_))
         _tlX35 = Control.Monad.Identity.runIdentity (attach_T_HsTokens (arg_tl_))
         (T_HsToken_vOut31 _hdIattrs _hdItok) = inv_HsToken_s32 _hdX32 (T_HsToken_vIn31 _hdOoptions)
         (T_HsTokens_vOut34 _tlItks) = inv_HsTokens_s35 _tlX35 (T_HsTokens_vIn34 _tlOoptions)
         _lhsOtks :: [(Pos,String)]
         _lhsOtks = rule428 _hdItok _tlItks
         _hdOoptions = rule429 _lhsIoptions
         _tlOoptions = rule430 _lhsIoptions
         __result_ = T_HsTokens_vOut34 _lhsOtks
         in __result_ )
     in C_HsTokens_s35 v34
   {-# INLINE rule428 #-}
   {-# LINE 1463 "src-ag/ExecutionPlan2TH.ag" #-}
   rule428 = \ ((_hdItok) :: (Pos,String)) ((_tlItks) :: [(Pos,String)]) ->
                     {-# LINE 1463 "src-ag/ExecutionPlan2TH.ag" #-}
                     _hdItok : _tlItks
                     {-# LINE 3228 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule429 #-}
   rule429 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule430 #-}
   rule430 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_HsTokens_Nil #-}
sem_HsTokens_Nil ::  T_HsTokens 
sem_HsTokens_Nil  = T_HsTokens (return st35) where
   {-# NOINLINE st35 #-}
   st35 = let
      v34 :: T_HsTokens_v34 
      v34 = \ (T_HsTokens_vIn34 _lhsIoptions) -> ( let
         _lhsOtks :: [(Pos,String)]
         _lhsOtks = rule431  ()
         __result_ = T_HsTokens_vOut34 _lhsOtks
         in __result_ )
     in C_HsTokens_s35 v34
   {-# INLINE rule431 #-}
   {-# LINE 1464 "src-ag/ExecutionPlan2TH.ag" #-}
   rule431 = \  (_ :: ()) ->
                     {-# LINE 1464 "src-ag/ExecutionPlan2TH.ag" #-}
                     []
                     {-# LINE 3252 "src-generated/ExecutionPlan2TH.hs" #-}

-- HsTokensRoot ------------------------------------------------
-- wrapper
data Inh_HsTokensRoot  = Inh_HsTokensRoot { options_Inh_HsTokensRoot :: (Options) }
data Syn_HsTokensRoot  = Syn_HsTokensRoot {  }
{-# INLINABLE wrap_HsTokensRoot #-}
wrap_HsTokensRoot :: T_HsTokensRoot  -> Inh_HsTokensRoot  -> (Syn_HsTokensRoot )
wrap_HsTokensRoot (T_HsTokensRoot act) (Inh_HsTokensRoot _lhsIoptions) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg37 = T_HsTokensRoot_vIn37 _lhsIoptions
        (T_HsTokensRoot_vOut37 ) <- return (inv_HsTokensRoot_s38 sem arg37)
        return (Syn_HsTokensRoot )
   )

-- cata
{-# INLINE sem_HsTokensRoot #-}
sem_HsTokensRoot :: HsTokensRoot  -> T_HsTokensRoot 
sem_HsTokensRoot ( HsTokensRoot tokens_ ) = sem_HsTokensRoot_HsTokensRoot ( sem_HsTokens tokens_ )

-- semantic domain
newtype T_HsTokensRoot  = T_HsTokensRoot {
                                         attach_T_HsTokensRoot :: Identity (T_HsTokensRoot_s38 )
                                         }
newtype T_HsTokensRoot_s38  = C_HsTokensRoot_s38 {
                                                 inv_HsTokensRoot_s38 :: (T_HsTokensRoot_v37 )
                                                 }
data T_HsTokensRoot_s39  = C_HsTokensRoot_s39
type T_HsTokensRoot_v37  = (T_HsTokensRoot_vIn37 ) -> (T_HsTokensRoot_vOut37 )
data T_HsTokensRoot_vIn37  = T_HsTokensRoot_vIn37 (Options)
data T_HsTokensRoot_vOut37  = T_HsTokensRoot_vOut37 
{-# NOINLINE sem_HsTokensRoot_HsTokensRoot #-}
sem_HsTokensRoot_HsTokensRoot :: T_HsTokens  -> T_HsTokensRoot 
sem_HsTokensRoot_HsTokensRoot arg_tokens_ = T_HsTokensRoot (return st38) where
   {-# NOINLINE st38 #-}
   st38 = let
      v37 :: T_HsTokensRoot_v37 
      v37 = \ (T_HsTokensRoot_vIn37 _lhsIoptions) -> ( let
         _tokensX35 = Control.Monad.Identity.runIdentity (attach_T_HsTokens (arg_tokens_))
         (T_HsTokens_vOut34 _tokensItks) = inv_HsTokens_s35 _tokensX35 (T_HsTokens_vIn34 _tokensOoptions)
         _tokensOoptions = rule432 _lhsIoptions
         __result_ = T_HsTokensRoot_vOut37 
         in __result_ )
     in C_HsTokensRoot_s38 v37
   {-# INLINE rule432 #-}
   rule432 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions

-- Pattern -----------------------------------------------------
-- wrapper
data Inh_Pattern  = Inh_Pattern { allInhmap_Inh_Pattern :: (Map NontermIdent Attributes), allSynmap_Inh_Pattern :: (Map NontermIdent Attributes), anyLazyKind_Inh_Pattern :: (Bool), inhmap_Inh_Pattern :: (Attributes), localAttrTypes_Inh_Pattern :: (Map Identifier Type), options_Inh_Pattern :: (Options), synmap_Inh_Pattern :: (Attributes) }
data Syn_Pattern  = Syn_Pattern { attrs_Syn_Pattern :: (Set String), copy_Syn_Pattern :: (Pattern), isUnderscore_Syn_Pattern :: (Bool) }
{-# INLINABLE wrap_Pattern #-}
wrap_Pattern :: T_Pattern  -> Inh_Pattern  -> (Syn_Pattern )
wrap_Pattern (T_Pattern act) (Inh_Pattern _lhsIallInhmap _lhsIallSynmap _lhsIanyLazyKind _lhsIinhmap _lhsIlocalAttrTypes _lhsIoptions _lhsIsynmap) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg40 = T_Pattern_vIn40 _lhsIallInhmap _lhsIallSynmap _lhsIanyLazyKind _lhsIinhmap _lhsIlocalAttrTypes _lhsIoptions _lhsIsynmap
        (T_Pattern_vOut40 _lhsOattrs _lhsOcopy _lhsOisUnderscore) <- return (inv_Pattern_s41 sem arg40)
        return (Syn_Pattern _lhsOattrs _lhsOcopy _lhsOisUnderscore)
   )

-- cata
{-# NOINLINE sem_Pattern #-}
sem_Pattern :: Pattern  -> T_Pattern 
sem_Pattern ( Constr name_ pats_ ) = sem_Pattern_Constr name_ ( sem_Patterns pats_ )
sem_Pattern ( Product pos_ pats_ ) = sem_Pattern_Product pos_ ( sem_Patterns pats_ )
sem_Pattern ( Alias field_ attr_ pat_ ) = sem_Pattern_Alias field_ attr_ ( sem_Pattern pat_ )
sem_Pattern ( Irrefutable pat_ ) = sem_Pattern_Irrefutable ( sem_Pattern pat_ )
sem_Pattern ( Underscore pos_ ) = sem_Pattern_Underscore pos_

-- semantic domain
newtype T_Pattern  = T_Pattern {
                               attach_T_Pattern :: Identity (T_Pattern_s41 )
                               }
newtype T_Pattern_s41  = C_Pattern_s41 {
                                       inv_Pattern_s41 :: (T_Pattern_v40 )
                                       }
data T_Pattern_s42  = C_Pattern_s42
type T_Pattern_v40  = (T_Pattern_vIn40 ) -> (T_Pattern_vOut40 )
data T_Pattern_vIn40  = T_Pattern_vIn40 (Map NontermIdent Attributes) (Map NontermIdent Attributes) (Bool) (Attributes) (Map Identifier Type) (Options) (Attributes)
data T_Pattern_vOut40  = T_Pattern_vOut40 (Set String) (Pattern) (Bool)
{-# NOINLINE sem_Pattern_Constr #-}
sem_Pattern_Constr :: (ConstructorIdent) -> T_Patterns  -> T_Pattern 
sem_Pattern_Constr arg_name_ arg_pats_ = T_Pattern (return st41) where
   {-# NOINLINE st41 #-}
   st41 = let
      v40 :: T_Pattern_v40 
      v40 = \ (T_Pattern_vIn40 _lhsIallInhmap _lhsIallSynmap _lhsIanyLazyKind _lhsIinhmap _lhsIlocalAttrTypes _lhsIoptions _lhsIsynmap) -> ( let
         _patsX44 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_pats_))
         (T_Patterns_vOut43 _patsIattrs _patsIcopy) = inv_Patterns_s44 _patsX44 (T_Patterns_vIn43 _patsOallInhmap _patsOallSynmap _patsOanyLazyKind _patsOinhmap _patsOlocalAttrTypes _patsOoptions _patsOsynmap)
         _addbang = rule433 _lhsIoptions
         _addbang1 = rule434 _addbang _lhsIanyLazyKind
         _lhsOattrs :: Set String
         _lhsOattrs = rule435 _patsIattrs
         _copy = rule436 _patsIcopy arg_name_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule437 _copy
         _lhsOisUnderscore :: Bool
         _lhsOisUnderscore = rule438  ()
         _patsOallInhmap = rule439 _lhsIallInhmap
         _patsOallSynmap = rule440 _lhsIallSynmap
         _patsOanyLazyKind = rule441 _lhsIanyLazyKind
         _patsOinhmap = rule442 _lhsIinhmap
         _patsOlocalAttrTypes = rule443 _lhsIlocalAttrTypes
         _patsOoptions = rule444 _lhsIoptions
         _patsOsynmap = rule445 _lhsIsynmap
         __result_ = T_Pattern_vOut40 _lhsOattrs _lhsOcopy _lhsOisUnderscore
         in __result_ )
     in C_Pattern_s41 v40
   {-# INLINE rule433 #-}
   {-# LINE 1607 "src-ag/ExecutionPlan2TH.ag" #-}
   rule433 = \ ((_lhsIoptions) :: Options) ->
                                                     {-# LINE 1607 "src-ag/ExecutionPlan2TH.ag" #-}
                                                     \x -> if bangpats _lhsIoptions then TH.BangP x else x
                                                     {-# LINE 3368 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule434 #-}
   {-# LINE 1612 "src-ag/ExecutionPlan2TH.ag" #-}
   rule434 = \ _addbang ((_lhsIanyLazyKind) :: Bool) ->
                                                     {-# LINE 1612 "src-ag/ExecutionPlan2TH.ag" #-}
                                                     if _lhsIanyLazyKind then id else _addbang
                                                     {-# LINE 3374 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule435 #-}
   rule435 = \ ((_patsIattrs) :: Set String) ->
     _patsIattrs
   {-# INLINE rule436 #-}
   rule436 = \ ((_patsIcopy) :: Patterns) name_ ->
     Constr name_ _patsIcopy
   {-# INLINE rule437 #-}
   rule437 = \ _copy ->
     _copy
   {-# INLINE rule438 #-}
   rule438 = \  (_ :: ()) ->
     error "missing rule: Pattern.Constr.lhs.isUnderscore"
   {-# INLINE rule439 #-}
   rule439 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule440 #-}
   rule440 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule441 #-}
   rule441 = \ ((_lhsIanyLazyKind) :: Bool) ->
     _lhsIanyLazyKind
   {-# INLINE rule442 #-}
   rule442 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule443 #-}
   rule443 = \ ((_lhsIlocalAttrTypes) :: Map Identifier Type) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule444 #-}
   rule444 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule445 #-}
   rule445 = \ ((_lhsIsynmap) :: Attributes) ->
     _lhsIsynmap
{-# NOINLINE sem_Pattern_Product #-}
sem_Pattern_Product :: (Pos) -> T_Patterns  -> T_Pattern 
sem_Pattern_Product arg_pos_ arg_pats_ = T_Pattern (return st41) where
   {-# NOINLINE st41 #-}
   st41 = let
      v40 :: T_Pattern_v40 
      v40 = \ (T_Pattern_vIn40 _lhsIallInhmap _lhsIallSynmap _lhsIanyLazyKind _lhsIinhmap _lhsIlocalAttrTypes _lhsIoptions _lhsIsynmap) -> ( let
         _patsX44 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_pats_))
         (T_Patterns_vOut43 _patsIattrs _patsIcopy) = inv_Patterns_s44 _patsX44 (T_Patterns_vIn43 _patsOallInhmap _patsOallSynmap _patsOanyLazyKind _patsOinhmap _patsOlocalAttrTypes _patsOoptions _patsOsynmap)
         _addbang = rule446 _lhsIoptions
         _addbang1 = rule447 _addbang _lhsIanyLazyKind
         _lhsOattrs :: Set String
         _lhsOattrs = rule448 _patsIattrs
         _copy = rule449 _patsIcopy arg_pos_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule450 _copy
         _lhsOisUnderscore :: Bool
         _lhsOisUnderscore = rule451  ()
         _patsOallInhmap = rule452 _lhsIallInhmap
         _patsOallSynmap = rule453 _lhsIallSynmap
         _patsOanyLazyKind = rule454 _lhsIanyLazyKind
         _patsOinhmap = rule455 _lhsIinhmap
         _patsOlocalAttrTypes = rule456 _lhsIlocalAttrTypes
         _patsOoptions = rule457 _lhsIoptions
         _patsOsynmap = rule458 _lhsIsynmap
         __result_ = T_Pattern_vOut40 _lhsOattrs _lhsOcopy _lhsOisUnderscore
         in __result_ )
     in C_Pattern_s41 v40
   {-# INLINE rule446 #-}
   {-# LINE 1607 "src-ag/ExecutionPlan2TH.ag" #-}
   rule446 = \ ((_lhsIoptions) :: Options) ->
                                                     {-# LINE 1607 "src-ag/ExecutionPlan2TH.ag" #-}
                                                     \x -> if bangpats _lhsIoptions then TH.BangP x else x
                                                     {-# LINE 3441 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule447 #-}
   {-# LINE 1612 "src-ag/ExecutionPlan2TH.ag" #-}
   rule447 = \ _addbang ((_lhsIanyLazyKind) :: Bool) ->
                                                     {-# LINE 1612 "src-ag/ExecutionPlan2TH.ag" #-}
                                                     if _lhsIanyLazyKind then id else _addbang
                                                     {-# LINE 3447 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule448 #-}
   rule448 = \ ((_patsIattrs) :: Set String) ->
     _patsIattrs
   {-# INLINE rule449 #-}
   rule449 = \ ((_patsIcopy) :: Patterns) pos_ ->
     Product pos_ _patsIcopy
   {-# INLINE rule450 #-}
   rule450 = \ _copy ->
     _copy
   {-# INLINE rule451 #-}
   rule451 = \  (_ :: ()) ->
     error "missing rule: Pattern.Product.lhs.isUnderscore"
   {-# INLINE rule452 #-}
   rule452 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule453 #-}
   rule453 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule454 #-}
   rule454 = \ ((_lhsIanyLazyKind) :: Bool) ->
     _lhsIanyLazyKind
   {-# INLINE rule455 #-}
   rule455 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule456 #-}
   rule456 = \ ((_lhsIlocalAttrTypes) :: Map Identifier Type) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule457 #-}
   rule457 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule458 #-}
   rule458 = \ ((_lhsIsynmap) :: Attributes) ->
     _lhsIsynmap
{-# NOINLINE sem_Pattern_Alias #-}
sem_Pattern_Alias :: (Identifier) -> (Identifier) -> T_Pattern  -> T_Pattern 
sem_Pattern_Alias arg_field_ arg_attr_ arg_pat_ = T_Pattern (return st41) where
   {-# NOINLINE st41 #-}
   st41 = let
      v40 :: T_Pattern_v40 
      v40 = \ (T_Pattern_vIn40 _lhsIallInhmap _lhsIallSynmap _lhsIanyLazyKind _lhsIinhmap _lhsIlocalAttrTypes _lhsIoptions _lhsIsynmap) -> ( let
         _patX41 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat_))
         (T_Pattern_vOut40 _patIattrs _patIcopy _patIisUnderscore) = inv_Pattern_s41 _patX41 (T_Pattern_vIn40 _patOallInhmap _patOallSynmap _patOanyLazyKind _patOinhmap _patOlocalAttrTypes _patOoptions _patOsynmap)
         _addbang = rule459 _lhsIoptions
         _addbang1 = rule460 _addbang _lhsIanyLazyKind
         _lhsOattrs :: Set String
         _lhsOattrs = rule461 _patIattrs
         _copy = rule462 _patIcopy arg_attr_ arg_field_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule463 _copy
         _lhsOisUnderscore :: Bool
         _lhsOisUnderscore = rule464 _patIisUnderscore
         _patOallInhmap = rule465 _lhsIallInhmap
         _patOallSynmap = rule466 _lhsIallSynmap
         _patOanyLazyKind = rule467 _lhsIanyLazyKind
         _patOinhmap = rule468 _lhsIinhmap
         _patOlocalAttrTypes = rule469 _lhsIlocalAttrTypes
         _patOoptions = rule470 _lhsIoptions
         _patOsynmap = rule471 _lhsIsynmap
         __result_ = T_Pattern_vOut40 _lhsOattrs _lhsOcopy _lhsOisUnderscore
         in __result_ )
     in C_Pattern_s41 v40
   {-# INLINE rule459 #-}
   {-# LINE 1607 "src-ag/ExecutionPlan2TH.ag" #-}
   rule459 = \ ((_lhsIoptions) :: Options) ->
                                                     {-# LINE 1607 "src-ag/ExecutionPlan2TH.ag" #-}
                                                     \x -> if bangpats _lhsIoptions then TH.BangP x else x
                                                     {-# LINE 3514 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule460 #-}
   {-# LINE 1612 "src-ag/ExecutionPlan2TH.ag" #-}
   rule460 = \ _addbang ((_lhsIanyLazyKind) :: Bool) ->
                                                     {-# LINE 1612 "src-ag/ExecutionPlan2TH.ag" #-}
                                                     if _lhsIanyLazyKind then id else _addbang
                                                     {-# LINE 3520 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule461 #-}
   rule461 = \ ((_patIattrs) :: Set String) ->
     _patIattrs
   {-# INLINE rule462 #-}
   rule462 = \ ((_patIcopy) :: Pattern) attr_ field_ ->
     Alias field_ attr_ _patIcopy
   {-# INLINE rule463 #-}
   rule463 = \ _copy ->
     _copy
   {-# INLINE rule464 #-}
   rule464 = \ ((_patIisUnderscore) :: Bool) ->
     _patIisUnderscore
   {-# INLINE rule465 #-}
   rule465 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule466 #-}
   rule466 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule467 #-}
   rule467 = \ ((_lhsIanyLazyKind) :: Bool) ->
     _lhsIanyLazyKind
   {-# INLINE rule468 #-}
   rule468 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule469 #-}
   rule469 = \ ((_lhsIlocalAttrTypes) :: Map Identifier Type) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule470 #-}
   rule470 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule471 #-}
   rule471 = \ ((_lhsIsynmap) :: Attributes) ->
     _lhsIsynmap
{-# NOINLINE sem_Pattern_Irrefutable #-}
sem_Pattern_Irrefutable :: T_Pattern  -> T_Pattern 
sem_Pattern_Irrefutable arg_pat_ = T_Pattern (return st41) where
   {-# NOINLINE st41 #-}
   st41 = let
      v40 :: T_Pattern_v40 
      v40 = \ (T_Pattern_vIn40 _lhsIallInhmap _lhsIallSynmap _lhsIanyLazyKind _lhsIinhmap _lhsIlocalAttrTypes _lhsIoptions _lhsIsynmap) -> ( let
         _patX41 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_pat_))
         (T_Pattern_vOut40 _patIattrs _patIcopy _patIisUnderscore) = inv_Pattern_s41 _patX41 (T_Pattern_vIn40 _patOallInhmap _patOallSynmap _patOanyLazyKind _patOinhmap _patOlocalAttrTypes _patOoptions _patOsynmap)
         _lhsOattrs :: Set String
         _lhsOattrs = rule472 _patIattrs
         _copy = rule473 _patIcopy
         _lhsOcopy :: Pattern
         _lhsOcopy = rule474 _copy
         _lhsOisUnderscore :: Bool
         _lhsOisUnderscore = rule475 _patIisUnderscore
         _patOallInhmap = rule476 _lhsIallInhmap
         _patOallSynmap = rule477 _lhsIallSynmap
         _patOanyLazyKind = rule478 _lhsIanyLazyKind
         _patOinhmap = rule479 _lhsIinhmap
         _patOlocalAttrTypes = rule480 _lhsIlocalAttrTypes
         _patOoptions = rule481 _lhsIoptions
         _patOsynmap = rule482 _lhsIsynmap
         __result_ = T_Pattern_vOut40 _lhsOattrs _lhsOcopy _lhsOisUnderscore
         in __result_ )
     in C_Pattern_s41 v40
   {-# INLINE rule472 #-}
   rule472 = \ ((_patIattrs) :: Set String) ->
     _patIattrs
   {-# INLINE rule473 #-}
   rule473 = \ ((_patIcopy) :: Pattern) ->
     Irrefutable _patIcopy
   {-# INLINE rule474 #-}
   rule474 = \ _copy ->
     _copy
   {-# INLINE rule475 #-}
   rule475 = \ ((_patIisUnderscore) :: Bool) ->
     _patIisUnderscore
   {-# INLINE rule476 #-}
   rule476 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule477 #-}
   rule477 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule478 #-}
   rule478 = \ ((_lhsIanyLazyKind) :: Bool) ->
     _lhsIanyLazyKind
   {-# INLINE rule479 #-}
   rule479 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule480 #-}
   rule480 = \ ((_lhsIlocalAttrTypes) :: Map Identifier Type) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule481 #-}
   rule481 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule482 #-}
   rule482 = \ ((_lhsIsynmap) :: Attributes) ->
     _lhsIsynmap
{-# NOINLINE sem_Pattern_Underscore #-}
sem_Pattern_Underscore :: (Pos) -> T_Pattern 
sem_Pattern_Underscore arg_pos_ = T_Pattern (return st41) where
   {-# NOINLINE st41 #-}
   st41 = let
      v40 :: T_Pattern_v40 
      v40 = \ (T_Pattern_vIn40 _lhsIallInhmap _lhsIallSynmap _lhsIanyLazyKind _lhsIinhmap _lhsIlocalAttrTypes _lhsIoptions _lhsIsynmap) -> ( let
         _lhsOattrs :: Set String
         _lhsOattrs = rule483  ()
         _copy = rule484 arg_pos_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule485 _copy
         _lhsOisUnderscore :: Bool
         _lhsOisUnderscore = rule486  ()
         __result_ = T_Pattern_vOut40 _lhsOattrs _lhsOcopy _lhsOisUnderscore
         in __result_ )
     in C_Pattern_s41 v40
   {-# INLINE rule483 #-}
   rule483 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule484 #-}
   rule484 = \ pos_ ->
     Underscore pos_
   {-# INLINE rule485 #-}
   rule485 = \ _copy ->
     _copy
   {-# INLINE rule486 #-}
   rule486 = \  (_ :: ()) ->
     error "missing rule: Pattern.Underscore.lhs.isUnderscore"

-- Patterns ----------------------------------------------------
-- wrapper
data Inh_Patterns  = Inh_Patterns { allInhmap_Inh_Patterns :: (Map NontermIdent Attributes), allSynmap_Inh_Patterns :: (Map NontermIdent Attributes), anyLazyKind_Inh_Patterns :: (Bool), inhmap_Inh_Patterns :: (Attributes), localAttrTypes_Inh_Patterns :: (Map Identifier Type), options_Inh_Patterns :: (Options), synmap_Inh_Patterns :: (Attributes) }
data Syn_Patterns  = Syn_Patterns { attrs_Syn_Patterns :: (Set String), copy_Syn_Patterns :: (Patterns) }
{-# INLINABLE wrap_Patterns #-}
wrap_Patterns :: T_Patterns  -> Inh_Patterns  -> (Syn_Patterns )
wrap_Patterns (T_Patterns act) (Inh_Patterns _lhsIallInhmap _lhsIallSynmap _lhsIanyLazyKind _lhsIinhmap _lhsIlocalAttrTypes _lhsIoptions _lhsIsynmap) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg43 = T_Patterns_vIn43 _lhsIallInhmap _lhsIallSynmap _lhsIanyLazyKind _lhsIinhmap _lhsIlocalAttrTypes _lhsIoptions _lhsIsynmap
        (T_Patterns_vOut43 _lhsOattrs _lhsOcopy) <- return (inv_Patterns_s44 sem arg43)
        return (Syn_Patterns _lhsOattrs _lhsOcopy)
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
data T_Patterns_vIn43  = T_Patterns_vIn43 (Map NontermIdent Attributes) (Map NontermIdent Attributes) (Bool) (Attributes) (Map Identifier Type) (Options) (Attributes)
data T_Patterns_vOut43  = T_Patterns_vOut43 (Set String) (Patterns)
{-# NOINLINE sem_Patterns_Cons #-}
sem_Patterns_Cons :: T_Pattern  -> T_Patterns  -> T_Patterns 
sem_Patterns_Cons arg_hd_ arg_tl_ = T_Patterns (return st44) where
   {-# NOINLINE st44 #-}
   st44 = let
      v43 :: T_Patterns_v43 
      v43 = \ (T_Patterns_vIn43 _lhsIallInhmap _lhsIallSynmap _lhsIanyLazyKind _lhsIinhmap _lhsIlocalAttrTypes _lhsIoptions _lhsIsynmap) -> ( let
         _hdX41 = Control.Monad.Identity.runIdentity (attach_T_Pattern (arg_hd_))
         _tlX44 = Control.Monad.Identity.runIdentity (attach_T_Patterns (arg_tl_))
         (T_Pattern_vOut40 _hdIattrs _hdIcopy _hdIisUnderscore) = inv_Pattern_s41 _hdX41 (T_Pattern_vIn40 _hdOallInhmap _hdOallSynmap _hdOanyLazyKind _hdOinhmap _hdOlocalAttrTypes _hdOoptions _hdOsynmap)
         (T_Patterns_vOut43 _tlIattrs _tlIcopy) = inv_Patterns_s44 _tlX44 (T_Patterns_vIn43 _tlOallInhmap _tlOallSynmap _tlOanyLazyKind _tlOinhmap _tlOlocalAttrTypes _tlOoptions _tlOsynmap)
         _lhsOattrs :: Set String
         _lhsOattrs = rule487 _hdIattrs _tlIattrs
         _copy = rule488 _hdIcopy _tlIcopy
         _lhsOcopy :: Patterns
         _lhsOcopy = rule489 _copy
         _hdOallInhmap = rule490 _lhsIallInhmap
         _hdOallSynmap = rule491 _lhsIallSynmap
         _hdOanyLazyKind = rule492 _lhsIanyLazyKind
         _hdOinhmap = rule493 _lhsIinhmap
         _hdOlocalAttrTypes = rule494 _lhsIlocalAttrTypes
         _hdOoptions = rule495 _lhsIoptions
         _hdOsynmap = rule496 _lhsIsynmap
         _tlOallInhmap = rule497 _lhsIallInhmap
         _tlOallSynmap = rule498 _lhsIallSynmap
         _tlOanyLazyKind = rule499 _lhsIanyLazyKind
         _tlOinhmap = rule500 _lhsIinhmap
         _tlOlocalAttrTypes = rule501 _lhsIlocalAttrTypes
         _tlOoptions = rule502 _lhsIoptions
         _tlOsynmap = rule503 _lhsIsynmap
         __result_ = T_Patterns_vOut43 _lhsOattrs _lhsOcopy
         in __result_ )
     in C_Patterns_s44 v43
   {-# INLINE rule487 #-}
   rule487 = \ ((_hdIattrs) :: Set String) ((_tlIattrs) :: Set String) ->
     _hdIattrs `Set.union` _tlIattrs
   {-# INLINE rule488 #-}
   rule488 = \ ((_hdIcopy) :: Pattern) ((_tlIcopy) :: Patterns) ->
     (:) _hdIcopy _tlIcopy
   {-# INLINE rule489 #-}
   rule489 = \ _copy ->
     _copy
   {-# INLINE rule490 #-}
   rule490 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule491 #-}
   rule491 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule492 #-}
   rule492 = \ ((_lhsIanyLazyKind) :: Bool) ->
     _lhsIanyLazyKind
   {-# INLINE rule493 #-}
   rule493 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule494 #-}
   rule494 = \ ((_lhsIlocalAttrTypes) :: Map Identifier Type) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule495 #-}
   rule495 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule496 #-}
   rule496 = \ ((_lhsIsynmap) :: Attributes) ->
     _lhsIsynmap
   {-# INLINE rule497 #-}
   rule497 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule498 #-}
   rule498 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule499 #-}
   rule499 = \ ((_lhsIanyLazyKind) :: Bool) ->
     _lhsIanyLazyKind
   {-# INLINE rule500 #-}
   rule500 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule501 #-}
   rule501 = \ ((_lhsIlocalAttrTypes) :: Map Identifier Type) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule502 #-}
   rule502 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule503 #-}
   rule503 = \ ((_lhsIsynmap) :: Attributes) ->
     _lhsIsynmap
{-# NOINLINE sem_Patterns_Nil #-}
sem_Patterns_Nil ::  T_Patterns 
sem_Patterns_Nil  = T_Patterns (return st44) where
   {-# NOINLINE st44 #-}
   st44 = let
      v43 :: T_Patterns_v43 
      v43 = \ (T_Patterns_vIn43 _lhsIallInhmap _lhsIallSynmap _lhsIanyLazyKind _lhsIinhmap _lhsIlocalAttrTypes _lhsIoptions _lhsIsynmap) -> ( let
         _lhsOattrs :: Set String
         _lhsOattrs = rule504  ()
         _copy = rule505  ()
         _lhsOcopy :: Patterns
         _lhsOcopy = rule506 _copy
         __result_ = T_Patterns_vOut43 _lhsOattrs _lhsOcopy
         in __result_ )
     in C_Patterns_s44 v43
   {-# INLINE rule504 #-}
   rule504 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule505 #-}
   rule505 = \  (_ :: ()) ->
     []
   {-# INLINE rule506 #-}
   rule506 = \ _copy ->
     _copy

-- Visit -------------------------------------------------------
-- wrapper
data Inh_Visit  = Inh_Visit { allFromToStates_Inh_Visit :: (Map VisitIdentifier (Int,Int)), allInhmap_Inh_Visit :: (Map NontermIdent Attributes), allInitStates_Inh_Visit :: (Map NontermIdent Int), allSynmap_Inh_Visit :: (Map NontermIdent Attributes), allVisitKinds_Inh_Visit :: (Map VisitIdentifier VisitKind), allintramap_Inh_Visit :: (Map StateIdentifier (Map String (Maybe NonLocalAttr))), avisitdefs_Inh_Visit :: (Map VisitIdentifier (Set Identifier)), avisituses_Inh_Visit :: (Map VisitIdentifier (Set Identifier)), childTypes_Inh_Visit :: (Map Identifier Type), con_Inh_Visit :: (ConstructorIdent), inhmap_Inh_Visit :: (Attributes), nextVisits_Inh_Visit :: (Map StateIdentifier StateCtx), nt_Inh_Visit :: (NontermIdent), options_Inh_Visit :: (Options), params_Inh_Visit :: ([Identifier]), prevVisits_Inh_Visit :: (Map StateIdentifier StateCtx), ruledefs_Inh_Visit :: (Map Identifier (Set String)), ruleuses_Inh_Visit :: (Map Identifier (Map String (Maybe NonLocalAttr))), synmap_Inh_Visit :: (Attributes), terminaldefs_Inh_Visit :: (Set String) }
data Syn_Visit  = Syn_Visit { allvisits_Syn_Visit :: ( VisitStateState ), errors_Syn_Visit :: (Seq Error), fromToStates_Syn_Visit :: (Map VisitIdentifier (Int,Int)), intramap_Syn_Visit :: (Map StateIdentifier (Map String (Maybe NonLocalAttr))), lazyIntras_Syn_Visit :: (Set String), ruleKinds_Syn_Visit :: (Map Identifier (Set VisitKind)), ruleUsage_Syn_Visit :: (Map Identifier Int), usedArgs_Syn_Visit :: (Set String), visitKinds_Syn_Visit :: (Map VisitIdentifier VisitKind), visitdefs_Syn_Visit :: (Map VisitIdentifier (Set Identifier)), visituses_Syn_Visit :: (Map VisitIdentifier (Set Identifier)) }
{-# INLINABLE wrap_Visit #-}
wrap_Visit :: T_Visit  -> Inh_Visit  -> (Syn_Visit )
wrap_Visit (T_Visit act) (Inh_Visit _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallintramap _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIcon _lhsIinhmap _lhsInextVisits _lhsInt _lhsIoptions _lhsIparams _lhsIprevVisits _lhsIruledefs _lhsIruleuses _lhsIsynmap _lhsIterminaldefs) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg46 = T_Visit_vIn46 _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallintramap _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIcon _lhsIinhmap _lhsInextVisits _lhsInt _lhsIoptions _lhsIparams _lhsIprevVisits _lhsIruledefs _lhsIruleuses _lhsIsynmap _lhsIterminaldefs
        (T_Visit_vOut46 _lhsOallvisits _lhsOerrors _lhsOfromToStates _lhsOintramap _lhsOlazyIntras _lhsOruleKinds _lhsOruleUsage _lhsOusedArgs _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses) <- return (inv_Visit_s47 sem arg46)
        return (Syn_Visit _lhsOallvisits _lhsOerrors _lhsOfromToStates _lhsOintramap _lhsOlazyIntras _lhsOruleKinds _lhsOruleUsage _lhsOusedArgs _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses)
   )

-- cata
{-# INLINE sem_Visit #-}
sem_Visit :: Visit  -> T_Visit 
sem_Visit ( Visit ident_ from_ to_ inh_ syn_ steps_ kind_ ) = sem_Visit_Visit ident_ from_ to_ inh_ syn_ ( sem_VisitSteps steps_ ) kind_

-- semantic domain
newtype T_Visit  = T_Visit {
                           attach_T_Visit :: Identity (T_Visit_s47 )
                           }
newtype T_Visit_s47  = C_Visit_s47 {
                                   inv_Visit_s47 :: (T_Visit_v46 )
                                   }
data T_Visit_s48  = C_Visit_s48
type T_Visit_v46  = (T_Visit_vIn46 ) -> (T_Visit_vOut46 )
data T_Visit_vIn46  = T_Visit_vIn46 (Map VisitIdentifier (Int,Int)) (Map NontermIdent Attributes) (Map NontermIdent Int) (Map NontermIdent Attributes) (Map VisitIdentifier VisitKind) (Map StateIdentifier (Map String (Maybe NonLocalAttr))) (Map VisitIdentifier (Set Identifier)) (Map VisitIdentifier (Set Identifier)) (Map Identifier Type) (ConstructorIdent) (Attributes) (Map StateIdentifier StateCtx) (NontermIdent) (Options) ([Identifier]) (Map StateIdentifier StateCtx) (Map Identifier (Set String)) (Map Identifier (Map String (Maybe NonLocalAttr))) (Attributes) (Set String)
data T_Visit_vOut46  = T_Visit_vOut46 ( VisitStateState ) (Seq Error) (Map VisitIdentifier (Int,Int)) (Map StateIdentifier (Map String (Maybe NonLocalAttr))) (Set String) (Map Identifier (Set VisitKind)) (Map Identifier Int) (Set String) (Map VisitIdentifier VisitKind) (Map VisitIdentifier (Set Identifier)) (Map VisitIdentifier (Set Identifier))
{-# NOINLINE sem_Visit_Visit #-}
sem_Visit_Visit :: (VisitIdentifier) -> (StateIdentifier) -> (StateIdentifier) -> (Set Identifier) -> (Set Identifier) -> T_VisitSteps  -> (VisitKind) -> T_Visit 
sem_Visit_Visit arg_ident_ arg_from_ arg_to_ arg_inh_ _ arg_steps_ arg_kind_ = T_Visit (return st47) where
   {-# NOINLINE st47 #-}
   st47 = let
      v46 :: T_Visit_v46 
      v46 = \ (T_Visit_vIn46 _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallintramap _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIcon _lhsIinhmap _lhsInextVisits _lhsInt _lhsIoptions _lhsIparams _lhsIprevVisits _lhsIruledefs _lhsIruleuses _lhsIsynmap _lhsIterminaldefs) -> ( let
         _stepsX53 = Control.Monad.Identity.runIdentity (attach_T_VisitSteps (arg_steps_))
         (T_VisitSteps_vOut52 _stepsIdefs _stepsIerrors _stepsIindex _stepsIisLast _stepsIlazyIntras _stepsIprevMaxSimRefs _stepsIruleKinds _stepsIruleUsage _stepsIsize _stepsIusedArgs _stepsIuses _stepsIvisitKinds) = inv_VisitSteps_s53 _stepsX53 (T_VisitSteps_vIn52 _stepsOallFromToStates _stepsOallInitStates _stepsOallVisitKinds _stepsOavisitdefs _stepsOavisituses _stepsOchildTypes _stepsOfmtMode _stepsOindex _stepsOkind _stepsOoptions _stepsOprevMaxSimRefs _stepsOruledefs _stepsOruleuses)
         _lhsOallvisits ::  VisitStateState 
         _lhsOallvisits = rule507 arg_from_ arg_ident_ arg_to_
         _stepsOfmtMode = rule508 arg_kind_
         _inhVarNms = rule509 _lhsIoptions arg_inh_
         _lazyIntrasInh = rule510 _inhVarNms _stepsIdefs arg_kind_
         _lhsOlazyIntras :: Set String
         _lhsOlazyIntras = rule511 _lazyIntrasInh _stepsIlazyIntras
         _addbang = rule512 _lhsIoptions
         _addbang1 = rule513 _addbang arg_kind_
         _lhsOfromToStates :: Map VisitIdentifier (Int,Int)
         _lhsOfromToStates = rule514 arg_from_ arg_ident_ arg_to_
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule515 arg_ident_ arg_kind_
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule516 _stepsIerrors
         _lhsOintramap :: Map StateIdentifier (Map String (Maybe NonLocalAttr))
         _lhsOintramap = rule517  ()
         _lhsOruleKinds :: Map Identifier (Set VisitKind)
         _lhsOruleKinds = rule518 _stepsIruleKinds
         _lhsOruleUsage :: Map Identifier Int
         _lhsOruleUsage = rule519 _stepsIruleUsage
         _lhsOusedArgs :: Set String
         _lhsOusedArgs = rule520 _stepsIusedArgs
         _lhsOvisitdefs :: Map VisitIdentifier (Set Identifier)
         _lhsOvisitdefs = rule521  ()
         _lhsOvisituses :: Map VisitIdentifier (Set Identifier)
         _lhsOvisituses = rule522  ()
         _stepsOallFromToStates = rule523 _lhsIallFromToStates
         _stepsOallInitStates = rule524 _lhsIallInitStates
         _stepsOallVisitKinds = rule525 _lhsIallVisitKinds
         _stepsOavisitdefs = rule526 _lhsIavisitdefs
         _stepsOavisituses = rule527 _lhsIavisituses
         _stepsOchildTypes = rule528 _lhsIchildTypes
         _stepsOindex = rule529  ()
         _stepsOkind = rule530 arg_kind_
         _stepsOoptions = rule531 _lhsIoptions
         _stepsOprevMaxSimRefs = rule532  ()
         _stepsOruledefs = rule533 _lhsIruledefs
         _stepsOruleuses = rule534 _lhsIruleuses
         __result_ = T_Visit_vOut46 _lhsOallvisits _lhsOerrors _lhsOfromToStates _lhsOintramap _lhsOlazyIntras _lhsOruleKinds _lhsOruleUsage _lhsOusedArgs _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses
         in __result_ )
     in C_Visit_s47 v46
   {-# INLINE rule507 #-}
   {-# LINE 388 "src-ag/ExecutionPlan2TH.ag" #-}
   rule507 = \ from_ ident_ to_ ->
                            {-# LINE 388 "src-ag/ExecutionPlan2TH.ag" #-}
                            (ident_, from_, to_)
                            {-# LINE 3868 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule508 #-}
   {-# LINE 891 "src-ag/ExecutionPlan2TH.ag" #-}
   rule508 = \ kind_ ->
                    {-# LINE 891 "src-ag/ExecutionPlan2TH.ag" #-}
                    case kind_ of
                      VisitPure False -> FormatLetDecl
                      VisitPure True  -> FormatLetLine
                      VisitMonadic    -> FormatDo
                    {-# LINE 3877 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule509 #-}
   {-# LINE 1386 "src-ag/ExecutionPlan2TH.ag" #-}
   rule509 = \ ((_lhsIoptions) :: Options) inh_ ->
                            {-# LINE 1386 "src-ag/ExecutionPlan2TH.ag" #-}
                            Set.map (lhsname _lhsIoptions True) inh_
                            {-# LINE 3883 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule510 #-}
   {-# LINE 1445 "src-ag/ExecutionPlan2TH.ag" #-}
   rule510 = \ _inhVarNms ((_stepsIdefs) :: Set String) kind_ ->
                        {-# LINE 1445 "src-ag/ExecutionPlan2TH.ag" #-}
                        case kind_ of
                          VisitPure False -> _inhVarNms     `Set.union` _stepsIdefs
                          _               -> Set.empty
                        {-# LINE 3891 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule511 #-}
   {-# LINE 1448 "src-ag/ExecutionPlan2TH.ag" #-}
   rule511 = \ _lazyIntrasInh ((_stepsIlazyIntras) :: Set String) ->
                     {-# LINE 1448 "src-ag/ExecutionPlan2TH.ag" #-}
                     _lazyIntrasInh     `Set.union` _stepsIlazyIntras
                     {-# LINE 3897 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule512 #-}
   {-# LINE 1601 "src-ag/ExecutionPlan2TH.ag" #-}
   rule512 = \ ((_lhsIoptions) :: Options) ->
                                                    {-# LINE 1601 "src-ag/ExecutionPlan2TH.ag" #-}
                                                    \x -> if bangpats _lhsIoptions then TH.BangP x else x
                                                    {-# LINE 3903 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule513 #-}
   {-# LINE 1609 "src-ag/ExecutionPlan2TH.ag" #-}
   rule513 = \ _addbang kind_ ->
                                                     {-# LINE 1609 "src-ag/ExecutionPlan2TH.ag" #-}
                                                     if isLazyKind kind_ then id else _addbang
                                                     {-# LINE 3909 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule514 #-}
   {-# LINE 1636 "src-ag/ExecutionPlan2TH.ag" #-}
   rule514 = \ from_ ident_ to_ ->
                       {-# LINE 1636 "src-ag/ExecutionPlan2TH.ag" #-}
                       Map.singleton ident_ (from_, to_)
                       {-# LINE 3915 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule515 #-}
   {-# LINE 1680 "src-ag/ExecutionPlan2TH.ag" #-}
   rule515 = \ ident_ kind_ ->
                     {-# LINE 1680 "src-ag/ExecutionPlan2TH.ag" #-}
                     Map.singleton ident_ kind_
                     {-# LINE 3921 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule516 #-}
   rule516 = \ ((_stepsIerrors) :: Seq Error) ->
     _stepsIerrors
   {-# INLINE rule517 #-}
   rule517 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule518 #-}
   rule518 = \ ((_stepsIruleKinds) :: Map Identifier (Set VisitKind)) ->
     _stepsIruleKinds
   {-# INLINE rule519 #-}
   rule519 = \ ((_stepsIruleUsage) :: Map Identifier Int) ->
     _stepsIruleUsage
   {-# INLINE rule520 #-}
   rule520 = \ ((_stepsIusedArgs) :: Set String) ->
     _stepsIusedArgs
   {-# INLINE rule521 #-}
   rule521 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule522 #-}
   rule522 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule523 #-}
   rule523 = \ ((_lhsIallFromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _lhsIallFromToStates
   {-# INLINE rule524 #-}
   rule524 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule525 #-}
   rule525 = \ ((_lhsIallVisitKinds) :: Map VisitIdentifier VisitKind) ->
     _lhsIallVisitKinds
   {-# INLINE rule526 #-}
   rule526 = \ ((_lhsIavisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisitdefs
   {-# INLINE rule527 #-}
   rule527 = \ ((_lhsIavisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisituses
   {-# INLINE rule528 #-}
   rule528 = \ ((_lhsIchildTypes) :: Map Identifier Type) ->
     _lhsIchildTypes
   {-# INLINE rule529 #-}
   rule529 = \  (_ :: ()) ->
     error "missing rule: Visit.Visit.steps.index"
   {-# INLINE rule530 #-}
   rule530 = \ kind_ ->
     kind_
   {-# INLINE rule531 #-}
   rule531 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule532 #-}
   rule532 = \  (_ :: ()) ->
     error "missing rule: Visit.Visit.steps.prevMaxSimRefs"
   {-# INLINE rule533 #-}
   rule533 = \ ((_lhsIruledefs) :: Map Identifier (Set String)) ->
     _lhsIruledefs
   {-# INLINE rule534 #-}
   rule534 = \ ((_lhsIruleuses) :: Map Identifier (Map String (Maybe NonLocalAttr))) ->
     _lhsIruleuses

-- VisitStep ---------------------------------------------------
-- wrapper
data Inh_VisitStep  = Inh_VisitStep { allFromToStates_Inh_VisitStep :: (Map VisitIdentifier (Int,Int)), allInitStates_Inh_VisitStep :: (Map NontermIdent Int), allVisitKinds_Inh_VisitStep :: (Map VisitIdentifier VisitKind), avisitdefs_Inh_VisitStep :: (Map VisitIdentifier (Set Identifier)), avisituses_Inh_VisitStep :: (Map VisitIdentifier (Set Identifier)), childTypes_Inh_VisitStep :: (Map Identifier Type), fmtMode_Inh_VisitStep :: (FormatMode), index_Inh_VisitStep :: (Int), isLast_Inh_VisitStep :: (Bool), kind_Inh_VisitStep :: (VisitKind), options_Inh_VisitStep :: (Options), prevMaxSimRefs_Inh_VisitStep :: (Int), ruledefs_Inh_VisitStep :: (Map Identifier (Set String)), ruleuses_Inh_VisitStep :: (Map Identifier (Map String (Maybe NonLocalAttr))) }
data Syn_VisitStep  = Syn_VisitStep { defs_Syn_VisitStep :: (Set String), errors_Syn_VisitStep :: (Seq Error), index_Syn_VisitStep :: (Int), isLast_Syn_VisitStep :: (Bool), lazyIntras_Syn_VisitStep :: (Set String), prevMaxSimRefs_Syn_VisitStep :: (Int), ruleKinds_Syn_VisitStep :: (Map Identifier (Set VisitKind)), ruleUsage_Syn_VisitStep :: (Map Identifier Int), usedArgs_Syn_VisitStep :: (Set String), uses_Syn_VisitStep :: (Map String (Maybe NonLocalAttr)), visitKinds_Syn_VisitStep :: (Map VisitIdentifier VisitKind) }
{-# INLINABLE wrap_VisitStep #-}
wrap_VisitStep :: T_VisitStep  -> Inh_VisitStep  -> (Syn_VisitStep )
wrap_VisitStep (T_VisitStep act) (Inh_VisitStep _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIfmtMode _lhsIindex _lhsIisLast _lhsIkind _lhsIoptions _lhsIprevMaxSimRefs _lhsIruledefs _lhsIruleuses) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg49 = T_VisitStep_vIn49 _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIfmtMode _lhsIindex _lhsIisLast _lhsIkind _lhsIoptions _lhsIprevMaxSimRefs _lhsIruledefs _lhsIruleuses
        (T_VisitStep_vOut49 _lhsOdefs _lhsOerrors _lhsOindex _lhsOisLast _lhsOlazyIntras _lhsOprevMaxSimRefs _lhsOruleKinds _lhsOruleUsage _lhsOusedArgs _lhsOuses _lhsOvisitKinds) <- return (inv_VisitStep_s50 sem arg49)
        return (Syn_VisitStep _lhsOdefs _lhsOerrors _lhsOindex _lhsOisLast _lhsOlazyIntras _lhsOprevMaxSimRefs _lhsOruleKinds _lhsOruleUsage _lhsOusedArgs _lhsOuses _lhsOvisitKinds)
   )

-- cata
{-# NOINLINE sem_VisitStep #-}
sem_VisitStep :: VisitStep  -> T_VisitStep 
sem_VisitStep ( Sem name_ ) = sem_VisitStep_Sem name_
sem_VisitStep ( ChildVisit child_ nonterm_ visit_ ) = sem_VisitStep_ChildVisit child_ nonterm_ visit_
sem_VisitStep ( PureGroup steps_ ordered_ ) = sem_VisitStep_PureGroup ( sem_VisitSteps steps_ ) ordered_
sem_VisitStep ( Sim steps_ ) = sem_VisitStep_Sim ( sem_VisitSteps steps_ )
sem_VisitStep ( ChildIntro child_ ) = sem_VisitStep_ChildIntro child_

-- semantic domain
newtype T_VisitStep  = T_VisitStep {
                                   attach_T_VisitStep :: Identity (T_VisitStep_s50 )
                                   }
newtype T_VisitStep_s50  = C_VisitStep_s50 {
                                           inv_VisitStep_s50 :: (T_VisitStep_v49 )
                                           }
data T_VisitStep_s51  = C_VisitStep_s51
type T_VisitStep_v49  = (T_VisitStep_vIn49 ) -> (T_VisitStep_vOut49 )
data T_VisitStep_vIn49  = T_VisitStep_vIn49 (Map VisitIdentifier (Int,Int)) (Map NontermIdent Int) (Map VisitIdentifier VisitKind) (Map VisitIdentifier (Set Identifier)) (Map VisitIdentifier (Set Identifier)) (Map Identifier Type) (FormatMode) (Int) (Bool) (VisitKind) (Options) (Int) (Map Identifier (Set String)) (Map Identifier (Map String (Maybe NonLocalAttr)))
data T_VisitStep_vOut49  = T_VisitStep_vOut49 (Set String) (Seq Error) (Int) (Bool) (Set String) (Int) (Map Identifier (Set VisitKind)) (Map Identifier Int) (Set String) (Map String (Maybe NonLocalAttr)) (Map VisitIdentifier VisitKind)
{-# NOINLINE sem_VisitStep_Sem #-}
sem_VisitStep_Sem :: (Identifier) -> T_VisitStep 
sem_VisitStep_Sem arg_name_ = T_VisitStep (return st50) where
   {-# NOINLINE st50 #-}
   st50 = let
      v49 :: T_VisitStep_v49 
      v49 = \ (T_VisitStep_vIn49 _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIfmtMode _lhsIindex _lhsIisLast _lhsIkind _lhsIoptions _lhsIprevMaxSimRefs _lhsIruledefs _lhsIruleuses) -> ( let
         _lhsOruleUsage :: Map Identifier Int
         _lhsOruleUsage = rule535 arg_name_
         _lhsOdefs :: Set String
         _lhsOdefs = rule536  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule537  ()
         _lhsOlazyIntras :: Set String
         _lhsOlazyIntras = rule538  ()
         _lhsOruleKinds :: Map Identifier (Set VisitKind)
         _lhsOruleKinds = rule539  ()
         _lhsOusedArgs :: Set String
         _lhsOusedArgs = rule540  ()
         _lhsOuses :: Map String (Maybe NonLocalAttr)
         _lhsOuses = rule541  ()
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule542  ()
         _lhsOindex :: Int
         _lhsOindex = rule543 _lhsIindex
         _lhsOisLast :: Bool
         _lhsOisLast = rule544 _lhsIisLast
         _lhsOprevMaxSimRefs :: Int
         _lhsOprevMaxSimRefs = rule545 _lhsIprevMaxSimRefs
         __result_ = T_VisitStep_vOut49 _lhsOdefs _lhsOerrors _lhsOindex _lhsOisLast _lhsOlazyIntras _lhsOprevMaxSimRefs _lhsOruleKinds _lhsOruleUsage _lhsOusedArgs _lhsOuses _lhsOvisitKinds
         in __result_ )
     in C_VisitStep_s50 v49
   {-# INLINE rule535 #-}
   {-# LINE 1333 "src-ag/ExecutionPlan2TH.ag" #-}
   rule535 = \ name_ ->
                                                 {-# LINE 1333 "src-ag/ExecutionPlan2TH.ag" #-}
                                                 Map.singleton name_ 1
                                                 {-# LINE 4051 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule536 #-}
   rule536 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule537 #-}
   rule537 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule538 #-}
   rule538 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule539 #-}
   rule539 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule540 #-}
   rule540 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule541 #-}
   rule541 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule542 #-}
   rule542 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule543 #-}
   rule543 = \ ((_lhsIindex) :: Int) ->
     _lhsIindex
   {-# INLINE rule544 #-}
   rule544 = \ ((_lhsIisLast) :: Bool) ->
     _lhsIisLast
   {-# INLINE rule545 #-}
   rule545 = \ ((_lhsIprevMaxSimRefs) :: Int) ->
     _lhsIprevMaxSimRefs
{-# NOINLINE sem_VisitStep_ChildVisit #-}
sem_VisitStep_ChildVisit :: (Identifier) -> (NontermIdent) -> (VisitIdentifier) -> T_VisitStep 
sem_VisitStep_ChildVisit _ _ arg_visit_ = T_VisitStep (return st50) where
   {-# NOINLINE st50 #-}
   st50 = let
      v49 :: T_VisitStep_v49 
      v49 = \ (T_VisitStep_vIn49 _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIfmtMode _lhsIindex _lhsIisLast _lhsIkind _lhsIoptions _lhsIprevMaxSimRefs _lhsIruledefs _lhsIruleuses) -> ( let
         _addbang = rule546 _lhsIoptions
         (_from,_to) = rule547 _lhsIallFromToStates arg_visit_
         _lhsOdefs :: Set String
         _lhsOdefs = rule548  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule549  ()
         _lhsOlazyIntras :: Set String
         _lhsOlazyIntras = rule550  ()
         _lhsOruleKinds :: Map Identifier (Set VisitKind)
         _lhsOruleKinds = rule551  ()
         _lhsOruleUsage :: Map Identifier Int
         _lhsOruleUsage = rule552  ()
         _lhsOusedArgs :: Set String
         _lhsOusedArgs = rule553  ()
         _lhsOuses :: Map String (Maybe NonLocalAttr)
         _lhsOuses = rule554  ()
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule555  ()
         _lhsOindex :: Int
         _lhsOindex = rule556 _lhsIindex
         _lhsOisLast :: Bool
         _lhsOisLast = rule557 _lhsIisLast
         _lhsOprevMaxSimRefs :: Int
         _lhsOprevMaxSimRefs = rule558 _lhsIprevMaxSimRefs
         __result_ = T_VisitStep_vOut49 _lhsOdefs _lhsOerrors _lhsOindex _lhsOisLast _lhsOlazyIntras _lhsOprevMaxSimRefs _lhsOruleKinds _lhsOruleUsage _lhsOusedArgs _lhsOuses _lhsOvisitKinds
         in __result_ )
     in C_VisitStep_s50 v49
   {-# INLINE rule546 #-}
   {-# LINE 1606 "src-ag/ExecutionPlan2TH.ag" #-}
   rule546 = \ ((_lhsIoptions) :: Options) ->
                                                    {-# LINE 1606 "src-ag/ExecutionPlan2TH.ag" #-}
                                                    \x -> if bangpats _lhsIoptions then TH.BangP x else x
                                                    {-# LINE 4121 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule547 #-}
   {-# LINE 1642 "src-ag/ExecutionPlan2TH.ag" #-}
   rule547 = \ ((_lhsIallFromToStates) :: Map VisitIdentifier (Int,Int)) visit_ ->
                         {-# LINE 1642 "src-ag/ExecutionPlan2TH.ag" #-}
                         Map.findWithDefault (error "visit not in allFromToStates") visit_ _lhsIallFromToStates
                         {-# LINE 4127 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule548 #-}
   rule548 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule549 #-}
   rule549 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule550 #-}
   rule550 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule551 #-}
   rule551 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule552 #-}
   rule552 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule553 #-}
   rule553 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule554 #-}
   rule554 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule555 #-}
   rule555 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule556 #-}
   rule556 = \ ((_lhsIindex) :: Int) ->
     _lhsIindex
   {-# INLINE rule557 #-}
   rule557 = \ ((_lhsIisLast) :: Bool) ->
     _lhsIisLast
   {-# INLINE rule558 #-}
   rule558 = \ ((_lhsIprevMaxSimRefs) :: Int) ->
     _lhsIprevMaxSimRefs
{-# NOINLINE sem_VisitStep_PureGroup #-}
sem_VisitStep_PureGroup :: T_VisitSteps  -> (Bool) -> T_VisitStep 
sem_VisitStep_PureGroup arg_steps_ arg_ordered_ = T_VisitStep (return st50) where
   {-# NOINLINE st50 #-}
   st50 = let
      v49 :: T_VisitStep_v49 
      v49 = \ (T_VisitStep_vIn49 _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIfmtMode _lhsIindex _lhsIisLast _lhsIkind _lhsIoptions _lhsIprevMaxSimRefs _lhsIruledefs _lhsIruleuses) -> ( let
         _stepsX53 = Control.Monad.Identity.runIdentity (attach_T_VisitSteps (arg_steps_))
         (T_VisitSteps_vOut52 _stepsIdefs _stepsIerrors _stepsIindex _stepsIisLast _stepsIlazyIntras _stepsIprevMaxSimRefs _stepsIruleKinds _stepsIruleUsage _stepsIsize _stepsIusedArgs _stepsIuses _stepsIvisitKinds) = inv_VisitSteps_s53 _stepsX53 (T_VisitSteps_vIn52 _stepsOallFromToStates _stepsOallInitStates _stepsOallVisitKinds _stepsOavisitdefs _stepsOavisituses _stepsOchildTypes _stepsOfmtMode _stepsOindex _stepsOkind _stepsOoptions _stepsOprevMaxSimRefs _stepsOruledefs _stepsOruleuses)
         _stepsOfmtMode = rule559 _lhsIfmtMode
         _lhsOlazyIntras :: Set String
         _lhsOlazyIntras = rule560 _stepsIdefs _stepsIlazyIntras arg_ordered_
         _lhsOdefs :: Set String
         _lhsOdefs = rule561 _stepsIdefs
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule562 _stepsIerrors
         _lhsOruleKinds :: Map Identifier (Set VisitKind)
         _lhsOruleKinds = rule563 _stepsIruleKinds
         _lhsOruleUsage :: Map Identifier Int
         _lhsOruleUsage = rule564 _stepsIruleUsage
         _lhsOusedArgs :: Set String
         _lhsOusedArgs = rule565 _stepsIusedArgs
         _lhsOuses :: Map String (Maybe NonLocalAttr)
         _lhsOuses = rule566 _stepsIuses
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule567 _stepsIvisitKinds
         _lhsOindex :: Int
         _lhsOindex = rule568 _stepsIindex
         _lhsOisLast :: Bool
         _lhsOisLast = rule569 _stepsIisLast
         _lhsOprevMaxSimRefs :: Int
         _lhsOprevMaxSimRefs = rule570 _stepsIprevMaxSimRefs
         _stepsOallFromToStates = rule571 _lhsIallFromToStates
         _stepsOallInitStates = rule572 _lhsIallInitStates
         _stepsOallVisitKinds = rule573 _lhsIallVisitKinds
         _stepsOavisitdefs = rule574 _lhsIavisitdefs
         _stepsOavisituses = rule575 _lhsIavisituses
         _stepsOchildTypes = rule576 _lhsIchildTypes
         _stepsOindex = rule577 _lhsIindex
         _stepsOkind = rule578 _lhsIkind
         _stepsOoptions = rule579 _lhsIoptions
         _stepsOprevMaxSimRefs = rule580 _lhsIprevMaxSimRefs
         _stepsOruledefs = rule581 _lhsIruledefs
         _stepsOruleuses = rule582 _lhsIruleuses
         __result_ = T_VisitStep_vOut49 _lhsOdefs _lhsOerrors _lhsOindex _lhsOisLast _lhsOlazyIntras _lhsOprevMaxSimRefs _lhsOruleKinds _lhsOruleUsage _lhsOusedArgs _lhsOuses _lhsOvisitKinds
         in __result_ )
     in C_VisitStep_s50 v49
   {-# INLINE rule559 #-}
   {-# LINE 897 "src-ag/ExecutionPlan2TH.ag" #-}
   rule559 = \ ((_lhsIfmtMode) :: FormatMode) ->
                    {-# LINE 897 "src-ag/ExecutionPlan2TH.ag" #-}
                    case _lhsIfmtMode of
                      FormatDo      -> FormatLetDecl
                      mode          -> mode
                    {-# LINE 4215 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule560 #-}
   {-# LINE 1451 "src-ag/ExecutionPlan2TH.ag" #-}
   rule560 = \ ((_stepsIdefs) :: Set String) ((_stepsIlazyIntras) :: Set String) ordered_ ->
                     {-# LINE 1451 "src-ag/ExecutionPlan2TH.ag" #-}
                     if ordered_
                     then _stepsIlazyIntras
                     else _stepsIdefs
                     {-# LINE 4223 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule561 #-}
   rule561 = \ ((_stepsIdefs) :: Set String) ->
     _stepsIdefs
   {-# INLINE rule562 #-}
   rule562 = \ ((_stepsIerrors) :: Seq Error) ->
     _stepsIerrors
   {-# INLINE rule563 #-}
   rule563 = \ ((_stepsIruleKinds) :: Map Identifier (Set VisitKind)) ->
     _stepsIruleKinds
   {-# INLINE rule564 #-}
   rule564 = \ ((_stepsIruleUsage) :: Map Identifier Int) ->
     _stepsIruleUsage
   {-# INLINE rule565 #-}
   rule565 = \ ((_stepsIusedArgs) :: Set String) ->
     _stepsIusedArgs
   {-# INLINE rule566 #-}
   rule566 = \ ((_stepsIuses) :: Map String (Maybe NonLocalAttr)) ->
     _stepsIuses
   {-# INLINE rule567 #-}
   rule567 = \ ((_stepsIvisitKinds) :: Map VisitIdentifier VisitKind) ->
     _stepsIvisitKinds
   {-# INLINE rule568 #-}
   rule568 = \ ((_stepsIindex) :: Int) ->
     _stepsIindex
   {-# INLINE rule569 #-}
   rule569 = \ ((_stepsIisLast) :: Bool) ->
     _stepsIisLast
   {-# INLINE rule570 #-}
   rule570 = \ ((_stepsIprevMaxSimRefs) :: Int) ->
     _stepsIprevMaxSimRefs
   {-# INLINE rule571 #-}
   rule571 = \ ((_lhsIallFromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _lhsIallFromToStates
   {-# INLINE rule572 #-}
   rule572 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule573 #-}
   rule573 = \ ((_lhsIallVisitKinds) :: Map VisitIdentifier VisitKind) ->
     _lhsIallVisitKinds
   {-# INLINE rule574 #-}
   rule574 = \ ((_lhsIavisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisitdefs
   {-# INLINE rule575 #-}
   rule575 = \ ((_lhsIavisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisituses
   {-# INLINE rule576 #-}
   rule576 = \ ((_lhsIchildTypes) :: Map Identifier Type) ->
     _lhsIchildTypes
   {-# INLINE rule577 #-}
   rule577 = \ ((_lhsIindex) :: Int) ->
     _lhsIindex
   {-# INLINE rule578 #-}
   rule578 = \ ((_lhsIkind) :: VisitKind) ->
     _lhsIkind
   {-# INLINE rule579 #-}
   rule579 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule580 #-}
   rule580 = \ ((_lhsIprevMaxSimRefs) :: Int) ->
     _lhsIprevMaxSimRefs
   {-# INLINE rule581 #-}
   rule581 = \ ((_lhsIruledefs) :: Map Identifier (Set String)) ->
     _lhsIruledefs
   {-# INLINE rule582 #-}
   rule582 = \ ((_lhsIruleuses) :: Map Identifier (Map String (Maybe NonLocalAttr))) ->
     _lhsIruleuses
{-# NOINLINE sem_VisitStep_Sim #-}
sem_VisitStep_Sim :: T_VisitSteps  -> T_VisitStep 
sem_VisitStep_Sim arg_steps_ = T_VisitStep (return st50) where
   {-# NOINLINE st50 #-}
   st50 = let
      v49 :: T_VisitStep_v49 
      v49 = \ (T_VisitStep_vIn49 _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIfmtMode _lhsIindex _lhsIisLast _lhsIkind _lhsIoptions _lhsIprevMaxSimRefs _lhsIruledefs _lhsIruleuses) -> ( let
         _stepsX53 = Control.Monad.Identity.runIdentity (attach_T_VisitSteps (arg_steps_))
         (T_VisitSteps_vOut52 _stepsIdefs _stepsIerrors _stepsIindex _stepsIisLast _stepsIlazyIntras _stepsIprevMaxSimRefs _stepsIruleKinds _stepsIruleUsage _stepsIsize _stepsIusedArgs _stepsIuses _stepsIvisitKinds) = inv_VisitSteps_s53 _stepsX53 (T_VisitSteps_vIn52 _stepsOallFromToStates _stepsOallInitStates _stepsOallVisitKinds _stepsOavisitdefs _stepsOavisituses _stepsOchildTypes _stepsOfmtMode _stepsOindex _stepsOkind _stepsOoptions _stepsOprevMaxSimRefs _stepsOruledefs _stepsOruleuses)
         _lhsOdefs :: Set String
         _lhsOdefs = rule583 _stepsIdefs
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule584 _stepsIerrors
         _lhsOlazyIntras :: Set String
         _lhsOlazyIntras = rule585 _stepsIlazyIntras
         _lhsOruleKinds :: Map Identifier (Set VisitKind)
         _lhsOruleKinds = rule586 _stepsIruleKinds
         _lhsOruleUsage :: Map Identifier Int
         _lhsOruleUsage = rule587 _stepsIruleUsage
         _lhsOusedArgs :: Set String
         _lhsOusedArgs = rule588 _stepsIusedArgs
         _lhsOuses :: Map String (Maybe NonLocalAttr)
         _lhsOuses = rule589 _stepsIuses
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule590 _stepsIvisitKinds
         _lhsOindex :: Int
         _lhsOindex = rule591 _stepsIindex
         _lhsOisLast :: Bool
         _lhsOisLast = rule592 _stepsIisLast
         _lhsOprevMaxSimRefs :: Int
         _lhsOprevMaxSimRefs = rule593 _stepsIprevMaxSimRefs
         _stepsOallFromToStates = rule594 _lhsIallFromToStates
         _stepsOallInitStates = rule595 _lhsIallInitStates
         _stepsOallVisitKinds = rule596 _lhsIallVisitKinds
         _stepsOavisitdefs = rule597 _lhsIavisitdefs
         _stepsOavisituses = rule598 _lhsIavisituses
         _stepsOchildTypes = rule599 _lhsIchildTypes
         _stepsOfmtMode = rule600 _lhsIfmtMode
         _stepsOindex = rule601 _lhsIindex
         _stepsOkind = rule602 _lhsIkind
         _stepsOoptions = rule603 _lhsIoptions
         _stepsOprevMaxSimRefs = rule604 _lhsIprevMaxSimRefs
         _stepsOruledefs = rule605 _lhsIruledefs
         _stepsOruleuses = rule606 _lhsIruleuses
         __result_ = T_VisitStep_vOut49 _lhsOdefs _lhsOerrors _lhsOindex _lhsOisLast _lhsOlazyIntras _lhsOprevMaxSimRefs _lhsOruleKinds _lhsOruleUsage _lhsOusedArgs _lhsOuses _lhsOvisitKinds
         in __result_ )
     in C_VisitStep_s50 v49
   {-# INLINE rule583 #-}
   rule583 = \ ((_stepsIdefs) :: Set String) ->
     _stepsIdefs
   {-# INLINE rule584 #-}
   rule584 = \ ((_stepsIerrors) :: Seq Error) ->
     _stepsIerrors
   {-# INLINE rule585 #-}
   rule585 = \ ((_stepsIlazyIntras) :: Set String) ->
     _stepsIlazyIntras
   {-# INLINE rule586 #-}
   rule586 = \ ((_stepsIruleKinds) :: Map Identifier (Set VisitKind)) ->
     _stepsIruleKinds
   {-# INLINE rule587 #-}
   rule587 = \ ((_stepsIruleUsage) :: Map Identifier Int) ->
     _stepsIruleUsage
   {-# INLINE rule588 #-}
   rule588 = \ ((_stepsIusedArgs) :: Set String) ->
     _stepsIusedArgs
   {-# INLINE rule589 #-}
   rule589 = \ ((_stepsIuses) :: Map String (Maybe NonLocalAttr)) ->
     _stepsIuses
   {-# INLINE rule590 #-}
   rule590 = \ ((_stepsIvisitKinds) :: Map VisitIdentifier VisitKind) ->
     _stepsIvisitKinds
   {-# INLINE rule591 #-}
   rule591 = \ ((_stepsIindex) :: Int) ->
     _stepsIindex
   {-# INLINE rule592 #-}
   rule592 = \ ((_stepsIisLast) :: Bool) ->
     _stepsIisLast
   {-# INLINE rule593 #-}
   rule593 = \ ((_stepsIprevMaxSimRefs) :: Int) ->
     _stepsIprevMaxSimRefs
   {-# INLINE rule594 #-}
   rule594 = \ ((_lhsIallFromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _lhsIallFromToStates
   {-# INLINE rule595 #-}
   rule595 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule596 #-}
   rule596 = \ ((_lhsIallVisitKinds) :: Map VisitIdentifier VisitKind) ->
     _lhsIallVisitKinds
   {-# INLINE rule597 #-}
   rule597 = \ ((_lhsIavisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisitdefs
   {-# INLINE rule598 #-}
   rule598 = \ ((_lhsIavisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisituses
   {-# INLINE rule599 #-}
   rule599 = \ ((_lhsIchildTypes) :: Map Identifier Type) ->
     _lhsIchildTypes
   {-# INLINE rule600 #-}
   rule600 = \ ((_lhsIfmtMode) :: FormatMode) ->
     _lhsIfmtMode
   {-# INLINE rule601 #-}
   rule601 = \ ((_lhsIindex) :: Int) ->
     _lhsIindex
   {-# INLINE rule602 #-}
   rule602 = \ ((_lhsIkind) :: VisitKind) ->
     _lhsIkind
   {-# INLINE rule603 #-}
   rule603 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule604 #-}
   rule604 = \ ((_lhsIprevMaxSimRefs) :: Int) ->
     _lhsIprevMaxSimRefs
   {-# INLINE rule605 #-}
   rule605 = \ ((_lhsIruledefs) :: Map Identifier (Set String)) ->
     _lhsIruledefs
   {-# INLINE rule606 #-}
   rule606 = \ ((_lhsIruleuses) :: Map Identifier (Map String (Maybe NonLocalAttr))) ->
     _lhsIruleuses
{-# NOINLINE sem_VisitStep_ChildIntro #-}
sem_VisitStep_ChildIntro :: (Identifier) -> T_VisitStep 
sem_VisitStep_ChildIntro _ = T_VisitStep (return st50) where
   {-# NOINLINE st50 #-}
   st50 = let
      v49 :: T_VisitStep_v49 
      v49 = \ (T_VisitStep_vIn49 _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIfmtMode _lhsIindex _lhsIisLast _lhsIkind _lhsIoptions _lhsIprevMaxSimRefs _lhsIruledefs _lhsIruleuses) -> ( let
         _lhsOdefs :: Set String
         _lhsOdefs = rule607  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule608  ()
         _lhsOlazyIntras :: Set String
         _lhsOlazyIntras = rule609  ()
         _lhsOruleKinds :: Map Identifier (Set VisitKind)
         _lhsOruleKinds = rule610  ()
         _lhsOruleUsage :: Map Identifier Int
         _lhsOruleUsage = rule611  ()
         _lhsOusedArgs :: Set String
         _lhsOusedArgs = rule612  ()
         _lhsOuses :: Map String (Maybe NonLocalAttr)
         _lhsOuses = rule613  ()
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule614  ()
         _lhsOindex :: Int
         _lhsOindex = rule615 _lhsIindex
         _lhsOisLast :: Bool
         _lhsOisLast = rule616 _lhsIisLast
         _lhsOprevMaxSimRefs :: Int
         _lhsOprevMaxSimRefs = rule617 _lhsIprevMaxSimRefs
         __result_ = T_VisitStep_vOut49 _lhsOdefs _lhsOerrors _lhsOindex _lhsOisLast _lhsOlazyIntras _lhsOprevMaxSimRefs _lhsOruleKinds _lhsOruleUsage _lhsOusedArgs _lhsOuses _lhsOvisitKinds
         in __result_ )
     in C_VisitStep_s50 v49
   {-# INLINE rule607 #-}
   rule607 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule608 #-}
   rule608 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule609 #-}
   rule609 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule610 #-}
   rule610 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule611 #-}
   rule611 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule612 #-}
   rule612 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule613 #-}
   rule613 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule614 #-}
   rule614 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule615 #-}
   rule615 = \ ((_lhsIindex) :: Int) ->
     _lhsIindex
   {-# INLINE rule616 #-}
   rule616 = \ ((_lhsIisLast) :: Bool) ->
     _lhsIisLast
   {-# INLINE rule617 #-}
   rule617 = \ ((_lhsIprevMaxSimRefs) :: Int) ->
     _lhsIprevMaxSimRefs

-- VisitSteps --------------------------------------------------
-- wrapper
data Inh_VisitSteps  = Inh_VisitSteps { allFromToStates_Inh_VisitSteps :: (Map VisitIdentifier (Int,Int)), allInitStates_Inh_VisitSteps :: (Map NontermIdent Int), allVisitKinds_Inh_VisitSteps :: (Map VisitIdentifier VisitKind), avisitdefs_Inh_VisitSteps :: (Map VisitIdentifier (Set Identifier)), avisituses_Inh_VisitSteps :: (Map VisitIdentifier (Set Identifier)), childTypes_Inh_VisitSteps :: (Map Identifier Type), fmtMode_Inh_VisitSteps :: (FormatMode), index_Inh_VisitSteps :: (Int), kind_Inh_VisitSteps :: (VisitKind), options_Inh_VisitSteps :: (Options), prevMaxSimRefs_Inh_VisitSteps :: (Int), ruledefs_Inh_VisitSteps :: (Map Identifier (Set String)), ruleuses_Inh_VisitSteps :: (Map Identifier (Map String (Maybe NonLocalAttr))) }
data Syn_VisitSteps  = Syn_VisitSteps { defs_Syn_VisitSteps :: (Set String), errors_Syn_VisitSteps :: (Seq Error), index_Syn_VisitSteps :: (Int), isLast_Syn_VisitSteps :: (Bool), lazyIntras_Syn_VisitSteps :: (Set String), prevMaxSimRefs_Syn_VisitSteps :: (Int), ruleKinds_Syn_VisitSteps :: (Map Identifier (Set VisitKind)), ruleUsage_Syn_VisitSteps :: (Map Identifier Int), size_Syn_VisitSteps :: (Int), usedArgs_Syn_VisitSteps :: (Set String), uses_Syn_VisitSteps :: (Map String (Maybe NonLocalAttr)), visitKinds_Syn_VisitSteps :: (Map VisitIdentifier VisitKind) }
{-# INLINABLE wrap_VisitSteps #-}
wrap_VisitSteps :: T_VisitSteps  -> Inh_VisitSteps  -> (Syn_VisitSteps )
wrap_VisitSteps (T_VisitSteps act) (Inh_VisitSteps _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIfmtMode _lhsIindex _lhsIkind _lhsIoptions _lhsIprevMaxSimRefs _lhsIruledefs _lhsIruleuses) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg52 = T_VisitSteps_vIn52 _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIfmtMode _lhsIindex _lhsIkind _lhsIoptions _lhsIprevMaxSimRefs _lhsIruledefs _lhsIruleuses
        (T_VisitSteps_vOut52 _lhsOdefs _lhsOerrors _lhsOindex _lhsOisLast _lhsOlazyIntras _lhsOprevMaxSimRefs _lhsOruleKinds _lhsOruleUsage _lhsOsize _lhsOusedArgs _lhsOuses _lhsOvisitKinds) <- return (inv_VisitSteps_s53 sem arg52)
        return (Syn_VisitSteps _lhsOdefs _lhsOerrors _lhsOindex _lhsOisLast _lhsOlazyIntras _lhsOprevMaxSimRefs _lhsOruleKinds _lhsOruleUsage _lhsOsize _lhsOusedArgs _lhsOuses _lhsOvisitKinds)
   )

-- cata
{-# NOINLINE sem_VisitSteps #-}
sem_VisitSteps :: VisitSteps  -> T_VisitSteps 
sem_VisitSteps list = Prelude.foldr sem_VisitSteps_Cons sem_VisitSteps_Nil (Prelude.map sem_VisitStep list)

-- semantic domain
newtype T_VisitSteps  = T_VisitSteps {
                                     attach_T_VisitSteps :: Identity (T_VisitSteps_s53 )
                                     }
newtype T_VisitSteps_s53  = C_VisitSteps_s53 {
                                             inv_VisitSteps_s53 :: (T_VisitSteps_v52 )
                                             }
data T_VisitSteps_s54  = C_VisitSteps_s54
type T_VisitSteps_v52  = (T_VisitSteps_vIn52 ) -> (T_VisitSteps_vOut52 )
data T_VisitSteps_vIn52  = T_VisitSteps_vIn52 (Map VisitIdentifier (Int,Int)) (Map NontermIdent Int) (Map VisitIdentifier VisitKind) (Map VisitIdentifier (Set Identifier)) (Map VisitIdentifier (Set Identifier)) (Map Identifier Type) (FormatMode) (Int) (VisitKind) (Options) (Int) (Map Identifier (Set String)) (Map Identifier (Map String (Maybe NonLocalAttr)))
data T_VisitSteps_vOut52  = T_VisitSteps_vOut52 (Set String) (Seq Error) (Int) (Bool) (Set String) (Int) (Map Identifier (Set VisitKind)) (Map Identifier Int) (Int) (Set String) (Map String (Maybe NonLocalAttr)) (Map VisitIdentifier VisitKind)
{-# NOINLINE sem_VisitSteps_Cons #-}
sem_VisitSteps_Cons :: T_VisitStep  -> T_VisitSteps  -> T_VisitSteps 
sem_VisitSteps_Cons arg_hd_ arg_tl_ = T_VisitSteps (return st53) where
   {-# NOINLINE st53 #-}
   st53 = let
      v52 :: T_VisitSteps_v52 
      v52 = \ (T_VisitSteps_vIn52 _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIfmtMode _lhsIindex _lhsIkind _lhsIoptions _lhsIprevMaxSimRefs _lhsIruledefs _lhsIruleuses) -> ( let
         _hdX50 = Control.Monad.Identity.runIdentity (attach_T_VisitStep (arg_hd_))
         _tlX53 = Control.Monad.Identity.runIdentity (attach_T_VisitSteps (arg_tl_))
         (T_VisitStep_vOut49 _hdIdefs _hdIerrors _hdIindex _hdIisLast _hdIlazyIntras _hdIprevMaxSimRefs _hdIruleKinds _hdIruleUsage _hdIusedArgs _hdIuses _hdIvisitKinds) = inv_VisitStep_s50 _hdX50 (T_VisitStep_vIn49 _hdOallFromToStates _hdOallInitStates _hdOallVisitKinds _hdOavisitdefs _hdOavisituses _hdOchildTypes _hdOfmtMode _hdOindex _hdOisLast _hdOkind _hdOoptions _hdOprevMaxSimRefs _hdOruledefs _hdOruleuses)
         (T_VisitSteps_vOut52 _tlIdefs _tlIerrors _tlIindex _tlIisLast _tlIlazyIntras _tlIprevMaxSimRefs _tlIruleKinds _tlIruleUsage _tlIsize _tlIusedArgs _tlIuses _tlIvisitKinds) = inv_VisitSteps_s53 _tlX53 (T_VisitSteps_vIn52 _tlOallFromToStates _tlOallInitStates _tlOallVisitKinds _tlOavisitdefs _tlOavisituses _tlOchildTypes _tlOfmtMode _tlOindex _tlOkind _tlOoptions _tlOprevMaxSimRefs _tlOruledefs _tlOruleuses)
         _lhsOdefs :: Set String
         _lhsOdefs = rule618 _hdIdefs _tlIdefs
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule619 _hdIerrors _tlIerrors
         _lhsOlazyIntras :: Set String
         _lhsOlazyIntras = rule620 _hdIlazyIntras _tlIlazyIntras
         _lhsOruleKinds :: Map Identifier (Set VisitKind)
         _lhsOruleKinds = rule621 _hdIruleKinds _tlIruleKinds
         _lhsOruleUsage :: Map Identifier Int
         _lhsOruleUsage = rule622 _hdIruleUsage _tlIruleUsage
         _lhsOusedArgs :: Set String
         _lhsOusedArgs = rule623 _hdIusedArgs _tlIusedArgs
         _lhsOuses :: Map String (Maybe NonLocalAttr)
         _lhsOuses = rule624 _hdIuses _tlIuses
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule625 _hdIvisitKinds _tlIvisitKinds
         _lhsOindex :: Int
         _lhsOindex = rule626 _tlIindex
         _lhsOisLast :: Bool
         _lhsOisLast = rule627 _tlIisLast
         _lhsOprevMaxSimRefs :: Int
         _lhsOprevMaxSimRefs = rule628 _tlIprevMaxSimRefs
         _lhsOsize :: Int
         _lhsOsize = rule629 _tlIsize
         _hdOallFromToStates = rule630 _lhsIallFromToStates
         _hdOallInitStates = rule631 _lhsIallInitStates
         _hdOallVisitKinds = rule632 _lhsIallVisitKinds
         _hdOavisitdefs = rule633 _lhsIavisitdefs
         _hdOavisituses = rule634 _lhsIavisituses
         _hdOchildTypes = rule635 _lhsIchildTypes
         _hdOfmtMode = rule636 _lhsIfmtMode
         _hdOindex = rule637 _lhsIindex
         _hdOisLast = rule638  ()
         _hdOkind = rule639 _lhsIkind
         _hdOoptions = rule640 _lhsIoptions
         _hdOprevMaxSimRefs = rule641 _lhsIprevMaxSimRefs
         _hdOruledefs = rule642 _lhsIruledefs
         _hdOruleuses = rule643 _lhsIruleuses
         _tlOallFromToStates = rule644 _lhsIallFromToStates
         _tlOallInitStates = rule645 _lhsIallInitStates
         _tlOallVisitKinds = rule646 _lhsIallVisitKinds
         _tlOavisitdefs = rule647 _lhsIavisitdefs
         _tlOavisituses = rule648 _lhsIavisituses
         _tlOchildTypes = rule649 _lhsIchildTypes
         _tlOfmtMode = rule650 _lhsIfmtMode
         _tlOindex = rule651 _hdIindex
         _tlOkind = rule652 _lhsIkind
         _tlOoptions = rule653 _lhsIoptions
         _tlOprevMaxSimRefs = rule654 _hdIprevMaxSimRefs
         _tlOruledefs = rule655 _lhsIruledefs
         _tlOruleuses = rule656 _lhsIruleuses
         __result_ = T_VisitSteps_vOut52 _lhsOdefs _lhsOerrors _lhsOindex _lhsOisLast _lhsOlazyIntras _lhsOprevMaxSimRefs _lhsOruleKinds _lhsOruleUsage _lhsOsize _lhsOusedArgs _lhsOuses _lhsOvisitKinds
         in __result_ )
     in C_VisitSteps_s53 v52
   {-# INLINE rule618 #-}
   rule618 = \ ((_hdIdefs) :: Set String) ((_tlIdefs) :: Set String) ->
     _hdIdefs `Set.union` _tlIdefs
   {-# INLINE rule619 #-}
   rule619 = \ ((_hdIerrors) :: Seq Error) ((_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# INLINE rule620 #-}
   rule620 = \ ((_hdIlazyIntras) :: Set String) ((_tlIlazyIntras) :: Set String) ->
     _hdIlazyIntras `Set.union` _tlIlazyIntras
   {-# INLINE rule621 #-}
   rule621 = \ ((_hdIruleKinds) :: Map Identifier (Set VisitKind)) ((_tlIruleKinds) :: Map Identifier (Set VisitKind)) ->
     _hdIruleKinds `unionWithMappend` _tlIruleKinds
   {-# INLINE rule622 #-}
   rule622 = \ ((_hdIruleUsage) :: Map Identifier Int) ((_tlIruleUsage) :: Map Identifier Int) ->
     _hdIruleUsage `unionWithSum` _tlIruleUsage
   {-# INLINE rule623 #-}
   rule623 = \ ((_hdIusedArgs) :: Set String) ((_tlIusedArgs) :: Set String) ->
     _hdIusedArgs `Set.union` _tlIusedArgs
   {-# INLINE rule624 #-}
   rule624 = \ ((_hdIuses) :: Map String (Maybe NonLocalAttr)) ((_tlIuses) :: Map String (Maybe NonLocalAttr)) ->
     _hdIuses `Map.union` _tlIuses
   {-# INLINE rule625 #-}
   rule625 = \ ((_hdIvisitKinds) :: Map VisitIdentifier VisitKind) ((_tlIvisitKinds) :: Map VisitIdentifier VisitKind) ->
     _hdIvisitKinds `mappend` _tlIvisitKinds
   {-# INLINE rule626 #-}
   rule626 = \ ((_tlIindex) :: Int) ->
     _tlIindex
   {-# INLINE rule627 #-}
   rule627 = \ ((_tlIisLast) :: Bool) ->
     _tlIisLast
   {-# INLINE rule628 #-}
   rule628 = \ ((_tlIprevMaxSimRefs) :: Int) ->
     _tlIprevMaxSimRefs
   {-# INLINE rule629 #-}
   rule629 = \ ((_tlIsize) :: Int) ->
     _tlIsize
   {-# INLINE rule630 #-}
   rule630 = \ ((_lhsIallFromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _lhsIallFromToStates
   {-# INLINE rule631 #-}
   rule631 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule632 #-}
   rule632 = \ ((_lhsIallVisitKinds) :: Map VisitIdentifier VisitKind) ->
     _lhsIallVisitKinds
   {-# INLINE rule633 #-}
   rule633 = \ ((_lhsIavisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisitdefs
   {-# INLINE rule634 #-}
   rule634 = \ ((_lhsIavisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisituses
   {-# INLINE rule635 #-}
   rule635 = \ ((_lhsIchildTypes) :: Map Identifier Type) ->
     _lhsIchildTypes
   {-# INLINE rule636 #-}
   rule636 = \ ((_lhsIfmtMode) :: FormatMode) ->
     _lhsIfmtMode
   {-# INLINE rule637 #-}
   rule637 = \ ((_lhsIindex) :: Int) ->
     _lhsIindex
   {-# INLINE rule638 #-}
   rule638 = \  (_ :: ()) ->
     error "missing rule: VisitSteps.Cons.hd.isLast"
   {-# INLINE rule639 #-}
   rule639 = \ ((_lhsIkind) :: VisitKind) ->
     _lhsIkind
   {-# INLINE rule640 #-}
   rule640 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule641 #-}
   rule641 = \ ((_lhsIprevMaxSimRefs) :: Int) ->
     _lhsIprevMaxSimRefs
   {-# INLINE rule642 #-}
   rule642 = \ ((_lhsIruledefs) :: Map Identifier (Set String)) ->
     _lhsIruledefs
   {-# INLINE rule643 #-}
   rule643 = \ ((_lhsIruleuses) :: Map Identifier (Map String (Maybe NonLocalAttr))) ->
     _lhsIruleuses
   {-# INLINE rule644 #-}
   rule644 = \ ((_lhsIallFromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _lhsIallFromToStates
   {-# INLINE rule645 #-}
   rule645 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule646 #-}
   rule646 = \ ((_lhsIallVisitKinds) :: Map VisitIdentifier VisitKind) ->
     _lhsIallVisitKinds
   {-# INLINE rule647 #-}
   rule647 = \ ((_lhsIavisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisitdefs
   {-# INLINE rule648 #-}
   rule648 = \ ((_lhsIavisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisituses
   {-# INLINE rule649 #-}
   rule649 = \ ((_lhsIchildTypes) :: Map Identifier Type) ->
     _lhsIchildTypes
   {-# INLINE rule650 #-}
   rule650 = \ ((_lhsIfmtMode) :: FormatMode) ->
     _lhsIfmtMode
   {-# INLINE rule651 #-}
   rule651 = \ ((_hdIindex) :: Int) ->
     _hdIindex
   {-# INLINE rule652 #-}
   rule652 = \ ((_lhsIkind) :: VisitKind) ->
     _lhsIkind
   {-# INLINE rule653 #-}
   rule653 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule654 #-}
   rule654 = \ ((_hdIprevMaxSimRefs) :: Int) ->
     _hdIprevMaxSimRefs
   {-# INLINE rule655 #-}
   rule655 = \ ((_lhsIruledefs) :: Map Identifier (Set String)) ->
     _lhsIruledefs
   {-# INLINE rule656 #-}
   rule656 = \ ((_lhsIruleuses) :: Map Identifier (Map String (Maybe NonLocalAttr))) ->
     _lhsIruleuses
{-# NOINLINE sem_VisitSteps_Nil #-}
sem_VisitSteps_Nil ::  T_VisitSteps 
sem_VisitSteps_Nil  = T_VisitSteps (return st53) where
   {-# NOINLINE st53 #-}
   st53 = let
      v52 :: T_VisitSteps_v52 
      v52 = \ (T_VisitSteps_vIn52 _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIfmtMode _lhsIindex _lhsIkind _lhsIoptions _lhsIprevMaxSimRefs _lhsIruledefs _lhsIruleuses) -> ( let
         _lhsOdefs :: Set String
         _lhsOdefs = rule657  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule658  ()
         _lhsOlazyIntras :: Set String
         _lhsOlazyIntras = rule659  ()
         _lhsOruleKinds :: Map Identifier (Set VisitKind)
         _lhsOruleKinds = rule660  ()
         _lhsOruleUsage :: Map Identifier Int
         _lhsOruleUsage = rule661  ()
         _lhsOusedArgs :: Set String
         _lhsOusedArgs = rule662  ()
         _lhsOuses :: Map String (Maybe NonLocalAttr)
         _lhsOuses = rule663  ()
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule664  ()
         _lhsOindex :: Int
         _lhsOindex = rule665 _lhsIindex
         _lhsOisLast :: Bool
         _lhsOisLast = rule666  ()
         _lhsOprevMaxSimRefs :: Int
         _lhsOprevMaxSimRefs = rule667 _lhsIprevMaxSimRefs
         _lhsOsize :: Int
         _lhsOsize = rule668  ()
         __result_ = T_VisitSteps_vOut52 _lhsOdefs _lhsOerrors _lhsOindex _lhsOisLast _lhsOlazyIntras _lhsOprevMaxSimRefs _lhsOruleKinds _lhsOruleUsage _lhsOsize _lhsOusedArgs _lhsOuses _lhsOvisitKinds
         in __result_ )
     in C_VisitSteps_s53 v52
   {-# INLINE rule657 #-}
   rule657 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule658 #-}
   rule658 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule659 #-}
   rule659 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule660 #-}
   rule660 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule661 #-}
   rule661 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule662 #-}
   rule662 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule663 #-}
   rule663 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule664 #-}
   rule664 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule665 #-}
   rule665 = \ ((_lhsIindex) :: Int) ->
     _lhsIindex
   {-# INLINE rule666 #-}
   rule666 = \  (_ :: ()) ->
     error "missing rule: VisitSteps.Nil.lhs.isLast"
   {-# INLINE rule667 #-}
   rule667 = \ ((_lhsIprevMaxSimRefs) :: Int) ->
     _lhsIprevMaxSimRefs
   {-# INLINE rule668 #-}
   rule668 = \  (_ :: ()) ->
     error "missing rule: VisitSteps.Nil.lhs.size"

-- Visits ------------------------------------------------------
-- wrapper
data Inh_Visits  = Inh_Visits { allFromToStates_Inh_Visits :: (Map VisitIdentifier (Int,Int)), allInhmap_Inh_Visits :: (Map NontermIdent Attributes), allInitStates_Inh_Visits :: (Map NontermIdent Int), allSynmap_Inh_Visits :: (Map NontermIdent Attributes), allVisitKinds_Inh_Visits :: (Map VisitIdentifier VisitKind), allintramap_Inh_Visits :: (Map StateIdentifier (Map String (Maybe NonLocalAttr))), avisitdefs_Inh_Visits :: (Map VisitIdentifier (Set Identifier)), avisituses_Inh_Visits :: (Map VisitIdentifier (Set Identifier)), childTypes_Inh_Visits :: (Map Identifier Type), con_Inh_Visits :: (ConstructorIdent), inhmap_Inh_Visits :: (Attributes), nextVisits_Inh_Visits :: (Map StateIdentifier StateCtx), nt_Inh_Visits :: (NontermIdent), options_Inh_Visits :: (Options), params_Inh_Visits :: ([Identifier]), prevVisits_Inh_Visits :: (Map StateIdentifier StateCtx), ruledefs_Inh_Visits :: (Map Identifier (Set String)), ruleuses_Inh_Visits :: (Map Identifier (Map String (Maybe NonLocalAttr))), synmap_Inh_Visits :: (Attributes), terminaldefs_Inh_Visits :: (Set String) }
data Syn_Visits  = Syn_Visits { allvisits_Syn_Visits :: ([VisitStateState]), errors_Syn_Visits :: (Seq Error), fromToStates_Syn_Visits :: (Map VisitIdentifier (Int,Int)), intramap_Syn_Visits :: (Map StateIdentifier (Map String (Maybe NonLocalAttr))), lazyIntras_Syn_Visits :: (Set String), ruleKinds_Syn_Visits :: (Map Identifier (Set VisitKind)), ruleUsage_Syn_Visits :: (Map Identifier Int), usedArgs_Syn_Visits :: (Set String), visitKinds_Syn_Visits :: (Map VisitIdentifier VisitKind), visitdefs_Syn_Visits :: (Map VisitIdentifier (Set Identifier)), visituses_Syn_Visits :: (Map VisitIdentifier (Set Identifier)) }
{-# INLINABLE wrap_Visits #-}
wrap_Visits :: T_Visits  -> Inh_Visits  -> (Syn_Visits )
wrap_Visits (T_Visits act) (Inh_Visits _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallintramap _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIcon _lhsIinhmap _lhsInextVisits _lhsInt _lhsIoptions _lhsIparams _lhsIprevVisits _lhsIruledefs _lhsIruleuses _lhsIsynmap _lhsIterminaldefs) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg55 = T_Visits_vIn55 _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallintramap _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIcon _lhsIinhmap _lhsInextVisits _lhsInt _lhsIoptions _lhsIparams _lhsIprevVisits _lhsIruledefs _lhsIruleuses _lhsIsynmap _lhsIterminaldefs
        (T_Visits_vOut55 _lhsOallvisits _lhsOerrors _lhsOfromToStates _lhsOintramap _lhsOlazyIntras _lhsOruleKinds _lhsOruleUsage _lhsOusedArgs _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses) <- return (inv_Visits_s56 sem arg55)
        return (Syn_Visits _lhsOallvisits _lhsOerrors _lhsOfromToStates _lhsOintramap _lhsOlazyIntras _lhsOruleKinds _lhsOruleUsage _lhsOusedArgs _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses)
   )

-- cata
{-# NOINLINE sem_Visits #-}
sem_Visits :: Visits  -> T_Visits 
sem_Visits list = Prelude.foldr sem_Visits_Cons sem_Visits_Nil (Prelude.map sem_Visit list)

-- semantic domain
newtype T_Visits  = T_Visits {
                             attach_T_Visits :: Identity (T_Visits_s56 )
                             }
newtype T_Visits_s56  = C_Visits_s56 {
                                     inv_Visits_s56 :: (T_Visits_v55 )
                                     }
data T_Visits_s57  = C_Visits_s57
type T_Visits_v55  = (T_Visits_vIn55 ) -> (T_Visits_vOut55 )
data T_Visits_vIn55  = T_Visits_vIn55 (Map VisitIdentifier (Int,Int)) (Map NontermIdent Attributes) (Map NontermIdent Int) (Map NontermIdent Attributes) (Map VisitIdentifier VisitKind) (Map StateIdentifier (Map String (Maybe NonLocalAttr))) (Map VisitIdentifier (Set Identifier)) (Map VisitIdentifier (Set Identifier)) (Map Identifier Type) (ConstructorIdent) (Attributes) (Map StateIdentifier StateCtx) (NontermIdent) (Options) ([Identifier]) (Map StateIdentifier StateCtx) (Map Identifier (Set String)) (Map Identifier (Map String (Maybe NonLocalAttr))) (Attributes) (Set String)
data T_Visits_vOut55  = T_Visits_vOut55 ([VisitStateState]) (Seq Error) (Map VisitIdentifier (Int,Int)) (Map StateIdentifier (Map String (Maybe NonLocalAttr))) (Set String) (Map Identifier (Set VisitKind)) (Map Identifier Int) (Set String) (Map VisitIdentifier VisitKind) (Map VisitIdentifier (Set Identifier)) (Map VisitIdentifier (Set Identifier))
{-# NOINLINE sem_Visits_Cons #-}
sem_Visits_Cons :: T_Visit  -> T_Visits  -> T_Visits 
sem_Visits_Cons arg_hd_ arg_tl_ = T_Visits (return st56) where
   {-# NOINLINE st56 #-}
   st56 = let
      v55 :: T_Visits_v55 
      v55 = \ (T_Visits_vIn55 _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallintramap _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIcon _lhsIinhmap _lhsInextVisits _lhsInt _lhsIoptions _lhsIparams _lhsIprevVisits _lhsIruledefs _lhsIruleuses _lhsIsynmap _lhsIterminaldefs) -> ( let
         _hdX47 = Control.Monad.Identity.runIdentity (attach_T_Visit (arg_hd_))
         _tlX56 = Control.Monad.Identity.runIdentity (attach_T_Visits (arg_tl_))
         (T_Visit_vOut46 _hdIallvisits _hdIerrors _hdIfromToStates _hdIintramap _hdIlazyIntras _hdIruleKinds _hdIruleUsage _hdIusedArgs _hdIvisitKinds _hdIvisitdefs _hdIvisituses) = inv_Visit_s47 _hdX47 (T_Visit_vIn46 _hdOallFromToStates _hdOallInhmap _hdOallInitStates _hdOallSynmap _hdOallVisitKinds _hdOallintramap _hdOavisitdefs _hdOavisituses _hdOchildTypes _hdOcon _hdOinhmap _hdOnextVisits _hdOnt _hdOoptions _hdOparams _hdOprevVisits _hdOruledefs _hdOruleuses _hdOsynmap _hdOterminaldefs)
         (T_Visits_vOut55 _tlIallvisits _tlIerrors _tlIfromToStates _tlIintramap _tlIlazyIntras _tlIruleKinds _tlIruleUsage _tlIusedArgs _tlIvisitKinds _tlIvisitdefs _tlIvisituses) = inv_Visits_s56 _tlX56 (T_Visits_vIn55 _tlOallFromToStates _tlOallInhmap _tlOallInitStates _tlOallSynmap _tlOallVisitKinds _tlOallintramap _tlOavisitdefs _tlOavisituses _tlOchildTypes _tlOcon _tlOinhmap _tlOnextVisits _tlOnt _tlOoptions _tlOparams _tlOprevVisits _tlOruledefs _tlOruleuses _tlOsynmap _tlOterminaldefs)
         _lhsOallvisits :: [VisitStateState]
         _lhsOallvisits = rule669 _hdIallvisits _tlIallvisits
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule670 _hdIerrors _tlIerrors
         _lhsOfromToStates :: Map VisitIdentifier (Int,Int)
         _lhsOfromToStates = rule671 _hdIfromToStates _tlIfromToStates
         _lhsOintramap :: Map StateIdentifier (Map String (Maybe NonLocalAttr))
         _lhsOintramap = rule672 _hdIintramap _tlIintramap
         _lhsOlazyIntras :: Set String
         _lhsOlazyIntras = rule673 _hdIlazyIntras _tlIlazyIntras
         _lhsOruleKinds :: Map Identifier (Set VisitKind)
         _lhsOruleKinds = rule674 _hdIruleKinds _tlIruleKinds
         _lhsOruleUsage :: Map Identifier Int
         _lhsOruleUsage = rule675 _hdIruleUsage _tlIruleUsage
         _lhsOusedArgs :: Set String
         _lhsOusedArgs = rule676 _hdIusedArgs _tlIusedArgs
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule677 _hdIvisitKinds _tlIvisitKinds
         _lhsOvisitdefs :: Map VisitIdentifier (Set Identifier)
         _lhsOvisitdefs = rule678 _hdIvisitdefs _tlIvisitdefs
         _lhsOvisituses :: Map VisitIdentifier (Set Identifier)
         _lhsOvisituses = rule679 _hdIvisituses _tlIvisituses
         _hdOallFromToStates = rule680 _lhsIallFromToStates
         _hdOallInhmap = rule681 _lhsIallInhmap
         _hdOallInitStates = rule682 _lhsIallInitStates
         _hdOallSynmap = rule683 _lhsIallSynmap
         _hdOallVisitKinds = rule684 _lhsIallVisitKinds
         _hdOallintramap = rule685 _lhsIallintramap
         _hdOavisitdefs = rule686 _lhsIavisitdefs
         _hdOavisituses = rule687 _lhsIavisituses
         _hdOchildTypes = rule688 _lhsIchildTypes
         _hdOcon = rule689 _lhsIcon
         _hdOinhmap = rule690 _lhsIinhmap
         _hdOnextVisits = rule691 _lhsInextVisits
         _hdOnt = rule692 _lhsInt
         _hdOoptions = rule693 _lhsIoptions
         _hdOparams = rule694 _lhsIparams
         _hdOprevVisits = rule695 _lhsIprevVisits
         _hdOruledefs = rule696 _lhsIruledefs
         _hdOruleuses = rule697 _lhsIruleuses
         _hdOsynmap = rule698 _lhsIsynmap
         _hdOterminaldefs = rule699 _lhsIterminaldefs
         _tlOallFromToStates = rule700 _lhsIallFromToStates
         _tlOallInhmap = rule701 _lhsIallInhmap
         _tlOallInitStates = rule702 _lhsIallInitStates
         _tlOallSynmap = rule703 _lhsIallSynmap
         _tlOallVisitKinds = rule704 _lhsIallVisitKinds
         _tlOallintramap = rule705 _lhsIallintramap
         _tlOavisitdefs = rule706 _lhsIavisitdefs
         _tlOavisituses = rule707 _lhsIavisituses
         _tlOchildTypes = rule708 _lhsIchildTypes
         _tlOcon = rule709 _lhsIcon
         _tlOinhmap = rule710 _lhsIinhmap
         _tlOnextVisits = rule711 _lhsInextVisits
         _tlOnt = rule712 _lhsInt
         _tlOoptions = rule713 _lhsIoptions
         _tlOparams = rule714 _lhsIparams
         _tlOprevVisits = rule715 _lhsIprevVisits
         _tlOruledefs = rule716 _lhsIruledefs
         _tlOruleuses = rule717 _lhsIruleuses
         _tlOsynmap = rule718 _lhsIsynmap
         _tlOterminaldefs = rule719 _lhsIterminaldefs
         __result_ = T_Visits_vOut55 _lhsOallvisits _lhsOerrors _lhsOfromToStates _lhsOintramap _lhsOlazyIntras _lhsOruleKinds _lhsOruleUsage _lhsOusedArgs _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses
         in __result_ )
     in C_Visits_s56 v55
   {-# INLINE rule669 #-}
   rule669 = \ ((_hdIallvisits) ::  VisitStateState ) ((_tlIallvisits) :: [VisitStateState]) ->
     _hdIallvisits : _tlIallvisits
   {-# INLINE rule670 #-}
   rule670 = \ ((_hdIerrors) :: Seq Error) ((_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# INLINE rule671 #-}
   rule671 = \ ((_hdIfromToStates) :: Map VisitIdentifier (Int,Int)) ((_tlIfromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _hdIfromToStates `mappend` _tlIfromToStates
   {-# INLINE rule672 #-}
   rule672 = \ ((_hdIintramap) :: Map StateIdentifier (Map String (Maybe NonLocalAttr))) ((_tlIintramap) :: Map StateIdentifier (Map String (Maybe NonLocalAttr))) ->
     _hdIintramap `uwMapUnion` _tlIintramap
   {-# INLINE rule673 #-}
   rule673 = \ ((_hdIlazyIntras) :: Set String) ((_tlIlazyIntras) :: Set String) ->
     _hdIlazyIntras `Set.union` _tlIlazyIntras
   {-# INLINE rule674 #-}
   rule674 = \ ((_hdIruleKinds) :: Map Identifier (Set VisitKind)) ((_tlIruleKinds) :: Map Identifier (Set VisitKind)) ->
     _hdIruleKinds `unionWithMappend` _tlIruleKinds
   {-# INLINE rule675 #-}
   rule675 = \ ((_hdIruleUsage) :: Map Identifier Int) ((_tlIruleUsage) :: Map Identifier Int) ->
     _hdIruleUsage `unionWithSum` _tlIruleUsage
   {-# INLINE rule676 #-}
   rule676 = \ ((_hdIusedArgs) :: Set String) ((_tlIusedArgs) :: Set String) ->
     _hdIusedArgs `Set.union` _tlIusedArgs
   {-# INLINE rule677 #-}
   rule677 = \ ((_hdIvisitKinds) :: Map VisitIdentifier VisitKind) ((_tlIvisitKinds) :: Map VisitIdentifier VisitKind) ->
     _hdIvisitKinds `mappend` _tlIvisitKinds
   {-# INLINE rule678 #-}
   rule678 = \ ((_hdIvisitdefs) :: Map VisitIdentifier (Set Identifier)) ((_tlIvisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _hdIvisitdefs `uwSetUnion` _tlIvisitdefs
   {-# INLINE rule679 #-}
   rule679 = \ ((_hdIvisituses) :: Map VisitIdentifier (Set Identifier)) ((_tlIvisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _hdIvisituses `uwSetUnion` _tlIvisituses
   {-# INLINE rule680 #-}
   rule680 = \ ((_lhsIallFromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _lhsIallFromToStates
   {-# INLINE rule681 #-}
   rule681 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule682 #-}
   rule682 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule683 #-}
   rule683 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule684 #-}
   rule684 = \ ((_lhsIallVisitKinds) :: Map VisitIdentifier VisitKind) ->
     _lhsIallVisitKinds
   {-# INLINE rule685 #-}
   rule685 = \ ((_lhsIallintramap) :: Map StateIdentifier (Map String (Maybe NonLocalAttr))) ->
     _lhsIallintramap
   {-# INLINE rule686 #-}
   rule686 = \ ((_lhsIavisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisitdefs
   {-# INLINE rule687 #-}
   rule687 = \ ((_lhsIavisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisituses
   {-# INLINE rule688 #-}
   rule688 = \ ((_lhsIchildTypes) :: Map Identifier Type) ->
     _lhsIchildTypes
   {-# INLINE rule689 #-}
   rule689 = \ ((_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# INLINE rule690 #-}
   rule690 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule691 #-}
   rule691 = \ ((_lhsInextVisits) :: Map StateIdentifier StateCtx) ->
     _lhsInextVisits
   {-# INLINE rule692 #-}
   rule692 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule693 #-}
   rule693 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule694 #-}
   rule694 = \ ((_lhsIparams) :: [Identifier]) ->
     _lhsIparams
   {-# INLINE rule695 #-}
   rule695 = \ ((_lhsIprevVisits) :: Map StateIdentifier StateCtx) ->
     _lhsIprevVisits
   {-# INLINE rule696 #-}
   rule696 = \ ((_lhsIruledefs) :: Map Identifier (Set String)) ->
     _lhsIruledefs
   {-# INLINE rule697 #-}
   rule697 = \ ((_lhsIruleuses) :: Map Identifier (Map String (Maybe NonLocalAttr))) ->
     _lhsIruleuses
   {-# INLINE rule698 #-}
   rule698 = \ ((_lhsIsynmap) :: Attributes) ->
     _lhsIsynmap
   {-# INLINE rule699 #-}
   rule699 = \ ((_lhsIterminaldefs) :: Set String) ->
     _lhsIterminaldefs
   {-# INLINE rule700 #-}
   rule700 = \ ((_lhsIallFromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _lhsIallFromToStates
   {-# INLINE rule701 #-}
   rule701 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule702 #-}
   rule702 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule703 #-}
   rule703 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule704 #-}
   rule704 = \ ((_lhsIallVisitKinds) :: Map VisitIdentifier VisitKind) ->
     _lhsIallVisitKinds
   {-# INLINE rule705 #-}
   rule705 = \ ((_lhsIallintramap) :: Map StateIdentifier (Map String (Maybe NonLocalAttr))) ->
     _lhsIallintramap
   {-# INLINE rule706 #-}
   rule706 = \ ((_lhsIavisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisitdefs
   {-# INLINE rule707 #-}
   rule707 = \ ((_lhsIavisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisituses
   {-# INLINE rule708 #-}
   rule708 = \ ((_lhsIchildTypes) :: Map Identifier Type) ->
     _lhsIchildTypes
   {-# INLINE rule709 #-}
   rule709 = \ ((_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# INLINE rule710 #-}
   rule710 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule711 #-}
   rule711 = \ ((_lhsInextVisits) :: Map StateIdentifier StateCtx) ->
     _lhsInextVisits
   {-# INLINE rule712 #-}
   rule712 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule713 #-}
   rule713 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule714 #-}
   rule714 = \ ((_lhsIparams) :: [Identifier]) ->
     _lhsIparams
   {-# INLINE rule715 #-}
   rule715 = \ ((_lhsIprevVisits) :: Map StateIdentifier StateCtx) ->
     _lhsIprevVisits
   {-# INLINE rule716 #-}
   rule716 = \ ((_lhsIruledefs) :: Map Identifier (Set String)) ->
     _lhsIruledefs
   {-# INLINE rule717 #-}
   rule717 = \ ((_lhsIruleuses) :: Map Identifier (Map String (Maybe NonLocalAttr))) ->
     _lhsIruleuses
   {-# INLINE rule718 #-}
   rule718 = \ ((_lhsIsynmap) :: Attributes) ->
     _lhsIsynmap
   {-# INLINE rule719 #-}
   rule719 = \ ((_lhsIterminaldefs) :: Set String) ->
     _lhsIterminaldefs
{-# NOINLINE sem_Visits_Nil #-}
sem_Visits_Nil ::  T_Visits 
sem_Visits_Nil  = T_Visits (return st56) where
   {-# NOINLINE st56 #-}
   st56 = let
      v55 :: T_Visits_v55 
      v55 = \ (T_Visits_vIn55 _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallintramap _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIcon _lhsIinhmap _lhsInextVisits _lhsInt _lhsIoptions _lhsIparams _lhsIprevVisits _lhsIruledefs _lhsIruleuses _lhsIsynmap _lhsIterminaldefs) -> ( let
         _lhsOallvisits :: [VisitStateState]
         _lhsOallvisits = rule720  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule721  ()
         _lhsOfromToStates :: Map VisitIdentifier (Int,Int)
         _lhsOfromToStates = rule722  ()
         _lhsOintramap :: Map StateIdentifier (Map String (Maybe NonLocalAttr))
         _lhsOintramap = rule723  ()
         _lhsOlazyIntras :: Set String
         _lhsOlazyIntras = rule724  ()
         _lhsOruleKinds :: Map Identifier (Set VisitKind)
         _lhsOruleKinds = rule725  ()
         _lhsOruleUsage :: Map Identifier Int
         _lhsOruleUsage = rule726  ()
         _lhsOusedArgs :: Set String
         _lhsOusedArgs = rule727  ()
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule728  ()
         _lhsOvisitdefs :: Map VisitIdentifier (Set Identifier)
         _lhsOvisitdefs = rule729  ()
         _lhsOvisituses :: Map VisitIdentifier (Set Identifier)
         _lhsOvisituses = rule730  ()
         __result_ = T_Visits_vOut55 _lhsOallvisits _lhsOerrors _lhsOfromToStates _lhsOintramap _lhsOlazyIntras _lhsOruleKinds _lhsOruleUsage _lhsOusedArgs _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses
         in __result_ )
     in C_Visits_s56 v55
   {-# INLINE rule720 #-}
   rule720 = \  (_ :: ()) ->
     []
   {-# INLINE rule721 #-}
   rule721 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule722 #-}
   rule722 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule723 #-}
   rule723 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule724 #-}
   rule724 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule725 #-}
   rule725 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule726 #-}
   rule726 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule727 #-}
   rule727 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule728 #-}
   rule728 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule729 #-}
   rule729 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule730 #-}
   rule730 = \  (_ :: ()) ->
     Map.empty
