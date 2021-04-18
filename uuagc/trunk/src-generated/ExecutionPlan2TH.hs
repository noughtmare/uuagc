{-# LANGUAGE Rank2Types, GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE DeriveLift #-}

{-# LANGUAGE DeriveLift #-}

{-# LANGUAGE DeriveLift #-}
module ExecutionPlan2TH where
{-# LINE 6 "src-ag/HsToken.ag" #-}

import CommonTypes
import UU.Scanner.Position(Pos)
import Language.Haskell.TH.Syntax (Lift)
import LiftOrphans ()
{-# LINE 17 "src-generated/ExecutionPlan2TH.hs" #-}

{-# LINE 6 "src-ag/Expression.ag" #-}

import UU.Scanner.Position(Pos)
import HsToken
import Language.Haskell.TH.Syntax (Lift)
import LiftOrphans ()
{-# LINE 25 "src-generated/ExecutionPlan2TH.hs" #-}

{-# LINE 6 "src-ag/Patterns.ag" #-}

-- Patterns.ag imports
import UU.Scanner.Position(Pos)
import CommonTypes (ConstructorIdent,Identifier)
import Language.Haskell.TH.Syntax (Lift)
import LiftOrphans ()
{-# LINE 34 "src-generated/ExecutionPlan2TH.hs" #-}

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
{-# LINE 48 "src-generated/ExecutionPlan2TH.hs" #-}

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
{-# LINE 81 "src-generated/ExecutionPlan2TH.hs" #-}
import Control.Monad.Identity (Identity)
import qualified Control.Monad.Identity
{-# LINE 39 "src-ag/ExecutionPlan2TH.ag" #-}

(.->) :: TH.Type -> TH.Type -> TH.Type
(.->) x y = TH.AppT (TH.AppT TH.ArrowT x) y

nobang :: TH.Bang
nobang = TH.Bang TH.NoSourceUnpackedness TH.NoSourceStrictness	
{-# LINE 91 "src-generated/ExecutionPlan2TH.hs" #-}

{-# LINE 163 "src-ag/ExecutionPlan2TH.ag" #-}

classCtxsToCxt :: ClassContext -> TH.Cxt
classCtxsToCxt cctx =
  [ foldl TH.AppT
          (TH.ConT (TH.mkName (getName con)))
          (map (TH.VarT . TH.mkName) params)
  | (con, params) <- cctx
  ]

classConstrsToCxt :: [Type] -> TH.Cxt
classConstrsToCxt = map typeToTH

typeToTH :: Type -> TH.Type
typeToTH = either (error . ("Cannot convert type: " ++)) id . Meta.parseType . typeToHaskellString (error "Self types should have been resolved by now") []

identToName :: Identifier -> TH.Name
identToName = TH.mkName . getName

quantsTH :: ([TH.TyVarBndr] -> TH.Cxt -> x -> x) -> [Identifier] -> TH.Cxt -> x -> x
quantsTH foral ids ctx con = snd $ foldr (\x ~(b, xs) -> (False, foral [] (if b then ctx else []) xs)) (True, con) ids
{-# LINE 114 "src-generated/ExecutionPlan2TH.hs" #-}

{-# LINE 199 "src-ag/ExecutionPlan2TH.ag" #-}

conTH :: TH.Name -> Either [TH.VarBangType] [TH.BangType] -> TH.Con
conTH nt (Left args) = TH.RecC nt args
conTH nt (Right args) = TH.NormalC nt args
{-# LINE 121 "src-generated/ExecutionPlan2TH.hs" #-}

{-# LINE 399 "src-ag/ExecutionPlan2TH.ag" #-}
type VisitStateState = (VisitIdentifier,StateIdentifier, StateIdentifier)
{-# LINE 125 "src-generated/ExecutionPlan2TH.hs" #-}

{-# LINE 495 "src-ag/ExecutionPlan2TH.ag" #-}

-- conNmTVisit nt vId      = "T_" >|< nt >|< "_v"    >|< vId
-- conNmTVisitIn nt vId    = "T_" >|< nt >|< "_vIn"  >|< vId
-- conNmTVisitOut nt vId   = "T_" >|< nt >|< "_vOut" >|< vId
-- conNmTNextVisit nt stId = "T_" >|< nt >|< "_s"    >|< stId

monadTypeTH :: Options -> TH.Type -> TH.Type
monadTypeTH opts
  | parallelInvoke opts = TH.AppT (TH.ConT (TH.mkName "IO"))
  | otherwise           = TH.AppT (TH.ConT (TH.mkName "Identity"))
{-# LINE 138 "src-generated/ExecutionPlan2TH.hs" #-}

{-# LINE 866 "src-ag/ExecutionPlan2TH.ag" #-}

resultValName :: String
resultValName = "__result_"

nextStName :: String
nextStName = "__st_"
{-# LINE 147 "src-generated/ExecutionPlan2TH.hs" #-}

{-# LINE 937 "src-ag/ExecutionPlan2TH.ag" #-}

parResultName :: String
parResultName = "__outcome_"

-- fmtDecl :: PP a => Bool -> FormatMode -> a -> PP_Doc
-- fmtDecl declPure fmt decl = case fmt of
--   FormatLetDecl -> pp decl
--   FormatLetLine -> "let" >#< decl >#< "in"
--   FormatDo | declPure  -> "let" >#< decl
--            | otherwise -> pp decl
{-# LINE 160 "src-generated/ExecutionPlan2TH.hs" #-}

{-# LINE 1263 "src-ag/ExecutionPlan2TH.ag" #-}

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
{-# LINE 193 "src-generated/ExecutionPlan2TH.hs" #-}

{-# LINE 1372 "src-ag/ExecutionPlan2TH.ag" #-}

unionWithSum = Map.unionWith (+)
{-# LINE 198 "src-generated/ExecutionPlan2TH.hs" #-}

{-# LINE 1395 "src-ag/ExecutionPlan2TH.ag" #-}

uwSetUnion :: (Ord a, Ord b) => Map a (Set b) -> Map a (Set b) -> Map a (Set b)
uwSetUnion = Map.unionWith Set.union

uwMapUnion :: (Ord a, Ord b) => Map a (Map b c) -> Map a (Map b c) -> Map a (Map b c)
uwMapUnion = Map.unionWith Map.union
{-# LINE 207 "src-generated/ExecutionPlan2TH.hs" #-}

{-# LINE 1612 "src-ag/ExecutionPlan2TH.ag" #-}

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
{-# LINE 227 "src-generated/ExecutionPlan2TH.hs" #-}

{-# LINE 1762 "src-ag/ExecutionPlan2TH.ag" #-}

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
{-# LINE 271 "src-generated/ExecutionPlan2TH.hs" #-}
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
         _lhsOdatatype :: Maybe TH.BangType
         _lhsOdatatype = rule4 _field arg_kind_
         _lhsOargnamesw ::  Maybe TH.Exp 
         _lhsOargnamesw = rule5 _nt arg_kind_ arg_name_
         _lhsOargtps ::   Maybe (TH.Type -> TH.Type)  
         _lhsOargtps = rule6 arg_kind_ arg_tp_
         _argpats = rule7 arg_kind_ arg_name_
         _nt = rule8 arg_tp_
         _addbang = rule9 _lhsIoptions
         _bang = rule10 _lhsIoptions
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
   {-# LINE 212 "src-ag/ExecutionPlan2TH.ag" #-}
   rule0 = \ tp_ ->
                    {-# LINE 212 "src-ag/ExecutionPlan2TH.ag" #-}
                    typeToTH (removeDeforested tp_)
                    {-# LINE 343 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule1 #-}
   {-# LINE 213 "src-ag/ExecutionPlan2TH.ag" #-}
   rule1 = \ ((_lhsIcon) :: ConstructorIdent) ((_lhsInt) :: NontermIdent) name_ ->
                     {-# LINE 213 "src-ag/ExecutionPlan2TH.ag" #-}
                     recordFieldname _lhsInt _lhsIcon name_
                     {-# LINE 349 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule2 #-}
   {-# LINE 214 "src-ag/ExecutionPlan2TH.ag" #-}
   rule2 = \ _bang _tpTH ->
                    {-# LINE 214 "src-ag/ExecutionPlan2TH.ag" #-}
                    (_bang    , _tpTH    )
                    {-# LINE 355 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule3 #-}
   {-# LINE 215 "src-ag/ExecutionPlan2TH.ag" #-}
   rule3 = \ _bang _strNm _tpTH ->
                       {-# LINE 215 "src-ag/ExecutionPlan2TH.ag" #-}
                       (TH.mkName _strNm    , _bang    , _tpTH    )
                       {-# LINE 361 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule4 #-}
   {-# LINE 216 "src-ag/ExecutionPlan2TH.ag" #-}
   rule4 = \ _field kind_ ->
                             {-# LINE 216 "src-ag/ExecutionPlan2TH.ag" #-}
                             case kind_ of
                               ChildAttr -> empty
                               _         -> pure _field
                             {-# LINE 369 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule5 #-}
   {-# LINE 365 "src-ag/ExecutionPlan2TH.ag" #-}
   rule5 = \ _nt kind_ name_ ->
                             {-# LINE 365 "src-ag/ExecutionPlan2TH.ag" #-}
                             case kind_ of
                               ChildSyntax     -> Just $ TH.VarE (TH.mkName ("sem_" ++ getName _nt    ))
                                                        `TH.AppE` TH.VarE (TH.mkName (getName name_ ++ "_"))
                               ChildAttr       -> Nothing
                               ChildReplace tp -> Just $ TH.VarE (TH.mkName ("sem_" ++ getName (extractNonterminal tp)))
                                                        `TH.AppE` TH.VarE (TH.mkName (getName name_ ++ "_"))
                             {-# LINE 380 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule6 #-}
   {-# LINE 665 "src-ag/ExecutionPlan2TH.ag" #-}
   rule6 = \ kind_ tp_ ->
                            {-# LINE 665 "src-ag/ExecutionPlan2TH.ag" #-}
                            case kind_ of
                              ChildSyntax     -> pure (typeToTH tp_ .->)
                              ChildReplace tp -> pure (typeToTH tp .->)
                              _               -> empty
                            {-# LINE 389 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule7 #-}
   {-# LINE 669 "src-ag/ExecutionPlan2TH.ag" #-}
   rule7 = \ kind_ name_ ->
                           {-# LINE 669 "src-ag/ExecutionPlan2TH.ag" #-}
                           case kind_ of
                             ChildSyntax    -> Just (TH.VarP (TH.mkName (getName name_ ++ "_")))
                             ChildReplace _ -> Just (TH.VarP (TH.mkName (getName name_ ++ "_")))
                             _              -> Nothing
                           {-# LINE 398 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule8 #-}
   {-# LINE 1061 "src-ag/ExecutionPlan2TH.ag" #-}
   rule8 = \ tp_ ->
                            {-# LINE 1061 "src-ag/ExecutionPlan2TH.ag" #-}
                            extractNonterminal tp_
                            {-# LINE 404 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule9 #-}
   {-# LINE 1641 "src-ag/ExecutionPlan2TH.ag" #-}
   rule9 = \ ((_lhsIoptions) :: Options) ->
                                                    {-# LINE 1641 "src-ag/ExecutionPlan2TH.ag" #-}
                                                    \x -> if bangpats _lhsIoptions then TH.BangP x else x
                                                    {-# LINE 410 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule10 #-}
   {-# LINE 1642 "src-ag/ExecutionPlan2TH.ag" #-}
   rule10 = \ ((_lhsIoptions) :: Options) ->
                                                    {-# LINE 1642 "src-ag/ExecutionPlan2TH.ag" #-}
                                                    TH.Bang TH.NoSourceUnpackedness (if strictData _lhsIoptions then TH.SourceStrict else TH.NoSourceStrictness)
                                                    {-# LINE 416 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule11 #-}
   {-# LINE 1695 "src-ag/ExecutionPlan2TH.ag" #-}
   rule11 = \ name_ tp_ ->
                     {-# LINE 1695 "src-ag/ExecutionPlan2TH.ag" #-}
                     Map.singleton name_ tp_
                     {-# LINE 422 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule12 #-}
   {-# LINE 1739 "src-ag/ExecutionPlan2TH.ag" #-}
   rule12 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) _nt ->
                 {-# LINE 1739 "src-ag/ExecutionPlan2TH.ag" #-}
                 Map.findWithDefault (error "nonterminal not in allInitStates map") _nt     _lhsIallInitStates
                 {-# LINE 428 "src-generated/ExecutionPlan2TH.hs" #-}
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
         _lhsOdatatype :: Maybe TH.BangType
         _lhsOdatatype = rule21 _field
         _lhsOdatatypeVar :: Maybe TH.VarBangType
         _lhsOdatatypeVar = rule22 _fieldVar
         _lhsOargnamesw ::  Maybe TH.Exp 
         _lhsOargnamesw = rule23 arg_name_
         _lhsOargtps ::   Maybe (TH.Type -> TH.Type)  
         _lhsOargtps = rule24 arg_tp_
         _argpats = rule25 _addbang arg_name_
         _addbang = rule26 _lhsIoptions
         _bang = rule27 _lhsIoptions
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
   {-# LINE 212 "src-ag/ExecutionPlan2TH.ag" #-}
   rule17 = \ tp_ ->
                    {-# LINE 212 "src-ag/ExecutionPlan2TH.ag" #-}
                    typeToTH (removeDeforested tp_)
                    {-# LINE 479 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule18 #-}
   {-# LINE 213 "src-ag/ExecutionPlan2TH.ag" #-}
   rule18 = \ ((_lhsIcon) :: ConstructorIdent) ((_lhsInt) :: NontermIdent) name_ ->
                     {-# LINE 213 "src-ag/ExecutionPlan2TH.ag" #-}
                     recordFieldname _lhsInt _lhsIcon name_
                     {-# LINE 485 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule19 #-}
   {-# LINE 214 "src-ag/ExecutionPlan2TH.ag" #-}
   rule19 = \ _bang _tpTH ->
                    {-# LINE 214 "src-ag/ExecutionPlan2TH.ag" #-}
                    (_bang    , _tpTH    )
                    {-# LINE 491 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule20 #-}
   {-# LINE 215 "src-ag/ExecutionPlan2TH.ag" #-}
   rule20 = \ _bang _strNm _tpTH ->
                       {-# LINE 215 "src-ag/ExecutionPlan2TH.ag" #-}
                       (TH.mkName _strNm    , _bang    , _tpTH    )
                       {-# LINE 497 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule21 #-}
   {-# LINE 220 "src-ag/ExecutionPlan2TH.ag" #-}
   rule21 = \ _field ->
                             {-# LINE 220 "src-ag/ExecutionPlan2TH.ag" #-}
                             Just _field
                             {-# LINE 503 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule22 #-}
   {-# LINE 221 "src-ag/ExecutionPlan2TH.ag" #-}
   rule22 = \ _fieldVar ->
                                {-# LINE 221 "src-ag/ExecutionPlan2TH.ag" #-}
                                Just _fieldVar
                                {-# LINE 509 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule23 #-}
   {-# LINE 371 "src-ag/ExecutionPlan2TH.ag" #-}
   rule23 = \ name_ ->
                             {-# LINE 371 "src-ag/ExecutionPlan2TH.ag" #-}
                             Just $ TH.VarE (TH.mkName (fieldname name_))
                             {-# LINE 515 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule24 #-}
   {-# LINE 673 "src-ag/ExecutionPlan2TH.ag" #-}
   rule24 = \ tp_ ->
                           {-# LINE 673 "src-ag/ExecutionPlan2TH.ag" #-}
                           pure $ \res -> (typeToTH tp_) .-> res
                           {-# LINE 521 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule25 #-}
   {-# LINE 674 "src-ag/ExecutionPlan2TH.ag" #-}
   rule25 = \ _addbang name_ ->
                           {-# LINE 674 "src-ag/ExecutionPlan2TH.ag" #-}
                           pure $ _addbang     $ TH.VarP (TH.mkName (fieldname name_))
                           {-# LINE 527 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule26 #-}
   {-# LINE 1643 "src-ag/ExecutionPlan2TH.ag" #-}
   rule26 = \ ((_lhsIoptions) :: Options) ->
                                                    {-# LINE 1643 "src-ag/ExecutionPlan2TH.ag" #-}
                                                    \x -> if bangpats _lhsIoptions then TH.BangP x else x
                                                    {-# LINE 533 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule27 #-}
   {-# LINE 1644 "src-ag/ExecutionPlan2TH.ag" #-}
   rule27 = \ ((_lhsIoptions) :: Options) ->
                                                    {-# LINE 1644 "src-ag/ExecutionPlan2TH.ag" #-}
                                                    TH.Bang TH.NoSourceUnpackedness (if strictData _lhsIoptions then TH.SourceStrict else TH.NoSourceStrictness)
                                                    {-# LINE 539 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule28 #-}
   {-# LINE 1695 "src-ag/ExecutionPlan2TH.ag" #-}
   rule28 = \ name_ tp_ ->
                     {-# LINE 1695 "src-ag/ExecutionPlan2TH.ag" #-}
                     Map.singleton name_ tp_
                     {-# LINE 545 "src-generated/ExecutionPlan2TH.hs" #-}
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
sem_ENonterminal_ENonterminal arg_nt_ arg_params_ arg_classCtxs_ arg_initial_ _ arg_nextVisits_ arg_prevVisits_ arg_prods_ arg_recursive_ _ = T_ENonterminal (return st8) where
   {-# NOINLINE st8 #-}
   st8 = let
      v7 :: T_ENonterminal_v7 
      v7 = \ (T_ENonterminal_vIn7 _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIavisitdefs _lhsIavisituses _lhsIderivings _lhsIimportBlocks _lhsIinhmap _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsIoptions _lhsIpragmaBlocks _lhsIsynmap _lhsItextBlocks _lhsItypeSyns _lhsIwrappers) -> ( let
         _prodsX17 = Control.Monad.Identity.runIdentity (attach_T_EProductions (arg_prods_))
         (T_EProductions_vOut16 _prodsIallvisits _prodsIcount _prodsIdatatype _prodsIerrors _prodsIfromToStates _prodsIsemFunBndDefs _prodsIsemFunBndTps _prodsIsem_nt _prodsIvisitKinds _prodsIvisitdefs _prodsIvisituses) = inv_EProductions_s17 _prodsX17 (T_EProductions_vIn16 _prodsOallFromToStates _prodsOallInhmap _prodsOallInitStates _prodsOallSynmap _prodsOallVisitKinds _prodsOallstates _prodsOavisitdefs _prodsOavisituses _prodsOclassCtxs _prodsOimportBlocks _prodsOinhmap _prodsOinitial _prodsOlocalAttrTypes _prodsOmainFile _prodsOmainName _prodsOmoduleHeader _prodsOnextVisits _prodsOnt _prodsOntType _prodsOoptions _prodsOparams _prodsOpragmaBlocks _prodsOprevVisits _prodsOrename _prodsOsynmap _prodsOtextBlocks)
         _prodsOrename = rule68 _lhsIoptions
         _prodsOnt = rule69 arg_nt_
         _prodsOparams = rule70 arg_params_
         _prodsOclassCtxs = rule71 arg_classCtxs_
         _lhsOoutput :: [TH.Dec]
         _lhsOoutput = rule72 _datatype _hasWrapper _lhsIoptions _sem_nt _t_init _wr_inh _wr_syn _wrapper
         _hasWrapper = rule73 _lhsIwrappers arg_nt_
         _aliasPre = rule74 _t_params arg_nt_
         _datatype = rule75 _aliasPre _derivings _lhsItypeSyns _prodsIdatatype _t_params arg_nt_
         _derivings = rule76 _lhsIderivings arg_nt_
         _fsemname = rule77  ()
         _semname = rule78 _fsemname arg_nt_
         _frecarg = rule79 _fsemname
         _sem_tp = rule80 _classTH _quantTH _t_params _t_type arg_nt_
         _quantTH = rule81 arg_params_
         _classTH = rule82 arg_classCtxs_
         _sem_nt = rule83 _frecarg _fsemname _lhsItypeSyns _prodsIsem_nt _semPragma _sem_tp _semname arg_nt_
         _inlineNt = rule84 _hasWrapper _lhsIoptions _prodsIcount arg_recursive_
         _semPragma = rule85 _inlineNt _lhsIoptions _semname
         (Just _prodsOinhmap) = rule86 _lhsIinhmap arg_nt_
         (Just _prodsOsynmap) = rule87 _lhsIsynmap arg_nt_
         _prodsOallInhmap = rule88 _lhsIinhmap
         _prodsOallSynmap = rule89 _lhsIsynmap
         _outedges = rule90 _prodsIallvisits
         _inedges = rule91 _prodsIallvisits
         _allstates = rule92 _inedges _outedges arg_initial_
         _stvisits = rule93 _prodsIallvisits
         _t_type = rule94 arg_nt_
         _t_params = rule95 arg_params_
         _t_init = rule96 _lhsIoptions _t_params _t_type arg_initial_
         _wr_inh = rule97 _genwrap _wr_inhs
         _wr_syn = rule98 _genwrap _wr_syns
         _genwrap = rule99 _bang _t_params arg_nt_
         _synAttrs = rule100 _lhsIinhmap arg_nt_
         _wr_inhs = rule101 _synAttrs _wr_filter
         _wr_inhs1 = rule102 _synAttrs
         _wr_filter = rule103 _lhsIoptions
         _wr_syns = rule104 _lhsIsynmap arg_nt_
         _inhlist = rule105 _lhsIoptions _wr_inhs
         _inhlist1 = rule106 _lhsIoptions _wr_inhs1
         _synlist = rule107 _lhsIoptions _wr_syns
         _wrapname = rule108 arg_nt_
         _inhname = rule109 arg_nt_
         _synname = rule110 arg_nt_
         _firstVisitInfo = rule111 arg_initial_ arg_nextVisits_
         _wrapper = rule112 _classTH _inhname _lhsIoptions _quantTH _synname _t_params _t_type _wrapPragma _wrapname
         _wrapPragma = rule113 _lhsIoptions _wrapname
         _lhsOsemFunBndDefs :: Seq (TH.Name, TH.Exp)
         _lhsOsemFunBndDefs = rule114 _prodsIsemFunBndDefs _semFunBndDef
         _lhsOsemFunBndTps :: Seq (TH.VarBangType)
         _lhsOsemFunBndTps = rule115 _prodsIsemFunBndTps _semFunBndTp
         _semFunBndDef = rule116 _semFunBndNm _semname
         _semFunBndTp = rule117 _semFunBndNm _sem_tp
         _semFunBndNm = rule118 arg_nt_
         _prodsOinitial = rule119 arg_initial_
         _prodsOallstates = rule120 _allstates
         _addbang = rule121 _lhsIoptions
         _bang = rule122 _lhsIoptions
         _addbangWrap = rule123  ()
         _prodsOnextVisits = rule124 arg_nextVisits_
         _prodsOprevVisits = rule125 arg_prevVisits_
         _prodsOlocalAttrTypes = rule126 _lhsIlocalAttrTypes arg_nt_
         _lhsOinitStates :: Map NontermIdent Int
         _lhsOinitStates = rule127 arg_initial_ arg_nt_
         _ntType = rule128 arg_nt_ arg_params_
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule129 _prodsIerrors
         _lhsOfromToStates :: Map VisitIdentifier (Int,Int)
         _lhsOfromToStates = rule130 _prodsIfromToStates
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule131 _prodsIvisitKinds
         _lhsOvisitdefs :: Map VisitIdentifier (Set Identifier)
         _lhsOvisitdefs = rule132 _prodsIvisitdefs
         _lhsOvisituses :: Map VisitIdentifier (Set Identifier)
         _lhsOvisituses = rule133 _prodsIvisituses
         _prodsOallFromToStates = rule134 _lhsIallFromToStates
         _prodsOallInitStates = rule135 _lhsIallInitStates
         _prodsOallVisitKinds = rule136 _lhsIallVisitKinds
         _prodsOavisitdefs = rule137 _lhsIavisitdefs
         _prodsOavisituses = rule138 _lhsIavisituses
         _prodsOimportBlocks = rule139 _lhsIimportBlocks
         _prodsOmainFile = rule140 _lhsImainFile
         _prodsOmainName = rule141 _lhsImainName
         _prodsOmoduleHeader = rule142 _lhsImoduleHeader
         _prodsOntType = rule143 _ntType
         _prodsOoptions = rule144 _lhsIoptions
         _prodsOpragmaBlocks = rule145 _lhsIpragmaBlocks
         _prodsOtextBlocks = rule146 _lhsItextBlocks
         __result_ = T_ENonterminal_vOut7 _lhsOerrors _lhsOfromToStates _lhsOinitStates _lhsOoutput _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses
         in __result_ )
     in C_ENonterminal_s8 v7
   {-# INLINE rule68 #-}
   {-# LINE 71 "src-ag/ExecutionPlan2TH.ag" #-}
   rule68 = \ ((_lhsIoptions) :: Options) ->
                                  {-# LINE 71 "src-ag/ExecutionPlan2TH.ag" #-}
                                  rename _lhsIoptions
                                  {-# LINE 906 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule69 #-}
   {-# LINE 79 "src-ag/ExecutionPlan2TH.ag" #-}
   rule69 = \ nt_ ->
                              {-# LINE 79 "src-ag/ExecutionPlan2TH.ag" #-}
                              nt_
                              {-# LINE 912 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule70 #-}
   {-# LINE 91 "src-ag/ExecutionPlan2TH.ag" #-}
   rule70 = \ params_ ->
                   {-# LINE 91 "src-ag/ExecutionPlan2TH.ag" #-}
                   params_
                   {-# LINE 918 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule71 #-}
   {-# LINE 95 "src-ag/ExecutionPlan2TH.ag" #-}
   rule71 = \ classCtxs_ ->
                      {-# LINE 95 "src-ag/ExecutionPlan2TH.ag" #-}
                      classCtxs_
                      {-# LINE 924 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule72 #-}
   {-# LINE 112 "src-ag/ExecutionPlan2TH.ag" #-}
   rule72 = \ _datatype _hasWrapper ((_lhsIoptions) :: Options) _sem_nt _t_init _wr_inh _wr_syn _wrapper ->
                                {-# LINE 112 "src-ag/ExecutionPlan2TH.ag" #-}
                                concat
                                [ if dataTypes _lhsIoptions
                                    then [_datatype    ]
                                    else []
                                , if _hasWrapper
                                    then [ _wr_inh
                                         , _wr_syn
                                         ]
                                         ++ _wrapper
                                    else []
                                , if folds _lhsIoptions
                                    then _sem_nt
                                    else []
                                , if semfuns _lhsIoptions
                                    then [ _t_init
                                         ]
                                    else []
                                ]
                                {-# LINE 947 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule73 #-}
   {-# LINE 134 "src-ag/ExecutionPlan2TH.ag" #-}
   rule73 = \ ((_lhsIwrappers) :: Set NontermIdent) nt_ ->
                                    {-# LINE 134 "src-ag/ExecutionPlan2TH.ag" #-}
                                    nt_ `Set.member` _lhsIwrappers
                                    {-# LINE 953 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule74 #-}
   {-# LINE 147 "src-ag/ExecutionPlan2TH.ag" #-}
   rule74 = \ _t_params nt_ ->
                                  {-# LINE 147 "src-ag/ExecutionPlan2TH.ag" #-}
                                  TH.TySynD (identToName nt_) (map TH.PlainTV _t_params    ) :: TH.Type -> TH.Dec
                                  {-# LINE 959 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule75 #-}
   {-# LINE 148 "src-ag/ExecutionPlan2TH.ag" #-}
   rule75 = \ _aliasPre _derivings ((_lhsItypeSyns) :: TypeSyns) ((_prodsIdatatype) :: [TH.Con]) _t_params nt_ ->
                                  {-# LINE 148 "src-ag/ExecutionPlan2TH.ag" #-}
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
                                  {-# LINE 974 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule76 #-}
   {-# LINE 159 "src-ag/ExecutionPlan2TH.ag" #-}
   rule76 = \ ((_lhsIderivings) :: Derivings) nt_ ->
                                   {-# LINE 159 "src-ag/ExecutionPlan2TH.ag" #-}
                                   case Map.lookup nt_ _lhsIderivings of
                                      Just s -> [TH.DerivClause Nothing (map (TH.ConT . identToName) (Set.toList s))]
                                      Nothing -> []
                                   {-# LINE 982 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule77 #-}
   {-# LINE 227 "src-ag/ExecutionPlan2TH.ag" #-}
   rule77 = \  (_ :: ()) ->
                                  {-# LINE 227 "src-ag/ExecutionPlan2TH.ag" #-}
                                  \x -> "sem_" ++ show x
                                  {-# LINE 988 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule78 #-}
   {-# LINE 228 "src-ag/ExecutionPlan2TH.ag" #-}
   rule78 = \ _fsemname nt_ ->
                                 {-# LINE 228 "src-ag/ExecutionPlan2TH.ag" #-}
                                 _fsemname     nt_
                                 {-# LINE 994 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule79 #-}
   {-# LINE 229 "src-ag/ExecutionPlan2TH.ag" #-}
   rule79 = \ _fsemname ->
                                 {-# LINE 229 "src-ag/ExecutionPlan2TH.ag" #-}
                                 \t x -> case t of
                                            NT nt _ _ -> TH.VarE (TH.mkName (_fsemname nt)) `TH.AppE` x
                                            _         -> x
                                 {-# LINE 1002 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule80 #-}
   {-# LINE 239 "src-ag/ExecutionPlan2TH.ag" #-}
   rule80 = \ _classTH _quantTH _t_params _t_type nt_ ->
                                 {-# LINE 239 "src-ag/ExecutionPlan2TH.ag" #-}
                                 _quantTH     _classTH     (foldl TH.AppT (TH.ConT (identToName nt_)) (map TH.VarT _t_params    )
                                                        .-> foldl TH.AppT (TH.ConT (TH.mkName _t_type    )) (map TH.VarT _t_params    ))
                                 {-# LINE 1009 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule81 #-}
   {-# LINE 241 "src-ag/ExecutionPlan2TH.ag" #-}
   rule81 = \ params_ ->
                                 {-# LINE 241 "src-ag/ExecutionPlan2TH.ag" #-}
                                 quantsTH TH.ForallT params_
                                 {-# LINE 1015 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule82 #-}
   {-# LINE 242 "src-ag/ExecutionPlan2TH.ag" #-}
   rule82 = \ classCtxs_ ->
                                 {-# LINE 242 "src-ag/ExecutionPlan2TH.ag" #-}
                                 classCtxsToCxt classCtxs_
                                 {-# LINE 1021 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule83 #-}
   {-# LINE 243 "src-ag/ExecutionPlan2TH.ag" #-}
   rule83 = \ _frecarg _fsemname ((_lhsItypeSyns) :: TypeSyns) ((_prodsIsem_nt) :: [TH.Clause]) _semPragma _sem_tp _semname nt_ ->
                                 {-# LINE 243 "src-ag/ExecutionPlan2TH.ag" #-}
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
                                 {-# LINE 1119 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule84 #-}
   {-# LINE 338 "src-ag/ExecutionPlan2TH.ag" #-}
   rule84 = \ _hasWrapper ((_lhsIoptions) :: Options) ((_prodsIcount) :: Int) recursive_ ->
                                  {-# LINE 338 "src-ag/ExecutionPlan2TH.ag" #-}
                                  not (lateHigherOrderBinding _lhsIoptions) && not recursive_ && (_prodsIcount == 1 || (aggressiveInlinePragmas _lhsIoptions && not _hasWrapper    ))
                                  {-# LINE 1125 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule85 #-}
   {-# LINE 339 "src-ag/ExecutionPlan2TH.ag" #-}
   rule85 = \ _inlineNt ((_lhsIoptions) :: Options) _semname ->
                                  {-# LINE 339 "src-ag/ExecutionPlan2TH.ag" #-}
                                  if noInlinePragmas _lhsIoptions
                                  then empty
                                  else if _inlineNt
                                       then pure $ inlineTH (TH.mkName _semname    )
                                       else if helpInlining _lhsIoptions && not (lateHigherOrderBinding _lhsIoptions)
                                            then pure $ inlinableTH (TH.mkName _semname    )
                                            else pure $ noInlineTH (TH.mkName _semname    )
                                  {-# LINE 1137 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule86 #-}
   {-# LINE 391 "src-ag/ExecutionPlan2TH.ag" #-}
   rule86 = \ ((_lhsIinhmap) :: Map NontermIdent Attributes) nt_ ->
                                         {-# LINE 391 "src-ag/ExecutionPlan2TH.ag" #-}
                                         Map.lookup nt_ _lhsIinhmap
                                         {-# LINE 1143 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule87 #-}
   {-# LINE 392 "src-ag/ExecutionPlan2TH.ag" #-}
   rule87 = \ ((_lhsIsynmap) :: Map NontermIdent Attributes) nt_ ->
                                         {-# LINE 392 "src-ag/ExecutionPlan2TH.ag" #-}
                                         Map.lookup nt_ _lhsIsynmap
                                         {-# LINE 1149 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule88 #-}
   {-# LINE 393 "src-ag/ExecutionPlan2TH.ag" #-}
   rule88 = \ ((_lhsIinhmap) :: Map NontermIdent Attributes) ->
                                     {-# LINE 393 "src-ag/ExecutionPlan2TH.ag" #-}
                                     _lhsIinhmap
                                     {-# LINE 1155 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule89 #-}
   {-# LINE 394 "src-ag/ExecutionPlan2TH.ag" #-}
   rule89 = \ ((_lhsIsynmap) :: Map NontermIdent Attributes) ->
                                     {-# LINE 394 "src-ag/ExecutionPlan2TH.ag" #-}
                                     _lhsIsynmap
                                     {-# LINE 1161 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule90 #-}
   {-# LINE 415 "src-ag/ExecutionPlan2TH.ag" #-}
   rule90 = \ ((_prodsIallvisits) :: [VisitStateState]) ->
                                   {-# LINE 415 "src-ag/ExecutionPlan2TH.ag" #-}
                                   Set.fromList $ map (\(_,f,_) -> f) _prodsIallvisits
                                   {-# LINE 1167 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule91 #-}
   {-# LINE 416 "src-ag/ExecutionPlan2TH.ag" #-}
   rule91 = \ ((_prodsIallvisits) :: [VisitStateState]) ->
                                   {-# LINE 416 "src-ag/ExecutionPlan2TH.ag" #-}
                                   Set.fromList $ map (\(_,_,t) -> t) _prodsIallvisits
                                   {-# LINE 1173 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule92 #-}
   {-# LINE 417 "src-ag/ExecutionPlan2TH.ag" #-}
   rule92 = \ _inedges _outedges initial_ ->
                                   {-# LINE 417 "src-ag/ExecutionPlan2TH.ag" #-}
                                   Set.insert initial_ $ _inedges     `Set.union` _outedges
                                   {-# LINE 1179 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule93 #-}
   {-# LINE 418 "src-ag/ExecutionPlan2TH.ag" #-}
   rule93 = \ ((_prodsIallvisits) :: [VisitStateState]) ->
                                   {-# LINE 418 "src-ag/ExecutionPlan2TH.ag" #-}
                                   \st -> filter (\(v,f,t) -> f == st) _prodsIallvisits
                                   {-# LINE 1185 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule94 #-}
   {-# LINE 419 "src-ag/ExecutionPlan2TH.ag" #-}
   rule94 = \ nt_ ->
                                   {-# LINE 419 "src-ag/ExecutionPlan2TH.ag" #-}
                                   "T_" ++ getName nt_
                                   {-# LINE 1191 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule95 #-}
   {-# LINE 420 "src-ag/ExecutionPlan2TH.ag" #-}
   rule95 = \ params_ ->
                                   {-# LINE 420 "src-ag/ExecutionPlan2TH.ag" #-}
                                   map (TH.mkName . getName) params_
                                   {-# LINE 1197 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule96 #-}
   {-# LINE 421 "src-ag/ExecutionPlan2TH.ag" #-}
   rule96 = \ ((_lhsIoptions) :: Options) _t_params _t_type initial_ ->
                                   {-# LINE 421 "src-ag/ExecutionPlan2TH.ag" #-}
                                   TH.NewtypeD [] (TH.mkName _t_type    ) (map TH.PlainTV _t_params    ) Nothing
                                   (TH.RecC (TH.mkName _t_type    )
                                         [(TH.mkName ("attach_" ++ _t_type    ), nobang, monadTypeTH _lhsIoptions (foldl TH.AppT (TH.ConT (TH.mkName (_t_type     ++ "_s" ++ show initial_))) (map TH.VarT _t_params    )))
                                         ]) []
                                   {-# LINE 1206 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule97 #-}
   {-# LINE 511 "src-ag/ExecutionPlan2TH.ag" #-}
   rule97 = \ _genwrap _wr_inhs ->
                                  {-# LINE 511 "src-ag/ExecutionPlan2TH.ag" #-}
                                  _genwrap     "Inh" _wr_inhs
                                  {-# LINE 1212 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule98 #-}
   {-# LINE 512 "src-ag/ExecutionPlan2TH.ag" #-}
   rule98 = \ _genwrap _wr_syns ->
                                  {-# LINE 512 "src-ag/ExecutionPlan2TH.ag" #-}
                                  _genwrap     "Syn" _wr_syns     :: TH.Dec
                                  {-# LINE 1218 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule99 #-}
   {-# LINE 513 "src-ag/ExecutionPlan2TH.ag" #-}
   rule99 = \ _bang _t_params nt_ ->
                                  {-# LINE 513 "src-ag/ExecutionPlan2TH.ag" #-}
                                  \nm attr -> TH.DataD
                                    []
                                    (TH.mkName (nm ++ "_" ++ getName nt_))
                                    (map TH.PlainTV _t_params    )
                                    Nothing
                                    [TH.RecC (TH.mkName (nm ++ "_" ++ getName nt_))
                                             (map (\(i,t) -> ( TH.mkName (getName i ++ "_" ++ nm ++ "_" ++ getName nt_)
                                                             , _bang
                                                             , either (error . ("Cannot convert type: " ++)) id
                                                               $ Meta.parseType
                                                               $ typeToHaskellString (Just nt_) [] t
                                                             ))
                                                  attr)]
                                    []
                                  {-# LINE 1237 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule100 #-}
   {-# LINE 527 "src-ag/ExecutionPlan2TH.ag" #-}
   rule100 = \ ((_lhsIinhmap) :: Map NontermIdent Attributes) nt_ ->
                                  {-# LINE 527 "src-ag/ExecutionPlan2TH.ag" #-}
                                  fromJust $ Map.lookup nt_ _lhsIinhmap
                                  {-# LINE 1243 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule101 #-}
   {-# LINE 528 "src-ag/ExecutionPlan2TH.ag" #-}
   rule101 = \ _synAttrs _wr_filter ->
                                  {-# LINE 528 "src-ag/ExecutionPlan2TH.ag" #-}
                                  Map.toList $ _wr_filter     $ _synAttrs
                                  {-# LINE 1249 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule102 #-}
   {-# LINE 529 "src-ag/ExecutionPlan2TH.ag" #-}
   rule102 = \ _synAttrs ->
                                  {-# LINE 529 "src-ag/ExecutionPlan2TH.ag" #-}
                                  Map.toList _synAttrs
                                  {-# LINE 1255 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule103 #-}
   {-# LINE 530 "src-ag/ExecutionPlan2TH.ag" #-}
   rule103 = \ ((_lhsIoptions) :: Options) ->
                                   {-# LINE 530 "src-ag/ExecutionPlan2TH.ag" #-}
                                   if lateHigherOrderBinding _lhsIoptions
                                   then Map.delete idLateBindingAttr
                                   else id
                                   {-# LINE 1263 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule104 #-}
   {-# LINE 533 "src-ag/ExecutionPlan2TH.ag" #-}
   rule104 = \ ((_lhsIsynmap) :: Map NontermIdent Attributes) nt_ ->
                                  {-# LINE 533 "src-ag/ExecutionPlan2TH.ag" #-}
                                  Map.toList $ fromJust $ Map.lookup nt_ _lhsIsynmap
                                  {-# LINE 1269 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule105 #-}
   {-# LINE 534 "src-ag/ExecutionPlan2TH.ag" #-}
   rule105 = \ ((_lhsIoptions) :: Options) _wr_inhs ->
                                  {-# LINE 534 "src-ag/ExecutionPlan2TH.ag" #-}
                                  map (lhsname _lhsIoptions True . fst) _wr_inhs
                                  {-# LINE 1275 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule106 #-}
   {-# LINE 535 "src-ag/ExecutionPlan2TH.ag" #-}
   rule106 = \ ((_lhsIoptions) :: Options) _wr_inhs1 ->
                                  {-# LINE 535 "src-ag/ExecutionPlan2TH.ag" #-}
                                  map (lhsname _lhsIoptions True . fst) _wr_inhs1
                                  {-# LINE 1281 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule107 #-}
   {-# LINE 536 "src-ag/ExecutionPlan2TH.ag" #-}
   rule107 = \ ((_lhsIoptions) :: Options) _wr_syns ->
                                  {-# LINE 536 "src-ag/ExecutionPlan2TH.ag" #-}
                                  map (lhsname _lhsIoptions False . fst) _wr_syns
                                  {-# LINE 1287 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule108 #-}
   {-# LINE 537 "src-ag/ExecutionPlan2TH.ag" #-}
   rule108 = \ nt_ ->
                                  {-# LINE 537 "src-ag/ExecutionPlan2TH.ag" #-}
                                  TH.mkName ("wrap_" ++ show nt_)
                                  {-# LINE 1293 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule109 #-}
   {-# LINE 538 "src-ag/ExecutionPlan2TH.ag" #-}
   rule109 = \ nt_ ->
                                  {-# LINE 538 "src-ag/ExecutionPlan2TH.ag" #-}
                                  TH.mkName ("Inh_" ++ show nt_)
                                  {-# LINE 1299 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule110 #-}
   {-# LINE 539 "src-ag/ExecutionPlan2TH.ag" #-}
   rule110 = \ nt_ ->
                                  {-# LINE 539 "src-ag/ExecutionPlan2TH.ag" #-}
                                  TH.mkName ("Syn_" ++ show nt_)
                                  {-# LINE 1305 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule111 #-}
   {-# LINE 540 "src-ag/ExecutionPlan2TH.ag" #-}
   rule111 = \ initial_ nextVisits_ ->
                                        {-# LINE 540 "src-ag/ExecutionPlan2TH.ag" #-}
                                        Map.findWithDefault ManyVis initial_ nextVisits_
                                        {-# LINE 1311 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule112 #-}
   {-# LINE 541 "src-ag/ExecutionPlan2TH.ag" #-}
   rule112 = \ _classTH _inhname ((_lhsIoptions) :: Options) _quantTH _synname _t_params _t_type _wrapPragma _wrapname ->
                                  {-# LINE 541 "src-ag/ExecutionPlan2TH.ag" #-}
                                  _wrapPragma     ++
                                  [ TH.SigD _wrapname     (_quantTH     _classTH
                                      (foldl TH.AppT (TH.ConT (TH.mkName _t_type    )) (map TH.VarT _t_params    )
                                      .-> foldl TH.AppT (TH.ConT _inhname    ) (map TH.VarT _t_params    )
                                      .-> ( if monadicWrappers _lhsIoptions
                                              then monadTypeTH _lhsIoptions
                                              else id) (foldl TH.AppT (TH.ConT _synname    ) (map TH.VarT _t_params    ))))
                                  ]
                                  {-# LINE 1324 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule113 #-}
   {-# LINE 598 "src-ag/ExecutionPlan2TH.ag" #-}
   rule113 = \ ((_lhsIoptions) :: Options) _wrapname ->
                                    {-# LINE 598 "src-ag/ExecutionPlan2TH.ag" #-}
                                    if parallelInvoke _lhsIoptions && not (monadicWrappers _lhsIoptions)
                                    then pure $ noInlineTH _wrapname
                                    else if noInlinePragmas _lhsIoptions
                                         then empty
                                         else pure $ inlinableTH _wrapname
                                    {-# LINE 1334 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule114 #-}
   {-# LINE 610 "src-ag/ExecutionPlan2TH.ag" #-}
   rule114 = \ ((_prodsIsemFunBndDefs) :: Seq (TH.Name, TH.Exp)) _semFunBndDef ->
                        {-# LINE 610 "src-ag/ExecutionPlan2TH.ag" #-}
                        _semFunBndDef     Seq.<| _prodsIsemFunBndDefs
                        {-# LINE 1340 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule115 #-}
   {-# LINE 611 "src-ag/ExecutionPlan2TH.ag" #-}
   rule115 = \ ((_prodsIsemFunBndTps) :: Seq (TH.VarBangType)) _semFunBndTp ->
                        {-# LINE 611 "src-ag/ExecutionPlan2TH.ag" #-}
                        _semFunBndTp     Seq.<| _prodsIsemFunBndTps
                        {-# LINE 1346 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule116 #-}
   {-# LINE 612 "src-ag/ExecutionPlan2TH.ag" #-}
   rule116 = \ _semFunBndNm _semname ->
                        {-# LINE 612 "src-ag/ExecutionPlan2TH.ag" #-}
                        (_semFunBndNm    , TH.VarE (TH.mkName _semname    ))
                        {-# LINE 1352 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule117 #-}
   {-# LINE 613 "src-ag/ExecutionPlan2TH.ag" #-}
   rule117 = \ _semFunBndNm _sem_tp ->
                        {-# LINE 613 "src-ag/ExecutionPlan2TH.ag" #-}
                        (_semFunBndNm    , nobang, _sem_tp    )
                        {-# LINE 1358 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule118 #-}
   {-# LINE 614 "src-ag/ExecutionPlan2TH.ag" #-}
   rule118 = \ nt_ ->
                        {-# LINE 614 "src-ag/ExecutionPlan2TH.ag" #-}
                        TH.mkName (lateSemNtLabel nt_)
                        {-# LINE 1364 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule119 #-}
   {-# LINE 656 "src-ag/ExecutionPlan2TH.ag" #-}
   rule119 = \ initial_ ->
                                     {-# LINE 656 "src-ag/ExecutionPlan2TH.ag" #-}
                                     initial_
                                     {-# LINE 1370 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule120 #-}
   {-# LINE 657 "src-ag/ExecutionPlan2TH.ag" #-}
   rule120 = \ _allstates ->
                                     {-# LINE 657 "src-ag/ExecutionPlan2TH.ag" #-}
                                     _allstates
                                     {-# LINE 1376 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule121 #-}
   {-# LINE 1638 "src-ag/ExecutionPlan2TH.ag" #-}
   rule121 = \ ((_lhsIoptions) :: Options) ->
                                                    {-# LINE 1638 "src-ag/ExecutionPlan2TH.ag" #-}
                                                    \x -> if bangpats _lhsIoptions then TH.BangP x else x
                                                    {-# LINE 1382 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule122 #-}
   {-# LINE 1639 "src-ag/ExecutionPlan2TH.ag" #-}
   rule122 = \ ((_lhsIoptions) :: Options) ->
                                                    {-# LINE 1639 "src-ag/ExecutionPlan2TH.ag" #-}
                                                    TH.Bang TH.NoSourceUnpackedness (if strictData _lhsIoptions then TH.SourceStrict else TH.NoSourceStrictness)
                                                    {-# LINE 1388 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule123 #-}
   {-# LINE 1649 "src-ag/ExecutionPlan2TH.ag" #-}
   rule123 = \  (_ :: ()) ->
                                                        {-# LINE 1649 "src-ag/ExecutionPlan2TH.ag" #-}
                                                        id
                                                        {-# LINE 1394 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule124 #-}
   {-# LINE 1661 "src-ag/ExecutionPlan2TH.ag" #-}
   rule124 = \ nextVisits_ ->
                       {-# LINE 1661 "src-ag/ExecutionPlan2TH.ag" #-}
                       nextVisits_
                       {-# LINE 1400 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule125 #-}
   {-# LINE 1662 "src-ag/ExecutionPlan2TH.ag" #-}
   rule125 = \ prevVisits_ ->
                       {-# LINE 1662 "src-ag/ExecutionPlan2TH.ag" #-}
                       prevVisits_
                       {-# LINE 1406 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule126 #-}
   {-# LINE 1706 "src-ag/ExecutionPlan2TH.ag" #-}
   rule126 = \ ((_lhsIlocalAttrTypes) :: Map NontermIdent (Map ConstructorIdent (Map Identifier Type))) nt_ ->
                           {-# LINE 1706 "src-ag/ExecutionPlan2TH.ag" #-}
                           Map.findWithDefault Map.empty nt_ _lhsIlocalAttrTypes
                           {-# LINE 1412 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule127 #-}
   {-# LINE 1733 "src-ag/ExecutionPlan2TH.ag" #-}
   rule127 = \ initial_ nt_ ->
                     {-# LINE 1733 "src-ag/ExecutionPlan2TH.ag" #-}
                     Map.singleton nt_ initial_
                     {-# LINE 1418 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule128 #-}
   {-# LINE 1747 "src-ag/ExecutionPlan2TH.ag" #-}
   rule128 = \ nt_ params_ ->
                 {-# LINE 1747 "src-ag/ExecutionPlan2TH.ag" #-}
                 NT nt_ (map show params_) False
                 {-# LINE 1424 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule129 #-}
   rule129 = \ ((_prodsIerrors) :: Seq Error) ->
     _prodsIerrors
   {-# INLINE rule130 #-}
   rule130 = \ ((_prodsIfromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _prodsIfromToStates
   {-# INLINE rule131 #-}
   rule131 = \ ((_prodsIvisitKinds) :: Map VisitIdentifier VisitKind) ->
     _prodsIvisitKinds
   {-# INLINE rule132 #-}
   rule132 = \ ((_prodsIvisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _prodsIvisitdefs
   {-# INLINE rule133 #-}
   rule133 = \ ((_prodsIvisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _prodsIvisituses
   {-# INLINE rule134 #-}
   rule134 = \ ((_lhsIallFromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _lhsIallFromToStates
   {-# INLINE rule135 #-}
   rule135 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule136 #-}
   rule136 = \ ((_lhsIallVisitKinds) :: Map VisitIdentifier VisitKind) ->
     _lhsIallVisitKinds
   {-# INLINE rule137 #-}
   rule137 = \ ((_lhsIavisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisitdefs
   {-# INLINE rule138 #-}
   rule138 = \ ((_lhsIavisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisituses
   {-# INLINE rule139 #-}
   rule139 = \ ((_lhsIimportBlocks) :: [String]) ->
     _lhsIimportBlocks
   {-# INLINE rule140 #-}
   rule140 = \ ((_lhsImainFile) :: String) ->
     _lhsImainFile
   {-# INLINE rule141 #-}
   rule141 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule142 #-}
   rule142 = \ ((_lhsImoduleHeader) :: String -> String -> String -> Bool -> String) ->
     _lhsImoduleHeader
   {-# INLINE rule143 #-}
   rule143 = \ _ntType ->
     _ntType
   {-# INLINE rule144 #-}
   rule144 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule145 #-}
   rule145 = \ ((_lhsIpragmaBlocks) :: String) ->
     _lhsIpragmaBlocks
   {-# INLINE rule146 #-}
   rule146 = \ ((_lhsItextBlocks) :: [String]) ->
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
         _lhsOerrors = rule147 _hdIerrors _tlIerrors
         _lhsOfromToStates :: Map VisitIdentifier (Int,Int)
         _lhsOfromToStates = rule148 _hdIfromToStates _tlIfromToStates
         _lhsOinitStates :: Map NontermIdent Int
         _lhsOinitStates = rule149 _hdIinitStates _tlIinitStates
         _lhsOoutput :: [TH.Dec]
         _lhsOoutput = rule150 _hdIoutput _tlIoutput
         _lhsOsemFunBndDefs :: Seq (TH.Name, TH.Exp)
         _lhsOsemFunBndDefs = rule151 _hdIsemFunBndDefs _tlIsemFunBndDefs
         _lhsOsemFunBndTps :: Seq (TH.VarBangType)
         _lhsOsemFunBndTps = rule152 _hdIsemFunBndTps _tlIsemFunBndTps
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule153 _hdIvisitKinds _tlIvisitKinds
         _lhsOvisitdefs :: Map VisitIdentifier (Set Identifier)
         _lhsOvisitdefs = rule154 _hdIvisitdefs _tlIvisitdefs
         _lhsOvisituses :: Map VisitIdentifier (Set Identifier)
         _lhsOvisituses = rule155 _hdIvisituses _tlIvisituses
         _hdOallFromToStates = rule156 _lhsIallFromToStates
         _hdOallInitStates = rule157 _lhsIallInitStates
         _hdOallVisitKinds = rule158 _lhsIallVisitKinds
         _hdOavisitdefs = rule159 _lhsIavisitdefs
         _hdOavisituses = rule160 _lhsIavisituses
         _hdOderivings = rule161 _lhsIderivings
         _hdOimportBlocks = rule162 _lhsIimportBlocks
         _hdOinhmap = rule163 _lhsIinhmap
         _hdOlocalAttrTypes = rule164 _lhsIlocalAttrTypes
         _hdOmainFile = rule165 _lhsImainFile
         _hdOmainName = rule166 _lhsImainName
         _hdOmoduleHeader = rule167 _lhsImoduleHeader
         _hdOoptions = rule168 _lhsIoptions
         _hdOpragmaBlocks = rule169 _lhsIpragmaBlocks
         _hdOsynmap = rule170 _lhsIsynmap
         _hdOtextBlocks = rule171 _lhsItextBlocks
         _hdOtypeSyns = rule172 _lhsItypeSyns
         _hdOwrappers = rule173 _lhsIwrappers
         _tlOallFromToStates = rule174 _lhsIallFromToStates
         _tlOallInitStates = rule175 _lhsIallInitStates
         _tlOallVisitKinds = rule176 _lhsIallVisitKinds
         _tlOavisitdefs = rule177 _lhsIavisitdefs
         _tlOavisituses = rule178 _lhsIavisituses
         _tlOderivings = rule179 _lhsIderivings
         _tlOimportBlocks = rule180 _lhsIimportBlocks
         _tlOinhmap = rule181 _lhsIinhmap
         _tlOlocalAttrTypes = rule182 _lhsIlocalAttrTypes
         _tlOmainFile = rule183 _lhsImainFile
         _tlOmainName = rule184 _lhsImainName
         _tlOmoduleHeader = rule185 _lhsImoduleHeader
         _tlOoptions = rule186 _lhsIoptions
         _tlOpragmaBlocks = rule187 _lhsIpragmaBlocks
         _tlOsynmap = rule188 _lhsIsynmap
         _tlOtextBlocks = rule189 _lhsItextBlocks
         _tlOtypeSyns = rule190 _lhsItypeSyns
         _tlOwrappers = rule191 _lhsIwrappers
         __result_ = T_ENonterminals_vOut10 _lhsOerrors _lhsOfromToStates _lhsOinitStates _lhsOoutput _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses
         in __result_ )
     in C_ENonterminals_s11 v10
   {-# INLINE rule147 #-}
   rule147 = \ ((_hdIerrors) :: Seq Error) ((_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# INLINE rule148 #-}
   rule148 = \ ((_hdIfromToStates) :: Map VisitIdentifier (Int,Int)) ((_tlIfromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _hdIfromToStates `mappend` _tlIfromToStates
   {-# INLINE rule149 #-}
   rule149 = \ ((_hdIinitStates) :: Map NontermIdent Int) ((_tlIinitStates) :: Map NontermIdent Int) ->
     _hdIinitStates `mappend` _tlIinitStates
   {-# INLINE rule150 #-}
   rule150 = \ ((_hdIoutput) :: [TH.Dec]) ((_tlIoutput) :: [TH.Dec]) ->
     _hdIoutput ++ _tlIoutput
   {-# INLINE rule151 #-}
   rule151 = \ ((_hdIsemFunBndDefs) :: Seq (TH.Name, TH.Exp)) ((_tlIsemFunBndDefs) :: Seq (TH.Name, TH.Exp)) ->
     _hdIsemFunBndDefs Seq.>< _tlIsemFunBndDefs
   {-# INLINE rule152 #-}
   rule152 = \ ((_hdIsemFunBndTps) :: Seq (TH.VarBangType)) ((_tlIsemFunBndTps) :: Seq (TH.VarBangType)) ->
     _hdIsemFunBndTps Seq.>< _tlIsemFunBndTps
   {-# INLINE rule153 #-}
   rule153 = \ ((_hdIvisitKinds) :: Map VisitIdentifier VisitKind) ((_tlIvisitKinds) :: Map VisitIdentifier VisitKind) ->
     _hdIvisitKinds `mappend` _tlIvisitKinds
   {-# INLINE rule154 #-}
   rule154 = \ ((_hdIvisitdefs) :: Map VisitIdentifier (Set Identifier)) ((_tlIvisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _hdIvisitdefs `uwSetUnion` _tlIvisitdefs
   {-# INLINE rule155 #-}
   rule155 = \ ((_hdIvisituses) :: Map VisitIdentifier (Set Identifier)) ((_tlIvisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _hdIvisituses `uwSetUnion` _tlIvisituses
   {-# INLINE rule156 #-}
   rule156 = \ ((_lhsIallFromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _lhsIallFromToStates
   {-# INLINE rule157 #-}
   rule157 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule158 #-}
   rule158 = \ ((_lhsIallVisitKinds) :: Map VisitIdentifier VisitKind) ->
     _lhsIallVisitKinds
   {-# INLINE rule159 #-}
   rule159 = \ ((_lhsIavisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisitdefs
   {-# INLINE rule160 #-}
   rule160 = \ ((_lhsIavisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisituses
   {-# INLINE rule161 #-}
   rule161 = \ ((_lhsIderivings) :: Derivings) ->
     _lhsIderivings
   {-# INLINE rule162 #-}
   rule162 = \ ((_lhsIimportBlocks) :: [String]) ->
     _lhsIimportBlocks
   {-# INLINE rule163 #-}
   rule163 = \ ((_lhsIinhmap) :: Map NontermIdent Attributes) ->
     _lhsIinhmap
   {-# INLINE rule164 #-}
   rule164 = \ ((_lhsIlocalAttrTypes) :: Map NontermIdent (Map ConstructorIdent (Map Identifier Type))) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule165 #-}
   rule165 = \ ((_lhsImainFile) :: String) ->
     _lhsImainFile
   {-# INLINE rule166 #-}
   rule166 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule167 #-}
   rule167 = \ ((_lhsImoduleHeader) :: String -> String -> String -> Bool -> String) ->
     _lhsImoduleHeader
   {-# INLINE rule168 #-}
   rule168 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule169 #-}
   rule169 = \ ((_lhsIpragmaBlocks) :: String) ->
     _lhsIpragmaBlocks
   {-# INLINE rule170 #-}
   rule170 = \ ((_lhsIsynmap) :: Map NontermIdent Attributes) ->
     _lhsIsynmap
   {-# INLINE rule171 #-}
   rule171 = \ ((_lhsItextBlocks) :: [String]) ->
     _lhsItextBlocks
   {-# INLINE rule172 #-}
   rule172 = \ ((_lhsItypeSyns) :: TypeSyns) ->
     _lhsItypeSyns
   {-# INLINE rule173 #-}
   rule173 = \ ((_lhsIwrappers) :: Set NontermIdent) ->
     _lhsIwrappers
   {-# INLINE rule174 #-}
   rule174 = \ ((_lhsIallFromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _lhsIallFromToStates
   {-# INLINE rule175 #-}
   rule175 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule176 #-}
   rule176 = \ ((_lhsIallVisitKinds) :: Map VisitIdentifier VisitKind) ->
     _lhsIallVisitKinds
   {-# INLINE rule177 #-}
   rule177 = \ ((_lhsIavisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisitdefs
   {-# INLINE rule178 #-}
   rule178 = \ ((_lhsIavisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisituses
   {-# INLINE rule179 #-}
   rule179 = \ ((_lhsIderivings) :: Derivings) ->
     _lhsIderivings
   {-# INLINE rule180 #-}
   rule180 = \ ((_lhsIimportBlocks) :: [String]) ->
     _lhsIimportBlocks
   {-# INLINE rule181 #-}
   rule181 = \ ((_lhsIinhmap) :: Map NontermIdent Attributes) ->
     _lhsIinhmap
   {-# INLINE rule182 #-}
   rule182 = \ ((_lhsIlocalAttrTypes) :: Map NontermIdent (Map ConstructorIdent (Map Identifier Type))) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule183 #-}
   rule183 = \ ((_lhsImainFile) :: String) ->
     _lhsImainFile
   {-# INLINE rule184 #-}
   rule184 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule185 #-}
   rule185 = \ ((_lhsImoduleHeader) :: String -> String -> String -> Bool -> String) ->
     _lhsImoduleHeader
   {-# INLINE rule186 #-}
   rule186 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule187 #-}
   rule187 = \ ((_lhsIpragmaBlocks) :: String) ->
     _lhsIpragmaBlocks
   {-# INLINE rule188 #-}
   rule188 = \ ((_lhsIsynmap) :: Map NontermIdent Attributes) ->
     _lhsIsynmap
   {-# INLINE rule189 #-}
   rule189 = \ ((_lhsItextBlocks) :: [String]) ->
     _lhsItextBlocks
   {-# INLINE rule190 #-}
   rule190 = \ ((_lhsItypeSyns) :: TypeSyns) ->
     _lhsItypeSyns
   {-# INLINE rule191 #-}
   rule191 = \ ((_lhsIwrappers) :: Set NontermIdent) ->
     _lhsIwrappers
{-# NOINLINE sem_ENonterminals_Nil #-}
sem_ENonterminals_Nil ::  T_ENonterminals 
sem_ENonterminals_Nil  = T_ENonterminals (return st11) where
   {-# NOINLINE st11 #-}
   st11 = let
      v10 :: T_ENonterminals_v10 
      v10 = \ (T_ENonterminals_vIn10 _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIavisitdefs _lhsIavisituses _lhsIderivings _lhsIimportBlocks _lhsIinhmap _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsIoptions _lhsIpragmaBlocks _lhsIsynmap _lhsItextBlocks _lhsItypeSyns _lhsIwrappers) -> ( let
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule192  ()
         _lhsOfromToStates :: Map VisitIdentifier (Int,Int)
         _lhsOfromToStates = rule193  ()
         _lhsOinitStates :: Map NontermIdent Int
         _lhsOinitStates = rule194  ()
         _lhsOoutput :: [TH.Dec]
         _lhsOoutput = rule195  ()
         _lhsOsemFunBndDefs :: Seq (TH.Name, TH.Exp)
         _lhsOsemFunBndDefs = rule196  ()
         _lhsOsemFunBndTps :: Seq (TH.VarBangType)
         _lhsOsemFunBndTps = rule197  ()
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule198  ()
         _lhsOvisitdefs :: Map VisitIdentifier (Set Identifier)
         _lhsOvisitdefs = rule199  ()
         _lhsOvisituses :: Map VisitIdentifier (Set Identifier)
         _lhsOvisituses = rule200  ()
         __result_ = T_ENonterminals_vOut10 _lhsOerrors _lhsOfromToStates _lhsOinitStates _lhsOoutput _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses
         in __result_ )
     in C_ENonterminals_s11 v10
   {-# INLINE rule192 #-}
   rule192 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule193 #-}
   rule193 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule194 #-}
   rule194 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule195 #-}
   rule195 = \  (_ :: ()) ->
     []
   {-# INLINE rule196 #-}
   rule196 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule197 #-}
   rule197 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule198 #-}
   rule198 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule199 #-}
   rule199 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule200 #-}
   rule200 = \  (_ :: ()) ->
     Map.empty

-- EProduction -------------------------------------------------
-- wrapper
data Inh_EProduction  = Inh_EProduction { allFromToStates_Inh_EProduction :: (Map VisitIdentifier (Int,Int)), allInhmap_Inh_EProduction :: (Map NontermIdent Attributes), allInitStates_Inh_EProduction :: (Map NontermIdent Int), allSynmap_Inh_EProduction :: (Map NontermIdent Attributes), allVisitKinds_Inh_EProduction :: (Map VisitIdentifier VisitKind), allstates_Inh_EProduction :: (Set StateIdentifier), avisitdefs_Inh_EProduction :: (Map VisitIdentifier (Set Identifier)), avisituses_Inh_EProduction :: (Map VisitIdentifier (Set Identifier)), classCtxs_Inh_EProduction :: (ClassContext), importBlocks_Inh_EProduction :: ([String]), inhmap_Inh_EProduction :: (Attributes), initial_Inh_EProduction :: (StateIdentifier), localAttrTypes_Inh_EProduction :: (Map ConstructorIdent (Map Identifier Type)), mainFile_Inh_EProduction :: (String), mainName_Inh_EProduction :: (String), moduleHeader_Inh_EProduction :: (String -> String -> String -> Bool -> String), nextVisits_Inh_EProduction :: (Map StateIdentifier StateCtx), nt_Inh_EProduction :: (NontermIdent), ntType_Inh_EProduction :: (Type), options_Inh_EProduction :: (Options), params_Inh_EProduction :: ([Identifier]), pragmaBlocks_Inh_EProduction :: (String), prevVisits_Inh_EProduction :: (Map StateIdentifier StateCtx), rename_Inh_EProduction :: (Bool), synmap_Inh_EProduction :: (Attributes), textBlocks_Inh_EProduction :: ([String]) }
data Syn_EProduction  = Syn_EProduction { allvisits_Syn_EProduction :: ([VisitStateState]), count_Syn_EProduction :: (Int), datatype_Syn_EProduction :: (TH.Con), errors_Syn_EProduction :: (Seq Error), fromToStates_Syn_EProduction :: (Map VisitIdentifier (Int,Int)), semFunBndDefs_Syn_EProduction :: (Seq (TH.Name, TH.Exp)), semFunBndTps_Syn_EProduction :: (Seq (TH.VarBangType)), sem_nt_Syn_EProduction :: ([TH.Clause]), visitKinds_Syn_EProduction :: (Map VisitIdentifier VisitKind), visitdefs_Syn_EProduction :: (Map VisitIdentifier (Set Identifier)), visituses_Syn_EProduction :: (Map VisitIdentifier (Set Identifier)) }
{-# INLINABLE wrap_EProduction #-}
wrap_EProduction :: T_EProduction  -> Inh_EProduction  -> (Syn_EProduction )
wrap_EProduction (T_EProduction act) (Inh_EProduction _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallstates _lhsIavisitdefs _lhsIavisituses _lhsIclassCtxs _lhsIimportBlocks _lhsIinhmap _lhsIinitial _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInextVisits _lhsInt _lhsIntType _lhsIoptions _lhsIparams _lhsIpragmaBlocks _lhsIprevVisits _lhsIrename _lhsIsynmap _lhsItextBlocks) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg13 = T_EProduction_vIn13 _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallstates _lhsIavisitdefs _lhsIavisituses _lhsIclassCtxs _lhsIimportBlocks _lhsIinhmap _lhsIinitial _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInextVisits _lhsInt _lhsIntType _lhsIoptions _lhsIparams _lhsIpragmaBlocks _lhsIprevVisits _lhsIrename _lhsIsynmap _lhsItextBlocks
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
data T_EProduction_vIn13  = T_EProduction_vIn13 (Map VisitIdentifier (Int,Int)) (Map NontermIdent Attributes) (Map NontermIdent Int) (Map NontermIdent Attributes) (Map VisitIdentifier VisitKind) (Set StateIdentifier) (Map VisitIdentifier (Set Identifier)) (Map VisitIdentifier (Set Identifier)) (ClassContext) ([String]) (Attributes) (StateIdentifier) (Map ConstructorIdent (Map Identifier Type)) (String) (String) (String -> String -> String -> Bool -> String) (Map StateIdentifier StateCtx) (NontermIdent) (Type) (Options) ([Identifier]) (String) (Map StateIdentifier StateCtx) (Bool) (Attributes) ([String])
data T_EProduction_vOut13  = T_EProduction_vOut13 ([VisitStateState]) (Int) (TH.Con) (Seq Error) (Map VisitIdentifier (Int,Int)) (Seq (TH.Name, TH.Exp)) (Seq (TH.VarBangType)) ([TH.Clause]) (Map VisitIdentifier VisitKind) (Map VisitIdentifier (Set Identifier)) (Map VisitIdentifier (Set Identifier))
{-# NOINLINE sem_EProduction_EProduction #-}
sem_EProduction_EProduction :: (ConstructorIdent) -> ([Identifier]) -> ([Type]) -> T_ERules  -> T_EChildren  -> T_Visits  -> T_EProduction 
sem_EProduction_EProduction arg_con_ arg_params_ arg_constraints_ arg_rules_ arg_children_ arg_visits_ = T_EProduction (return st14) where
   {-# NOINLINE st14 #-}
   st14 = let
      v13 :: T_EProduction_v13 
      v13 = \ (T_EProduction_vIn13 _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallstates _lhsIavisitdefs _lhsIavisituses _lhsIclassCtxs _lhsIimportBlocks _lhsIinhmap _lhsIinitial _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInextVisits _lhsInt _lhsIntType _lhsIoptions _lhsIparams _lhsIpragmaBlocks _lhsIprevVisits _lhsIrename _lhsIsynmap _lhsItextBlocks) -> ( let
         _rulesX23 = Control.Monad.Identity.runIdentity (attach_T_ERules (arg_rules_))
         _childrenX5 = Control.Monad.Identity.runIdentity (attach_T_EChildren (arg_children_))
         _visitsX56 = Control.Monad.Identity.runIdentity (attach_T_Visits (arg_visits_))
         (T_ERules_vOut22 _rulesIerrors _rulesIruledefs _rulesIruleuses _rulesIusedArgs) = inv_ERules_s23 _rulesX23 (T_ERules_vIn22 _rulesOallInhmap _rulesOallSynmap _rulesOchildTypes _rulesOcon _rulesOimportBlocks _rulesOinhmap _rulesOlazyIntras _rulesOlocalAttrTypes _rulesOmainFile _rulesOmainName _rulesOmoduleHeader _rulesOnt _rulesOoptions _rulesOpragmaBlocks _rulesOruleKinds _rulesOsynmap _rulesOtextBlocks _rulesOusageInfo)
         (T_EChildren_vOut4 _childrenIargnamesw _childrenIargpats _childrenIargtps _childrenIchildTypes _childrenIdatatype _childrenIdatatypeVar _childrenIterminaldefs _childrenIusedArgs) = inv_EChildren_s5 _childrenX5 (T_EChildren_vIn4 _childrenOallInitStates _childrenOcon _childrenOimportBlocks _childrenOmainFile _childrenOmainName _childrenOmoduleHeader _childrenOnt _childrenOoptions _childrenOpragmaBlocks _childrenOtextBlocks)
         (T_Visits_vOut55 _visitsIallvisits _visitsIerrors _visitsIfromToStates _visitsIintramap _visitsIlazyIntras _visitsIruleKinds _visitsIruleUsage _visitsIusedArgs _visitsIvisitKinds _visitsIvisitdefs _visitsIvisituses) = inv_Visits_s56 _visitsX56 (T_Visits_vIn55 _visitsOallFromToStates _visitsOallInhmap _visitsOallInitStates _visitsOallSynmap _visitsOallVisitKinds _visitsOallintramap _visitsOavisitdefs _visitsOavisituses _visitsOchildTypes _visitsOcon _visitsOinhmap _visitsOnextVisits _visitsOnt _visitsOoptions _visitsOparams _visitsOprevVisits _visitsOruledefs _visitsOruleuses _visitsOsynmap _visitsOterminaldefs)
         _childrenOcon = rule201 arg_con_
         _rulesOcon = rule202 arg_con_
         _visitsOcon = rule203 arg_con_
         _lhsOdatatype :: TH.Con
         _lhsOdatatype = rule204 _childrenIdatatype _childrenIdatatypeVar _classTH1 _lhsInt _lhsIoptions _lhsIrename _quantTH1 arg_con_
         _classTH1 = rule205 arg_constraints_
         _quantTH1 = rule206 arg_params_
         _lhsOcount :: Int
         _lhsOcount = rule207  ()
         _lhsOsem_nt :: [TH.Clause]
         _lhsOsem_nt = rule208 _childrenIargnamesw _childrenIargpats _lhsInt _lhsIrename arg_con_
         _lhsOsemFunBndDefs :: Seq (TH.Name, TH.Exp)
         _lhsOsemFunBndDefs = rule209 _semFunBndDef
         _lhsOsemFunBndTps :: Seq (TH.VarBangType)
         _lhsOsemFunBndTps = rule210 _semFunBndTp
         _semFunBndDef = rule211 _semFunBndNm _semname
         _semFunBndTp = rule212 _semFunBndNm _sem_tp
         _semFunBndNm = rule213 _lhsInt arg_con_
         _t_type = rule214 _lhsInt
         _t_params = rule215 _lhsIparams
         _semname = rule216 _lhsInt arg_con_
         _sem_tp = rule217 _childrenIargtps _classTH2 _quantTH2 _t_params _t_type
         _classTH2 = rule218 _lhsIclassCtxs arg_constraints_
         _quantTH2 = rule219 _lhsIparams arg_params_
         _rulesOusageInfo = rule220 _visitsIruleUsage
         _lazyIntras = rule221 _visitsIlazyIntras
         _addbang = rule222 _lhsIoptions
         _childTypes = rule223 _childrenIchildTypes _lhsIntType
         _localAttrTypes = rule224 _lhsIlocalAttrTypes arg_con_
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule225 _rulesIerrors _visitsIerrors
         _lhsOfromToStates :: Map VisitIdentifier (Int,Int)
         _lhsOfromToStates = rule226 _visitsIfromToStates
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule227 _visitsIvisitKinds
         _lhsOvisitdefs :: Map VisitIdentifier (Set Identifier)
         _lhsOvisitdefs = rule228 _visitsIvisitdefs
         _lhsOvisituses :: Map VisitIdentifier (Set Identifier)
         _lhsOvisituses = rule229 _visitsIvisituses
         _lhsOallvisits :: [VisitStateState]
         _lhsOallvisits = rule230 _visitsIallvisits
         _rulesOallInhmap = rule231 _lhsIallInhmap
         _rulesOallSynmap = rule232 _lhsIallSynmap
         _rulesOchildTypes = rule233 _childTypes
         _rulesOimportBlocks = rule234 _lhsIimportBlocks
         _rulesOinhmap = rule235 _lhsIinhmap
         _rulesOlazyIntras = rule236 _lazyIntras
         _rulesOlocalAttrTypes = rule237 _localAttrTypes
         _rulesOmainFile = rule238 _lhsImainFile
         _rulesOmainName = rule239 _lhsImainName
         _rulesOmoduleHeader = rule240 _lhsImoduleHeader
         _rulesOnt = rule241 _lhsInt
         _rulesOoptions = rule242 _lhsIoptions
         _rulesOpragmaBlocks = rule243 _lhsIpragmaBlocks
         _rulesOruleKinds = rule244  ()
         _rulesOsynmap = rule245 _lhsIsynmap
         _rulesOtextBlocks = rule246 _lhsItextBlocks
         _childrenOallInitStates = rule247 _lhsIallInitStates
         _childrenOimportBlocks = rule248 _lhsIimportBlocks
         _childrenOmainFile = rule249 _lhsImainFile
         _childrenOmainName = rule250 _lhsImainName
         _childrenOmoduleHeader = rule251 _lhsImoduleHeader
         _childrenOnt = rule252 _lhsInt
         _childrenOoptions = rule253 _lhsIoptions
         _childrenOpragmaBlocks = rule254 _lhsIpragmaBlocks
         _childrenOtextBlocks = rule255 _lhsItextBlocks
         _visitsOallFromToStates = rule256 _lhsIallFromToStates
         _visitsOallInhmap = rule257 _lhsIallInhmap
         _visitsOallInitStates = rule258 _lhsIallInitStates
         _visitsOallSynmap = rule259 _lhsIallSynmap
         _visitsOallVisitKinds = rule260 _lhsIallVisitKinds
         _visitsOallintramap = rule261  ()
         _visitsOavisitdefs = rule262 _lhsIavisitdefs
         _visitsOavisituses = rule263 _lhsIavisituses
         _visitsOchildTypes = rule264 _childTypes
         _visitsOinhmap = rule265 _lhsIinhmap
         _visitsOnextVisits = rule266 _lhsInextVisits
         _visitsOnt = rule267 _lhsInt
         _visitsOoptions = rule268 _lhsIoptions
         _visitsOparams = rule269 _lhsIparams
         _visitsOprevVisits = rule270 _lhsIprevVisits
         _visitsOruledefs = rule271 _rulesIruledefs
         _visitsOruleuses = rule272 _rulesIruleuses
         _visitsOsynmap = rule273 _lhsIsynmap
         _visitsOterminaldefs = rule274 _childrenIterminaldefs
         __result_ = T_EProduction_vOut13 _lhsOallvisits _lhsOcount _lhsOdatatype _lhsOerrors _lhsOfromToStates _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOsem_nt _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses
         in __result_ )
     in C_EProduction_s14 v13
   {-# INLINE rule201 #-}
   {-# LINE 85 "src-ag/ExecutionPlan2TH.ag" #-}
   rule201 = \ con_ ->
                                 {-# LINE 85 "src-ag/ExecutionPlan2TH.ag" #-}
                                 con_
                                 {-# LINE 1905 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule202 #-}
   {-# LINE 86 "src-ag/ExecutionPlan2TH.ag" #-}
   rule202 = \ con_ ->
                                 {-# LINE 86 "src-ag/ExecutionPlan2TH.ag" #-}
                                 con_
                                 {-# LINE 1911 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule203 #-}
   {-# LINE 87 "src-ag/ExecutionPlan2TH.ag" #-}
   rule203 = \ con_ ->
                                 {-# LINE 87 "src-ag/ExecutionPlan2TH.ag" #-}
                                 con_
                                 {-# LINE 1917 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule204 #-}
   {-# LINE 191 "src-ag/ExecutionPlan2TH.ag" #-}
   rule204 = \ ((_childrenIdatatype) :: [TH.BangType]) ((_childrenIdatatypeVar) :: [TH.VarBangType]) _classTH1 ((_lhsInt) :: NontermIdent) ((_lhsIoptions) :: Options) ((_lhsIrename) :: Bool) _quantTH1 con_ ->
                                 {-# LINE 191 "src-ag/ExecutionPlan2TH.ag" #-}
                                 _quantTH1     _classTH1
                                 (conTH (TH.mkName (conname _lhsIrename _lhsInt con_))
                                        (if dataRecords _lhsIoptions
                                           then Left _childrenIdatatypeVar
                                           else Right _childrenIdatatype))
                                 {-# LINE 1927 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule205 #-}
   {-# LINE 196 "src-ag/ExecutionPlan2TH.ag" #-}
   rule205 = \ constraints_ ->
                                 {-# LINE 196 "src-ag/ExecutionPlan2TH.ag" #-}
                                 map typeToTH constraints_
                                 {-# LINE 1933 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule206 #-}
   {-# LINE 197 "src-ag/ExecutionPlan2TH.ag" #-}
   rule206 = \ params_ ->
                                 {-# LINE 197 "src-ag/ExecutionPlan2TH.ag" #-}
                                 quantsTH TH.ForallC params_
                                 {-# LINE 1939 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule207 #-}
   {-# LINE 349 "src-ag/ExecutionPlan2TH.ag" #-}
   rule207 = \  (_ :: ()) ->
                                              {-# LINE 349 "src-ag/ExecutionPlan2TH.ag" #-}
                                              1
                                              {-# LINE 1945 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule208 #-}
   {-# LINE 354 "src-ag/ExecutionPlan2TH.ag" #-}
   rule208 = \ ((_childrenIargnamesw) :: [TH.Exp]) ((_childrenIargpats) ::  [TH.Pat] ) ((_lhsInt) :: NontermIdent) ((_lhsIrename) :: Bool) con_ ->
                               {-# LINE 354 "src-ag/ExecutionPlan2TH.ag" #-}
                               [ TH.Clause [TH.ConP (TH.mkName (conname _lhsIrename _lhsInt con_)) _childrenIargpats]
                                           (TH.NormalB (foldl TH.AppE
                                                              (TH.VarE (TH.mkName ("sem_" ++ getName _lhsInt ++ "_" ++ getName con_)))
                                                              _childrenIargnamesw))
                                           []
                               ]
                               {-# LINE 1956 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule209 #-}
   {-# LINE 617 "src-ag/ExecutionPlan2TH.ag" #-}
   rule209 = \ _semFunBndDef ->
                        {-# LINE 617 "src-ag/ExecutionPlan2TH.ag" #-}
                        Seq.singleton _semFunBndDef
                        {-# LINE 1962 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule210 #-}
   {-# LINE 618 "src-ag/ExecutionPlan2TH.ag" #-}
   rule210 = \ _semFunBndTp ->
                        {-# LINE 618 "src-ag/ExecutionPlan2TH.ag" #-}
                        Seq.singleton _semFunBndTp
                        {-# LINE 1968 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule211 #-}
   {-# LINE 619 "src-ag/ExecutionPlan2TH.ag" #-}
   rule211 = \ _semFunBndNm _semname ->
                        {-# LINE 619 "src-ag/ExecutionPlan2TH.ag" #-}
                        (_semFunBndNm    , TH.VarE _semname    )
                        {-# LINE 1974 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule212 #-}
   {-# LINE 620 "src-ag/ExecutionPlan2TH.ag" #-}
   rule212 = \ _semFunBndNm _sem_tp ->
                        {-# LINE 620 "src-ag/ExecutionPlan2TH.ag" #-}
                        (_semFunBndNm    , nobang, _sem_tp    )
                        {-# LINE 1980 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule213 #-}
   {-# LINE 621 "src-ag/ExecutionPlan2TH.ag" #-}
   rule213 = \ ((_lhsInt) :: NontermIdent) con_ ->
                        {-# LINE 621 "src-ag/ExecutionPlan2TH.ag" #-}
                        TH.mkName (lateSemConLabel _lhsInt con_)
                        {-# LINE 1986 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule214 #-}
   {-# LINE 683 "src-ag/ExecutionPlan2TH.ag" #-}
   rule214 = \ ((_lhsInt) :: NontermIdent) ->
                                 {-# LINE 683 "src-ag/ExecutionPlan2TH.ag" #-}
                                 TH.ConT (TH.mkName ("T_" ++ getName _lhsInt))
                                 {-# LINE 1992 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule215 #-}
   {-# LINE 684 "src-ag/ExecutionPlan2TH.ag" #-}
   rule215 = \ ((_lhsIparams) :: [Identifier]) ->
                                 {-# LINE 684 "src-ag/ExecutionPlan2TH.ag" #-}
                                 map (TH.VarT . TH.mkName . getName) _lhsIparams
                                 {-# LINE 1998 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule216 #-}
   {-# LINE 695 "src-ag/ExecutionPlan2TH.ag" #-}
   rule216 = \ ((_lhsInt) :: NontermIdent) con_ ->
                                 {-# LINE 695 "src-ag/ExecutionPlan2TH.ag" #-}
                                 TH.mkName ("sem_" ++ show _lhsInt ++ "_" ++ show con_)
                                 {-# LINE 2004 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule217 #-}
   {-# LINE 696 "src-ag/ExecutionPlan2TH.ag" #-}
   rule217 = \ ((_childrenIargtps) ::  TH.Type -> TH.Type ) _classTH2 _quantTH2 _t_params _t_type ->
                                 {-# LINE 696 "src-ag/ExecutionPlan2TH.ag" #-}
                                 _quantTH2     _classTH2     (_childrenIargtps (foldl TH.AppT _t_type     _t_params    ))
                                 {-# LINE 2010 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule218 #-}
   {-# LINE 697 "src-ag/ExecutionPlan2TH.ag" #-}
   rule218 = \ ((_lhsIclassCtxs) :: ClassContext) constraints_ ->
                                 {-# LINE 697 "src-ag/ExecutionPlan2TH.ag" #-}
                                 classCtxsToCxt _lhsIclassCtxs ++ classConstrsToCxt constraints_
                                 {-# LINE 2016 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule219 #-}
   {-# LINE 698 "src-ag/ExecutionPlan2TH.ag" #-}
   rule219 = \ ((_lhsIparams) :: [Identifier]) params_ ->
                                 {-# LINE 698 "src-ag/ExecutionPlan2TH.ag" #-}
                                 quantsTH TH.ForallT (_lhsIparams ++ params_)
                                 {-# LINE 2022 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule220 #-}
   {-# LINE 1368 "src-ag/ExecutionPlan2TH.ag" #-}
   rule220 = \ ((_visitsIruleUsage) :: Map Identifier Int) ->
                                                   {-# LINE 1368 "src-ag/ExecutionPlan2TH.ag" #-}
                                                   _visitsIruleUsage
                                                   {-# LINE 2028 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule221 #-}
   {-# LINE 1492 "src-ag/ExecutionPlan2TH.ag" #-}
   rule221 = \ ((_visitsIlazyIntras) :: Set String) ->
                     {-# LINE 1492 "src-ag/ExecutionPlan2TH.ag" #-}
                     _visitsIlazyIntras
                     {-# LINE 2034 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule222 #-}
   {-# LINE 1640 "src-ag/ExecutionPlan2TH.ag" #-}
   rule222 = \ ((_lhsIoptions) :: Options) ->
                                                    {-# LINE 1640 "src-ag/ExecutionPlan2TH.ag" #-}
                                                    \x -> if bangpats _lhsIoptions then TH.BangP x else x
                                                    {-# LINE 2040 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule223 #-}
   {-# LINE 1692 "src-ag/ExecutionPlan2TH.ag" #-}
   rule223 = \ ((_childrenIchildTypes) :: Map Identifier Type) ((_lhsIntType) :: Type) ->
                     {-# LINE 1692 "src-ag/ExecutionPlan2TH.ag" #-}
                     Map.singleton _LHS _lhsIntType `Map.union` _childrenIchildTypes
                     {-# LINE 2046 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule224 #-}
   {-# LINE 1709 "src-ag/ExecutionPlan2TH.ag" #-}
   rule224 = \ ((_lhsIlocalAttrTypes) :: Map ConstructorIdent (Map Identifier Type)) con_ ->
                           {-# LINE 1709 "src-ag/ExecutionPlan2TH.ag" #-}
                           Map.findWithDefault Map.empty con_ _lhsIlocalAttrTypes
                           {-# LINE 2052 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule225 #-}
   rule225 = \ ((_rulesIerrors) :: Seq Error) ((_visitsIerrors) :: Seq Error) ->
     _rulesIerrors Seq.>< _visitsIerrors
   {-# INLINE rule226 #-}
   rule226 = \ ((_visitsIfromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _visitsIfromToStates
   {-# INLINE rule227 #-}
   rule227 = \ ((_visitsIvisitKinds) :: Map VisitIdentifier VisitKind) ->
     _visitsIvisitKinds
   {-# INLINE rule228 #-}
   rule228 = \ ((_visitsIvisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _visitsIvisitdefs
   {-# INLINE rule229 #-}
   rule229 = \ ((_visitsIvisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _visitsIvisituses
   {-# INLINE rule230 #-}
   rule230 = \ ((_visitsIallvisits) :: [VisitStateState]) ->
     _visitsIallvisits
   {-# INLINE rule231 #-}
   rule231 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule232 #-}
   rule232 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule233 #-}
   rule233 = \ _childTypes ->
     _childTypes
   {-# INLINE rule234 #-}
   rule234 = \ ((_lhsIimportBlocks) :: [String]) ->
     _lhsIimportBlocks
   {-# INLINE rule235 #-}
   rule235 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule236 #-}
   rule236 = \ _lazyIntras ->
     _lazyIntras
   {-# INLINE rule237 #-}
   rule237 = \ _localAttrTypes ->
     _localAttrTypes
   {-# INLINE rule238 #-}
   rule238 = \ ((_lhsImainFile) :: String) ->
     _lhsImainFile
   {-# INLINE rule239 #-}
   rule239 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule240 #-}
   rule240 = \ ((_lhsImoduleHeader) :: String -> String -> String -> Bool -> String) ->
     _lhsImoduleHeader
   {-# INLINE rule241 #-}
   rule241 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule242 #-}
   rule242 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule243 #-}
   rule243 = \ ((_lhsIpragmaBlocks) :: String) ->
     _lhsIpragmaBlocks
   {-# INLINE rule244 #-}
   rule244 = \  (_ :: ()) ->
     error "missing rule: EProduction.EProduction.rules.ruleKinds"
   {-# INLINE rule245 #-}
   rule245 = \ ((_lhsIsynmap) :: Attributes) ->
     _lhsIsynmap
   {-# INLINE rule246 #-}
   rule246 = \ ((_lhsItextBlocks) :: [String]) ->
     _lhsItextBlocks
   {-# INLINE rule247 #-}
   rule247 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule248 #-}
   rule248 = \ ((_lhsIimportBlocks) :: [String]) ->
     _lhsIimportBlocks
   {-# INLINE rule249 #-}
   rule249 = \ ((_lhsImainFile) :: String) ->
     _lhsImainFile
   {-# INLINE rule250 #-}
   rule250 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule251 #-}
   rule251 = \ ((_lhsImoduleHeader) :: String -> String -> String -> Bool -> String) ->
     _lhsImoduleHeader
   {-# INLINE rule252 #-}
   rule252 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule253 #-}
   rule253 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule254 #-}
   rule254 = \ ((_lhsIpragmaBlocks) :: String) ->
     _lhsIpragmaBlocks
   {-# INLINE rule255 #-}
   rule255 = \ ((_lhsItextBlocks) :: [String]) ->
     _lhsItextBlocks
   {-# INLINE rule256 #-}
   rule256 = \ ((_lhsIallFromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _lhsIallFromToStates
   {-# INLINE rule257 #-}
   rule257 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule258 #-}
   rule258 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule259 #-}
   rule259 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule260 #-}
   rule260 = \ ((_lhsIallVisitKinds) :: Map VisitIdentifier VisitKind) ->
     _lhsIallVisitKinds
   {-# INLINE rule261 #-}
   rule261 = \  (_ :: ()) ->
     error "missing rule: EProduction.EProduction.visits.allintramap"
   {-# INLINE rule262 #-}
   rule262 = \ ((_lhsIavisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisitdefs
   {-# INLINE rule263 #-}
   rule263 = \ ((_lhsIavisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisituses
   {-# INLINE rule264 #-}
   rule264 = \ _childTypes ->
     _childTypes
   {-# INLINE rule265 #-}
   rule265 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule266 #-}
   rule266 = \ ((_lhsInextVisits) :: Map StateIdentifier StateCtx) ->
     _lhsInextVisits
   {-# INLINE rule267 #-}
   rule267 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule268 #-}
   rule268 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule269 #-}
   rule269 = \ ((_lhsIparams) :: [Identifier]) ->
     _lhsIparams
   {-# INLINE rule270 #-}
   rule270 = \ ((_lhsIprevVisits) :: Map StateIdentifier StateCtx) ->
     _lhsIprevVisits
   {-# INLINE rule271 #-}
   rule271 = \ ((_rulesIruledefs) :: Map Identifier (Set String)) ->
     _rulesIruledefs
   {-# INLINE rule272 #-}
   rule272 = \ ((_rulesIruleuses) :: Map Identifier (Map String (Maybe NonLocalAttr))) ->
     _rulesIruleuses
   {-# INLINE rule273 #-}
   rule273 = \ ((_lhsIsynmap) :: Attributes) ->
     _lhsIsynmap
   {-# INLINE rule274 #-}
   rule274 = \ ((_childrenIterminaldefs) :: Set String) ->
     _childrenIterminaldefs

-- EProductions ------------------------------------------------
-- wrapper
data Inh_EProductions  = Inh_EProductions { allFromToStates_Inh_EProductions :: (Map VisitIdentifier (Int,Int)), allInhmap_Inh_EProductions :: (Map NontermIdent Attributes), allInitStates_Inh_EProductions :: (Map NontermIdent Int), allSynmap_Inh_EProductions :: (Map NontermIdent Attributes), allVisitKinds_Inh_EProductions :: (Map VisitIdentifier VisitKind), allstates_Inh_EProductions :: (Set StateIdentifier), avisitdefs_Inh_EProductions :: (Map VisitIdentifier (Set Identifier)), avisituses_Inh_EProductions :: (Map VisitIdentifier (Set Identifier)), classCtxs_Inh_EProductions :: (ClassContext), importBlocks_Inh_EProductions :: ([String]), inhmap_Inh_EProductions :: (Attributes), initial_Inh_EProductions :: (StateIdentifier), localAttrTypes_Inh_EProductions :: (Map ConstructorIdent (Map Identifier Type)), mainFile_Inh_EProductions :: (String), mainName_Inh_EProductions :: (String), moduleHeader_Inh_EProductions :: (String -> String -> String -> Bool -> String), nextVisits_Inh_EProductions :: (Map StateIdentifier StateCtx), nt_Inh_EProductions :: (NontermIdent), ntType_Inh_EProductions :: (Type), options_Inh_EProductions :: (Options), params_Inh_EProductions :: ([Identifier]), pragmaBlocks_Inh_EProductions :: (String), prevVisits_Inh_EProductions :: (Map StateIdentifier StateCtx), rename_Inh_EProductions :: (Bool), synmap_Inh_EProductions :: (Attributes), textBlocks_Inh_EProductions :: ([String]) }
data Syn_EProductions  = Syn_EProductions { allvisits_Syn_EProductions :: ([VisitStateState]), count_Syn_EProductions :: (Int), datatype_Syn_EProductions :: ([TH.Con]), errors_Syn_EProductions :: (Seq Error), fromToStates_Syn_EProductions :: (Map VisitIdentifier (Int,Int)), semFunBndDefs_Syn_EProductions :: (Seq (TH.Name, TH.Exp)), semFunBndTps_Syn_EProductions :: (Seq (TH.VarBangType)), sem_nt_Syn_EProductions :: ([TH.Clause]), visitKinds_Syn_EProductions :: (Map VisitIdentifier VisitKind), visitdefs_Syn_EProductions :: (Map VisitIdentifier (Set Identifier)), visituses_Syn_EProductions :: (Map VisitIdentifier (Set Identifier)) }
{-# INLINABLE wrap_EProductions #-}
wrap_EProductions :: T_EProductions  -> Inh_EProductions  -> (Syn_EProductions )
wrap_EProductions (T_EProductions act) (Inh_EProductions _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallstates _lhsIavisitdefs _lhsIavisituses _lhsIclassCtxs _lhsIimportBlocks _lhsIinhmap _lhsIinitial _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInextVisits _lhsInt _lhsIntType _lhsIoptions _lhsIparams _lhsIpragmaBlocks _lhsIprevVisits _lhsIrename _lhsIsynmap _lhsItextBlocks) =
   Control.Monad.Identity.runIdentity (
     do sem <- act
        let arg16 = T_EProductions_vIn16 _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallstates _lhsIavisitdefs _lhsIavisituses _lhsIclassCtxs _lhsIimportBlocks _lhsIinhmap _lhsIinitial _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInextVisits _lhsInt _lhsIntType _lhsIoptions _lhsIparams _lhsIpragmaBlocks _lhsIprevVisits _lhsIrename _lhsIsynmap _lhsItextBlocks
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
data T_EProductions_vIn16  = T_EProductions_vIn16 (Map VisitIdentifier (Int,Int)) (Map NontermIdent Attributes) (Map NontermIdent Int) (Map NontermIdent Attributes) (Map VisitIdentifier VisitKind) (Set StateIdentifier) (Map VisitIdentifier (Set Identifier)) (Map VisitIdentifier (Set Identifier)) (ClassContext) ([String]) (Attributes) (StateIdentifier) (Map ConstructorIdent (Map Identifier Type)) (String) (String) (String -> String -> String -> Bool -> String) (Map StateIdentifier StateCtx) (NontermIdent) (Type) (Options) ([Identifier]) (String) (Map StateIdentifier StateCtx) (Bool) (Attributes) ([String])
data T_EProductions_vOut16  = T_EProductions_vOut16 ([VisitStateState]) (Int) ([TH.Con]) (Seq Error) (Map VisitIdentifier (Int,Int)) (Seq (TH.Name, TH.Exp)) (Seq (TH.VarBangType)) ([TH.Clause]) (Map VisitIdentifier VisitKind) (Map VisitIdentifier (Set Identifier)) (Map VisitIdentifier (Set Identifier))
{-# NOINLINE sem_EProductions_Cons #-}
sem_EProductions_Cons :: T_EProduction  -> T_EProductions  -> T_EProductions 
sem_EProductions_Cons arg_hd_ arg_tl_ = T_EProductions (return st17) where
   {-# NOINLINE st17 #-}
   st17 = let
      v16 :: T_EProductions_v16 
      v16 = \ (T_EProductions_vIn16 _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallstates _lhsIavisitdefs _lhsIavisituses _lhsIclassCtxs _lhsIimportBlocks _lhsIinhmap _lhsIinitial _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInextVisits _lhsInt _lhsIntType _lhsIoptions _lhsIparams _lhsIpragmaBlocks _lhsIprevVisits _lhsIrename _lhsIsynmap _lhsItextBlocks) -> ( let
         _hdX14 = Control.Monad.Identity.runIdentity (attach_T_EProduction (arg_hd_))
         _tlX17 = Control.Monad.Identity.runIdentity (attach_T_EProductions (arg_tl_))
         (T_EProduction_vOut13 _hdIallvisits _hdIcount _hdIdatatype _hdIerrors _hdIfromToStates _hdIsemFunBndDefs _hdIsemFunBndTps _hdIsem_nt _hdIvisitKinds _hdIvisitdefs _hdIvisituses) = inv_EProduction_s14 _hdX14 (T_EProduction_vIn13 _hdOallFromToStates _hdOallInhmap _hdOallInitStates _hdOallSynmap _hdOallVisitKinds _hdOallstates _hdOavisitdefs _hdOavisituses _hdOclassCtxs _hdOimportBlocks _hdOinhmap _hdOinitial _hdOlocalAttrTypes _hdOmainFile _hdOmainName _hdOmoduleHeader _hdOnextVisits _hdOnt _hdOntType _hdOoptions _hdOparams _hdOpragmaBlocks _hdOprevVisits _hdOrename _hdOsynmap _hdOtextBlocks)
         (T_EProductions_vOut16 _tlIallvisits _tlIcount _tlIdatatype _tlIerrors _tlIfromToStates _tlIsemFunBndDefs _tlIsemFunBndTps _tlIsem_nt _tlIvisitKinds _tlIvisitdefs _tlIvisituses) = inv_EProductions_s17 _tlX17 (T_EProductions_vIn16 _tlOallFromToStates _tlOallInhmap _tlOallInitStates _tlOallSynmap _tlOallVisitKinds _tlOallstates _tlOavisitdefs _tlOavisituses _tlOclassCtxs _tlOimportBlocks _tlOinhmap _tlOinitial _tlOlocalAttrTypes _tlOmainFile _tlOmainName _tlOmoduleHeader _tlOnextVisits _tlOnt _tlOntType _tlOoptions _tlOparams _tlOpragmaBlocks _tlOprevVisits _tlOrename _tlOsynmap _tlOtextBlocks)
         _lhsOallvisits :: [VisitStateState]
         _lhsOallvisits = rule275 _hdIallvisits
         _lhsOcount :: Int
         _lhsOcount = rule276 _hdIcount _tlIcount
         _lhsOdatatype :: [TH.Con]
         _lhsOdatatype = rule277 _hdIdatatype _tlIdatatype
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule278 _hdIerrors _tlIerrors
         _lhsOfromToStates :: Map VisitIdentifier (Int,Int)
         _lhsOfromToStates = rule279 _hdIfromToStates _tlIfromToStates
         _lhsOsemFunBndDefs :: Seq (TH.Name, TH.Exp)
         _lhsOsemFunBndDefs = rule280 _hdIsemFunBndDefs _tlIsemFunBndDefs
         _lhsOsemFunBndTps :: Seq (TH.VarBangType)
         _lhsOsemFunBndTps = rule281 _hdIsemFunBndTps _tlIsemFunBndTps
         _lhsOsem_nt :: [TH.Clause]
         _lhsOsem_nt = rule282 _hdIsem_nt _tlIsem_nt
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule283 _hdIvisitKinds _tlIvisitKinds
         _lhsOvisitdefs :: Map VisitIdentifier (Set Identifier)
         _lhsOvisitdefs = rule284 _hdIvisitdefs _tlIvisitdefs
         _lhsOvisituses :: Map VisitIdentifier (Set Identifier)
         _lhsOvisituses = rule285 _hdIvisituses _tlIvisituses
         _hdOallFromToStates = rule286 _lhsIallFromToStates
         _hdOallInhmap = rule287 _lhsIallInhmap
         _hdOallInitStates = rule288 _lhsIallInitStates
         _hdOallSynmap = rule289 _lhsIallSynmap
         _hdOallVisitKinds = rule290 _lhsIallVisitKinds
         _hdOallstates = rule291 _lhsIallstates
         _hdOavisitdefs = rule292 _lhsIavisitdefs
         _hdOavisituses = rule293 _lhsIavisituses
         _hdOclassCtxs = rule294 _lhsIclassCtxs
         _hdOimportBlocks = rule295 _lhsIimportBlocks
         _hdOinhmap = rule296 _lhsIinhmap
         _hdOinitial = rule297 _lhsIinitial
         _hdOlocalAttrTypes = rule298 _lhsIlocalAttrTypes
         _hdOmainFile = rule299 _lhsImainFile
         _hdOmainName = rule300 _lhsImainName
         _hdOmoduleHeader = rule301 _lhsImoduleHeader
         _hdOnextVisits = rule302 _lhsInextVisits
         _hdOnt = rule303 _lhsInt
         _hdOntType = rule304 _lhsIntType
         _hdOoptions = rule305 _lhsIoptions
         _hdOparams = rule306 _lhsIparams
         _hdOpragmaBlocks = rule307 _lhsIpragmaBlocks
         _hdOprevVisits = rule308 _lhsIprevVisits
         _hdOrename = rule309 _lhsIrename
         _hdOsynmap = rule310 _lhsIsynmap
         _hdOtextBlocks = rule311 _lhsItextBlocks
         _tlOallFromToStates = rule312 _lhsIallFromToStates
         _tlOallInhmap = rule313 _lhsIallInhmap
         _tlOallInitStates = rule314 _lhsIallInitStates
         _tlOallSynmap = rule315 _lhsIallSynmap
         _tlOallVisitKinds = rule316 _lhsIallVisitKinds
         _tlOallstates = rule317 _lhsIallstates
         _tlOavisitdefs = rule318 _lhsIavisitdefs
         _tlOavisituses = rule319 _lhsIavisituses
         _tlOclassCtxs = rule320 _lhsIclassCtxs
         _tlOimportBlocks = rule321 _lhsIimportBlocks
         _tlOinhmap = rule322 _lhsIinhmap
         _tlOinitial = rule323 _lhsIinitial
         _tlOlocalAttrTypes = rule324 _lhsIlocalAttrTypes
         _tlOmainFile = rule325 _lhsImainFile
         _tlOmainName = rule326 _lhsImainName
         _tlOmoduleHeader = rule327 _lhsImoduleHeader
         _tlOnextVisits = rule328 _lhsInextVisits
         _tlOnt = rule329 _lhsInt
         _tlOntType = rule330 _lhsIntType
         _tlOoptions = rule331 _lhsIoptions
         _tlOparams = rule332 _lhsIparams
         _tlOpragmaBlocks = rule333 _lhsIpragmaBlocks
         _tlOprevVisits = rule334 _lhsIprevVisits
         _tlOrename = rule335 _lhsIrename
         _tlOsynmap = rule336 _lhsIsynmap
         _tlOtextBlocks = rule337 _lhsItextBlocks
         __result_ = T_EProductions_vOut16 _lhsOallvisits _lhsOcount _lhsOdatatype _lhsOerrors _lhsOfromToStates _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOsem_nt _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses
         in __result_ )
     in C_EProductions_s17 v16
   {-# INLINE rule275 #-}
   {-# LINE 410 "src-ag/ExecutionPlan2TH.ag" #-}
   rule275 = \ ((_hdIallvisits) :: [VisitStateState]) ->
                           {-# LINE 410 "src-ag/ExecutionPlan2TH.ag" #-}
                           _hdIallvisits
                           {-# LINE 2327 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule276 #-}
   rule276 = \ ((_hdIcount) :: Int) ((_tlIcount) :: Int) ->
     _hdIcount + _tlIcount
   {-# INLINE rule277 #-}
   rule277 = \ ((_hdIdatatype) :: TH.Con) ((_tlIdatatype) :: [TH.Con]) ->
     _hdIdatatype : _tlIdatatype
   {-# INLINE rule278 #-}
   rule278 = \ ((_hdIerrors) :: Seq Error) ((_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# INLINE rule279 #-}
   rule279 = \ ((_hdIfromToStates) :: Map VisitIdentifier (Int,Int)) ((_tlIfromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _hdIfromToStates `mappend` _tlIfromToStates
   {-# INLINE rule280 #-}
   rule280 = \ ((_hdIsemFunBndDefs) :: Seq (TH.Name, TH.Exp)) ((_tlIsemFunBndDefs) :: Seq (TH.Name, TH.Exp)) ->
     _hdIsemFunBndDefs Seq.>< _tlIsemFunBndDefs
   {-# INLINE rule281 #-}
   rule281 = \ ((_hdIsemFunBndTps) :: Seq (TH.VarBangType)) ((_tlIsemFunBndTps) :: Seq (TH.VarBangType)) ->
     _hdIsemFunBndTps Seq.>< _tlIsemFunBndTps
   {-# INLINE rule282 #-}
   rule282 = \ ((_hdIsem_nt) :: [TH.Clause]) ((_tlIsem_nt) :: [TH.Clause]) ->
     _hdIsem_nt ++ _tlIsem_nt
   {-# INLINE rule283 #-}
   rule283 = \ ((_hdIvisitKinds) :: Map VisitIdentifier VisitKind) ((_tlIvisitKinds) :: Map VisitIdentifier VisitKind) ->
     _hdIvisitKinds `mappend` _tlIvisitKinds
   {-# INLINE rule284 #-}
   rule284 = \ ((_hdIvisitdefs) :: Map VisitIdentifier (Set Identifier)) ((_tlIvisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _hdIvisitdefs `uwSetUnion` _tlIvisitdefs
   {-# INLINE rule285 #-}
   rule285 = \ ((_hdIvisituses) :: Map VisitIdentifier (Set Identifier)) ((_tlIvisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _hdIvisituses `uwSetUnion` _tlIvisituses
   {-# INLINE rule286 #-}
   rule286 = \ ((_lhsIallFromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _lhsIallFromToStates
   {-# INLINE rule287 #-}
   rule287 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule288 #-}
   rule288 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule289 #-}
   rule289 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule290 #-}
   rule290 = \ ((_lhsIallVisitKinds) :: Map VisitIdentifier VisitKind) ->
     _lhsIallVisitKinds
   {-# INLINE rule291 #-}
   rule291 = \ ((_lhsIallstates) :: Set StateIdentifier) ->
     _lhsIallstates
   {-# INLINE rule292 #-}
   rule292 = \ ((_lhsIavisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisitdefs
   {-# INLINE rule293 #-}
   rule293 = \ ((_lhsIavisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisituses
   {-# INLINE rule294 #-}
   rule294 = \ ((_lhsIclassCtxs) :: ClassContext) ->
     _lhsIclassCtxs
   {-# INLINE rule295 #-}
   rule295 = \ ((_lhsIimportBlocks) :: [String]) ->
     _lhsIimportBlocks
   {-# INLINE rule296 #-}
   rule296 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule297 #-}
   rule297 = \ ((_lhsIinitial) :: StateIdentifier) ->
     _lhsIinitial
   {-# INLINE rule298 #-}
   rule298 = \ ((_lhsIlocalAttrTypes) :: Map ConstructorIdent (Map Identifier Type)) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule299 #-}
   rule299 = \ ((_lhsImainFile) :: String) ->
     _lhsImainFile
   {-# INLINE rule300 #-}
   rule300 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule301 #-}
   rule301 = \ ((_lhsImoduleHeader) :: String -> String -> String -> Bool -> String) ->
     _lhsImoduleHeader
   {-# INLINE rule302 #-}
   rule302 = \ ((_lhsInextVisits) :: Map StateIdentifier StateCtx) ->
     _lhsInextVisits
   {-# INLINE rule303 #-}
   rule303 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule304 #-}
   rule304 = \ ((_lhsIntType) :: Type) ->
     _lhsIntType
   {-# INLINE rule305 #-}
   rule305 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule306 #-}
   rule306 = \ ((_lhsIparams) :: [Identifier]) ->
     _lhsIparams
   {-# INLINE rule307 #-}
   rule307 = \ ((_lhsIpragmaBlocks) :: String) ->
     _lhsIpragmaBlocks
   {-# INLINE rule308 #-}
   rule308 = \ ((_lhsIprevVisits) :: Map StateIdentifier StateCtx) ->
     _lhsIprevVisits
   {-# INLINE rule309 #-}
   rule309 = \ ((_lhsIrename) :: Bool) ->
     _lhsIrename
   {-# INLINE rule310 #-}
   rule310 = \ ((_lhsIsynmap) :: Attributes) ->
     _lhsIsynmap
   {-# INLINE rule311 #-}
   rule311 = \ ((_lhsItextBlocks) :: [String]) ->
     _lhsItextBlocks
   {-# INLINE rule312 #-}
   rule312 = \ ((_lhsIallFromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _lhsIallFromToStates
   {-# INLINE rule313 #-}
   rule313 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule314 #-}
   rule314 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule315 #-}
   rule315 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule316 #-}
   rule316 = \ ((_lhsIallVisitKinds) :: Map VisitIdentifier VisitKind) ->
     _lhsIallVisitKinds
   {-# INLINE rule317 #-}
   rule317 = \ ((_lhsIallstates) :: Set StateIdentifier) ->
     _lhsIallstates
   {-# INLINE rule318 #-}
   rule318 = \ ((_lhsIavisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisitdefs
   {-# INLINE rule319 #-}
   rule319 = \ ((_lhsIavisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisituses
   {-# INLINE rule320 #-}
   rule320 = \ ((_lhsIclassCtxs) :: ClassContext) ->
     _lhsIclassCtxs
   {-# INLINE rule321 #-}
   rule321 = \ ((_lhsIimportBlocks) :: [String]) ->
     _lhsIimportBlocks
   {-# INLINE rule322 #-}
   rule322 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule323 #-}
   rule323 = \ ((_lhsIinitial) :: StateIdentifier) ->
     _lhsIinitial
   {-# INLINE rule324 #-}
   rule324 = \ ((_lhsIlocalAttrTypes) :: Map ConstructorIdent (Map Identifier Type)) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule325 #-}
   rule325 = \ ((_lhsImainFile) :: String) ->
     _lhsImainFile
   {-# INLINE rule326 #-}
   rule326 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule327 #-}
   rule327 = \ ((_lhsImoduleHeader) :: String -> String -> String -> Bool -> String) ->
     _lhsImoduleHeader
   {-# INLINE rule328 #-}
   rule328 = \ ((_lhsInextVisits) :: Map StateIdentifier StateCtx) ->
     _lhsInextVisits
   {-# INLINE rule329 #-}
   rule329 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule330 #-}
   rule330 = \ ((_lhsIntType) :: Type) ->
     _lhsIntType
   {-# INLINE rule331 #-}
   rule331 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule332 #-}
   rule332 = \ ((_lhsIparams) :: [Identifier]) ->
     _lhsIparams
   {-# INLINE rule333 #-}
   rule333 = \ ((_lhsIpragmaBlocks) :: String) ->
     _lhsIpragmaBlocks
   {-# INLINE rule334 #-}
   rule334 = \ ((_lhsIprevVisits) :: Map StateIdentifier StateCtx) ->
     _lhsIprevVisits
   {-# INLINE rule335 #-}
   rule335 = \ ((_lhsIrename) :: Bool) ->
     _lhsIrename
   {-# INLINE rule336 #-}
   rule336 = \ ((_lhsIsynmap) :: Attributes) ->
     _lhsIsynmap
   {-# INLINE rule337 #-}
   rule337 = \ ((_lhsItextBlocks) :: [String]) ->
     _lhsItextBlocks
{-# NOINLINE sem_EProductions_Nil #-}
sem_EProductions_Nil ::  T_EProductions 
sem_EProductions_Nil  = T_EProductions (return st17) where
   {-# NOINLINE st17 #-}
   st17 = let
      v16 :: T_EProductions_v16 
      v16 = \ (T_EProductions_vIn16 _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallstates _lhsIavisitdefs _lhsIavisituses _lhsIclassCtxs _lhsIimportBlocks _lhsIinhmap _lhsIinitial _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInextVisits _lhsInt _lhsIntType _lhsIoptions _lhsIparams _lhsIpragmaBlocks _lhsIprevVisits _lhsIrename _lhsIsynmap _lhsItextBlocks) -> ( let
         _lhsOallvisits :: [VisitStateState]
         _lhsOallvisits = rule338  ()
         _lhsOcount :: Int
         _lhsOcount = rule339  ()
         _lhsOdatatype :: [TH.Con]
         _lhsOdatatype = rule340  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule341  ()
         _lhsOfromToStates :: Map VisitIdentifier (Int,Int)
         _lhsOfromToStates = rule342  ()
         _lhsOsemFunBndDefs :: Seq (TH.Name, TH.Exp)
         _lhsOsemFunBndDefs = rule343  ()
         _lhsOsemFunBndTps :: Seq (TH.VarBangType)
         _lhsOsemFunBndTps = rule344  ()
         _lhsOsem_nt :: [TH.Clause]
         _lhsOsem_nt = rule345  ()
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule346  ()
         _lhsOvisitdefs :: Map VisitIdentifier (Set Identifier)
         _lhsOvisitdefs = rule347  ()
         _lhsOvisituses :: Map VisitIdentifier (Set Identifier)
         _lhsOvisituses = rule348  ()
         __result_ = T_EProductions_vOut16 _lhsOallvisits _lhsOcount _lhsOdatatype _lhsOerrors _lhsOfromToStates _lhsOsemFunBndDefs _lhsOsemFunBndTps _lhsOsem_nt _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses
         in __result_ )
     in C_EProductions_s17 v16
   {-# INLINE rule338 #-}
   {-# LINE 411 "src-ag/ExecutionPlan2TH.ag" #-}
   rule338 = \  (_ :: ()) ->
                           {-# LINE 411 "src-ag/ExecutionPlan2TH.ag" #-}
                           error "Every nonterminal should have at least 1 production"
                           {-# LINE 2551 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule339 #-}
   rule339 = \  (_ :: ()) ->
     0
   {-# INLINE rule340 #-}
   rule340 = \  (_ :: ()) ->
     []
   {-# INLINE rule341 #-}
   rule341 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule342 #-}
   rule342 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule343 #-}
   rule343 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule344 #-}
   rule344 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule345 #-}
   rule345 = \  (_ :: ()) ->
     []
   {-# INLINE rule346 #-}
   rule346 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule347 #-}
   rule347 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule348 #-}
   rule348 = \  (_ :: ()) ->
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
         _used = rule349 _lhsIusageInfo arg_name_
         _kinds = rule350 _lhsIruleKinds arg_name_
         _anyLazyKind = rule351 _kinds
         _addbang = rule352 _lhsIoptions
         _addbang1 = rule353 _addbang _anyLazyKind
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule354 _used arg_mbError_
         _lhsOruledefs :: Map Identifier (Set String)
         _lhsOruledefs = rule355  ()
         _lhsOruleuses :: Map Identifier (Map String (Maybe NonLocalAttr))
         _lhsOruleuses = rule356  ()
         _lhsOusedArgs :: Set String
         _lhsOusedArgs = rule357  ()
         _patternOallInhmap = rule358 _lhsIallInhmap
         _patternOallSynmap = rule359 _lhsIallSynmap
         _patternOanyLazyKind = rule360 _anyLazyKind
         _patternOinhmap = rule361 _lhsIinhmap
         _patternOlocalAttrTypes = rule362 _lhsIlocalAttrTypes
         _patternOoptions = rule363 _lhsIoptions
         _patternOsynmap = rule364 _lhsIsynmap
         _rhsOoptions = rule365 _lhsIoptions
         __result_ = T_ERule_vOut19 _lhsOerrors _lhsOruledefs _lhsOruleuses _lhsOusedArgs
         in __result_ )
     in C_ERule_s20 v19
   {-# INLINE rule349 #-}
   {-# LINE 1370 "src-ag/ExecutionPlan2TH.ag" #-}
   rule349 = \ ((_lhsIusageInfo) :: Map Identifier Int) name_ ->
                                                 {-# LINE 1370 "src-ag/ExecutionPlan2TH.ag" #-}
                                                 Map.findWithDefault 0 name_ _lhsIusageInfo
                                                 {-# LINE 2653 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule350 #-}
   {-# LINE 1386 "src-ag/ExecutionPlan2TH.ag" #-}
   rule350 = \ ((_lhsIruleKinds) :: Map Identifier (Set VisitKind)) name_ ->
                {-# LINE 1386 "src-ag/ExecutionPlan2TH.ag" #-}
                Map.findWithDefault Set.empty name_ _lhsIruleKinds
                {-# LINE 2659 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule351 #-}
   {-# LINE 1387 "src-ag/ExecutionPlan2TH.ag" #-}
   rule351 = \ _kinds ->
                      {-# LINE 1387 "src-ag/ExecutionPlan2TH.ag" #-}
                      Set.fold (\k r -> isLazyKind k || r) False _kinds
                      {-# LINE 2665 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule352 #-}
   {-# LINE 1636 "src-ag/ExecutionPlan2TH.ag" #-}
   rule352 = \ ((_lhsIoptions) :: Options) ->
                                                    {-# LINE 1636 "src-ag/ExecutionPlan2TH.ag" #-}
                                                    \x -> if bangpats _lhsIoptions then TH.BangP x else x
                                                    {-# LINE 2671 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule353 #-}
   {-# LINE 1650 "src-ag/ExecutionPlan2TH.ag" #-}
   rule353 = \ _addbang _anyLazyKind ->
                                                     {-# LINE 1650 "src-ag/ExecutionPlan2TH.ag" #-}
                                                     if _anyLazyKind     then id else _addbang
                                                     {-# LINE 2677 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule354 #-}
   {-# LINE 1756 "src-ag/ExecutionPlan2TH.ag" #-}
   rule354 = \ _used mbError_ ->
                 {-# LINE 1756 "src-ag/ExecutionPlan2TH.ag" #-}
                 case mbError_ of
                   Just e | _used     > 0 -> Seq.singleton e
                   _                      -> Seq.empty
                 {-# LINE 2685 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule355 #-}
   rule355 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule356 #-}
   rule356 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule357 #-}
   rule357 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule358 #-}
   rule358 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule359 #-}
   rule359 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule360 #-}
   rule360 = \ _anyLazyKind ->
     _anyLazyKind
   {-# INLINE rule361 #-}
   rule361 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule362 #-}
   rule362 = \ ((_lhsIlocalAttrTypes) :: Map Identifier Type) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule363 #-}
   rule363 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule364 #-}
   rule364 = \ ((_lhsIsynmap) :: Attributes) ->
     _lhsIsynmap
   {-# INLINE rule365 #-}
   rule365 = \ ((_lhsIoptions) :: Options) ->
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
         _lhsOerrors = rule366 _hdIerrors _tlIerrors
         _lhsOruledefs :: Map Identifier (Set String)
         _lhsOruledefs = rule367 _hdIruledefs _tlIruledefs
         _lhsOruleuses :: Map Identifier (Map String (Maybe NonLocalAttr))
         _lhsOruleuses = rule368 _hdIruleuses _tlIruleuses
         _lhsOusedArgs :: Set String
         _lhsOusedArgs = rule369 _hdIusedArgs _tlIusedArgs
         _hdOallInhmap = rule370 _lhsIallInhmap
         _hdOallSynmap = rule371 _lhsIallSynmap
         _hdOchildTypes = rule372 _lhsIchildTypes
         _hdOcon = rule373 _lhsIcon
         _hdOimportBlocks = rule374 _lhsIimportBlocks
         _hdOinhmap = rule375 _lhsIinhmap
         _hdOlazyIntras = rule376 _lhsIlazyIntras
         _hdOlocalAttrTypes = rule377 _lhsIlocalAttrTypes
         _hdOmainFile = rule378 _lhsImainFile
         _hdOmainName = rule379 _lhsImainName
         _hdOmoduleHeader = rule380 _lhsImoduleHeader
         _hdOnt = rule381 _lhsInt
         _hdOoptions = rule382 _lhsIoptions
         _hdOpragmaBlocks = rule383 _lhsIpragmaBlocks
         _hdOruleKinds = rule384 _lhsIruleKinds
         _hdOsynmap = rule385 _lhsIsynmap
         _hdOtextBlocks = rule386 _lhsItextBlocks
         _hdOusageInfo = rule387 _lhsIusageInfo
         _tlOallInhmap = rule388 _lhsIallInhmap
         _tlOallSynmap = rule389 _lhsIallSynmap
         _tlOchildTypes = rule390 _lhsIchildTypes
         _tlOcon = rule391 _lhsIcon
         _tlOimportBlocks = rule392 _lhsIimportBlocks
         _tlOinhmap = rule393 _lhsIinhmap
         _tlOlazyIntras = rule394 _lhsIlazyIntras
         _tlOlocalAttrTypes = rule395 _lhsIlocalAttrTypes
         _tlOmainFile = rule396 _lhsImainFile
         _tlOmainName = rule397 _lhsImainName
         _tlOmoduleHeader = rule398 _lhsImoduleHeader
         _tlOnt = rule399 _lhsInt
         _tlOoptions = rule400 _lhsIoptions
         _tlOpragmaBlocks = rule401 _lhsIpragmaBlocks
         _tlOruleKinds = rule402 _lhsIruleKinds
         _tlOsynmap = rule403 _lhsIsynmap
         _tlOtextBlocks = rule404 _lhsItextBlocks
         _tlOusageInfo = rule405 _lhsIusageInfo
         __result_ = T_ERules_vOut22 _lhsOerrors _lhsOruledefs _lhsOruleuses _lhsOusedArgs
         in __result_ )
     in C_ERules_s23 v22
   {-# INLINE rule366 #-}
   rule366 = \ ((_hdIerrors) :: Seq Error) ((_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# INLINE rule367 #-}
   rule367 = \ ((_hdIruledefs) :: Map Identifier (Set String)) ((_tlIruledefs) :: Map Identifier (Set String)) ->
     _hdIruledefs `uwSetUnion` _tlIruledefs
   {-# INLINE rule368 #-}
   rule368 = \ ((_hdIruleuses) :: Map Identifier (Map String (Maybe NonLocalAttr))) ((_tlIruleuses) :: Map Identifier (Map String (Maybe NonLocalAttr))) ->
     _hdIruleuses `uwMapUnion` _tlIruleuses
   {-# INLINE rule369 #-}
   rule369 = \ ((_hdIusedArgs) :: Set String) ((_tlIusedArgs) :: Set String) ->
     _hdIusedArgs `Set.union` _tlIusedArgs
   {-# INLINE rule370 #-}
   rule370 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule371 #-}
   rule371 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule372 #-}
   rule372 = \ ((_lhsIchildTypes) :: Map Identifier Type) ->
     _lhsIchildTypes
   {-# INLINE rule373 #-}
   rule373 = \ ((_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# INLINE rule374 #-}
   rule374 = \ ((_lhsIimportBlocks) :: [String]) ->
     _lhsIimportBlocks
   {-# INLINE rule375 #-}
   rule375 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule376 #-}
   rule376 = \ ((_lhsIlazyIntras) :: Set String) ->
     _lhsIlazyIntras
   {-# INLINE rule377 #-}
   rule377 = \ ((_lhsIlocalAttrTypes) :: Map Identifier Type) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule378 #-}
   rule378 = \ ((_lhsImainFile) :: String) ->
     _lhsImainFile
   {-# INLINE rule379 #-}
   rule379 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule380 #-}
   rule380 = \ ((_lhsImoduleHeader) :: String -> String -> String -> Bool -> String) ->
     _lhsImoduleHeader
   {-# INLINE rule381 #-}
   rule381 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule382 #-}
   rule382 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule383 #-}
   rule383 = \ ((_lhsIpragmaBlocks) :: String) ->
     _lhsIpragmaBlocks
   {-# INLINE rule384 #-}
   rule384 = \ ((_lhsIruleKinds) :: Map Identifier (Set VisitKind)) ->
     _lhsIruleKinds
   {-# INLINE rule385 #-}
   rule385 = \ ((_lhsIsynmap) :: Attributes) ->
     _lhsIsynmap
   {-# INLINE rule386 #-}
   rule386 = \ ((_lhsItextBlocks) :: [String]) ->
     _lhsItextBlocks
   {-# INLINE rule387 #-}
   rule387 = \ ((_lhsIusageInfo) :: Map Identifier Int) ->
     _lhsIusageInfo
   {-# INLINE rule388 #-}
   rule388 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule389 #-}
   rule389 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule390 #-}
   rule390 = \ ((_lhsIchildTypes) :: Map Identifier Type) ->
     _lhsIchildTypes
   {-# INLINE rule391 #-}
   rule391 = \ ((_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# INLINE rule392 #-}
   rule392 = \ ((_lhsIimportBlocks) :: [String]) ->
     _lhsIimportBlocks
   {-# INLINE rule393 #-}
   rule393 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule394 #-}
   rule394 = \ ((_lhsIlazyIntras) :: Set String) ->
     _lhsIlazyIntras
   {-# INLINE rule395 #-}
   rule395 = \ ((_lhsIlocalAttrTypes) :: Map Identifier Type) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule396 #-}
   rule396 = \ ((_lhsImainFile) :: String) ->
     _lhsImainFile
   {-# INLINE rule397 #-}
   rule397 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule398 #-}
   rule398 = \ ((_lhsImoduleHeader) :: String -> String -> String -> Bool -> String) ->
     _lhsImoduleHeader
   {-# INLINE rule399 #-}
   rule399 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule400 #-}
   rule400 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule401 #-}
   rule401 = \ ((_lhsIpragmaBlocks) :: String) ->
     _lhsIpragmaBlocks
   {-# INLINE rule402 #-}
   rule402 = \ ((_lhsIruleKinds) :: Map Identifier (Set VisitKind)) ->
     _lhsIruleKinds
   {-# INLINE rule403 #-}
   rule403 = \ ((_lhsIsynmap) :: Attributes) ->
     _lhsIsynmap
   {-# INLINE rule404 #-}
   rule404 = \ ((_lhsItextBlocks) :: [String]) ->
     _lhsItextBlocks
   {-# INLINE rule405 #-}
   rule405 = \ ((_lhsIusageInfo) :: Map Identifier Int) ->
     _lhsIusageInfo
{-# NOINLINE sem_ERules_Nil #-}
sem_ERules_Nil ::  T_ERules 
sem_ERules_Nil  = T_ERules (return st23) where
   {-# NOINLINE st23 #-}
   st23 = let
      v22 :: T_ERules_v22 
      v22 = \ (T_ERules_vIn22 _lhsIallInhmap _lhsIallSynmap _lhsIchildTypes _lhsIcon _lhsIimportBlocks _lhsIinhmap _lhsIlazyIntras _lhsIlocalAttrTypes _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInt _lhsIoptions _lhsIpragmaBlocks _lhsIruleKinds _lhsIsynmap _lhsItextBlocks _lhsIusageInfo) -> ( let
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule406  ()
         _lhsOruledefs :: Map Identifier (Set String)
         _lhsOruledefs = rule407  ()
         _lhsOruleuses :: Map Identifier (Map String (Maybe NonLocalAttr))
         _lhsOruleuses = rule408  ()
         _lhsOusedArgs :: Set String
         _lhsOusedArgs = rule409  ()
         __result_ = T_ERules_vOut22 _lhsOerrors _lhsOruledefs _lhsOruleuses _lhsOusedArgs
         in __result_ )
     in C_ERules_s23 v22
   {-# INLINE rule406 #-}
   rule406 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule407 #-}
   rule407 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule408 #-}
   rule408 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule409 #-}
   rule409 = \  (_ :: ()) ->
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
         _lhsOoutput = rule410 _commonExtra _nontsIoutput _wrappersExtra
         _nontsOwrappers = rule411 arg_wrappers_
         _nontsOtypeSyns = rule412 arg_typeSyns_
         _nontsOderivings = rule413 arg_derivings_
         _wrappersExtra = rule414 _lateSemBndDef _lhsIoptions
         _commonExtra = rule415 _lateSemBndTp _lhsIoptions
         _lateSemBndTp = rule416 _lhsImainName _nontsIsemFunBndTps
         _lateSemBndDef = rule417 _lhsImainName _lhsIoptions _nontsIsemFunBndDefs arg_wrappers_
         _nontsOallFromToStates = rule418 _nontsIfromToStates
         _nontsOallVisitKinds = rule419 _nontsIvisitKinds
         _nontsOallInitStates = rule420 _nontsIinitStates
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule421 _nontsIerrors
         _nontsOavisitdefs = rule422  ()
         _nontsOavisituses = rule423  ()
         _nontsOimportBlocks = rule424 _lhsIimportBlocks
         _nontsOinhmap = rule425 _lhsIinhmap
         _nontsOlocalAttrTypes = rule426 _lhsIlocalAttrTypes
         _nontsOmainFile = rule427 _lhsImainFile
         _nontsOmainName = rule428 _lhsImainName
         _nontsOmoduleHeader = rule429 _lhsImoduleHeader
         _nontsOoptions = rule430 _lhsIoptions
         _nontsOpragmaBlocks = rule431 _lhsIpragmaBlocks
         _nontsOsynmap = rule432 _lhsIsynmap
         _nontsOtextBlocks = rule433 _lhsItextBlocks
         __result_ = T_ExecutionPlan_vOut25 _lhsOerrors _lhsOoutput
         in __result_ )
     in C_ExecutionPlan_s26 v25
   {-# INLINE rule410 #-}
   {-# LINE 103 "src-ag/ExecutionPlan2TH.ag" #-}
   rule410 = \ _commonExtra ((_nontsIoutput) :: [TH.Dec]) _wrappersExtra ->
                                 {-# LINE 103 "src-ag/ExecutionPlan2TH.ag" #-}
                                 _nontsIoutput ++ _commonExtra     ++ _wrappersExtra
                                 {-# LINE 3032 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule411 #-}
   {-# LINE 109 "src-ag/ExecutionPlan2TH.ag" #-}
   rule411 = \ wrappers_ ->
                                     {-# LINE 109 "src-ag/ExecutionPlan2TH.ag" #-}
                                     wrappers_
                                     {-# LINE 3038 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule412 #-}
   {-# LINE 143 "src-ag/ExecutionPlan2TH.ag" #-}
   rule412 = \ typeSyns_ ->
                                     {-# LINE 143 "src-ag/ExecutionPlan2TH.ag" #-}
                                     typeSyns_
                                     {-# LINE 3044 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule413 #-}
   {-# LINE 144 "src-ag/ExecutionPlan2TH.ag" #-}
   rule413 = \ derivings_ ->
                                      {-# LINE 144 "src-ag/ExecutionPlan2TH.ag" #-}
                                      derivings_
                                      {-# LINE 3050 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule414 #-}
   {-# LINE 625 "src-ag/ExecutionPlan2TH.ag" #-}
   rule414 = \ _lateSemBndDef ((_lhsIoptions) :: Options) ->
                        {-# LINE 625 "src-ag/ExecutionPlan2TH.ag" #-}
                        if lateHigherOrderBinding _lhsIoptions
                        then _lateSemBndDef
                        else []
                        {-# LINE 3058 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule415 #-}
   {-# LINE 628 "src-ag/ExecutionPlan2TH.ag" #-}
   rule415 = \ _lateSemBndTp ((_lhsIoptions) :: Options) ->
                        {-# LINE 628 "src-ag/ExecutionPlan2TH.ag" #-}
                        if lateHigherOrderBinding _lhsIoptions
                        then _lateSemBndTp
                        else []
                        {-# LINE 3066 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule416 #-}
   {-# LINE 631 "src-ag/ExecutionPlan2TH.ag" #-}
   rule416 = \ ((_lhsImainName) :: String) ((_nontsIsemFunBndTps) :: Seq (TH.VarBangType)) ->
                       {-# LINE 631 "src-ag/ExecutionPlan2TH.ag" #-}
                       [TH.DataD [] (TH.mkName (lateBindingTypeNm _lhsImainName)) [] Nothing [TH.RecC (TH.mkName (lateBindingTypeNm _lhsImainName)) (toList _nontsIsemFunBndTps)] []]
                       {-# LINE 3072 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule417 #-}
   {-# LINE 633 "src-ag/ExecutionPlan2TH.ag" #-}
   rule417 = \ ((_lhsImainName) :: String) ((_lhsIoptions) :: Options) ((_nontsIsemFunBndDefs) :: Seq (TH.Name, TH.Exp)) wrappers_ ->
        {-# LINE 633 "src-ag/ExecutionPlan2TH.ag" #-}
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
        {-# LINE 3090 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule418 #-}
   {-# LINE 1678 "src-ag/ExecutionPlan2TH.ag" #-}
   rule418 = \ ((_nontsIfromToStates) :: Map VisitIdentifier (Int,Int)) ->
                            {-# LINE 1678 "src-ag/ExecutionPlan2TH.ag" #-}
                            _nontsIfromToStates
                            {-# LINE 3096 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule419 #-}
   {-# LINE 1722 "src-ag/ExecutionPlan2TH.ag" #-}
   rule419 = \ ((_nontsIvisitKinds) :: Map VisitIdentifier VisitKind) ->
                          {-# LINE 1722 "src-ag/ExecutionPlan2TH.ag" #-}
                          _nontsIvisitKinds
                          {-# LINE 3102 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule420 #-}
   {-# LINE 1736 "src-ag/ExecutionPlan2TH.ag" #-}
   rule420 = \ ((_nontsIinitStates) :: Map NontermIdent Int) ->
                          {-# LINE 1736 "src-ag/ExecutionPlan2TH.ag" #-}
                          _nontsIinitStates
                          {-# LINE 3108 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule421 #-}
   rule421 = \ ((_nontsIerrors) :: Seq Error) ->
     _nontsIerrors
   {-# INLINE rule422 #-}
   rule422 = \  (_ :: ()) ->
     error "missing rule: ExecutionPlan.ExecutionPlan.nonts.avisitdefs"
   {-# INLINE rule423 #-}
   rule423 = \  (_ :: ()) ->
     error "missing rule: ExecutionPlan.ExecutionPlan.nonts.avisituses"
   {-# INLINE rule424 #-}
   rule424 = \ ((_lhsIimportBlocks) :: [String]) ->
     _lhsIimportBlocks
   {-# INLINE rule425 #-}
   rule425 = \ ((_lhsIinhmap) :: Map NontermIdent Attributes) ->
     _lhsIinhmap
   {-# INLINE rule426 #-}
   rule426 = \ ((_lhsIlocalAttrTypes) :: Map NontermIdent (Map ConstructorIdent (Map Identifier Type))) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule427 #-}
   rule427 = \ ((_lhsImainFile) :: String) ->
     _lhsImainFile
   {-# INLINE rule428 #-}
   rule428 = \ ((_lhsImainName) :: String) ->
     _lhsImainName
   {-# INLINE rule429 #-}
   rule429 = \ ((_lhsImoduleHeader) :: String -> String -> String -> Bool -> String) ->
     _lhsImoduleHeader
   {-# INLINE rule430 #-}
   rule430 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule431 #-}
   rule431 = \ ((_lhsIpragmaBlocks) :: String) ->
     _lhsIpragmaBlocks
   {-# INLINE rule432 #-}
   rule432 = \ ((_lhsIsynmap) :: Map NontermIdent Attributes) ->
     _lhsIsynmap
   {-# INLINE rule433 #-}
   rule433 = \ ((_lhsItextBlocks) :: [String]) ->
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
         _lhsOattrs = rule434  ()
         _lhsOpos :: Pos
         _lhsOpos = rule435 arg_pos_
         _lhsOtks :: [HsToken]
         _lhsOtks = rule436 arg_tks_
         __result_ = T_Expression_vOut28 _lhsOattrs _lhsOpos _lhsOtks
         in __result_ )
     in C_Expression_s29 v28
   {-# INLINE rule434 #-}
   rule434 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule435 #-}
   rule435 = \ pos_ ->
     pos_
   {-# INLINE rule436 #-}
   rule436 = \ tks_ ->
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
         _tok = rule437 arg_pos_ arg_var_
         _lhsOattrs :: Map String (Maybe NonLocalAttr)
         _lhsOattrs = rule438  ()
         _lhsOtok :: (Pos,String)
         _lhsOtok = rule439 _tok
         __result_ = T_HsToken_vOut31 _lhsOattrs _lhsOtok
         in __result_ )
     in C_HsToken_s32 v31
   {-# INLINE rule437 #-}
   {-# LINE 1503 "src-ag/ExecutionPlan2TH.ag" #-}
   rule437 = \ pos_ var_ ->
                          {-# LINE 1503 "src-ag/ExecutionPlan2TH.ag" #-}
                          (pos_,fieldname var_)
                          {-# LINE 3260 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule438 #-}
   rule438 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule439 #-}
   rule439 = \ _tok ->
     _tok
{-# NOINLINE sem_HsToken_AGField #-}
sem_HsToken_AGField :: (Identifier) -> (Identifier) -> (Pos) -> (Maybe String) -> T_HsToken 
sem_HsToken_AGField arg_field_ arg_attr_ arg_pos_ arg_rdesc_ = T_HsToken (return st32) where
   {-# NOINLINE st32 #-}
   st32 = let
      v31 :: T_HsToken_v31 
      v31 = \ (T_HsToken_vIn31 _lhsIoptions) -> ( let
         _addTrace = rule440 arg_attr_ arg_field_ arg_rdesc_
         _lhsOtok :: (Pos,String)
         _lhsOtok = rule441 _addTrace _lhsIoptions arg_attr_ arg_field_ arg_pos_
         _lhsOattrs :: Map String (Maybe NonLocalAttr)
         _lhsOattrs = rule442  ()
         __result_ = T_HsToken_vOut31 _lhsOattrs _lhsOtok
         in __result_ )
     in C_HsToken_s32 v31
   {-# INLINE rule440 #-}
   {-# LINE 1507 "src-ag/ExecutionPlan2TH.ag" #-}
   rule440 = \ attr_ field_ rdesc_ ->
                        {-# LINE 1507 "src-ag/ExecutionPlan2TH.ag" #-}
                        case rdesc_ of
                          Just d  -> \x -> "(trace " ++ show (d ++ " -> " ++ show field_ ++ "." ++ show attr_) ++ " (" ++ x ++ "))"
                          Nothing -> id
                        {-# LINE 3289 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule441 #-}
   {-# LINE 1510 "src-ag/ExecutionPlan2TH.ag" #-}
   rule441 = \ _addTrace ((_lhsIoptions) :: Options) attr_ field_ pos_ ->
                   {-# LINE 1510 "src-ag/ExecutionPlan2TH.ag" #-}
                   (pos_, _addTrace     $ attrname _lhsIoptions True field_ attr_)
                   {-# LINE 3295 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule442 #-}
   rule442 = \  (_ :: ()) ->
     Map.empty
{-# NOINLINE sem_HsToken_HsToken #-}
sem_HsToken_HsToken :: (String) -> (Pos) -> T_HsToken 
sem_HsToken_HsToken arg_value_ arg_pos_ = T_HsToken (return st32) where
   {-# NOINLINE st32 #-}
   st32 = let
      v31 :: T_HsToken_v31 
      v31 = \ (T_HsToken_vIn31 _lhsIoptions) -> ( let
         _lhsOtok :: (Pos,String)
         _lhsOtok = rule443 arg_pos_ arg_value_
         _lhsOattrs :: Map String (Maybe NonLocalAttr)
         _lhsOattrs = rule444  ()
         __result_ = T_HsToken_vOut31 _lhsOattrs _lhsOtok
         in __result_ )
     in C_HsToken_s32 v31
   {-# INLINE rule443 #-}
   {-# LINE 1512 "src-ag/ExecutionPlan2TH.ag" #-}
   rule443 = \ pos_ value_ ->
                         {-# LINE 1512 "src-ag/ExecutionPlan2TH.ag" #-}
                         (pos_, value_)
                         {-# LINE 3318 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule444 #-}
   rule444 = \  (_ :: ()) ->
     Map.empty
{-# NOINLINE sem_HsToken_CharToken #-}
sem_HsToken_CharToken :: (String) -> (Pos) -> T_HsToken 
sem_HsToken_CharToken arg_value_ arg_pos_ = T_HsToken (return st32) where
   {-# NOINLINE st32 #-}
   st32 = let
      v31 :: T_HsToken_v31 
      v31 = \ (T_HsToken_vIn31 _lhsIoptions) -> ( let
         _lhsOtok :: (Pos,String)
         _lhsOtok = rule445 arg_pos_ arg_value_
         _lhsOattrs :: Map String (Maybe NonLocalAttr)
         _lhsOattrs = rule446  ()
         __result_ = T_HsToken_vOut31 _lhsOattrs _lhsOtok
         in __result_ )
     in C_HsToken_s32 v31
   {-# INLINE rule445 #-}
   {-# LINE 1514 "src-ag/ExecutionPlan2TH.ag" #-}
   rule445 = \ pos_ value_ ->
                           {-# LINE 1514 "src-ag/ExecutionPlan2TH.ag" #-}
                           (pos_, if null value_
                                     then ""
                                     else showCharShort (head value_)
                           )
                           {-# LINE 3344 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule446 #-}
   rule446 = \  (_ :: ()) ->
     Map.empty
{-# NOINLINE sem_HsToken_StrToken #-}
sem_HsToken_StrToken :: (String) -> (Pos) -> T_HsToken 
sem_HsToken_StrToken arg_value_ arg_pos_ = T_HsToken (return st32) where
   {-# NOINLINE st32 #-}
   st32 = let
      v31 :: T_HsToken_v31 
      v31 = \ (T_HsToken_vIn31 _lhsIoptions) -> ( let
         _lhsOtok :: (Pos,String)
         _lhsOtok = rule447 arg_pos_ arg_value_
         _lhsOattrs :: Map String (Maybe NonLocalAttr)
         _lhsOattrs = rule448  ()
         __result_ = T_HsToken_vOut31 _lhsOattrs _lhsOtok
         in __result_ )
     in C_HsToken_s32 v31
   {-# INLINE rule447 #-}
   {-# LINE 1519 "src-ag/ExecutionPlan2TH.ag" #-}
   rule447 = \ pos_ value_ ->
                           {-# LINE 1519 "src-ag/ExecutionPlan2TH.ag" #-}
                           (pos_, showStrShort value_)
                           {-# LINE 3367 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule448 #-}
   rule448 = \  (_ :: ()) ->
     Map.empty
{-# NOINLINE sem_HsToken_Err #-}
sem_HsToken_Err :: (String) -> (Pos) -> T_HsToken 
sem_HsToken_Err _ arg_pos_ = T_HsToken (return st32) where
   {-# NOINLINE st32 #-}
   st32 = let
      v31 :: T_HsToken_v31 
      v31 = \ (T_HsToken_vIn31 _lhsIoptions) -> ( let
         _lhsOtok :: (Pos,String)
         _lhsOtok = rule449 arg_pos_
         _lhsOattrs :: Map String (Maybe NonLocalAttr)
         _lhsOattrs = rule450  ()
         __result_ = T_HsToken_vOut31 _lhsOattrs _lhsOtok
         in __result_ )
     in C_HsToken_s32 v31
   {-# INLINE rule449 #-}
   {-# LINE 1520 "src-ag/ExecutionPlan2TH.ag" #-}
   rule449 = \ pos_ ->
                           {-# LINE 1520 "src-ag/ExecutionPlan2TH.ag" #-}
                           (pos_, "")
                           {-# LINE 3390 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule450 #-}
   rule450 = \  (_ :: ()) ->
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
         _lhsOtks = rule451 _hdItok _tlItks
         _hdOoptions = rule452 _lhsIoptions
         _tlOoptions = rule453 _lhsIoptions
         __result_ = T_HsTokens_vOut34 _lhsOtks
         in __result_ )
     in C_HsTokens_s35 v34
   {-# INLINE rule451 #-}
   {-# LINE 1499 "src-ag/ExecutionPlan2TH.ag" #-}
   rule451 = \ ((_hdItok) :: (Pos,String)) ((_tlItks) :: [(Pos,String)]) ->
                     {-# LINE 1499 "src-ag/ExecutionPlan2TH.ag" #-}
                     _hdItok : _tlItks
                     {-# LINE 3448 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule452 #-}
   rule452 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule453 #-}
   rule453 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
{-# NOINLINE sem_HsTokens_Nil #-}
sem_HsTokens_Nil ::  T_HsTokens 
sem_HsTokens_Nil  = T_HsTokens (return st35) where
   {-# NOINLINE st35 #-}
   st35 = let
      v34 :: T_HsTokens_v34 
      v34 = \ (T_HsTokens_vIn34 _lhsIoptions) -> ( let
         _lhsOtks :: [(Pos,String)]
         _lhsOtks = rule454  ()
         __result_ = T_HsTokens_vOut34 _lhsOtks
         in __result_ )
     in C_HsTokens_s35 v34
   {-# INLINE rule454 #-}
   {-# LINE 1500 "src-ag/ExecutionPlan2TH.ag" #-}
   rule454 = \  (_ :: ()) ->
                     {-# LINE 1500 "src-ag/ExecutionPlan2TH.ag" #-}
                     []
                     {-# LINE 3472 "src-generated/ExecutionPlan2TH.hs" #-}

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
         _tokensOoptions = rule455 _lhsIoptions
         __result_ = T_HsTokensRoot_vOut37 
         in __result_ )
     in C_HsTokensRoot_s38 v37
   {-# INLINE rule455 #-}
   rule455 = \ ((_lhsIoptions) :: Options) ->
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
         _addbang = rule456 _lhsIoptions
         _addbang1 = rule457 _addbang _lhsIanyLazyKind
         _lhsOattrs :: Set String
         _lhsOattrs = rule458 _patsIattrs
         _copy = rule459 _patsIcopy arg_name_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule460 _copy
         _lhsOisUnderscore :: Bool
         _lhsOisUnderscore = rule461  ()
         _patsOallInhmap = rule462 _lhsIallInhmap
         _patsOallSynmap = rule463 _lhsIallSynmap
         _patsOanyLazyKind = rule464 _lhsIanyLazyKind
         _patsOinhmap = rule465 _lhsIinhmap
         _patsOlocalAttrTypes = rule466 _lhsIlocalAttrTypes
         _patsOoptions = rule467 _lhsIoptions
         _patsOsynmap = rule468 _lhsIsynmap
         __result_ = T_Pattern_vOut40 _lhsOattrs _lhsOcopy _lhsOisUnderscore
         in __result_ )
     in C_Pattern_s41 v40
   {-# INLINE rule456 #-}
   {-# LINE 1646 "src-ag/ExecutionPlan2TH.ag" #-}
   rule456 = \ ((_lhsIoptions) :: Options) ->
                                                     {-# LINE 1646 "src-ag/ExecutionPlan2TH.ag" #-}
                                                     \x -> if bangpats _lhsIoptions then TH.BangP x else x
                                                     {-# LINE 3588 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule457 #-}
   {-# LINE 1651 "src-ag/ExecutionPlan2TH.ag" #-}
   rule457 = \ _addbang ((_lhsIanyLazyKind) :: Bool) ->
                                                     {-# LINE 1651 "src-ag/ExecutionPlan2TH.ag" #-}
                                                     if _lhsIanyLazyKind then id else _addbang
                                                     {-# LINE 3594 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule458 #-}
   rule458 = \ ((_patsIattrs) :: Set String) ->
     _patsIattrs
   {-# INLINE rule459 #-}
   rule459 = \ ((_patsIcopy) :: Patterns) name_ ->
     Constr name_ _patsIcopy
   {-# INLINE rule460 #-}
   rule460 = \ _copy ->
     _copy
   {-# INLINE rule461 #-}
   rule461 = \  (_ :: ()) ->
     error "missing rule: Pattern.Constr.lhs.isUnderscore"
   {-# INLINE rule462 #-}
   rule462 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule463 #-}
   rule463 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule464 #-}
   rule464 = \ ((_lhsIanyLazyKind) :: Bool) ->
     _lhsIanyLazyKind
   {-# INLINE rule465 #-}
   rule465 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule466 #-}
   rule466 = \ ((_lhsIlocalAttrTypes) :: Map Identifier Type) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule467 #-}
   rule467 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule468 #-}
   rule468 = \ ((_lhsIsynmap) :: Attributes) ->
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
         _addbang = rule469 _lhsIoptions
         _addbang1 = rule470 _addbang _lhsIanyLazyKind
         _lhsOattrs :: Set String
         _lhsOattrs = rule471 _patsIattrs
         _copy = rule472 _patsIcopy arg_pos_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule473 _copy
         _lhsOisUnderscore :: Bool
         _lhsOisUnderscore = rule474  ()
         _patsOallInhmap = rule475 _lhsIallInhmap
         _patsOallSynmap = rule476 _lhsIallSynmap
         _patsOanyLazyKind = rule477 _lhsIanyLazyKind
         _patsOinhmap = rule478 _lhsIinhmap
         _patsOlocalAttrTypes = rule479 _lhsIlocalAttrTypes
         _patsOoptions = rule480 _lhsIoptions
         _patsOsynmap = rule481 _lhsIsynmap
         __result_ = T_Pattern_vOut40 _lhsOattrs _lhsOcopy _lhsOisUnderscore
         in __result_ )
     in C_Pattern_s41 v40
   {-# INLINE rule469 #-}
   {-# LINE 1646 "src-ag/ExecutionPlan2TH.ag" #-}
   rule469 = \ ((_lhsIoptions) :: Options) ->
                                                     {-# LINE 1646 "src-ag/ExecutionPlan2TH.ag" #-}
                                                     \x -> if bangpats _lhsIoptions then TH.BangP x else x
                                                     {-# LINE 3661 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule470 #-}
   {-# LINE 1651 "src-ag/ExecutionPlan2TH.ag" #-}
   rule470 = \ _addbang ((_lhsIanyLazyKind) :: Bool) ->
                                                     {-# LINE 1651 "src-ag/ExecutionPlan2TH.ag" #-}
                                                     if _lhsIanyLazyKind then id else _addbang
                                                     {-# LINE 3667 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule471 #-}
   rule471 = \ ((_patsIattrs) :: Set String) ->
     _patsIattrs
   {-# INLINE rule472 #-}
   rule472 = \ ((_patsIcopy) :: Patterns) pos_ ->
     Product pos_ _patsIcopy
   {-# INLINE rule473 #-}
   rule473 = \ _copy ->
     _copy
   {-# INLINE rule474 #-}
   rule474 = \  (_ :: ()) ->
     error "missing rule: Pattern.Product.lhs.isUnderscore"
   {-# INLINE rule475 #-}
   rule475 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule476 #-}
   rule476 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule477 #-}
   rule477 = \ ((_lhsIanyLazyKind) :: Bool) ->
     _lhsIanyLazyKind
   {-# INLINE rule478 #-}
   rule478 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule479 #-}
   rule479 = \ ((_lhsIlocalAttrTypes) :: Map Identifier Type) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule480 #-}
   rule480 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule481 #-}
   rule481 = \ ((_lhsIsynmap) :: Attributes) ->
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
         _addbang = rule482 _lhsIoptions
         _addbang1 = rule483 _addbang _lhsIanyLazyKind
         _lhsOattrs :: Set String
         _lhsOattrs = rule484 _patIattrs
         _copy = rule485 _patIcopy arg_attr_ arg_field_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule486 _copy
         _lhsOisUnderscore :: Bool
         _lhsOisUnderscore = rule487 _patIisUnderscore
         _patOallInhmap = rule488 _lhsIallInhmap
         _patOallSynmap = rule489 _lhsIallSynmap
         _patOanyLazyKind = rule490 _lhsIanyLazyKind
         _patOinhmap = rule491 _lhsIinhmap
         _patOlocalAttrTypes = rule492 _lhsIlocalAttrTypes
         _patOoptions = rule493 _lhsIoptions
         _patOsynmap = rule494 _lhsIsynmap
         __result_ = T_Pattern_vOut40 _lhsOattrs _lhsOcopy _lhsOisUnderscore
         in __result_ )
     in C_Pattern_s41 v40
   {-# INLINE rule482 #-}
   {-# LINE 1646 "src-ag/ExecutionPlan2TH.ag" #-}
   rule482 = \ ((_lhsIoptions) :: Options) ->
                                                     {-# LINE 1646 "src-ag/ExecutionPlan2TH.ag" #-}
                                                     \x -> if bangpats _lhsIoptions then TH.BangP x else x
                                                     {-# LINE 3734 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule483 #-}
   {-# LINE 1651 "src-ag/ExecutionPlan2TH.ag" #-}
   rule483 = \ _addbang ((_lhsIanyLazyKind) :: Bool) ->
                                                     {-# LINE 1651 "src-ag/ExecutionPlan2TH.ag" #-}
                                                     if _lhsIanyLazyKind then id else _addbang
                                                     {-# LINE 3740 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule484 #-}
   rule484 = \ ((_patIattrs) :: Set String) ->
     _patIattrs
   {-# INLINE rule485 #-}
   rule485 = \ ((_patIcopy) :: Pattern) attr_ field_ ->
     Alias field_ attr_ _patIcopy
   {-# INLINE rule486 #-}
   rule486 = \ _copy ->
     _copy
   {-# INLINE rule487 #-}
   rule487 = \ ((_patIisUnderscore) :: Bool) ->
     _patIisUnderscore
   {-# INLINE rule488 #-}
   rule488 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule489 #-}
   rule489 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule490 #-}
   rule490 = \ ((_lhsIanyLazyKind) :: Bool) ->
     _lhsIanyLazyKind
   {-# INLINE rule491 #-}
   rule491 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule492 #-}
   rule492 = \ ((_lhsIlocalAttrTypes) :: Map Identifier Type) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule493 #-}
   rule493 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule494 #-}
   rule494 = \ ((_lhsIsynmap) :: Attributes) ->
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
         _lhsOattrs = rule495 _patIattrs
         _copy = rule496 _patIcopy
         _lhsOcopy :: Pattern
         _lhsOcopy = rule497 _copy
         _lhsOisUnderscore :: Bool
         _lhsOisUnderscore = rule498 _patIisUnderscore
         _patOallInhmap = rule499 _lhsIallInhmap
         _patOallSynmap = rule500 _lhsIallSynmap
         _patOanyLazyKind = rule501 _lhsIanyLazyKind
         _patOinhmap = rule502 _lhsIinhmap
         _patOlocalAttrTypes = rule503 _lhsIlocalAttrTypes
         _patOoptions = rule504 _lhsIoptions
         _patOsynmap = rule505 _lhsIsynmap
         __result_ = T_Pattern_vOut40 _lhsOattrs _lhsOcopy _lhsOisUnderscore
         in __result_ )
     in C_Pattern_s41 v40
   {-# INLINE rule495 #-}
   rule495 = \ ((_patIattrs) :: Set String) ->
     _patIattrs
   {-# INLINE rule496 #-}
   rule496 = \ ((_patIcopy) :: Pattern) ->
     Irrefutable _patIcopy
   {-# INLINE rule497 #-}
   rule497 = \ _copy ->
     _copy
   {-# INLINE rule498 #-}
   rule498 = \ ((_patIisUnderscore) :: Bool) ->
     _patIisUnderscore
   {-# INLINE rule499 #-}
   rule499 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule500 #-}
   rule500 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule501 #-}
   rule501 = \ ((_lhsIanyLazyKind) :: Bool) ->
     _lhsIanyLazyKind
   {-# INLINE rule502 #-}
   rule502 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule503 #-}
   rule503 = \ ((_lhsIlocalAttrTypes) :: Map Identifier Type) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule504 #-}
   rule504 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule505 #-}
   rule505 = \ ((_lhsIsynmap) :: Attributes) ->
     _lhsIsynmap
{-# NOINLINE sem_Pattern_Underscore #-}
sem_Pattern_Underscore :: (Pos) -> T_Pattern 
sem_Pattern_Underscore arg_pos_ = T_Pattern (return st41) where
   {-# NOINLINE st41 #-}
   st41 = let
      v40 :: T_Pattern_v40 
      v40 = \ (T_Pattern_vIn40 _lhsIallInhmap _lhsIallSynmap _lhsIanyLazyKind _lhsIinhmap _lhsIlocalAttrTypes _lhsIoptions _lhsIsynmap) -> ( let
         _lhsOattrs :: Set String
         _lhsOattrs = rule506  ()
         _copy = rule507 arg_pos_
         _lhsOcopy :: Pattern
         _lhsOcopy = rule508 _copy
         _lhsOisUnderscore :: Bool
         _lhsOisUnderscore = rule509  ()
         __result_ = T_Pattern_vOut40 _lhsOattrs _lhsOcopy _lhsOisUnderscore
         in __result_ )
     in C_Pattern_s41 v40
   {-# INLINE rule506 #-}
   rule506 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule507 #-}
   rule507 = \ pos_ ->
     Underscore pos_
   {-# INLINE rule508 #-}
   rule508 = \ _copy ->
     _copy
   {-# INLINE rule509 #-}
   rule509 = \  (_ :: ()) ->
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
         _lhsOattrs = rule510 _hdIattrs _tlIattrs
         _copy = rule511 _hdIcopy _tlIcopy
         _lhsOcopy :: Patterns
         _lhsOcopy = rule512 _copy
         _hdOallInhmap = rule513 _lhsIallInhmap
         _hdOallSynmap = rule514 _lhsIallSynmap
         _hdOanyLazyKind = rule515 _lhsIanyLazyKind
         _hdOinhmap = rule516 _lhsIinhmap
         _hdOlocalAttrTypes = rule517 _lhsIlocalAttrTypes
         _hdOoptions = rule518 _lhsIoptions
         _hdOsynmap = rule519 _lhsIsynmap
         _tlOallInhmap = rule520 _lhsIallInhmap
         _tlOallSynmap = rule521 _lhsIallSynmap
         _tlOanyLazyKind = rule522 _lhsIanyLazyKind
         _tlOinhmap = rule523 _lhsIinhmap
         _tlOlocalAttrTypes = rule524 _lhsIlocalAttrTypes
         _tlOoptions = rule525 _lhsIoptions
         _tlOsynmap = rule526 _lhsIsynmap
         __result_ = T_Patterns_vOut43 _lhsOattrs _lhsOcopy
         in __result_ )
     in C_Patterns_s44 v43
   {-# INLINE rule510 #-}
   rule510 = \ ((_hdIattrs) :: Set String) ((_tlIattrs) :: Set String) ->
     _hdIattrs `Set.union` _tlIattrs
   {-# INLINE rule511 #-}
   rule511 = \ ((_hdIcopy) :: Pattern) ((_tlIcopy) :: Patterns) ->
     (:) _hdIcopy _tlIcopy
   {-# INLINE rule512 #-}
   rule512 = \ _copy ->
     _copy
   {-# INLINE rule513 #-}
   rule513 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule514 #-}
   rule514 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule515 #-}
   rule515 = \ ((_lhsIanyLazyKind) :: Bool) ->
     _lhsIanyLazyKind
   {-# INLINE rule516 #-}
   rule516 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule517 #-}
   rule517 = \ ((_lhsIlocalAttrTypes) :: Map Identifier Type) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule518 #-}
   rule518 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule519 #-}
   rule519 = \ ((_lhsIsynmap) :: Attributes) ->
     _lhsIsynmap
   {-# INLINE rule520 #-}
   rule520 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule521 #-}
   rule521 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule522 #-}
   rule522 = \ ((_lhsIanyLazyKind) :: Bool) ->
     _lhsIanyLazyKind
   {-# INLINE rule523 #-}
   rule523 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule524 #-}
   rule524 = \ ((_lhsIlocalAttrTypes) :: Map Identifier Type) ->
     _lhsIlocalAttrTypes
   {-# INLINE rule525 #-}
   rule525 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule526 #-}
   rule526 = \ ((_lhsIsynmap) :: Attributes) ->
     _lhsIsynmap
{-# NOINLINE sem_Patterns_Nil #-}
sem_Patterns_Nil ::  T_Patterns 
sem_Patterns_Nil  = T_Patterns (return st44) where
   {-# NOINLINE st44 #-}
   st44 = let
      v43 :: T_Patterns_v43 
      v43 = \ (T_Patterns_vIn43 _lhsIallInhmap _lhsIallSynmap _lhsIanyLazyKind _lhsIinhmap _lhsIlocalAttrTypes _lhsIoptions _lhsIsynmap) -> ( let
         _lhsOattrs :: Set String
         _lhsOattrs = rule527  ()
         _copy = rule528  ()
         _lhsOcopy :: Patterns
         _lhsOcopy = rule529 _copy
         __result_ = T_Patterns_vOut43 _lhsOattrs _lhsOcopy
         in __result_ )
     in C_Patterns_s44 v43
   {-# INLINE rule527 #-}
   rule527 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule528 #-}
   rule528 = \  (_ :: ()) ->
     []
   {-# INLINE rule529 #-}
   rule529 = \ _copy ->
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
         _lhsOallvisits = rule530 arg_from_ arg_ident_ arg_to_
         _stepsOfmtMode = rule531 arg_kind_
         _inhVarNms = rule532 _lhsIoptions arg_inh_
         _lazyIntrasInh = rule533 _inhVarNms _stepsIdefs arg_kind_
         _lhsOlazyIntras :: Set String
         _lhsOlazyIntras = rule534 _lazyIntrasInh _stepsIlazyIntras
         _addbang = rule535 _lhsIoptions
         _addbang1 = rule536 _addbang arg_kind_
         _lhsOfromToStates :: Map VisitIdentifier (Int,Int)
         _lhsOfromToStates = rule537 arg_from_ arg_ident_ arg_to_
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule538 arg_ident_ arg_kind_
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule539 _stepsIerrors
         _lhsOintramap :: Map StateIdentifier (Map String (Maybe NonLocalAttr))
         _lhsOintramap = rule540  ()
         _lhsOruleKinds :: Map Identifier (Set VisitKind)
         _lhsOruleKinds = rule541 _stepsIruleKinds
         _lhsOruleUsage :: Map Identifier Int
         _lhsOruleUsage = rule542 _stepsIruleUsage
         _lhsOusedArgs :: Set String
         _lhsOusedArgs = rule543 _stepsIusedArgs
         _lhsOvisitdefs :: Map VisitIdentifier (Set Identifier)
         _lhsOvisitdefs = rule544  ()
         _lhsOvisituses :: Map VisitIdentifier (Set Identifier)
         _lhsOvisituses = rule545  ()
         _stepsOallFromToStates = rule546 _lhsIallFromToStates
         _stepsOallInitStates = rule547 _lhsIallInitStates
         _stepsOallVisitKinds = rule548 _lhsIallVisitKinds
         _stepsOavisitdefs = rule549 _lhsIavisitdefs
         _stepsOavisituses = rule550 _lhsIavisituses
         _stepsOchildTypes = rule551 _lhsIchildTypes
         _stepsOindex = rule552  ()
         _stepsOkind = rule553 arg_kind_
         _stepsOoptions = rule554 _lhsIoptions
         _stepsOprevMaxSimRefs = rule555  ()
         _stepsOruledefs = rule556 _lhsIruledefs
         _stepsOruleuses = rule557 _lhsIruleuses
         __result_ = T_Visit_vOut46 _lhsOallvisits _lhsOerrors _lhsOfromToStates _lhsOintramap _lhsOlazyIntras _lhsOruleKinds _lhsOruleUsage _lhsOusedArgs _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses
         in __result_ )
     in C_Visit_s47 v46
   {-# INLINE rule530 #-}
   {-# LINE 407 "src-ag/ExecutionPlan2TH.ag" #-}
   rule530 = \ from_ ident_ to_ ->
                            {-# LINE 407 "src-ag/ExecutionPlan2TH.ag" #-}
                            (ident_, from_, to_)
                            {-# LINE 4088 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule531 #-}
   {-# LINE 927 "src-ag/ExecutionPlan2TH.ag" #-}
   rule531 = \ kind_ ->
                    {-# LINE 927 "src-ag/ExecutionPlan2TH.ag" #-}
                    case kind_ of
                      VisitPure False -> FormatLetDecl
                      VisitPure True  -> FormatLetLine
                      VisitMonadic    -> FormatDo
                    {-# LINE 4097 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule532 #-}
   {-# LINE 1422 "src-ag/ExecutionPlan2TH.ag" #-}
   rule532 = \ ((_lhsIoptions) :: Options) inh_ ->
                            {-# LINE 1422 "src-ag/ExecutionPlan2TH.ag" #-}
                            Set.map (lhsname _lhsIoptions True) inh_
                            {-# LINE 4103 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule533 #-}
   {-# LINE 1481 "src-ag/ExecutionPlan2TH.ag" #-}
   rule533 = \ _inhVarNms ((_stepsIdefs) :: Set String) kind_ ->
                        {-# LINE 1481 "src-ag/ExecutionPlan2TH.ag" #-}
                        case kind_ of
                          VisitPure False -> _inhVarNms     `Set.union` _stepsIdefs
                          _               -> Set.empty
                        {-# LINE 4111 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule534 #-}
   {-# LINE 1484 "src-ag/ExecutionPlan2TH.ag" #-}
   rule534 = \ _lazyIntrasInh ((_stepsIlazyIntras) :: Set String) ->
                     {-# LINE 1484 "src-ag/ExecutionPlan2TH.ag" #-}
                     _lazyIntrasInh     `Set.union` _stepsIlazyIntras
                     {-# LINE 4117 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule535 #-}
   {-# LINE 1637 "src-ag/ExecutionPlan2TH.ag" #-}
   rule535 = \ ((_lhsIoptions) :: Options) ->
                                                    {-# LINE 1637 "src-ag/ExecutionPlan2TH.ag" #-}
                                                    \x -> if bangpats _lhsIoptions then TH.BangP x else x
                                                    {-# LINE 4123 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule536 #-}
   {-# LINE 1648 "src-ag/ExecutionPlan2TH.ag" #-}
   rule536 = \ _addbang kind_ ->
                                                     {-# LINE 1648 "src-ag/ExecutionPlan2TH.ag" #-}
                                                     if isLazyKind kind_ then id else _addbang
                                                     {-# LINE 4129 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule537 #-}
   {-# LINE 1675 "src-ag/ExecutionPlan2TH.ag" #-}
   rule537 = \ from_ ident_ to_ ->
                       {-# LINE 1675 "src-ag/ExecutionPlan2TH.ag" #-}
                       Map.singleton ident_ (from_, to_)
                       {-# LINE 4135 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule538 #-}
   {-# LINE 1719 "src-ag/ExecutionPlan2TH.ag" #-}
   rule538 = \ ident_ kind_ ->
                     {-# LINE 1719 "src-ag/ExecutionPlan2TH.ag" #-}
                     Map.singleton ident_ kind_
                     {-# LINE 4141 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule539 #-}
   rule539 = \ ((_stepsIerrors) :: Seq Error) ->
     _stepsIerrors
   {-# INLINE rule540 #-}
   rule540 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule541 #-}
   rule541 = \ ((_stepsIruleKinds) :: Map Identifier (Set VisitKind)) ->
     _stepsIruleKinds
   {-# INLINE rule542 #-}
   rule542 = \ ((_stepsIruleUsage) :: Map Identifier Int) ->
     _stepsIruleUsage
   {-# INLINE rule543 #-}
   rule543 = \ ((_stepsIusedArgs) :: Set String) ->
     _stepsIusedArgs
   {-# INLINE rule544 #-}
   rule544 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule545 #-}
   rule545 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule546 #-}
   rule546 = \ ((_lhsIallFromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _lhsIallFromToStates
   {-# INLINE rule547 #-}
   rule547 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule548 #-}
   rule548 = \ ((_lhsIallVisitKinds) :: Map VisitIdentifier VisitKind) ->
     _lhsIallVisitKinds
   {-# INLINE rule549 #-}
   rule549 = \ ((_lhsIavisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisitdefs
   {-# INLINE rule550 #-}
   rule550 = \ ((_lhsIavisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisituses
   {-# INLINE rule551 #-}
   rule551 = \ ((_lhsIchildTypes) :: Map Identifier Type) ->
     _lhsIchildTypes
   {-# INLINE rule552 #-}
   rule552 = \  (_ :: ()) ->
     error "missing rule: Visit.Visit.steps.index"
   {-# INLINE rule553 #-}
   rule553 = \ kind_ ->
     kind_
   {-# INLINE rule554 #-}
   rule554 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule555 #-}
   rule555 = \  (_ :: ()) ->
     error "missing rule: Visit.Visit.steps.prevMaxSimRefs"
   {-# INLINE rule556 #-}
   rule556 = \ ((_lhsIruledefs) :: Map Identifier (Set String)) ->
     _lhsIruledefs
   {-# INLINE rule557 #-}
   rule557 = \ ((_lhsIruleuses) :: Map Identifier (Map String (Maybe NonLocalAttr))) ->
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
         _lhsOruleUsage = rule558 arg_name_
         _lhsOdefs :: Set String
         _lhsOdefs = rule559  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule560  ()
         _lhsOlazyIntras :: Set String
         _lhsOlazyIntras = rule561  ()
         _lhsOruleKinds :: Map Identifier (Set VisitKind)
         _lhsOruleKinds = rule562  ()
         _lhsOusedArgs :: Set String
         _lhsOusedArgs = rule563  ()
         _lhsOuses :: Map String (Maybe NonLocalAttr)
         _lhsOuses = rule564  ()
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule565  ()
         _lhsOindex :: Int
         _lhsOindex = rule566 _lhsIindex
         _lhsOisLast :: Bool
         _lhsOisLast = rule567 _lhsIisLast
         _lhsOprevMaxSimRefs :: Int
         _lhsOprevMaxSimRefs = rule568 _lhsIprevMaxSimRefs
         __result_ = T_VisitStep_vOut49 _lhsOdefs _lhsOerrors _lhsOindex _lhsOisLast _lhsOlazyIntras _lhsOprevMaxSimRefs _lhsOruleKinds _lhsOruleUsage _lhsOusedArgs _lhsOuses _lhsOvisitKinds
         in __result_ )
     in C_VisitStep_s50 v49
   {-# INLINE rule558 #-}
   {-# LINE 1369 "src-ag/ExecutionPlan2TH.ag" #-}
   rule558 = \ name_ ->
                                                 {-# LINE 1369 "src-ag/ExecutionPlan2TH.ag" #-}
                                                 Map.singleton name_ 1
                                                 {-# LINE 4271 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule559 #-}
   rule559 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule560 #-}
   rule560 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule561 #-}
   rule561 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule562 #-}
   rule562 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule563 #-}
   rule563 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule564 #-}
   rule564 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule565 #-}
   rule565 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule566 #-}
   rule566 = \ ((_lhsIindex) :: Int) ->
     _lhsIindex
   {-# INLINE rule567 #-}
   rule567 = \ ((_lhsIisLast) :: Bool) ->
     _lhsIisLast
   {-# INLINE rule568 #-}
   rule568 = \ ((_lhsIprevMaxSimRefs) :: Int) ->
     _lhsIprevMaxSimRefs
{-# NOINLINE sem_VisitStep_ChildVisit #-}
sem_VisitStep_ChildVisit :: (Identifier) -> (NontermIdent) -> (VisitIdentifier) -> T_VisitStep 
sem_VisitStep_ChildVisit _ _ arg_visit_ = T_VisitStep (return st50) where
   {-# NOINLINE st50 #-}
   st50 = let
      v49 :: T_VisitStep_v49 
      v49 = \ (T_VisitStep_vIn49 _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIfmtMode _lhsIindex _lhsIisLast _lhsIkind _lhsIoptions _lhsIprevMaxSimRefs _lhsIruledefs _lhsIruleuses) -> ( let
         _addbang = rule569 _lhsIoptions
         (_from,_to) = rule570 _lhsIallFromToStates arg_visit_
         _lhsOdefs :: Set String
         _lhsOdefs = rule571  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule572  ()
         _lhsOlazyIntras :: Set String
         _lhsOlazyIntras = rule573  ()
         _lhsOruleKinds :: Map Identifier (Set VisitKind)
         _lhsOruleKinds = rule574  ()
         _lhsOruleUsage :: Map Identifier Int
         _lhsOruleUsage = rule575  ()
         _lhsOusedArgs :: Set String
         _lhsOusedArgs = rule576  ()
         _lhsOuses :: Map String (Maybe NonLocalAttr)
         _lhsOuses = rule577  ()
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule578  ()
         _lhsOindex :: Int
         _lhsOindex = rule579 _lhsIindex
         _lhsOisLast :: Bool
         _lhsOisLast = rule580 _lhsIisLast
         _lhsOprevMaxSimRefs :: Int
         _lhsOprevMaxSimRefs = rule581 _lhsIprevMaxSimRefs
         __result_ = T_VisitStep_vOut49 _lhsOdefs _lhsOerrors _lhsOindex _lhsOisLast _lhsOlazyIntras _lhsOprevMaxSimRefs _lhsOruleKinds _lhsOruleUsage _lhsOusedArgs _lhsOuses _lhsOvisitKinds
         in __result_ )
     in C_VisitStep_s50 v49
   {-# INLINE rule569 #-}
   {-# LINE 1645 "src-ag/ExecutionPlan2TH.ag" #-}
   rule569 = \ ((_lhsIoptions) :: Options) ->
                                                    {-# LINE 1645 "src-ag/ExecutionPlan2TH.ag" #-}
                                                    \x -> if bangpats _lhsIoptions then TH.BangP x else x
                                                    {-# LINE 4341 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule570 #-}
   {-# LINE 1681 "src-ag/ExecutionPlan2TH.ag" #-}
   rule570 = \ ((_lhsIallFromToStates) :: Map VisitIdentifier (Int,Int)) visit_ ->
                         {-# LINE 1681 "src-ag/ExecutionPlan2TH.ag" #-}
                         Map.findWithDefault (error "visit not in allFromToStates") visit_ _lhsIallFromToStates
                         {-# LINE 4347 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule571 #-}
   rule571 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule572 #-}
   rule572 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule573 #-}
   rule573 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule574 #-}
   rule574 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule575 #-}
   rule575 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule576 #-}
   rule576 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule577 #-}
   rule577 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule578 #-}
   rule578 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule579 #-}
   rule579 = \ ((_lhsIindex) :: Int) ->
     _lhsIindex
   {-# INLINE rule580 #-}
   rule580 = \ ((_lhsIisLast) :: Bool) ->
     _lhsIisLast
   {-# INLINE rule581 #-}
   rule581 = \ ((_lhsIprevMaxSimRefs) :: Int) ->
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
         _stepsOfmtMode = rule582 _lhsIfmtMode
         _lhsOlazyIntras :: Set String
         _lhsOlazyIntras = rule583 _stepsIdefs _stepsIlazyIntras arg_ordered_
         _lhsOdefs :: Set String
         _lhsOdefs = rule584 _stepsIdefs
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule585 _stepsIerrors
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
         _stepsOindex = rule600 _lhsIindex
         _stepsOkind = rule601 _lhsIkind
         _stepsOoptions = rule602 _lhsIoptions
         _stepsOprevMaxSimRefs = rule603 _lhsIprevMaxSimRefs
         _stepsOruledefs = rule604 _lhsIruledefs
         _stepsOruleuses = rule605 _lhsIruleuses
         __result_ = T_VisitStep_vOut49 _lhsOdefs _lhsOerrors _lhsOindex _lhsOisLast _lhsOlazyIntras _lhsOprevMaxSimRefs _lhsOruleKinds _lhsOruleUsage _lhsOusedArgs _lhsOuses _lhsOvisitKinds
         in __result_ )
     in C_VisitStep_s50 v49
   {-# INLINE rule582 #-}
   {-# LINE 933 "src-ag/ExecutionPlan2TH.ag" #-}
   rule582 = \ ((_lhsIfmtMode) :: FormatMode) ->
                    {-# LINE 933 "src-ag/ExecutionPlan2TH.ag" #-}
                    case _lhsIfmtMode of
                      FormatDo      -> FormatLetDecl
                      mode          -> mode
                    {-# LINE 4435 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule583 #-}
   {-# LINE 1487 "src-ag/ExecutionPlan2TH.ag" #-}
   rule583 = \ ((_stepsIdefs) :: Set String) ((_stepsIlazyIntras) :: Set String) ordered_ ->
                     {-# LINE 1487 "src-ag/ExecutionPlan2TH.ag" #-}
                     if ordered_
                     then _stepsIlazyIntras
                     else _stepsIdefs
                     {-# LINE 4443 "src-generated/ExecutionPlan2TH.hs" #-}
   {-# INLINE rule584 #-}
   rule584 = \ ((_stepsIdefs) :: Set String) ->
     _stepsIdefs
   {-# INLINE rule585 #-}
   rule585 = \ ((_stepsIerrors) :: Seq Error) ->
     _stepsIerrors
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
   rule600 = \ ((_lhsIindex) :: Int) ->
     _lhsIindex
   {-# INLINE rule601 #-}
   rule601 = \ ((_lhsIkind) :: VisitKind) ->
     _lhsIkind
   {-# INLINE rule602 #-}
   rule602 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule603 #-}
   rule603 = \ ((_lhsIprevMaxSimRefs) :: Int) ->
     _lhsIprevMaxSimRefs
   {-# INLINE rule604 #-}
   rule604 = \ ((_lhsIruledefs) :: Map Identifier (Set String)) ->
     _lhsIruledefs
   {-# INLINE rule605 #-}
   rule605 = \ ((_lhsIruleuses) :: Map Identifier (Map String (Maybe NonLocalAttr))) ->
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
         _lhsOdefs = rule606 _stepsIdefs
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule607 _stepsIerrors
         _lhsOlazyIntras :: Set String
         _lhsOlazyIntras = rule608 _stepsIlazyIntras
         _lhsOruleKinds :: Map Identifier (Set VisitKind)
         _lhsOruleKinds = rule609 _stepsIruleKinds
         _lhsOruleUsage :: Map Identifier Int
         _lhsOruleUsage = rule610 _stepsIruleUsage
         _lhsOusedArgs :: Set String
         _lhsOusedArgs = rule611 _stepsIusedArgs
         _lhsOuses :: Map String (Maybe NonLocalAttr)
         _lhsOuses = rule612 _stepsIuses
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule613 _stepsIvisitKinds
         _lhsOindex :: Int
         _lhsOindex = rule614 _stepsIindex
         _lhsOisLast :: Bool
         _lhsOisLast = rule615 _stepsIisLast
         _lhsOprevMaxSimRefs :: Int
         _lhsOprevMaxSimRefs = rule616 _stepsIprevMaxSimRefs
         _stepsOallFromToStates = rule617 _lhsIallFromToStates
         _stepsOallInitStates = rule618 _lhsIallInitStates
         _stepsOallVisitKinds = rule619 _lhsIallVisitKinds
         _stepsOavisitdefs = rule620 _lhsIavisitdefs
         _stepsOavisituses = rule621 _lhsIavisituses
         _stepsOchildTypes = rule622 _lhsIchildTypes
         _stepsOfmtMode = rule623 _lhsIfmtMode
         _stepsOindex = rule624 _lhsIindex
         _stepsOkind = rule625 _lhsIkind
         _stepsOoptions = rule626 _lhsIoptions
         _stepsOprevMaxSimRefs = rule627 _lhsIprevMaxSimRefs
         _stepsOruledefs = rule628 _lhsIruledefs
         _stepsOruleuses = rule629 _lhsIruleuses
         __result_ = T_VisitStep_vOut49 _lhsOdefs _lhsOerrors _lhsOindex _lhsOisLast _lhsOlazyIntras _lhsOprevMaxSimRefs _lhsOruleKinds _lhsOruleUsage _lhsOusedArgs _lhsOuses _lhsOvisitKinds
         in __result_ )
     in C_VisitStep_s50 v49
   {-# INLINE rule606 #-}
   rule606 = \ ((_stepsIdefs) :: Set String) ->
     _stepsIdefs
   {-# INLINE rule607 #-}
   rule607 = \ ((_stepsIerrors) :: Seq Error) ->
     _stepsIerrors
   {-# INLINE rule608 #-}
   rule608 = \ ((_stepsIlazyIntras) :: Set String) ->
     _stepsIlazyIntras
   {-# INLINE rule609 #-}
   rule609 = \ ((_stepsIruleKinds) :: Map Identifier (Set VisitKind)) ->
     _stepsIruleKinds
   {-# INLINE rule610 #-}
   rule610 = \ ((_stepsIruleUsage) :: Map Identifier Int) ->
     _stepsIruleUsage
   {-# INLINE rule611 #-}
   rule611 = \ ((_stepsIusedArgs) :: Set String) ->
     _stepsIusedArgs
   {-# INLINE rule612 #-}
   rule612 = \ ((_stepsIuses) :: Map String (Maybe NonLocalAttr)) ->
     _stepsIuses
   {-# INLINE rule613 #-}
   rule613 = \ ((_stepsIvisitKinds) :: Map VisitIdentifier VisitKind) ->
     _stepsIvisitKinds
   {-# INLINE rule614 #-}
   rule614 = \ ((_stepsIindex) :: Int) ->
     _stepsIindex
   {-# INLINE rule615 #-}
   rule615 = \ ((_stepsIisLast) :: Bool) ->
     _stepsIisLast
   {-# INLINE rule616 #-}
   rule616 = \ ((_stepsIprevMaxSimRefs) :: Int) ->
     _stepsIprevMaxSimRefs
   {-# INLINE rule617 #-}
   rule617 = \ ((_lhsIallFromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _lhsIallFromToStates
   {-# INLINE rule618 #-}
   rule618 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule619 #-}
   rule619 = \ ((_lhsIallVisitKinds) :: Map VisitIdentifier VisitKind) ->
     _lhsIallVisitKinds
   {-# INLINE rule620 #-}
   rule620 = \ ((_lhsIavisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisitdefs
   {-# INLINE rule621 #-}
   rule621 = \ ((_lhsIavisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisituses
   {-# INLINE rule622 #-}
   rule622 = \ ((_lhsIchildTypes) :: Map Identifier Type) ->
     _lhsIchildTypes
   {-# INLINE rule623 #-}
   rule623 = \ ((_lhsIfmtMode) :: FormatMode) ->
     _lhsIfmtMode
   {-# INLINE rule624 #-}
   rule624 = \ ((_lhsIindex) :: Int) ->
     _lhsIindex
   {-# INLINE rule625 #-}
   rule625 = \ ((_lhsIkind) :: VisitKind) ->
     _lhsIkind
   {-# INLINE rule626 #-}
   rule626 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule627 #-}
   rule627 = \ ((_lhsIprevMaxSimRefs) :: Int) ->
     _lhsIprevMaxSimRefs
   {-# INLINE rule628 #-}
   rule628 = \ ((_lhsIruledefs) :: Map Identifier (Set String)) ->
     _lhsIruledefs
   {-# INLINE rule629 #-}
   rule629 = \ ((_lhsIruleuses) :: Map Identifier (Map String (Maybe NonLocalAttr))) ->
     _lhsIruleuses
{-# NOINLINE sem_VisitStep_ChildIntro #-}
sem_VisitStep_ChildIntro :: (Identifier) -> T_VisitStep 
sem_VisitStep_ChildIntro _ = T_VisitStep (return st50) where
   {-# NOINLINE st50 #-}
   st50 = let
      v49 :: T_VisitStep_v49 
      v49 = \ (T_VisitStep_vIn49 _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIfmtMode _lhsIindex _lhsIisLast _lhsIkind _lhsIoptions _lhsIprevMaxSimRefs _lhsIruledefs _lhsIruleuses) -> ( let
         _lhsOdefs :: Set String
         _lhsOdefs = rule630  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule631  ()
         _lhsOlazyIntras :: Set String
         _lhsOlazyIntras = rule632  ()
         _lhsOruleKinds :: Map Identifier (Set VisitKind)
         _lhsOruleKinds = rule633  ()
         _lhsOruleUsage :: Map Identifier Int
         _lhsOruleUsage = rule634  ()
         _lhsOusedArgs :: Set String
         _lhsOusedArgs = rule635  ()
         _lhsOuses :: Map String (Maybe NonLocalAttr)
         _lhsOuses = rule636  ()
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule637  ()
         _lhsOindex :: Int
         _lhsOindex = rule638 _lhsIindex
         _lhsOisLast :: Bool
         _lhsOisLast = rule639 _lhsIisLast
         _lhsOprevMaxSimRefs :: Int
         _lhsOprevMaxSimRefs = rule640 _lhsIprevMaxSimRefs
         __result_ = T_VisitStep_vOut49 _lhsOdefs _lhsOerrors _lhsOindex _lhsOisLast _lhsOlazyIntras _lhsOprevMaxSimRefs _lhsOruleKinds _lhsOruleUsage _lhsOusedArgs _lhsOuses _lhsOvisitKinds
         in __result_ )
     in C_VisitStep_s50 v49
   {-# INLINE rule630 #-}
   rule630 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule631 #-}
   rule631 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule632 #-}
   rule632 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule633 #-}
   rule633 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule634 #-}
   rule634 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule635 #-}
   rule635 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule636 #-}
   rule636 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule637 #-}
   rule637 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule638 #-}
   rule638 = \ ((_lhsIindex) :: Int) ->
     _lhsIindex
   {-# INLINE rule639 #-}
   rule639 = \ ((_lhsIisLast) :: Bool) ->
     _lhsIisLast
   {-# INLINE rule640 #-}
   rule640 = \ ((_lhsIprevMaxSimRefs) :: Int) ->
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
         _lhsOdefs = rule641 _hdIdefs _tlIdefs
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule642 _hdIerrors _tlIerrors
         _lhsOlazyIntras :: Set String
         _lhsOlazyIntras = rule643 _hdIlazyIntras _tlIlazyIntras
         _lhsOruleKinds :: Map Identifier (Set VisitKind)
         _lhsOruleKinds = rule644 _hdIruleKinds _tlIruleKinds
         _lhsOruleUsage :: Map Identifier Int
         _lhsOruleUsage = rule645 _hdIruleUsage _tlIruleUsage
         _lhsOusedArgs :: Set String
         _lhsOusedArgs = rule646 _hdIusedArgs _tlIusedArgs
         _lhsOuses :: Map String (Maybe NonLocalAttr)
         _lhsOuses = rule647 _hdIuses _tlIuses
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule648 _hdIvisitKinds _tlIvisitKinds
         _lhsOindex :: Int
         _lhsOindex = rule649 _tlIindex
         _lhsOisLast :: Bool
         _lhsOisLast = rule650 _tlIisLast
         _lhsOprevMaxSimRefs :: Int
         _lhsOprevMaxSimRefs = rule651 _tlIprevMaxSimRefs
         _lhsOsize :: Int
         _lhsOsize = rule652 _tlIsize
         _hdOallFromToStates = rule653 _lhsIallFromToStates
         _hdOallInitStates = rule654 _lhsIallInitStates
         _hdOallVisitKinds = rule655 _lhsIallVisitKinds
         _hdOavisitdefs = rule656 _lhsIavisitdefs
         _hdOavisituses = rule657 _lhsIavisituses
         _hdOchildTypes = rule658 _lhsIchildTypes
         _hdOfmtMode = rule659 _lhsIfmtMode
         _hdOindex = rule660 _lhsIindex
         _hdOisLast = rule661  ()
         _hdOkind = rule662 _lhsIkind
         _hdOoptions = rule663 _lhsIoptions
         _hdOprevMaxSimRefs = rule664 _lhsIprevMaxSimRefs
         _hdOruledefs = rule665 _lhsIruledefs
         _hdOruleuses = rule666 _lhsIruleuses
         _tlOallFromToStates = rule667 _lhsIallFromToStates
         _tlOallInitStates = rule668 _lhsIallInitStates
         _tlOallVisitKinds = rule669 _lhsIallVisitKinds
         _tlOavisitdefs = rule670 _lhsIavisitdefs
         _tlOavisituses = rule671 _lhsIavisituses
         _tlOchildTypes = rule672 _lhsIchildTypes
         _tlOfmtMode = rule673 _lhsIfmtMode
         _tlOindex = rule674 _hdIindex
         _tlOkind = rule675 _lhsIkind
         _tlOoptions = rule676 _lhsIoptions
         _tlOprevMaxSimRefs = rule677 _hdIprevMaxSimRefs
         _tlOruledefs = rule678 _lhsIruledefs
         _tlOruleuses = rule679 _lhsIruleuses
         __result_ = T_VisitSteps_vOut52 _lhsOdefs _lhsOerrors _lhsOindex _lhsOisLast _lhsOlazyIntras _lhsOprevMaxSimRefs _lhsOruleKinds _lhsOruleUsage _lhsOsize _lhsOusedArgs _lhsOuses _lhsOvisitKinds
         in __result_ )
     in C_VisitSteps_s53 v52
   {-# INLINE rule641 #-}
   rule641 = \ ((_hdIdefs) :: Set String) ((_tlIdefs) :: Set String) ->
     _hdIdefs `Set.union` _tlIdefs
   {-# INLINE rule642 #-}
   rule642 = \ ((_hdIerrors) :: Seq Error) ((_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# INLINE rule643 #-}
   rule643 = \ ((_hdIlazyIntras) :: Set String) ((_tlIlazyIntras) :: Set String) ->
     _hdIlazyIntras `Set.union` _tlIlazyIntras
   {-# INLINE rule644 #-}
   rule644 = \ ((_hdIruleKinds) :: Map Identifier (Set VisitKind)) ((_tlIruleKinds) :: Map Identifier (Set VisitKind)) ->
     _hdIruleKinds `unionWithMappend` _tlIruleKinds
   {-# INLINE rule645 #-}
   rule645 = \ ((_hdIruleUsage) :: Map Identifier Int) ((_tlIruleUsage) :: Map Identifier Int) ->
     _hdIruleUsage `unionWithSum` _tlIruleUsage
   {-# INLINE rule646 #-}
   rule646 = \ ((_hdIusedArgs) :: Set String) ((_tlIusedArgs) :: Set String) ->
     _hdIusedArgs `Set.union` _tlIusedArgs
   {-# INLINE rule647 #-}
   rule647 = \ ((_hdIuses) :: Map String (Maybe NonLocalAttr)) ((_tlIuses) :: Map String (Maybe NonLocalAttr)) ->
     _hdIuses `Map.union` _tlIuses
   {-# INLINE rule648 #-}
   rule648 = \ ((_hdIvisitKinds) :: Map VisitIdentifier VisitKind) ((_tlIvisitKinds) :: Map VisitIdentifier VisitKind) ->
     _hdIvisitKinds `mappend` _tlIvisitKinds
   {-# INLINE rule649 #-}
   rule649 = \ ((_tlIindex) :: Int) ->
     _tlIindex
   {-# INLINE rule650 #-}
   rule650 = \ ((_tlIisLast) :: Bool) ->
     _tlIisLast
   {-# INLINE rule651 #-}
   rule651 = \ ((_tlIprevMaxSimRefs) :: Int) ->
     _tlIprevMaxSimRefs
   {-# INLINE rule652 #-}
   rule652 = \ ((_tlIsize) :: Int) ->
     _tlIsize
   {-# INLINE rule653 #-}
   rule653 = \ ((_lhsIallFromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _lhsIallFromToStates
   {-# INLINE rule654 #-}
   rule654 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule655 #-}
   rule655 = \ ((_lhsIallVisitKinds) :: Map VisitIdentifier VisitKind) ->
     _lhsIallVisitKinds
   {-# INLINE rule656 #-}
   rule656 = \ ((_lhsIavisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisitdefs
   {-# INLINE rule657 #-}
   rule657 = \ ((_lhsIavisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisituses
   {-# INLINE rule658 #-}
   rule658 = \ ((_lhsIchildTypes) :: Map Identifier Type) ->
     _lhsIchildTypes
   {-# INLINE rule659 #-}
   rule659 = \ ((_lhsIfmtMode) :: FormatMode) ->
     _lhsIfmtMode
   {-# INLINE rule660 #-}
   rule660 = \ ((_lhsIindex) :: Int) ->
     _lhsIindex
   {-# INLINE rule661 #-}
   rule661 = \  (_ :: ()) ->
     error "missing rule: VisitSteps.Cons.hd.isLast"
   {-# INLINE rule662 #-}
   rule662 = \ ((_lhsIkind) :: VisitKind) ->
     _lhsIkind
   {-# INLINE rule663 #-}
   rule663 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule664 #-}
   rule664 = \ ((_lhsIprevMaxSimRefs) :: Int) ->
     _lhsIprevMaxSimRefs
   {-# INLINE rule665 #-}
   rule665 = \ ((_lhsIruledefs) :: Map Identifier (Set String)) ->
     _lhsIruledefs
   {-# INLINE rule666 #-}
   rule666 = \ ((_lhsIruleuses) :: Map Identifier (Map String (Maybe NonLocalAttr))) ->
     _lhsIruleuses
   {-# INLINE rule667 #-}
   rule667 = \ ((_lhsIallFromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _lhsIallFromToStates
   {-# INLINE rule668 #-}
   rule668 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule669 #-}
   rule669 = \ ((_lhsIallVisitKinds) :: Map VisitIdentifier VisitKind) ->
     _lhsIallVisitKinds
   {-# INLINE rule670 #-}
   rule670 = \ ((_lhsIavisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisitdefs
   {-# INLINE rule671 #-}
   rule671 = \ ((_lhsIavisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisituses
   {-# INLINE rule672 #-}
   rule672 = \ ((_lhsIchildTypes) :: Map Identifier Type) ->
     _lhsIchildTypes
   {-# INLINE rule673 #-}
   rule673 = \ ((_lhsIfmtMode) :: FormatMode) ->
     _lhsIfmtMode
   {-# INLINE rule674 #-}
   rule674 = \ ((_hdIindex) :: Int) ->
     _hdIindex
   {-# INLINE rule675 #-}
   rule675 = \ ((_lhsIkind) :: VisitKind) ->
     _lhsIkind
   {-# INLINE rule676 #-}
   rule676 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule677 #-}
   rule677 = \ ((_hdIprevMaxSimRefs) :: Int) ->
     _hdIprevMaxSimRefs
   {-# INLINE rule678 #-}
   rule678 = \ ((_lhsIruledefs) :: Map Identifier (Set String)) ->
     _lhsIruledefs
   {-# INLINE rule679 #-}
   rule679 = \ ((_lhsIruleuses) :: Map Identifier (Map String (Maybe NonLocalAttr))) ->
     _lhsIruleuses
{-# NOINLINE sem_VisitSteps_Nil #-}
sem_VisitSteps_Nil ::  T_VisitSteps 
sem_VisitSteps_Nil  = T_VisitSteps (return st53) where
   {-# NOINLINE st53 #-}
   st53 = let
      v52 :: T_VisitSteps_v52 
      v52 = \ (T_VisitSteps_vIn52 _lhsIallFromToStates _lhsIallInitStates _lhsIallVisitKinds _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIfmtMode _lhsIindex _lhsIkind _lhsIoptions _lhsIprevMaxSimRefs _lhsIruledefs _lhsIruleuses) -> ( let
         _lhsOdefs :: Set String
         _lhsOdefs = rule680  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule681  ()
         _lhsOlazyIntras :: Set String
         _lhsOlazyIntras = rule682  ()
         _lhsOruleKinds :: Map Identifier (Set VisitKind)
         _lhsOruleKinds = rule683  ()
         _lhsOruleUsage :: Map Identifier Int
         _lhsOruleUsage = rule684  ()
         _lhsOusedArgs :: Set String
         _lhsOusedArgs = rule685  ()
         _lhsOuses :: Map String (Maybe NonLocalAttr)
         _lhsOuses = rule686  ()
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule687  ()
         _lhsOindex :: Int
         _lhsOindex = rule688 _lhsIindex
         _lhsOisLast :: Bool
         _lhsOisLast = rule689  ()
         _lhsOprevMaxSimRefs :: Int
         _lhsOprevMaxSimRefs = rule690 _lhsIprevMaxSimRefs
         _lhsOsize :: Int
         _lhsOsize = rule691  ()
         __result_ = T_VisitSteps_vOut52 _lhsOdefs _lhsOerrors _lhsOindex _lhsOisLast _lhsOlazyIntras _lhsOprevMaxSimRefs _lhsOruleKinds _lhsOruleUsage _lhsOsize _lhsOusedArgs _lhsOuses _lhsOvisitKinds
         in __result_ )
     in C_VisitSteps_s53 v52
   {-# INLINE rule680 #-}
   rule680 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule681 #-}
   rule681 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule682 #-}
   rule682 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule683 #-}
   rule683 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule684 #-}
   rule684 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule685 #-}
   rule685 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule686 #-}
   rule686 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule687 #-}
   rule687 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule688 #-}
   rule688 = \ ((_lhsIindex) :: Int) ->
     _lhsIindex
   {-# INLINE rule689 #-}
   rule689 = \  (_ :: ()) ->
     error "missing rule: VisitSteps.Nil.lhs.isLast"
   {-# INLINE rule690 #-}
   rule690 = \ ((_lhsIprevMaxSimRefs) :: Int) ->
     _lhsIprevMaxSimRefs
   {-# INLINE rule691 #-}
   rule691 = \  (_ :: ()) ->
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
         _lhsOallvisits = rule692 _hdIallvisits _tlIallvisits
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule693 _hdIerrors _tlIerrors
         _lhsOfromToStates :: Map VisitIdentifier (Int,Int)
         _lhsOfromToStates = rule694 _hdIfromToStates _tlIfromToStates
         _lhsOintramap :: Map StateIdentifier (Map String (Maybe NonLocalAttr))
         _lhsOintramap = rule695 _hdIintramap _tlIintramap
         _lhsOlazyIntras :: Set String
         _lhsOlazyIntras = rule696 _hdIlazyIntras _tlIlazyIntras
         _lhsOruleKinds :: Map Identifier (Set VisitKind)
         _lhsOruleKinds = rule697 _hdIruleKinds _tlIruleKinds
         _lhsOruleUsage :: Map Identifier Int
         _lhsOruleUsage = rule698 _hdIruleUsage _tlIruleUsage
         _lhsOusedArgs :: Set String
         _lhsOusedArgs = rule699 _hdIusedArgs _tlIusedArgs
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule700 _hdIvisitKinds _tlIvisitKinds
         _lhsOvisitdefs :: Map VisitIdentifier (Set Identifier)
         _lhsOvisitdefs = rule701 _hdIvisitdefs _tlIvisitdefs
         _lhsOvisituses :: Map VisitIdentifier (Set Identifier)
         _lhsOvisituses = rule702 _hdIvisituses _tlIvisituses
         _hdOallFromToStates = rule703 _lhsIallFromToStates
         _hdOallInhmap = rule704 _lhsIallInhmap
         _hdOallInitStates = rule705 _lhsIallInitStates
         _hdOallSynmap = rule706 _lhsIallSynmap
         _hdOallVisitKinds = rule707 _lhsIallVisitKinds
         _hdOallintramap = rule708 _lhsIallintramap
         _hdOavisitdefs = rule709 _lhsIavisitdefs
         _hdOavisituses = rule710 _lhsIavisituses
         _hdOchildTypes = rule711 _lhsIchildTypes
         _hdOcon = rule712 _lhsIcon
         _hdOinhmap = rule713 _lhsIinhmap
         _hdOnextVisits = rule714 _lhsInextVisits
         _hdOnt = rule715 _lhsInt
         _hdOoptions = rule716 _lhsIoptions
         _hdOparams = rule717 _lhsIparams
         _hdOprevVisits = rule718 _lhsIprevVisits
         _hdOruledefs = rule719 _lhsIruledefs
         _hdOruleuses = rule720 _lhsIruleuses
         _hdOsynmap = rule721 _lhsIsynmap
         _hdOterminaldefs = rule722 _lhsIterminaldefs
         _tlOallFromToStates = rule723 _lhsIallFromToStates
         _tlOallInhmap = rule724 _lhsIallInhmap
         _tlOallInitStates = rule725 _lhsIallInitStates
         _tlOallSynmap = rule726 _lhsIallSynmap
         _tlOallVisitKinds = rule727 _lhsIallVisitKinds
         _tlOallintramap = rule728 _lhsIallintramap
         _tlOavisitdefs = rule729 _lhsIavisitdefs
         _tlOavisituses = rule730 _lhsIavisituses
         _tlOchildTypes = rule731 _lhsIchildTypes
         _tlOcon = rule732 _lhsIcon
         _tlOinhmap = rule733 _lhsIinhmap
         _tlOnextVisits = rule734 _lhsInextVisits
         _tlOnt = rule735 _lhsInt
         _tlOoptions = rule736 _lhsIoptions
         _tlOparams = rule737 _lhsIparams
         _tlOprevVisits = rule738 _lhsIprevVisits
         _tlOruledefs = rule739 _lhsIruledefs
         _tlOruleuses = rule740 _lhsIruleuses
         _tlOsynmap = rule741 _lhsIsynmap
         _tlOterminaldefs = rule742 _lhsIterminaldefs
         __result_ = T_Visits_vOut55 _lhsOallvisits _lhsOerrors _lhsOfromToStates _lhsOintramap _lhsOlazyIntras _lhsOruleKinds _lhsOruleUsage _lhsOusedArgs _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses
         in __result_ )
     in C_Visits_s56 v55
   {-# INLINE rule692 #-}
   rule692 = \ ((_hdIallvisits) ::  VisitStateState ) ((_tlIallvisits) :: [VisitStateState]) ->
     _hdIallvisits : _tlIallvisits
   {-# INLINE rule693 #-}
   rule693 = \ ((_hdIerrors) :: Seq Error) ((_tlIerrors) :: Seq Error) ->
     _hdIerrors Seq.>< _tlIerrors
   {-# INLINE rule694 #-}
   rule694 = \ ((_hdIfromToStates) :: Map VisitIdentifier (Int,Int)) ((_tlIfromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _hdIfromToStates `mappend` _tlIfromToStates
   {-# INLINE rule695 #-}
   rule695 = \ ((_hdIintramap) :: Map StateIdentifier (Map String (Maybe NonLocalAttr))) ((_tlIintramap) :: Map StateIdentifier (Map String (Maybe NonLocalAttr))) ->
     _hdIintramap `uwMapUnion` _tlIintramap
   {-# INLINE rule696 #-}
   rule696 = \ ((_hdIlazyIntras) :: Set String) ((_tlIlazyIntras) :: Set String) ->
     _hdIlazyIntras `Set.union` _tlIlazyIntras
   {-# INLINE rule697 #-}
   rule697 = \ ((_hdIruleKinds) :: Map Identifier (Set VisitKind)) ((_tlIruleKinds) :: Map Identifier (Set VisitKind)) ->
     _hdIruleKinds `unionWithMappend` _tlIruleKinds
   {-# INLINE rule698 #-}
   rule698 = \ ((_hdIruleUsage) :: Map Identifier Int) ((_tlIruleUsage) :: Map Identifier Int) ->
     _hdIruleUsage `unionWithSum` _tlIruleUsage
   {-# INLINE rule699 #-}
   rule699 = \ ((_hdIusedArgs) :: Set String) ((_tlIusedArgs) :: Set String) ->
     _hdIusedArgs `Set.union` _tlIusedArgs
   {-# INLINE rule700 #-}
   rule700 = \ ((_hdIvisitKinds) :: Map VisitIdentifier VisitKind) ((_tlIvisitKinds) :: Map VisitIdentifier VisitKind) ->
     _hdIvisitKinds `mappend` _tlIvisitKinds
   {-# INLINE rule701 #-}
   rule701 = \ ((_hdIvisitdefs) :: Map VisitIdentifier (Set Identifier)) ((_tlIvisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _hdIvisitdefs `uwSetUnion` _tlIvisitdefs
   {-# INLINE rule702 #-}
   rule702 = \ ((_hdIvisituses) :: Map VisitIdentifier (Set Identifier)) ((_tlIvisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _hdIvisituses `uwSetUnion` _tlIvisituses
   {-# INLINE rule703 #-}
   rule703 = \ ((_lhsIallFromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _lhsIallFromToStates
   {-# INLINE rule704 #-}
   rule704 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule705 #-}
   rule705 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule706 #-}
   rule706 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule707 #-}
   rule707 = \ ((_lhsIallVisitKinds) :: Map VisitIdentifier VisitKind) ->
     _lhsIallVisitKinds
   {-# INLINE rule708 #-}
   rule708 = \ ((_lhsIallintramap) :: Map StateIdentifier (Map String (Maybe NonLocalAttr))) ->
     _lhsIallintramap
   {-# INLINE rule709 #-}
   rule709 = \ ((_lhsIavisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisitdefs
   {-# INLINE rule710 #-}
   rule710 = \ ((_lhsIavisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisituses
   {-# INLINE rule711 #-}
   rule711 = \ ((_lhsIchildTypes) :: Map Identifier Type) ->
     _lhsIchildTypes
   {-# INLINE rule712 #-}
   rule712 = \ ((_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# INLINE rule713 #-}
   rule713 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule714 #-}
   rule714 = \ ((_lhsInextVisits) :: Map StateIdentifier StateCtx) ->
     _lhsInextVisits
   {-# INLINE rule715 #-}
   rule715 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule716 #-}
   rule716 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule717 #-}
   rule717 = \ ((_lhsIparams) :: [Identifier]) ->
     _lhsIparams
   {-# INLINE rule718 #-}
   rule718 = \ ((_lhsIprevVisits) :: Map StateIdentifier StateCtx) ->
     _lhsIprevVisits
   {-# INLINE rule719 #-}
   rule719 = \ ((_lhsIruledefs) :: Map Identifier (Set String)) ->
     _lhsIruledefs
   {-# INLINE rule720 #-}
   rule720 = \ ((_lhsIruleuses) :: Map Identifier (Map String (Maybe NonLocalAttr))) ->
     _lhsIruleuses
   {-# INLINE rule721 #-}
   rule721 = \ ((_lhsIsynmap) :: Attributes) ->
     _lhsIsynmap
   {-# INLINE rule722 #-}
   rule722 = \ ((_lhsIterminaldefs) :: Set String) ->
     _lhsIterminaldefs
   {-# INLINE rule723 #-}
   rule723 = \ ((_lhsIallFromToStates) :: Map VisitIdentifier (Int,Int)) ->
     _lhsIallFromToStates
   {-# INLINE rule724 #-}
   rule724 = \ ((_lhsIallInhmap) :: Map NontermIdent Attributes) ->
     _lhsIallInhmap
   {-# INLINE rule725 #-}
   rule725 = \ ((_lhsIallInitStates) :: Map NontermIdent Int) ->
     _lhsIallInitStates
   {-# INLINE rule726 #-}
   rule726 = \ ((_lhsIallSynmap) :: Map NontermIdent Attributes) ->
     _lhsIallSynmap
   {-# INLINE rule727 #-}
   rule727 = \ ((_lhsIallVisitKinds) :: Map VisitIdentifier VisitKind) ->
     _lhsIallVisitKinds
   {-# INLINE rule728 #-}
   rule728 = \ ((_lhsIallintramap) :: Map StateIdentifier (Map String (Maybe NonLocalAttr))) ->
     _lhsIallintramap
   {-# INLINE rule729 #-}
   rule729 = \ ((_lhsIavisitdefs) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisitdefs
   {-# INLINE rule730 #-}
   rule730 = \ ((_lhsIavisituses) :: Map VisitIdentifier (Set Identifier)) ->
     _lhsIavisituses
   {-# INLINE rule731 #-}
   rule731 = \ ((_lhsIchildTypes) :: Map Identifier Type) ->
     _lhsIchildTypes
   {-# INLINE rule732 #-}
   rule732 = \ ((_lhsIcon) :: ConstructorIdent) ->
     _lhsIcon
   {-# INLINE rule733 #-}
   rule733 = \ ((_lhsIinhmap) :: Attributes) ->
     _lhsIinhmap
   {-# INLINE rule734 #-}
   rule734 = \ ((_lhsInextVisits) :: Map StateIdentifier StateCtx) ->
     _lhsInextVisits
   {-# INLINE rule735 #-}
   rule735 = \ ((_lhsInt) :: NontermIdent) ->
     _lhsInt
   {-# INLINE rule736 #-}
   rule736 = \ ((_lhsIoptions) :: Options) ->
     _lhsIoptions
   {-# INLINE rule737 #-}
   rule737 = \ ((_lhsIparams) :: [Identifier]) ->
     _lhsIparams
   {-# INLINE rule738 #-}
   rule738 = \ ((_lhsIprevVisits) :: Map StateIdentifier StateCtx) ->
     _lhsIprevVisits
   {-# INLINE rule739 #-}
   rule739 = \ ((_lhsIruledefs) :: Map Identifier (Set String)) ->
     _lhsIruledefs
   {-# INLINE rule740 #-}
   rule740 = \ ((_lhsIruleuses) :: Map Identifier (Map String (Maybe NonLocalAttr))) ->
     _lhsIruleuses
   {-# INLINE rule741 #-}
   rule741 = \ ((_lhsIsynmap) :: Attributes) ->
     _lhsIsynmap
   {-# INLINE rule742 #-}
   rule742 = \ ((_lhsIterminaldefs) :: Set String) ->
     _lhsIterminaldefs
{-# NOINLINE sem_Visits_Nil #-}
sem_Visits_Nil ::  T_Visits 
sem_Visits_Nil  = T_Visits (return st56) where
   {-# NOINLINE st56 #-}
   st56 = let
      v55 :: T_Visits_v55 
      v55 = \ (T_Visits_vIn55 _lhsIallFromToStates _lhsIallInhmap _lhsIallInitStates _lhsIallSynmap _lhsIallVisitKinds _lhsIallintramap _lhsIavisitdefs _lhsIavisituses _lhsIchildTypes _lhsIcon _lhsIinhmap _lhsInextVisits _lhsInt _lhsIoptions _lhsIparams _lhsIprevVisits _lhsIruledefs _lhsIruleuses _lhsIsynmap _lhsIterminaldefs) -> ( let
         _lhsOallvisits :: [VisitStateState]
         _lhsOallvisits = rule743  ()
         _lhsOerrors :: Seq Error
         _lhsOerrors = rule744  ()
         _lhsOfromToStates :: Map VisitIdentifier (Int,Int)
         _lhsOfromToStates = rule745  ()
         _lhsOintramap :: Map StateIdentifier (Map String (Maybe NonLocalAttr))
         _lhsOintramap = rule746  ()
         _lhsOlazyIntras :: Set String
         _lhsOlazyIntras = rule747  ()
         _lhsOruleKinds :: Map Identifier (Set VisitKind)
         _lhsOruleKinds = rule748  ()
         _lhsOruleUsage :: Map Identifier Int
         _lhsOruleUsage = rule749  ()
         _lhsOusedArgs :: Set String
         _lhsOusedArgs = rule750  ()
         _lhsOvisitKinds :: Map VisitIdentifier VisitKind
         _lhsOvisitKinds = rule751  ()
         _lhsOvisitdefs :: Map VisitIdentifier (Set Identifier)
         _lhsOvisitdefs = rule752  ()
         _lhsOvisituses :: Map VisitIdentifier (Set Identifier)
         _lhsOvisituses = rule753  ()
         __result_ = T_Visits_vOut55 _lhsOallvisits _lhsOerrors _lhsOfromToStates _lhsOintramap _lhsOlazyIntras _lhsOruleKinds _lhsOruleUsage _lhsOusedArgs _lhsOvisitKinds _lhsOvisitdefs _lhsOvisituses
         in __result_ )
     in C_Visits_s56 v55
   {-# INLINE rule743 #-}
   rule743 = \  (_ :: ()) ->
     []
   {-# INLINE rule744 #-}
   rule744 = \  (_ :: ()) ->
     Seq.empty
   {-# INLINE rule745 #-}
   rule745 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule746 #-}
   rule746 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule747 #-}
   rule747 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule748 #-}
   rule748 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule749 #-}
   rule749 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule750 #-}
   rule750 = \  (_ :: ()) ->
     Set.empty
   {-# INLINE rule751 #-}
   rule751 = \  (_ :: ()) ->
     mempty
   {-# INLINE rule752 #-}
   rule752 = \  (_ :: ()) ->
     Map.empty
   {-# INLINE rule753 #-}
   rule753 = \  (_ :: ()) ->
     Map.empty