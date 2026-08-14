-- MAVRYK: PascaLIGO. Unverified (no GHC locally) — mirrors CameLigoCST.hs against the real
-- dump-cst schema (`ligo info dump-cst x.ligo --format json`); build-and-fix in a GHC env.
--
-- The CST types below mirror `src/stages/1-cst/pascaligo/CST.ml`; the MessagePack instances
-- decode the msgpack the compiler emits from that CST's `[@@deriving yojson_of]` (tags like
-- `E_Add`, records keyed by OCaml field name, wrapped tokens as {payload,region}, `reg` as
-- {region,value}, and identifiers as a `Var`/`Esc` variant). `toAST` maps into the shared
-- Skeleton AST exactly as CameLigoCST.toAST does. PascaLIGO is statement/instruction/block
-- based (unlike CameLIGO), so blocks map to `AST.Seq` and instructions to their expression
-- analogues. Field names were taken from CST.ml and cross-checked against a real dump for the
-- sampled constructs; a first GHC build will surface any remaining mismatches to fix.
module Language.LIGO.AST.Parser.PascaLigoCST
  ( CST
  , toAST
  ) where

import Control.MessagePack
  (asumMsg, guardMsg, withMsgMap, withMsgVariant, (.:), (.:?))
import Data.Default (def)
import Data.List.NonEmpty ((<|))
import Data.List.NonEmpty qualified as NE -- MAVRYK: PascaLIGO
import Data.MessagePack (MessagePack)
import Data.MessagePack.Types (fromObjectWith)

import Duplo (Comonad (extract), fastMake)

import Language.LIGO.AST.Parser.Common
import Language.LIGO.AST.Skeleton (Info, LIGO)
import Language.LIGO.AST.Skeleton qualified as AST
import Language.LIGO.Range

-- MAVRYK: PascaLIGO. The frontend restores [nseq] as the local tuple ('a * 'a list)
-- (see src/stages/1-cst/pascaligo/CST.ml); its yojson serializes as a single-element
-- array wrapping the flattened sequence — [[e0,e1,…]] — unlike CameLIGO's [Ne_list],
-- which is the flat [e0,e1,…]. These peel that extra layer off an nseq-encoded field.
unNseq :: [[a]] -> [a] -- MAVRYK: PascaLIGO
unNseq = concat

-- MAVRYK: PascaLIGO. An [nseq] field serializes in one of two shapes depending on position:
-- the ppx tuple [hd, [tl…]] (e.g. module_body.declarations, map_lookup.keys) or a single-wrapped
-- flat list [[e0,e1,…]] (e.g. the top-level t.decl). [NseqList] decodes either into a plain list.
newtype NseqList a = NseqList { unNseqList :: [a] }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

instance MessagePack a => MessagePack (NseqList a) where
  fromObjectWith cfg obj = NseqList <$> asumMsg
    [ (\(hd, tl) -> hd : tl) <$> fromObjectWith cfg obj -- ppx tuple [hd, [tl]]
    , unNseq <$> fromObjectWith cfg obj                 -- single-wrapped flat [[…]]
    ]

-- MAVRYK: PascaLIGO. [parameters]/[call_args] are wrapped in [par reg] ((…)); peel the
-- [Reg]+[Par'] layers to reach the inner list the decoder wants.
unPar :: Par a -> a -- MAVRYK: PascaLIGO
unPar = pInside . rValue

-----------
-- Types --
-----------

-- | An identifier: @variable@/@type_name@/@module_name@ serialize as a @Var@|@Esc@ variant.
newtype Variable = Variable { unVariable :: WrappedLexeme }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-- | A CST of a @PascaLIGO@ contract file.
data CST = CST
  { cstDecl :: [Declaration]
  , cstEof :: WrappedLexeme
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-- | Top-level (and module-level) declaration.
data Declaration
  = DAttr (Reg (Tuple1 Declaration))
  | DConst (Reg ConstDecl)
  | DFun (Reg FunDecl)
  | DModule (Reg ModuleDecl)
  | DSignature (Reg SignatureDecl)
  | DType (Reg TypeDecl)
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-- | @const x : t = e@ / @const x = e@.
data ConstDecl = ConstDecl
  { cdPattern :: Pattern
  , cdConstType :: Maybe TypeAnnotation
  , cdInit :: Expr
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-- | @function f (params) : t is e@.
data FunDecl = FunDecl
  { fdFunName :: Variable
  , fdParameters :: [ParamDecl]
  , fdRetType :: Maybe TypeAnnotation
  , fdReturn :: Expr
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-- | One @const/var name : t@ parameter.
data ParamDecl = ParamDecl
  { pdPattern :: Pattern
  , pdParamType :: Maybe TypeAnnotation
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-- | @type t is …@.
data TypeDecl = TypeDecl
  { tdName :: Variable
  , tdParams :: Maybe (Par (Tuple Variable))
  , tdTypeExpr :: TypeExpr
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-- | @module M is …@ / @module M : S is …@.
data ModuleDecl = ModuleDecl
  { mdName :: WrappedLexeme -- MAVRYK: PascaLIGO. [module_name = lexeme wrap], not a Var/Esc variable.
  , mdAnnotation :: Maybe (Tuple1 SignatureExpr)
  , mdModuleExpr :: ModuleExpr
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data ModuleExpr
  = MBody (Reg ModuleBody)
  | MPath (Reg (ModulePath Variable))
  | MVar Variable
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

newtype ModuleBody = ModuleBody { mbDeclarations :: [Declaration] }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-- | @module type N is sig … end@.
data SignatureDecl = SignatureDecl
  { sdName :: Variable
  , sdSignatureExpr :: SignatureExpr
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data SignatureExpr
  = SSig (Reg SignatureBody)
  | SPath (Reg (ModulePath Variable))
  | SVar Variable
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

newtype SignatureBody = SignatureBody { sbSigItems :: [SigItem] }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data SigItem
  = SigAttr (Reg (Tuple1 SigItem))
  | SigInclude (Reg (Tuple1 SignatureExpr))
  | SType (Reg SigType)
  | SValue (Reg SigValue)
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data SigType = SigType
  { stTypeName :: Variable
  , stTypeRhs :: Maybe (Tuple1 TypeExpr)
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data SigValue = SigValue
  { svVar :: Variable
  , svValType :: TypeExpr
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-- | A module path with accessor (@M.N.field@).
data ModulePath a = ModulePath
  { mpModulePath :: [Variable]
  , mpField :: a
  }
  deriving stock (Show, Generic, Functor)
  deriving anyclass (NFData)

-- | A value in parentheses/brackets/braces.
newtype Par' a = Par' { pInside :: a }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

type Par a = Reg (Par' a)

type Tuple a = NonEmpty a

type TypeAnnotation = Tuple1 TypeExpr

type SomeBinOp = Reg (BinOp WrappedLexeme)
type SomeUnOp = Reg (UnOp WrappedLexeme)

-- | A @compound@ construct (@list [..]@, @set [..]@, @record [..]@, @map [..]@).
newtype Compound a = Compound { cElements :: [a] }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-------------------
-- Type expressions
-------------------

data TypeExpr
  = TApp (Reg (TypeExpr, Par (Tuple TypeExpr)))
  | TAttr (Tuple1 TypeExpr)
  | TCart (Reg (TypeExpr, NonEmpty TypeExpr))
  | TFun (Reg (TypeExpr, TypeExpr))
  | TInt WrappedTupleLexeme
  | TModPath (Reg (ModulePath TypeExpr))
  | TPar (Par TypeExpr)
  | TParameterOf (Reg (NonEmpty Variable))
  | TRecord (Reg (Compound (Reg FieldDecl)))
  | TString WrappedLexeme
  | TSum (Reg SumType)
  -- MAVRYK: PascaLIGO. Anonymous union type "t1 | t2 | ..." (mirrors [T_Union of union_type reg];
  -- union_type = (type_expr, vbar) nsepseq, and vbar is opaque, so it decodes as [NonEmpty TypeExpr]
  -- exactly like [TParameterOf]).
  | TUnion (Reg (NonEmpty TypeExpr))
  | TVar Variable
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data FieldDecl = FieldDecl
  { fdFieldName :: Variable -- MAVRYK: PascaLIGO. [field_name = variable] (Var/Esc), not a plain wrap.
  , fdFieldType :: Maybe TypeAnnotation
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

newtype SumType = SumType { vtVariants :: NonEmpty (Reg Variant) }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data Variant = Variant
  { vCtor :: WrappedLexeme
  , vCtorArgs :: Maybe (Tuple1 TypeExpr)
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

----------
-- Patterns
----------

data Pattern
  = PApp (Reg (Pattern, Maybe (Par (Tuple Pattern))))
  | PAttr (Tuple1 Pattern)
  | PBytes WrappedTupleLexeme
  | PCons (Reg (Pattern, Pattern))
  | PCtor WrappedLexeme
  | PInt WrappedTupleLexeme
  | PList (Reg (Compound Pattern))
  | PModPath (Reg (ModulePath Pattern))
  | PMumav WrappedTupleLexeme
  | PMav WrappedTupleLexeme
  | PNat WrappedTupleLexeme
  | PNil WrappedLexeme
  | PPar (Par Pattern)
  | PRecord (Reg (Compound (Reg (Field Pattern Pattern)))) -- MAVRYK: PascaLIGO. field_lhs is a pattern (P_Var name), not a lexeme.
  | PString WrappedLexeme
  | PTuple (Par (Tuple Pattern))
  | PTyped (Reg (Pattern, TypeAnnotation))
  | PVar Variable
  | PVerbatim WrappedLexeme
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-- | A record field (shared by patterns, records, updates).
data Field lhs rhs
  = Punned lhs -- MAVRYK: PascaLIGO. {attributes, pun} — the pun lhs directly, not [Reg (Tuple1 …)].
  | Complete (FullField lhs rhs) -- MAVRYK: PascaLIGO. {attributes, field_lhs, field_lens, field_rhs} — not [Reg]-wrapped.
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data FullField lhs rhs = FullField
  { ffFieldLhs :: lhs
  , ffFieldRhs :: rhs
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-------------
-- Expressions
-------------

data Expr
  = EAdd SomeBinOp
  | EAnd SomeBinOp
  | EApp Call
  | EAttr (Tuple1 Expr)
  | EBigMap (Reg (Compound (Reg Binding)))
  | EBlock (Reg BlockWith)
  | EBytes WrappedTupleLexeme
  | ECase (Reg (Case Expr))
  | ECat SomeBinOp
  | ECodeInj (Reg CodeInj)
  | ECond (Reg (Conditional Expr))
  | ECons SomeBinOp
  | EContractOf (Reg (NonEmpty Variable))
  | ECtor WrappedLexeme
  | EDiv SomeBinOp
  | EEqual SomeBinOp
  | EFun (Reg FunExpr)
  | EGeq SomeBinOp
  | EGt SomeBinOp
  | EInt WrappedTupleLexeme
  | ELeq SomeBinOp
  | EList (Reg (Compound Expr))
  | ELt SomeBinOp
  | EMap (Reg (Compound (Reg Binding)))
  | EMapLookup (Reg MapLookup)
  | EMod SomeBinOp
  | EModPath (Reg (ModulePath Expr))
  | EMult SomeBinOp
  | EMumav WrappedTupleLexeme
  | EMav WrappedTupleLexeme
  | ENat WrappedTupleLexeme
  | ENeg SomeUnOp
  | ENeq SomeBinOp
  | ENil WrappedLexeme
  | ENot SomeUnOp
  | EOr SomeBinOp
  | EPar (Par Expr)
  | EProj (Reg Projection)
  | ERecord (Reg (Compound (Reg (Field Expr Expr)))) -- MAVRYK: PascaLIGO. field_lhs is an expr (E_Var name), not a lexeme.
  | ESet (Reg (Compound Expr))
  | ESetMem (Reg SetMembership)
  | EString WrappedLexeme
  | ESub SomeBinOp
  | ETuple (Par (Tuple Expr))
  | ETyped (Par (Expr, TypeAnnotation))
  | EUpdate (Reg Update)
  | EVar Variable
  | EVerbatim WrappedLexeme
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-- | @f (a, b)@ — @call@ is @(expr * call_args) reg@; call_args is a par-wrapped list.
type Call = Reg (Expr, Par [Expr])

data BinOp a = BinOp { boArg1 :: Expr, boOp :: a, boArg2 :: Expr }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data UnOp a = UnOp { uoOp :: a, uoArg :: Expr }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data Binding = Binding { bKey :: Expr, bValue :: Expr }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-- | @block { … } with e@.
data BlockWith = BlockWith { bwBlock :: Reg Block, bwExpr :: Expr }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data CodeInj = CodeInj { ciLanguage :: Wrap (Reg Text), ciCode :: Expr }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data FunExpr = FunExpr
  { feParameters :: [ParamDecl]
  , feRetType :: Maybe TypeAnnotation
  , feReturn :: Expr
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data MapLookup = MapLookup { mlMap :: Expr, mlKeys :: NonEmpty (Par Expr) }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data Projection = Projection
  { pRecordOrTuple :: Expr
  , pFieldPath :: NonEmpty Selection
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data Selection
  = FieldName Variable -- MAVRYK: PascaLIGO. Projection field is [variable] (Var/Esc), not a plain wrap.
  | Component WrappedTupleLexeme
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data SetMembership = SetMembership { smSet :: Expr, smElement :: Expr }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data Update = Update { uStructure :: Expr, uUpdate :: Expr }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

--------------------------
-- Statements & instructions
--------------------------

-- | A block: @{ s1; s2; … }@ / @begin … end@.
newtype Block = Block { blkStatements :: [Statement] }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data Statement
  = SAttr (Tuple1 Statement)
  | SDecl Declaration
  | SInstr Instruction
  | SVarDecl (Reg VarDecl)
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data VarDecl = VarDecl
  { vdPattern :: Pattern
  , vdVarType :: Maybe TypeAnnotation
  , vdInit :: Expr
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data Instruction
  = IAssign (Reg Assignment)
  | ICall Call
  | ICase (Reg (Case TestClause))
  | ICond (Reg (Conditional TestClause))
  | IFor (Reg ForInt)
  | IForIn ForIn
  | IPatch (Reg Patch)
  | IRemove (Reg Removal)
  | ISkip WrappedLexeme
  | IWhile (Reg WhileLoop)
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data Assignment = Assignment { asLhs :: Expr, asRhs :: Expr }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-- | A @case@ scrutinee + clauses (branch type @Expr@ for exprs, @TestClause@ for instrs).
data Case branch = Case
  { caseExpr :: Expr
  , caseCases :: NonEmpty (Reg (CaseClause branch))
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data CaseClause branch = CaseClause { ccPattern :: Pattern, ccRhs :: branch }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data TestClause
  = ClauseInstr Instruction
  | ClauseBlock (Reg Block)
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data Conditional branch = Conditional
  { cndTest :: Expr
  , cndIfSo :: branch
  , cndIfNot :: Maybe (Tuple1 branch)
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data ForInt = ForInt
  { fiIndex :: Variable
  , fiInit :: Expr
  , fiBound :: Expr
  , fiStep :: Maybe (Tuple1 Expr)
  , fiBlock :: Reg Block
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data ForIn
  = ForMap (Reg ForMap')
  | ForSetOrList (Reg ForSetOrList')
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data ForMap' = ForMap'
  { fmBinding :: (Variable, Variable)
  , fmCollection :: Expr
  , fmBlock :: Reg Block
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data ForSetOrList' = ForSetOrList'
  { fsVar :: Variable
  , fsCollection :: Expr
  , fsBlock :: Reg Block
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data Patch = Patch { pchCollection :: Expr, pchPatch :: Expr }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data Removal = Removal { rmItem :: Expr, rmCollection :: Expr }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data WhileLoop = WhileLoop { wlCond :: Expr, wlBlock :: Reg Block }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-----------------
-- MessagePack --
-----------------

instance MessagePack Variable where
  -- MAVRYK: PascaLIGO. [variable] is the Var/Esc variant (["Var"|"Esc", wrap]), but several
  -- name positions are plain [lexeme wrap]s instead (module_name, ctor, …). Accept both: the
  -- variant form first, then a bare wrap fallback, so every name decodes to a Variable uniformly.
  fromObjectWith cfg obj = asumMsg
    [ withMsgVariant "Variable" (\(name, arg) -> asumMsg
        [ Variable <$> (guardMsg (name == "Var") >> fromObjectWith cfg arg)
        , Variable <$> (guardMsg (name == "Esc") >> fromObjectWith cfg arg)
        ]) obj
    , Variable <$> fromObjectWith cfg obj
    ]

instance MessagePack CST where
  fromObjectWith _ = withMsgMap "CST" \o -> do
    cstDecl <- unNseqList <$> (o .: "decl") -- MAVRYK: PascaLIGO (nseq, either shape)
    cstEof <- o .: "eof"
    pure CST{..}

instance MessagePack Declaration where
  fromObjectWith cfg = withMsgVariant "Declaration" \(name, arg) -> asumMsg
    [ DAttr      <$> (guardMsg (name == "D_Attr"     ) >> fromObjectWith cfg arg)
    , DConst     <$> (guardMsg (name == "D_Const"    ) >> fromObjectWith cfg arg)
    , DFun       <$> (guardMsg (name == "D_Fun"      ) >> fromObjectWith cfg arg)
    , DModule    <$> (guardMsg (name == "D_Module"   ) >> fromObjectWith cfg arg)
    , DSignature <$> (guardMsg (name == "D_Signature") >> fromObjectWith cfg arg)
    , DType      <$> (guardMsg (name == "D_Type"     ) >> fromObjectWith cfg arg)
    ]

instance MessagePack ConstDecl where
  fromObjectWith _ = withMsgMap "ConstDecl" \o -> do
    cdPattern <- o .: "pattern"
    cdConstType <- o .:? "const_type"
    cdInit <- o .: "init"
    pure ConstDecl{..}

instance MessagePack FunDecl where
  fromObjectWith _ = withMsgMap "FunDecl" \o -> do
    fdFunName <- o .: "fun_name"
    fdParameters <- (map rValue . unPar) <$> (o .: "parameters") -- MAVRYK: PascaLIGO ([par reg] of [param_decl reg])
    fdRetType <- o .:? "ret_type"
    fdReturn <- o .: "return"
    pure FunDecl{..}

instance MessagePack ParamDecl where
  fromObjectWith _ = withMsgMap "ParamDecl" \o -> do
    pdPattern <- o .: "pattern"
    pdParamType <- o .:? "param_type"
    pure ParamDecl{..}

instance MessagePack TypeDecl where
  fromObjectWith _ = withMsgMap "TypeDecl" \o -> do
    tdName <- o .: "name"
    tdParams <- o .:? "params"
    tdTypeExpr <- o .: "type_expr"
    pure TypeDecl{..}

instance MessagePack ModuleDecl where
  fromObjectWith _ = withMsgMap "ModuleDecl" \o -> do
    mdName <- o .: "name"
    mdAnnotation <- o .:? "annotation"
    mdModuleExpr <- o .: "module_expr"
    pure ModuleDecl{..}

instance MessagePack ModuleExpr where
  fromObjectWith cfg = withMsgVariant "ModuleExpr" \(name, arg) -> asumMsg
    [ MBody <$> (guardMsg (name == "M_Body") >> fromObjectWith cfg arg)
    , MPath <$> (guardMsg (name == "M_Path") >> fromObjectWith cfg arg)
    , MVar  <$> (guardMsg (name == "M_Var" ) >> fromObjectWith cfg arg)
    ]

instance MessagePack ModuleBody where
  fromObjectWith _ = withMsgMap "ModuleBody" \o -> do
    mbDeclarations <- unNseqList <$> (o .: "declarations") -- MAVRYK: PascaLIGO (nseq, either shape)
    pure ModuleBody{..}

instance MessagePack SignatureDecl where
  fromObjectWith _ = withMsgMap "SignatureDecl" \o -> do
    sdName <- o .: "name"
    sdSignatureExpr <- o .: "signature_expr"
    pure SignatureDecl{..}

instance MessagePack SignatureExpr where
  fromObjectWith cfg = withMsgVariant "SignatureExpr" \(name, arg) -> asumMsg
    [ SSig  <$> (guardMsg (name == "S_Sig" ) >> fromObjectWith cfg arg)
    , SPath <$> (guardMsg (name == "S_Path") >> fromObjectWith cfg arg)
    , SVar  <$> (guardMsg (name == "S_Var" ) >> fromObjectWith cfg arg)
    ]

instance MessagePack SignatureBody where
  fromObjectWith _ = withMsgMap "SignatureBody" \o -> do
    sbSigItems <- o .: "sig_items"
    pure SignatureBody{..}

instance MessagePack SigItem where
  fromObjectWith cfg = withMsgVariant "SigItem" \(name, arg) -> asumMsg
    [ SigAttr    <$> (guardMsg (name == "Sig_Attr"   ) >> fromObjectWith cfg arg)
    , SigInclude <$> (guardMsg (name == "Sig_Include") >> fromObjectWith cfg arg)
    , SType    <$> (guardMsg (name == "Sig_Type"   ) >> fromObjectWith cfg arg)
    , SValue   <$> (guardMsg (name == "Sig_Value"  ) >> fromObjectWith cfg arg)
    ]

instance MessagePack SigType where
  fromObjectWith _ = withMsgMap "SigType" \o -> do
    stTypeName <- o .: "name"
    stTypeRhs <- o .:? "type_rhs"
    pure SigType{..}

instance MessagePack SigValue where
  fromObjectWith _ = withMsgMap "SigValue" \o -> do
    svVar <- o .: "var"
    svValType <- o .: "val_type"
    pure SigValue{..}

instance MessagePack a => MessagePack (ModulePath a) where
  fromObjectWith _ = withMsgMap "ModulePath" \o -> do
    mpModulePath <- o .: "module_path"
    mpField <- o .: "field"
    pure ModulePath{..}

instance MessagePack a => MessagePack (Par' a) where
  fromObjectWith _ = withMsgMap "Par" \o -> do
    pInside <- o .: "inside"
    pure Par'{..}

instance MessagePack a => MessagePack (Compound a) where
  fromObjectWith _ = withMsgMap "Compound" \o -> do
    cElements <- o .: "elements"
    pure Compound{..}

instance MessagePack TypeExpr where
  fromObjectWith cfg = withMsgVariant "TypeExpr" \(name, arg) -> asumMsg
    [ TApp         <$> (guardMsg (name == "T_App"        ) >> fromObjectWith cfg arg)
    , TAttr        <$> (guardMsg (name == "T_Attr"       ) >> fromObjectWith cfg arg)
    , TCart        <$> (guardMsg (name == "T_Cart"       ) >> fromObjectWith cfg arg)
    , TFun         <$> (guardMsg (name == "T_Fun"        ) >> fromObjectWith cfg arg)
    , TInt         <$> (guardMsg (name == "T_Int"        ) >> fromObjectWith cfg arg)
    , TModPath     <$> (guardMsg (name == "T_ModPath"    ) >> fromObjectWith cfg arg)
    , TPar         <$> (guardMsg (name == "T_Par"        ) >> fromObjectWith cfg arg)
    , TParameterOf <$> (guardMsg (name == "T_ParameterOf") >> fromObjectWith cfg arg)
    , TRecord      <$> (guardMsg (name == "T_Record"     ) >> fromObjectWith cfg arg)
    , TString      <$> (guardMsg (name == "T_String"     ) >> fromObjectWith cfg arg)
    , TSum         <$> (guardMsg (name == "T_Sum"        ) >> fromObjectWith cfg arg)
    , TUnion       <$> (guardMsg (name == "T_Union"      ) >> fromObjectWith cfg arg)
    , TVar         <$> (guardMsg (name == "T_Var"        ) >> fromObjectWith cfg arg)
    ]

instance MessagePack FieldDecl where
  fromObjectWith _ = withMsgMap "FieldDecl" \o -> do
    fdFieldName <- o .: "field_name"
    fdFieldType <- o .:? "field_type"
    pure FieldDecl{..}

instance MessagePack SumType where
  fromObjectWith _ = withMsgMap "SumType" \o -> do
    vtVariants <- o .: "variants"
    pure SumType{..}

instance MessagePack Variant where
  fromObjectWith _ = withMsgMap "Variant" \o -> do
    vCtor <- o .: "ctor"
    vCtorArgs <- o .:? "ctor_args"
    pure Variant{..}

instance MessagePack Pattern where
  fromObjectWith cfg = withMsgVariant "Pattern" \(name, arg) -> asumMsg
    [ PApp      <$> (guardMsg (name == "P_App"     ) >> fromObjectWith cfg arg)
    , PAttr     <$> (guardMsg (name == "P_Attr"    ) >> fromObjectWith cfg arg)
    , PBytes    <$> (guardMsg (name == "P_Bytes"   ) >> fromObjectWith cfg arg)
    , PCons     <$> (guardMsg (name == "P_Cons"    ) >> fromObjectWith cfg arg)
    , PCtor     <$> (guardMsg (name == "P_Ctor"    ) >> fromObjectWith cfg arg)
    , PInt      <$> (guardMsg (name == "P_Int"     ) >> fromObjectWith cfg arg)
    , PList     <$> (guardMsg (name == "P_List"    ) >> fromObjectWith cfg arg)
    , PModPath  <$> (guardMsg (name == "P_ModPath" ) >> fromObjectWith cfg arg)
    , PMumav    <$> (guardMsg (name == "P_Mumav"   ) >> fromObjectWith cfg arg)
    , PMav      <$> (guardMsg (name == "P_Mav"     ) >> fromObjectWith cfg arg)
    , PNat      <$> (guardMsg (name == "P_Nat"     ) >> fromObjectWith cfg arg)
    , PNil      <$> (guardMsg (name == "P_Nil"     ) >> fromObjectWith cfg arg)
    , PPar      <$> (guardMsg (name == "P_Par"     ) >> fromObjectWith cfg arg)
    , PRecord   <$> (guardMsg (name == "P_Record"  ) >> fromObjectWith cfg arg)
    , PString   <$> (guardMsg (name == "P_String"  ) >> fromObjectWith cfg arg)
    , PTuple    <$> (guardMsg (name == "P_Tuple"   ) >> fromObjectWith cfg arg)
    , PTyped    <$> (guardMsg (name == "P_Typed"   ) >> fromObjectWith cfg arg)
    , PVar      <$> (guardMsg (name == "P_Var"     ) >> fromObjectWith cfg arg)
    , PVerbatim <$> (guardMsg (name == "P_Verbatim") >> fromObjectWith cfg arg)
    ]

instance (MessagePack lhs, MessagePack rhs) => MessagePack (Field lhs rhs) where
  fromObjectWith cfg = withMsgVariant "Field" \(name, arg) -> asumMsg
    [ Punned   <$> (guardMsg (name == "Punned"  ) >> withMsgMap "Punned" (\o -> o .: "pun") arg) -- MAVRYK: PascaLIGO ({attributes, pun})
    , Complete <$> (guardMsg (name == "Complete") >> fromObjectWith cfg arg)
    ]

instance (MessagePack lhs, MessagePack rhs) => MessagePack (FullField lhs rhs) where
  fromObjectWith _ = withMsgMap "FullField" \o -> do
    ffFieldLhs <- o .: "field_lhs"
    ffFieldRhs <- o .: "field_rhs"
    pure FullField{..}

instance MessagePack Expr where
  fromObjectWith cfg = withMsgVariant "Expr" \(name, arg) -> asumMsg
    [ EAdd        <$> (guardMsg (name == "E_Add"       ) >> fromObjectWith cfg arg)
    , EAnd        <$> (guardMsg (name == "E_And"       ) >> fromObjectWith cfg arg)
    , EApp        <$> (guardMsg (name == "E_App"       ) >> fromObjectWith cfg arg)
    , EAttr       <$> (guardMsg (name == "E_Attr"      ) >> fromObjectWith cfg arg)
    , EBigMap     <$> (guardMsg (name == "E_BigMap"    ) >> fromObjectWith cfg arg)
    , EBlock      <$> (guardMsg (name == "E_Block"     ) >> fromObjectWith cfg arg)
    , EBytes      <$> (guardMsg (name == "E_Bytes"     ) >> fromObjectWith cfg arg)
    , ECase       <$> (guardMsg (name == "E_Case"      ) >> fromObjectWith cfg arg)
    , ECat        <$> (guardMsg (name == "E_Cat"       ) >> fromObjectWith cfg arg)
    , ECodeInj    <$> (guardMsg (name == "E_CodeInj"   ) >> fromObjectWith cfg arg)
    , ECond       <$> (guardMsg (name == "E_Cond"      ) >> fromObjectWith cfg arg)
    , ECons       <$> (guardMsg (name == "E_Cons"      ) >> fromObjectWith cfg arg)
    , EContractOf <$> (guardMsg (name == "E_ContractOf") >> fromObjectWith cfg arg)
    , ECtor       <$> (guardMsg (name == "E_Ctor"      ) >> fromObjectWith cfg arg)
    , EDiv        <$> (guardMsg (name == "E_Div"       ) >> fromObjectWith cfg arg)
    , EEqual      <$> (guardMsg (name == "E_Equal"     ) >> fromObjectWith cfg arg)
    , EFun        <$> (guardMsg (name == "E_Fun"       ) >> fromObjectWith cfg arg)
    , EGeq        <$> (guardMsg (name == "E_Geq"       ) >> fromObjectWith cfg arg)
    , EGt         <$> (guardMsg (name == "E_Gt"        ) >> fromObjectWith cfg arg)
    , EInt        <$> (guardMsg (name == "E_Int"       ) >> fromObjectWith cfg arg)
    , ELeq        <$> (guardMsg (name == "E_Leq"       ) >> fromObjectWith cfg arg)
    , EList       <$> (guardMsg (name == "E_List"      ) >> fromObjectWith cfg arg)
    , ELt         <$> (guardMsg (name == "E_Lt"        ) >> fromObjectWith cfg arg)
    , EMap        <$> (guardMsg (name == "E_Map"       ) >> fromObjectWith cfg arg)
    , EMapLookup  <$> (guardMsg (name == "E_MapLookup" ) >> fromObjectWith cfg arg)
    , EMod        <$> (guardMsg (name == "E_Mod"       ) >> fromObjectWith cfg arg)
    , EModPath    <$> (guardMsg (name == "E_ModPath"   ) >> fromObjectWith cfg arg)
    , EMult       <$> (guardMsg (name == "E_Mult"      ) >> fromObjectWith cfg arg)
    , EMumav      <$> (guardMsg (name == "E_Mumav"     ) >> fromObjectWith cfg arg)
    , EMav        <$> (guardMsg (name == "E_Mav"       ) >> fromObjectWith cfg arg)
    , ENat        <$> (guardMsg (name == "E_Nat"       ) >> fromObjectWith cfg arg)
    , ENeg        <$> (guardMsg (name == "E_Neg"       ) >> fromObjectWith cfg arg)
    , ENeq        <$> (guardMsg (name == "E_Neq"       ) >> fromObjectWith cfg arg)
    , ENil        <$> (guardMsg (name == "E_Nil"       ) >> fromObjectWith cfg arg)
    , ENot        <$> (guardMsg (name == "E_Not"       ) >> fromObjectWith cfg arg)
    , EOr         <$> (guardMsg (name == "E_Or"        ) >> fromObjectWith cfg arg)
    , EPar        <$> (guardMsg (name == "E_Par"       ) >> fromObjectWith cfg arg)
    , EProj       <$> (guardMsg (name == "E_Proj"      ) >> fromObjectWith cfg arg)
    , ERecord     <$> (guardMsg (name == "E_Record"    ) >> fromObjectWith cfg arg)
    , ESet        <$> (guardMsg (name == "E_Set"       ) >> fromObjectWith cfg arg)
    , ESetMem     <$> (guardMsg (name == "E_SetMem"    ) >> fromObjectWith cfg arg)
    , EString     <$> (guardMsg (name == "E_String"    ) >> fromObjectWith cfg arg)
    , ESub        <$> (guardMsg (name == "E_Sub"       ) >> fromObjectWith cfg arg)
    , ETuple      <$> (guardMsg (name == "E_Tuple"     ) >> fromObjectWith cfg arg)
    , ETyped      <$> (guardMsg (name == "E_Typed"     ) >> fromObjectWith cfg arg)
    , EUpdate     <$> (guardMsg (name == "E_Update"    ) >> fromObjectWith cfg arg)
    , EVar        <$> (guardMsg (name == "E_Var"       ) >> fromObjectWith cfg arg)
    , EVerbatim   <$> (guardMsg (name == "E_Verbatim"  ) >> fromObjectWith cfg arg)
    ]

instance MessagePack a => MessagePack (BinOp a) where
  fromObjectWith _ = withMsgMap "BinOp" \o -> do
    boArg1 <- o .: "arg1"
    boOp <- o .: "op"
    boArg2 <- o .: "arg2"
    pure BinOp{..}

instance MessagePack a => MessagePack (UnOp a) where
  fromObjectWith _ = withMsgMap "UnOp" \o -> do
    uoOp <- o .: "op"
    uoArg <- o .: "arg"
    pure UnOp{..}

instance MessagePack Binding where
  fromObjectWith _ = withMsgMap "Binding" \o -> do
    bKey <- o .: "key"
    bValue <- o .: "value"
    pure Binding{..}

instance MessagePack BlockWith where
  fromObjectWith _ = withMsgMap "BlockWith" \o -> do
    bwBlock <- o .: "block"
    bwExpr <- o .: "expr"
    pure BlockWith{..}

instance MessagePack CodeInj where
  fromObjectWith _ = withMsgMap "CodeInj" \o -> do
    ciLanguage <- o .: "language"
    ciCode <- o .: "code"
    pure CodeInj{..}

instance MessagePack FunExpr where
  fromObjectWith _ = withMsgMap "FunExpr" \o -> do
    feParameters <- (map rValue . unPar) <$> (o .: "parameters") -- MAVRYK: PascaLIGO ([par reg] of [param_decl reg])
    feRetType <- o .:? "ret_type"
    feReturn <- o .: "return"
    pure FunExpr{..}

instance MessagePack MapLookup where
  fromObjectWith _ = withMsgMap "MapLookup" \o -> do
    mlMap <- o .: "map"
    mlKeys <- (NE.fromList . unNseqList) <$> (o .: "keys") -- MAVRYK: PascaLIGO (nseq, either shape)
    pure MapLookup{..}

instance MessagePack Projection where
  fromObjectWith _ = withMsgMap "Projection" \o -> do
    pRecordOrTuple <- o .: "record_or_tuple"
    pFieldPath <- o .: "field_path"
    pure Projection{..}

instance MessagePack Selection where
  fromObjectWith cfg = withMsgVariant "Selection" \(name, arg) -> asumMsg
    [ FieldName <$> (guardMsg (name == "FieldName") >> fromObjectWith cfg arg)
    , Component <$> (guardMsg (name == "Component") >> fromObjectWith cfg arg)
    ]

instance MessagePack SetMembership where
  fromObjectWith _ = withMsgMap "SetMembership" \o -> do
    smSet <- o .: "set"
    smElement <- o .: "element"
    pure SetMembership{..}

instance MessagePack Update where
  fromObjectWith _ = withMsgMap "Update" \o -> do
    uStructure <- o .: "structure"
    uUpdate <- o .: "update"
    pure Update{..}

instance MessagePack Block where
  fromObjectWith _ = withMsgMap "Block" \o -> do
    blkStatements <- o .: "statements"
    pure Block{..}

instance MessagePack Statement where
  fromObjectWith cfg = withMsgVariant "Statement" \(name, arg) -> asumMsg
    [ SAttr    <$> (guardMsg (name == "S_Attr"   ) >> fromObjectWith cfg arg)
    , SDecl    <$> (guardMsg (name == "S_Decl"   ) >> fromObjectWith cfg arg)
    , SInstr   <$> (guardMsg (name == "S_Instr"  ) >> fromObjectWith cfg arg)
    , SVarDecl <$> (guardMsg (name == "S_VarDecl") >> fromObjectWith cfg arg)
    ]

instance MessagePack VarDecl where
  fromObjectWith _ = withMsgMap "VarDecl" \o -> do
    vdPattern <- o .: "pattern"
    vdVarType <- o .:? "var_type"
    vdInit <- o .: "init"
    pure VarDecl{..}

instance MessagePack Instruction where
  fromObjectWith cfg = withMsgVariant "Instruction" \(name, arg) -> asumMsg
    [ IAssign <$> (guardMsg (name == "I_Assign") >> fromObjectWith cfg arg)
    , ICall   <$> (guardMsg (name == "I_Call"  ) >> fromObjectWith cfg arg)
    , ICase   <$> (guardMsg (name == "I_Case"  ) >> fromObjectWith cfg arg)
    , ICond   <$> (guardMsg (name == "I_Cond"  ) >> fromObjectWith cfg arg)
    , IFor    <$> (guardMsg (name == "I_For"   ) >> fromObjectWith cfg arg)
    , IForIn  <$> (guardMsg (name == "I_ForIn" ) >> fromObjectWith cfg arg)
    , IPatch  <$> (guardMsg (name == "I_Patch" ) >> fromObjectWith cfg arg)
    , IRemove <$> (guardMsg (name == "I_Remove") >> fromObjectWith cfg arg)
    , ISkip   <$> (guardMsg (name == "I_Skip"  ) >> fromObjectWith cfg arg)
    , IWhile  <$> (guardMsg (name == "I_While" ) >> fromObjectWith cfg arg)
    ]

instance MessagePack Assignment where
  fromObjectWith _ = withMsgMap "Assignment" \o -> do
    asLhs <- o .: "lhs"
    asRhs <- o .: "rhs"
    pure Assignment{..}

instance MessagePack branch => MessagePack (Case branch) where
  fromObjectWith _ = withMsgMap "Case" \o -> do
    caseExpr <- o .: "expr"
    caseCases <- o .: "cases"
    pure Case{..}

instance MessagePack branch => MessagePack (CaseClause branch) where
  fromObjectWith _ = withMsgMap "CaseClause" \o -> do
    ccPattern <- o .: "pattern"
    ccRhs <- o .: "rhs"
    pure CaseClause{..}

instance MessagePack TestClause where
  fromObjectWith cfg = withMsgVariant "TestClause" \(name, arg) -> asumMsg
    [ ClauseInstr <$> (guardMsg (name == "ClauseInstr") >> fromObjectWith cfg arg)
    , ClauseBlock <$> (guardMsg (name == "ClauseBlock") >> fromObjectWith cfg arg)
    ]

instance MessagePack branch => MessagePack (Conditional branch) where
  fromObjectWith _ = withMsgMap "Conditional" \o -> do
    cndTest <- o .: "test"
    cndIfSo <- o .: "if_so"
    cndIfNot <- o .:? "if_not"
    pure Conditional{..}

instance MessagePack ForInt where
  fromObjectWith _ = withMsgMap "ForInt" \o -> do
    fiIndex <- o .: "index"
    fiInit <- o .: "init"
    fiBound <- o .: "bound"
    fiStep <- o .:? "step"
    fiBlock <- o .: "block"
    pure ForInt{..}

instance MessagePack ForIn where
  fromObjectWith cfg = withMsgVariant "ForIn" \(name, arg) -> asumMsg
    [ ForMap       <$> (guardMsg (name == "ForMap"      ) >> fromObjectWith cfg arg)
    , ForSetOrList <$> (guardMsg (name == "ForSetOrList") >> fromObjectWith cfg arg)
    ]

instance MessagePack ForMap' where
  fromObjectWith _ = withMsgMap "ForMap" \o -> do
    fmBinding <- o .: "binding"
    fmCollection <- o .: "collection"
    fmBlock <- o .: "block"
    pure ForMap'{..}

instance MessagePack ForSetOrList' where
  fromObjectWith _ = withMsgMap "ForSetOrList" \o -> do
    fsVar <- o .: "var"
    fsCollection <- o .: "collection"
    fsBlock <- o .: "block"
    pure ForSetOrList'{..}

instance MessagePack Patch where
  fromObjectWith _ = withMsgMap "Patch" \o -> do
    pchCollection <- o .: "collection"
    pchPatch <- o .: "patch"
    pure Patch{..}

instance MessagePack Removal where
  fromObjectWith _ = withMsgMap "Removal" \o -> do
    rmItem <- o .: "item"
    rmCollection <- o .: "collection"
    pure Removal{..}

instance MessagePack WhileLoop where
  fromObjectWith _ = withMsgMap "WhileLoop" \o -> do
    wlCond <- o .: "cond"
    wlBlock <- o .: "block"
    pure WhileLoop{..}

----------------
-- Conversion --
----------------

-- | Transform a @PascaLIGO@ CST into the unified Skeleton AST.
toAST :: CST -> LIGO Info
toAST CST{..} =
  let
    firstRange = point 1 1
    (lastRange, _) = unpackWrap cstEof
  in fastMake (firstRange `merged` lastRange) (AST.RawContract $ declConv <$> cstDecl)
  where
    declConv :: Declaration -> LIGO Info
    declConv = \case
      DAttr (unpackReg -> (_, Tuple1 d)) -> declConv d
      DConst (unpackReg -> (r, ConstDecl{..})) ->
        let
          typ = typeAnnotationConv <$> cdConstType
          expr = exprConv cdInit
        in fastMake r (AST.BConst False (patConv cdPattern) [] typ (Just expr))
      DFun (unpackReg -> (r, FunDecl{..})) ->
        let
          funcName = makeWrappedLexeme AST.Name (unVariable fdFunName)
          params = paramConv <$> fdParameters
          typ = typeAnnotationConv <$> fdRetType
          body = exprConv fdReturn
        in fastMake r (AST.BFunction False funcName [] params typ body)
      DType typeDecl -> typeDeclConv typeDecl
      DModule modDecl -> moduleDeclConv modDecl
      DSignature signatureDecl -> signatureDeclConv signatureDecl

    paramConv :: ParamDecl -> LIGO Info
    paramConv ParamDecl{..} = case pdParamType of
      Nothing -> patConv pdPattern
      Just ann ->
        let p = patConv pdPattern
        in fastMake (extract p) (AST.IsAnnot p (typeAnnotationConv ann))

    typeAnnotationConv :: TypeAnnotation -> LIGO Info
    typeAnnotationConv = typeExprConv . unTuple1

    typeDeclConv :: Reg TypeDecl -> LIGO Info
    typeDeclConv (unpackReg -> (r, TypeDecl{..})) =
      let
        name = makeWrappedLexeme AST.Name (unVariable tdName)
        params = tdParams <&> \(unpackReg -> (r', Par' vars)) ->
          fastMake r' (AST.QuotedTypeParams (makeWrappedLexeme AST.TypeVariableName . unVariable <$> toList vars))
        typ = typeExprConv tdTypeExpr
      in fastMake r (AST.BTypeDecl name params typ)

    moduleDeclConv :: Reg ModuleDecl -> LIGO Info
    moduleDeclConv (unpackReg -> (r, ModuleDecl{..})) =
      let
        annMb = signatureExprConv . unTuple1 <$> mdAnnotation
        name = makeWrappedLexeme AST.Name mdName
        modExpr = moduleExprConv mdModuleExpr
      in case mdModuleExpr of
        MBody{} -> fastMake r (AST.BModuleDecl name (one <$> annMb) modExpr)
        _ -> fastMake r (AST.BModuleAlias name annMb modExpr)

    moduleExprConv :: ModuleExpr -> LIGO Info
    moduleExprConv = \case
      MBody (unpackReg -> (r, ModuleBody{..})) ->
        fastMake r (AST.ModuleExpr $ declConv <$> mbDeclarations)
      MPath (unpackReg -> (r, modPath)) ->
        fastMake r (modPathConv $ makeWrappedLexeme AST.ModuleName . unVariable <$> modPath)
      MVar name'@(unVariable -> nm) ->
        let (r, _) = unpackWrap nm
        in fastMake r (modPathConv $ ModulePath [] (makeWrappedLexeme AST.ModuleName nm))

    signatureDeclConv :: Reg SignatureDecl -> LIGO Info
    signatureDeclConv (unpackReg -> (r, SignatureDecl{..})) =
      let
        name = makeWrappedLexeme AST.ModuleName (unVariable sdName)
        signature = signatureExprConv sdSignatureExpr
      in fastMake r (AST.BSignature name signature [])

    signatureExprConv :: SignatureExpr -> LIGO Info
    signatureExprConv = \case
      SPath (unpackReg -> (r, modPath)) ->
        fastMake r (modPathConv $ makeWrappedLexeme AST.ModuleName . unVariable <$> modPath)
      SVar name -> makeWrappedLexeme AST.ModuleName (unVariable name)
      SSig (unpackReg -> (r, SignatureBody{..})) ->
        fastMake r (AST.Signature $ sigItemConv <$> sbSigItems)
      where
        sigItemConv :: SigItem -> LIGO Info
        sigItemConv = \case
          SValue (unpackReg -> (r', SigValue{..})) ->
            fastMake r' (AST.SValue (makeWrappedLexeme AST.Name (unVariable svVar)) (typeExprConv svValType))
          SType (unpackReg -> (r', SigType{..})) ->
            fastMake r' (AST.SType (makeWrappedLexeme AST.TypeName (unVariable stTypeName)) (typeExprConv . unTuple1 <$> stTypeRhs))
          SigInclude (unpackReg -> (r', Tuple1 sigExpr)) ->
            fastMake r' (AST.SInclude $ signatureExprConv sigExpr)
          SigAttr (unpackReg -> (_, Tuple1 sigItem)) -> sigItemConv sigItem

    patConv :: Pattern -> LIGO Info
    patConv = \case
      PApp (unpackReg -> (r, (ctor, args))) ->
        let argPats = maybe [] (\(unpackReg -> (_, Par' ps)) -> toList $ patConv <$> ps) args
        in fastMake r (AST.IsConstr (patConv ctor) argPats)
      PAttr (Tuple1 pat) -> patConv pat
      PBytes (unpackWrap -> (r, Tuple1 bts)) -> makeConstantPat r (AST.CBytes bts)
      PCons (unpackReg -> (r, (x, xs))) -> fastMake r (AST.IsCons (patConv x) (patConv xs))
      PCtor ctor -> makeWrappedLexeme AST.Ctor ctor
      PInt (unpackWrap -> (r, Tuple1 n)) -> makeConstantPat r (AST.CInt n)
      PList (unpackReg -> (r, Compound pats)) -> fastMake r (AST.IsList (patConv <$> pats))
      PModPath (unpackReg -> (r, modPath)) -> fastMake r (modPathConv $ patConv <$> modPath)
      PMumav (unpackWrap -> (r, Tuple1 mav)) -> makeConstantPat r (AST.CMav mav)
      PMav (unpackWrap -> (r, Tuple1 mav)) -> makeConstantPat r (AST.CMav mav)
      PNat (unpackWrap -> (r, Tuple1 n)) -> makeConstantPat r (AST.CNat n)
      PNil (unpackWrap -> (r, _)) -> fastMake r (AST.IsList [])
      PPar (unpackReg -> (r, Par' pat)) -> fastMake r (AST.IsParen (patConv pat))
      PRecord (unpackReg -> (r, Compound recFields)) ->
        fastMake r (AST.IsRecord (fieldPatConv <$> recFields))
      PString (unpackWrap -> (r, str)) -> makeConstantPat r (AST.CString $ escapeText str)
      PTuple (unpackReg -> (r, Par' args)) -> fastMake r (AST.IsTuple (toList $ patConv <$> args))
      PTyped (unpackReg -> (r, (pat, ann))) -> fastMake r (AST.IsAnnot (patConv pat) (typeAnnotationConv ann))
      PVar (unVariable -> var@(unpackWrap -> (r, v)))
        | v == "_" -> fastMake r AST.IsWildcard
        | otherwise -> fastMake r (AST.IsVar (makeWrappedLexeme AST.NameDecl var))
      PVerbatim verb -> makeWrappedLexeme AST.Verbatim verb
      where
        makeConstantPat :: Range -> AST.Constant (LIGO Info) -> LIGO Info
        makeConstantPat r c = fastMake r $ AST.IsConstant (fastMake r c)

        fieldPatConv :: Reg (Field Pattern Pattern) -> LIGO Info
        fieldPatConv (unpackReg -> (r, f)) = case f of
          Punned lhs ->
            fastMake r (AST.IsRecordCapture (patFieldName lhs))
          Complete FullField{..} ->
            fastMake r (AST.IsRecordField (patFieldName ffFieldLhs) (patConv ffFieldRhs))
          where -- MAVRYK: PascaLIGO. field_lhs is a pattern (P_Var name); take the name, best-effort otherwise.
            patFieldName (PVar v) = makeWrappedLexeme AST.FieldName (unVariable v)
            patFieldName p = patConv p

    exprConv :: Expr -> LIGO Info
    exprConv = \case
      EAdd op -> makeBinOp op
      EAnd op -> makeBinOp op
      EApp (unpackReg -> (r, (f, unpackReg -> (_, Par' args)))) ->
        fastMake r (AST.Apply (exprConv f) (exprConv <$> args))
      EAttr (Tuple1 expr) -> exprConv expr
      EBigMap (unpackReg -> (r, Compound binds)) -> fastMake r (AST.List (bindingConv <$> binds))
      EBlock (unpackReg -> (r, BlockWith{..})) ->
        let stmts = blockConv bwBlock
        in fastMake r (AST.Seq (stmts <> [exprConv bwExpr]))
      EBytes (unpackWrap -> (r, Tuple1 bts)) -> makeConstantExpr r (AST.CBytes bts)
      ECase (unpackReg -> (r, Case{..})) ->
        let
          subject = exprConv caseExpr
          alts = toList $ caseCases <&> \(unpackReg -> (r', CaseClause{..})) ->
            fastMake r' (AST.Alt (patConv ccPattern) (exprConv ccRhs))
        in fastMake r (AST.Case subject alts)
      ECat op -> makeBinOp op
      ECodeInj (unpackReg -> (r, CodeInj{..})) ->
        let
          (langR, langName) = second (snd . unpackReg) $ unpackWrap ciLanguage
          lang = fastMake langR (AST.Attr langName)
        in fastMake r (AST.CodeInj lang (exprConv ciCode))
      ECond (unpackReg -> (r, Conditional{..})) ->
        fastMake r (AST.If (exprConv cndTest) (exprConv cndIfSo) (exprConv . unTuple1 <$> cndIfNot))
      ECons op -> makeBinOp op
      EContractOf (unpackReg -> (r, path)) -> fastMake r (AST.Contract (modAccessOf r path))
      ECtor ctor -> makeWrappedLexeme AST.Ctor ctor
      EDiv op -> makeBinOp op
      EEqual op -> makeBinOp op
      EFun (unpackReg -> (r, FunExpr{..})) ->
        let
          params = paramConv <$> feParameters
          typ = typeAnnotationConv <$> feRetType
          body = exprConv feReturn
        in fastMake r (AST.Lambda params [] typ body)
      EGeq op -> makeBinOp op
      EGt op -> makeBinOp op
      EInt (unpackWrap -> (r, Tuple1 n)) -> makeConstantExpr r (AST.CInt n)
      ELeq op -> makeBinOp op
      EList (unpackReg -> (r, Compound xs)) -> fastMake r (AST.List (exprConv <$> xs))
      ELt op -> makeBinOp op
      EMap (unpackReg -> (r, Compound binds)) -> fastMake r (AST.List (bindingConv <$> binds))
      EMapLookup (unpackReg -> (r, MapLookup{..})) ->
        let keys = toList $ mlKeys <&> \(unpackReg -> (_, Par' k)) -> exprConv k
        in fastMake r (AST.Apply (exprConv mlMap) keys)
      EMod op -> makeBinOp op
      EModPath (unpackReg -> (r, modPath)) -> fastMake r (modPathConv $ exprConv <$> modPath)
      EMult op -> makeBinOp op
      EMumav (unpackWrap -> (r, Tuple1 mav)) -> makeConstantExpr r (AST.CMav mav)
      EMav (unpackWrap -> (r, Tuple1 mav)) -> makeConstantExpr r (AST.CMav mav)
      ENat (unpackWrap -> (r, Tuple1 nat)) -> makeConstantExpr r (AST.CNat nat)
      ENeg op -> makeUnOp op
      ENeq op -> makeBinOp op
      ENil (unpackWrap -> (r, _)) -> fastMake r (AST.List [])
      ENot op -> makeUnOp op
      EOr op -> makeBinOp op
      EPar (unpackReg -> (r, Par' expr)) -> fastMake r (AST.Paren (exprConv expr))
      EProj proj -> projConv proj
      ERecord (unpackReg -> (r, Compound recFields)) ->
        fastMake r (AST.Record (fieldExprConv <$> recFields))
      ESet (unpackReg -> (r, Compound xs)) -> fastMake r (AST.List (exprConv <$> xs))
      ESetMem (unpackReg -> (r, SetMembership{..})) ->
        fastMake r (AST.Apply (exprConv smSet) [exprConv smElement])
      EString (unpackWrap -> (r, str)) -> makeConstantExpr r (AST.CString $ escapeText str)
      ESub op -> makeBinOp op
      ETuple (unpackReg -> (r, Par' xs)) -> fastMake r (AST.Tuple (toList $ exprConv <$> xs))
      ETyped (unpackReg -> (r, Par' (expr, typ))) ->
        fastMake r (AST.Annot (exprConv expr) (typeAnnotationConv typ))
      EUpdate (unpackReg -> (r, Update{..})) ->
        fastMake r (AST.RecordUpd (exprConv uStructure) [exprConv uUpdate])
      EVar v -> makeWrappedLexeme AST.Name (unVariable v)
      EVerbatim verb -> makeWrappedLexeme AST.Verbatim verb
      where
        makeBinOp :: SomeBinOp -> LIGO Info
        makeBinOp (unpackReg -> (r, BinOp{..})) =
          fastMake r (AST.BinOp (exprConv boArg1) (makeWrappedLexeme AST.Op boOp) (exprConv boArg2))

        makeUnOp :: SomeUnOp -> LIGO Info
        makeUnOp (unpackReg -> (r, UnOp{..})) =
          fastMake r (AST.UnOp (makeWrappedLexeme AST.Op uoOp) (exprConv uoArg))

        makeConstantExpr :: Range -> AST.Constant (LIGO Info) -> LIGO Info
        makeConstantExpr r c = fastMake r $ AST.Constant (fastMake r c)

        bindingConv :: Reg Binding -> LIGO Info
        bindingConv (unpackReg -> (r, Binding{..})) =
          fastMake r (AST.Tuple [exprConv bKey, exprConv bValue])

        projConv :: Reg Projection -> LIGO Info
        projConv (unpackReg -> (r, Projection{..})) =
          let
            name = exprConv pRecordOrTuple
            selection = toList $ pFieldPath <&> \case
              FieldName fName -> makeWrappedLexeme AST.FieldName (unVariable fName)
              Component n -> makeWrappedLexeme AST.CInt (unTuple1 <$> n)
          in fastMake r (AST.QualifiedName name selection)

        fieldExprConv :: Reg (Field Expr Expr) -> LIGO Info
        fieldExprConv (unpackReg -> (r, f)) = case f of
          Punned lhs ->
            fastMake r (AST.Capture (exprFieldName lhs))
          Complete FullField{..} ->
            fastMake r (AST.FieldAssignment [exprFieldName ffFieldLhs] (exprConv ffFieldRhs))
          where -- MAVRYK: PascaLIGO. field_lhs is an expr (E_Var name); take the name, best-effort otherwise.
            exprFieldName (EVar v) = makeWrappedLexeme AST.FieldName (unVariable v)
            exprFieldName e = exprConv e

    -- Blocks/statements: PascaLIGO is imperative; map a block to a sequence of node conversions.
    blockConv :: Reg Block -> [LIGO Info]
    blockConv (unpackReg -> (_, Block{..})) = statementConv <$> blkStatements

    statementConv :: Statement -> LIGO Info
    statementConv = \case
      SAttr (Tuple1 s) -> statementConv s
      SDecl d -> declConv d
      SInstr i -> instructionConv i
      SVarDecl (unpackReg -> (r, VarDecl{..})) ->
        let typ = typeAnnotationConv <$> vdVarType
        in fastMake r (AST.BConst False (patConv vdPattern) [] typ (Just (exprConv vdInit)))

    testClauseConv :: TestClause -> LIGO Info
    testClauseConv = \case
      ClauseInstr i -> instructionConv i
      ClauseBlock blk@(unpackReg -> (r, _)) -> fastMake r (AST.Seq (blockConv blk))

    instructionConv :: Instruction -> LIGO Info
    instructionConv = \case
      IAssign (unpackReg -> (r, Assignment{..})) ->
        fastMake r (AST.BinOp (exprConv asLhs) (fastMake r (AST.Op ":=")) (exprConv asRhs))
      ICall (unpackReg -> (r, (f, unpackReg -> (_, Par' args)))) ->
        fastMake r (AST.Apply (exprConv f) (exprConv <$> args))
      ICase (unpackReg -> (r, Case{..})) ->
        let
          subject = exprConv caseExpr
          alts = toList $ caseCases <&> \(unpackReg -> (r', CaseClause{..})) ->
            fastMake r' (AST.Alt (patConv ccPattern) (testClauseConv ccRhs))
        in fastMake r (AST.Case subject alts)
      ICond (unpackReg -> (r, Conditional{..})) ->
        fastMake r (AST.If (exprConv cndTest) (testClauseConv cndIfSo) (testClauseConv . unTuple1 <$> cndIfNot))
      IFor (unpackReg -> (r, ForInt{..})) ->
        let
          varName = makeWrappedLexeme AST.Name (unVariable fiIndex)
          from = exprConv fiInit
          initExpr = fastMake (extract varName `merged` extract from)
            (AST.BinOp varName (fastMake (extract varName) (AST.Op ":=")) from)
          to = exprConv fiBound
          body = fastMake r (AST.Seq (blockConv fiBlock))
        in fastMake r (AST.ForLoop (Just initExpr) Nothing [to] (Just body))
      IForIn forIn -> case forIn of
        ForMap (unpackReg -> (r, ForMap'{..})) ->
          let
            (k, _v) = fmBinding
            pat = fastMake (fst $ unpackWrap $ unVariable k) (AST.IsVar (makeWrappedLexeme AST.NameDecl (unVariable k)))
            col = exprConv fmCollection
            body = fastMake r (AST.Seq (blockConv fmBlock))
          in fastMake r (AST.ForOfLoop pat col body)
        ForSetOrList (unpackReg -> (r, ForSetOrList'{..})) ->
          let
            pat = fastMake (fst $ unpackWrap $ unVariable fsVar) (AST.IsVar (makeWrappedLexeme AST.NameDecl (unVariable fsVar)))
            col = exprConv fsCollection
            body = fastMake r (AST.Seq (blockConv fsBlock))
          in fastMake r (AST.ForOfLoop pat col body)
      IPatch (unpackReg -> (r, Patch{..})) ->
        fastMake r (AST.Seq [exprConv pchCollection, exprConv pchPatch])
      IRemove (unpackReg -> (r, Removal{..})) ->
        fastMake r (AST.Seq [exprConv rmItem, exprConv rmCollection])
      ISkip (unpackWrap -> (r, _)) -> fastMake r (AST.Seq [])
      IWhile (unpackReg -> (r, WhileLoop{..})) ->
        fastMake r (AST.WhileLoop (exprConv wlCond) (fastMake r (AST.Seq (blockConv wlBlock))))

    typeExprConv :: TypeExpr -> LIGO Info
    typeExprConv = \case
      TApp (unpackReg -> (r, (typ, unpackReg -> (_, Par' args)))) ->
        fastMake r (AST.TApply (typeExprConv typ) (toList $ typeExprConv <$> args))
      TAttr (Tuple1 typExpr) -> typeExprConv typExpr
      TCart (unpackReg -> (r, (x, xs))) ->
        fastMake r (AST.TProduct (toList $ typeExprConv <$> x <| xs))
      TFun (unpackReg -> (r, (dom, codom))) ->
        fastMake r (AST.TArrow (typeExprConv dom) (typeExprConv codom))
      TInt n@(unpackWrap -> (r, _)) ->
        fastMake r (AST.TInt (makeWrappedLexeme AST.CInt (unTuple1 <$> n)))
      TModPath (unpackReg -> (r, modAccess)) ->
        fastMake r (modPathConv $ typeExprConv <$> modAccess)
      TPar (unpackReg -> (r, Par' typ)) -> fastMake r (AST.TParen (typeExprConv typ))
      TParameterOf (unpackReg -> (r, parts)) -> fastMake r (AST.TParameter (modAccessOf r parts))
      TRecord (unpackReg -> (r, Compound decls)) ->
        let
          fieldDecls = decls <&> \(unpackReg -> (r', FieldDecl{..})) ->
            fastMake r' (AST.TField (makeWrappedLexeme AST.Name (unVariable fdFieldName)) (typeAnnotationConv <$> fdFieldType))
        in fastMake r (AST.TRecord def fieldDecls)
      TString str@(unpackWrap -> (r, _)) ->
        fastMake r (AST.TString (makeWrappedLexeme AST.CString (escapeText <$> str)))
      TSum (unpackReg -> (r, SumType{..})) ->
        let
          variants = vtVariants <&> \(unpackReg -> (r', Variant{..})) ->
            fastMake r' (AST.Variant (makeWrappedLexeme AST.Name vCtor) (typeExprConv . unTuple1 <$> maybeToList vCtorArgs))
        in fastMake r (AST.TSum def variants)
      -- MAVRYK: PascaLIGO. A union lowers to a sum, so represent it in the skeleton as an [AST.TSum]
      -- over the union's member type expressions (mirrors JsLigoCST.toAST's TUnion case).
      TUnion (unpackReg -> (r, objects)) ->
        fastMake r (AST.TSum def $ typeExprConv <$> objects)
      TVar (unVariable -> var@(unpackWrap -> (r, v)))
        | v == "_" -> fastMake r AST.TWildcard
        | otherwise -> makeWrappedLexeme AST.TypeName var

    -- | A module access from a non-empty list of module-name parts (@contract_of@/@parameter_of@).
    modAccessOf :: Range -> NonEmpty Variable -> LIGO Info
    modAccessOf r parts =
      let
        lastPart = unVariable (last parts)
        initParts = init parts
        modPath = ModulePath initParts (makeWrappedLexeme AST.ModuleName lastPart)
      in fastMake r (modPathConv modPath)

    modPathConv :: ModulePath (LIGO Info) -> AST.ModuleAccess (LIGO Info)
    modPathConv ModulePath{..} =
      AST.ModuleAccess (makeWrappedLexeme AST.ModuleName . unVariable <$> mpModulePath) mpField
