-- | Common stuff for debugger.
module Language.LIGO.Debugger.Common
  ( EmbeddedLigoMeta
  , ligoPositionToSrcPos
  , ligoRangeToSourceLocation
  , spineAtPoint
  , containsNode
  , tryToProcessLigoStatement
  , getStatementLocs
  , isRedundantIndexedInfo
  , isLigoStdLib
  , errorValueType
  , errorAddress
  , ReplacementException (..)
  , replacementErrorValueToException
  , refineStack
  , ligoRangeToRange
  , rangeToLigoRange
  , getMetaMbAndUnwrap
  , shouldIgnoreMeta
  , buildType
  , ExpressionSourceLocation (..)
  , getAllSourceLocations
  , getInterestingSourceLocations
  ) where

import Unsafe qualified

import AST (Binding, CaseOrDefaultStm, Ctor, Expr, LIGO, Pattern, QualifiedName)
import AST qualified
import Data.HashMap.Strict ((!?))
import Data.HashMap.Strict qualified as HM
import Data.List.NonEmpty (groupBy)
import Data.Set qualified as S
import Data.Set qualified as Set
import Data.Vinyl (Rec (RNil, (:&)))
import Duplo (layer, leq, spineTo)
import Fmt (Buildable (..), pretty)
import Parser (Info)
import Product (Contains)
import Range (Range (..), getRange)
import Text.Interpolation.Nyan

import Morley.Debugger.Core.Navigate (SourceLocation (..))
import Morley.Debugger.Core.Snapshots ()
import Morley.Michelson.ErrorPos (Pos (..), SrcPos (..))
import Morley.Michelson.Interpret (StkEl (seValue))
import Morley.Michelson.Parser (MichelsonSource (MSFile), utypeQ)
import Morley.Michelson.Text (MText)
import Morley.Michelson.Typed
  (Constrained (SomeValue), EpAddress (..), Instr (DIG, DUP, DUPN, Nested, Nop, PUSH, SWAP, UNIT),
  SomeValue, Value, Value' (..), pattern (:#), pattern ConcreteMeta, withValueTypeSanity)
import Morley.Michelson.Untyped qualified as U
import Morley.Tezos.Address (KindedAddress (ImplicitAddress), ta)
import Morley.Tezos.Address.Kinds (AddressKind (AddressKindImplicit))

import Language.LIGO.Debugger.CLI.Types
import Language.LIGO.Debugger.Error

-- | Type of meta that we embed in Michelson contract to later use it
-- in debugging.
type EmbeddedLigoMeta = LigoIndexedInfo 'Unique

ligoPositionToSrcPos :: HasCallStack => LigoPosition -> SrcPos
ligoPositionToSrcPos (LigoPosition l c) =
  SrcPos
    (Pos $ Unsafe.fromIntegral (toInteger l - 1))
    (Pos $ Unsafe.fromIntegral c)

ligoRangeToSourceLocation :: HasCallStack => LigoRange -> SourceLocation
ligoRangeToSourceLocation LigoRange{..} =
  SourceLocation (MSFile lrFile) (ligoPositionToSrcPos lrStart) (ligoPositionToSrcPos lrEnd)

-- | Returns all nodes which cover given range
-- ordered from the most local to the least local.
spineAtPoint
  :: Contains Range xs
  => Range -> LIGO xs -> [LIGO xs]
spineAtPoint pos = spineTo (\i -> pos `leq` getRange i)

getStatementLocs :: HasCallStack => Set SourceLocation -> HashMap FilePath (LIGO Info) -> Set SourceLocation
getStatementLocs locs parsedContracts =
  ranges
    <&> getStatementRanges
    & concat
    <&> rangeToSourceLocation
    & Set.fromList
  where
    sourceLocationToRange :: SourceLocation -> Range
    sourceLocationToRange (SourceLocation (MSFile file) startPos endPos) = Range
      { _rStart = posToTuple startPos
      , _rFinish = posToTuple endPos
      , _rFile = file
      }
      where
        posToTuple :: Integral i => SrcPos -> (i, i, i)
        posToTuple (SrcPos (Pos l) (Pos c)) =
          (Unsafe.fromIntegral (l + 1), Unsafe.fromIntegral (c + 1), 0)

    sourceLocationToRange loc = error [int||Got source location with Lorentz source #{loc}|]

    rangeToSourceLocation :: Range -> SourceLocation
    rangeToSourceLocation Range{..} =
      SourceLocation
        (MSFile _rFile)
        (tupleToPos _rStart)
        (tupleToPos _rFinish)
      where
        tupleToPos :: Integral i => (i, i, i) -> SrcPos
        tupleToPos (l, c, _) = SrcPos
          (Pos $ Unsafe.fromIntegral $ l - 1)
          (Pos $ Unsafe.fromIntegral $ c - 1)

    ranges = toList locs
      <&> sourceLocationToRange
      & filter (not . isLigoStdLib . _rFile)

    getStatementRanges :: Range -> [Range]
    getStatementRanges range@Range{..} = filterStatements $ spineAtPoint range parsedLigo
      where
        parsedLigo =
          fromMaybe
            (error [int||Can't find parsed ligo with filename #{_rFile}|])
            (parsedContracts HM.!? _rFile)

        -- We can use here @Unsafe.head@ because this filtering won't
        -- accept empty list.
        filterStatements :: [LIGO Info] -> [Range]
        filterStatements = fmap (getRange . Unsafe.head) . filter worthPicking . tails
          where
            worthPicking =
              tryToProcessLigoStatement
                (const . const $ True)
                (const False)
                False

-- | Here we decide whether the node is at the top-level
-- by its parent node.
isTopLevel :: LIGO Info -> Bool
isTopLevel parent
    -- This is a true top-level declaration.
  | Just AST.RawContract{} <- layer parent = True
    -- This is a declaration inside some module.
    -- By module we mean declaration via @module@
    -- syntax in CameLIGO and in the corresponding ones
    -- in other dialects.
  | Just AST.BModuleDecl{} <- layer parent = True
  | otherwise = False

-- | Checks whether a node is a transitive child of a given tree.
containsNode :: LIGO Info -> LIGO Info -> Bool
-- Comparing by ranges because @Eq@ instance behaves
-- weird with @LIGO Info@.
containsNode tree node = getRange node `elem` nodes
  where
    nodes = getRange <$> spineAtPoint (getRange node) tree

-- | Accepts a list of AST nodes, ordered from
-- the most local to the least local (see @spineAtPoint@)
-- and checks that the first node in this list is a statement one.
-- If so, then it performs @onSuccess@ action, otherwise @onFail@.
-- In empty case if will return @onEmpty@ value.
tryToProcessLigoStatement
  :: (LIGO Info -> [LIGO Info] -> res) -- ^ @onSuccess@
  -> ([LIGO Info] -> res) -- ^ @onFail@
  -> res -- ^ @onEmpty@
  -> [LIGO Info]
  -> res
tryToProcessLigoStatement onSuccess onFail onEmpty = \case
  [] -> onEmpty
  -- Assignments always look like statements.
  x@(layer -> Just AST.AssignOp{}) : xs -> onSuccess x xs

  -- @BConst@ node corresponds to some assignments in JsLIGO.
  -- We should ignore it only in case if it occurs at top-level.
  x@(layer -> Just AST.BConst{}) : xs@(y : _)
    | isTopLevel y -> onFail xs
    | otherwise -> onSuccess x xs

  -- Like with @BConst@ but couldn't be encountered at top-level.
  -- For now it seems unused but let's leave it as is.
  x@(layer -> Just AST.BVar{}) : xs -> onSuccess x xs

  -- Generic case. Some motivation. Let's define a scope.
  -- By scope we mean the places like @if@ branches,
  -- right hand side of @let-in@, branches in @match ... with@,
  -- etc, i.e. all places where the statement could appear.
  -- It's kinda hard to understand it from words, so,
  -- let's look at some examples:
  -- 1.
  --   @
  --   let foo = ... in (a, b)
  --   @
  --   Here we want this @(a, b)@ to be the statement on the
  --   right hand side of @let-in@.
  -- 2.
  --   @
  --   match opt with
  --   | Some x -> x
  --   | None -> 10 + 42
  --   @
  --   Here we see @match ... with@ branches and it's convenient
  --   to treat @x@ and @10 + 42@ as statements.
  --
  -- For more understanding you can check @couldBeLastInSomeScope@
  -- and @isNewScope@ and see what we treat as statements and scopes
  -- in generic case.
  x : xs@(y : _)
      -- Check that the node could be a statement.
    | couldBeLastInSomeScope x
      -- Check that the parent node could be a new scope.
    , isNewScope y x -> onSuccess x xs
  _ : xs -> onFail xs
  where
    couldBeLastInSomeScope :: LIGO Info -> Bool
    couldBeLastInSomeScope (layer -> Just expr) = case expr of
      -- Let's match on all @Expr@ constructors just in case
      -- this type may be extended one day.
      AST.Let{} -> False
      AST.Apply{} -> True
      AST.Constant{} -> True
      AST.BinOp{} -> True
      AST.UnOp{} -> True
      AST.Op{} -> False
      AST.Record{} -> True
      -- Here we don't treat @If@ as a statement in some scope
      -- because it contains branches and in this case and
      -- expression from some branch would be a statement
      -- (in case this expression passes @couldBeLastInSomeScope@ check).
      AST.If{} -> False
      AST.Ternary{} -> False
      AST.AssignOp{} -> True
      AST.List{} -> True
      AST.ListAccess{} -> True
      AST.Tuple{} -> True
      AST.Annot{} -> True
      AST.Case{} -> False
      AST.Break{} -> False
      AST.Return{} -> True
      AST.SwitchStm{} -> False
      AST.WhileLoop{} -> False
      AST.ForOfLoop{} -> False
      AST.Seq{} -> False
      AST.Lambda{} -> True
      AST.RecordUpd{} -> False
      AST.CodeInj{} -> False
      AST.Paren e -> couldBeLastInSomeScope e
    couldBeLastInSomeScope node
      | Just{} <- layer @Ctor node = True
      | Just{} <- layer @AST.Name node = True
      | Just{} <- layer @QualifiedName node = True
      | otherwise = False

    isNewScope :: LIGO Info -> LIGO Info -> Bool
    isNewScope info child
      | Just AST.If{} <- layer info = True
      | Just AST.Ternary{} <- layer info = True
      -- Case branch
      | Just AST.Alt{} <- layer info = True
      -- Branch in @SwitchStm@ in JsLIGO
      | Just{} <- layer @CaseOrDefaultStm info = True
      | Just (AST.WhileLoop _ body) <- layer info = containsNode body child
      | Just (AST.ForOfLoop _ _ body) <- layer info = containsNode body child
      | Just (AST.BFunction _ _ _ _ _ body) <- layer info = containsNode body child
      | Just AST.Let{} <- layer info = True
      | Just AST.Seq{} <- layer info = True
      | Just AST.Lambda{} <- layer info = True
      | otherwise = False

-- | Sometimes source mapper produces metas that is not really interesting for us.
-- For example: empty metas or metas with locations from stdlib.
isRedundantIndexedInfo :: LigoIndexedInfo u -> Bool
isRedundantIndexedInfo LigoIndexedInfo{..} = isNothing $ asum
  [ do
    loc <- liiLocation
    guard $ (not . isLigoStdLib . lrFile) loc

  , void liiEnvironment
  ]

-- | LIGO debug output, when optimizations are disabled, may mention locations
-- referring to LIGO's standard library that defines bindings to every single
-- Michelson instruction.
-- LIGO teams says that we should just ignore such locations.
isLigoStdLib :: FilePath -> Bool
isLigoStdLib path =
  path == ""

errorValueType :: U.Ty
errorValueType = [utypeQ|pair address string|]

errorAddress :: KindedAddress 'AddressKindImplicit
errorAddress = [ta|tz1fakefakefakefakefakefakefakcphLA5|]

-- | Something was found to be wrong after replacing Michelson code
-- in 'preprocessContract'.
newtype ReplacementException = ReplacementException MText
  deriving newtype (Show, Buildable)

instance Exception ReplacementException where
  displayException = pretty

instance DebuggerException ReplacementException where
  type ExceptionTag ReplacementException = "Replacement"
  debuggerExceptionType _ = MidLigoLayerException
  shouldInterruptDebuggingSession = False

-- | We're replacing some @Michelson@ instructions.
-- Sometimes we perform unsafe unwrapping in the replaced code.
-- In failure case we're doing something like @{ PUSH errValue; FAILWITH }@
-- where @errValue@ has @address@ type. The entrypoint in this address
-- contains the error message.
replacementErrorValueToException :: Value t -> Maybe ReplacementException
replacementErrorValueToException = \case
  (VPair (VAddress (EpAddress addr@ImplicitAddress{} _), VString errMsg))
    | addr == errorAddress -> do
    pure $ ReplacementException errMsg
  _ -> Nothing

stkElValue :: StkEl v -> SomeValue
stkElValue stkEl = let v = seValue stkEl in withValueTypeSanity v (SomeValue v)

-- | Leave only information that matters in LIGO.
refineStack :: Rec StkEl st -> [SomeValue]
refineStack =
  -- Note: it is important for this function to be lazy if we don't
  -- want to have full copy of stack skeleton (which is sequence of `:&`)
  -- in each snapshot, that would take O(snapshots num * avg stack size) memory.
  --
  -- And 'Rec' is strict datatype, so using functions like 'rmap' would not
  -- fit our purpose.
  \case
    RNil -> []
    stkEl :& st -> stkElValue stkEl : refineStack st

ligoRangeToRange :: LigoRange -> Range
ligoRangeToRange LigoRange{..} = Range
  { _rStart = toPosition lrStart
  , _rFinish = toPosition lrEnd
  , _rFile = lrFile
  }
  where
    toPosition LigoPosition{..} = (Unsafe.fromIntegral lpLine, Unsafe.fromIntegral $ lpCol + 1, 0)

rangeToLigoRange :: Range -> LigoRange
rangeToLigoRange Range{..} = LigoRange
  { lrStart = toLigoPosition _rStart
  , lrEnd = toLigoPosition _rFinish
  , lrFile = _rFile
  }
  where
    toLigoPosition (line, col, _) = LigoPosition (Unsafe.fromIntegral line) (Unsafe.fromIntegral $ col - 1)

-- | Tries to unwrap @EmbeddedLigoMeta@ from instr.
-- If successful, then it returns meta and inner instr.
-- Otherwise, no meta and the passed instr.
getMetaMbAndUnwrap :: Instr i o -> (Maybe EmbeddedLigoMeta, Instr i o)
getMetaMbAndUnwrap = \case
  ConcreteMeta embeddedMeta inner -> (Just embeddedMeta, inner)
  instr -> (Nothing, instr)

-- | Sometimes we want to ignore metas for some instructions.
shouldIgnoreMeta :: LigoRange -> Instr i o -> HashMap FilePath (LIGO Info) -> Bool
shouldIgnoreMeta ligoRange instr parsedContracts = shouldIgnoreMetaByInstruction || shouldIgnoreMetaByLocation
  where
    -- Sometimes it's not enough to ignore locations judging only by
    -- instruction that this location are referencing to.
    -- So, we can look into AST and make a decision.
    shouldIgnoreMetaByLocation = isJust do
      contract <- parsedContracts !? lrFile ligoRange

      -- Some nodes in AST may have the same locations.
      -- We want to get the topmost one.
      let stripDuplicateRanges nodes = nodes
            & groupBy (\lhs rhs -> getRange lhs == getRange rhs)
            <&> last

      case stripDuplicateRanges $ spineAtPoint (ligoRangeToRange ligoRange) contract of
        -- Locations for @()@ in function declaration in CameLIGO.
        -- We ignore locations for @UNIT@ but it's not enough.
        (layer @Ctor -> Just (AST.Ctor "Unit")) : _ -> pass

        -- Locations for the whole @let-in@s.
        (layer @Expr -> Just AST.Let{}) : _ -> pass

        -- In JsLIGO we have locations for @s = a + b@ inside
        -- @const s = a + b@ and this @const@ binding is the
        -- nearest node for this location. We skip it because
        -- it acts like @let-in@s in CameLIGO. We start evaluating
        -- @s = a + b@, evaluate the next instructions and after that
        -- we have @EventExpressionEvaluated@ for it.
        (layer @Binding -> Just AST.BConst{}) : _ -> pass

        -- The same reason as with @BConst@.
        (layer @Binding -> Just AST.BVar{}) : _ -> pass

        -- For some reason in JsLIGO operators are treated
        -- as expressions.
        (layer @Expr -> Just AST.Op{}) : _ -> pass

        -- Locations for function names in JsLIGO.
        (layer @Pattern -> Just AST.IsVar{}) : _ -> pass

        -- Locations for function arguments.
        (layer @Pattern -> Just{}) : (layer @Pattern -> Just pat) : (layer @Binding -> Just AST.BFunction{}) : _
            -- Argument has type annotation.
          | AST.IsAnnot{} <- pat -> pass
            -- Argument is just in parentheses.
          | AST.IsParen{} <- pat -> pass

        -- Locations for top-level functions
        (layer @Binding -> Just AST.BFunction{}) : node : _
          | isTopLevel node -> pass
        _ -> empty

    shouldIgnoreMetaByInstruction = case instr of
      -- @PUSH@es have location metas that point to constants.
      -- E.g. @PUSH int 42@ may have a location of @42@.
      --
      -- So, stopping at them and showing an evaluation
      -- seems useless. I see that @42@ evaluates to @42@
      -- without any debug info.
      Nested PUSH{} -> True
      Nested (PUSH{} :# _) -> True

      -- Same as for @PUSH@ but for standalone variables.
      -- Locations for them may be associated with the next
      -- instructions:
      -- 1. @SWAP@
      -- 2. @Nop@
      -- 3. @DUP@, @DUPN@
      -- 4. @DIG@
      Nested SWAP{} -> True
      Nested (SWAP{} :# _) -> True

      Nested Nop{} -> True
      Nested (Nop{} :# _) -> True

      Nested DUP{} -> True
      Nested (DUP{} :# _) -> True

      Nested DUPN{} -> True
      Nested (DUPN{} :# _) -> True

      Nested DIG{} -> True
      Nested (DIG{} :# _) -> True

      -- Motivation is the same as with @PUSH@es
      -- (@UNIT@ is actually equivalent to @PUSH unit ()@)
      Nested UNIT{} -> True
      Nested (UNIT{} :# _) -> True

      _ -> False

-- | An expression source location with boolean indicator
-- that tells us whether it is interesting or not.
-- /Interesting/ means that we want to use this location
-- in switching breakpoints.
data ExpressionSourceLocation = ExpressionSourceLocation
  { eslLigoRange :: LigoRange
  , eslShouldKeep :: HashMap FilePath (LIGO Info) -> Bool
  } deriving stock (Generic)
    deriving anyclass (NFData)

instance Eq ExpressionSourceLocation where
  lhs == rhs = eslLigoRange lhs == eslLigoRange rhs

instance Ord ExpressionSourceLocation where
  lhs <= rhs = eslLigoRange lhs <= eslLigoRange rhs

getAllSourceLocations :: Set ExpressionSourceLocation -> Set SourceLocation
getAllSourceLocations = S.map (ligoRangeToSourceLocation . eslLigoRange)

getInterestingSourceLocations :: HashMap FilePath (LIGO Info) -> Set ExpressionSourceLocation -> Set SourceLocation
getInterestingSourceLocations parsedContracts = getAllSourceLocations . S.filter (`eslShouldKeep` parsedContracts)
