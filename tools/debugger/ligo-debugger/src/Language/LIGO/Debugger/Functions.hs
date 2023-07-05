-- | Helpers for handling LIGO functions.

{-# OPTIONS_GHC -Wno-orphans #-}

module Language.LIGO.Debugger.Functions
  ( LambdaMeta (..)
  , LambdaEvent (..)
  , lmEventsL
  , matchesUniqueLambdaName
  , LambdaArg (..)
  , LambdaNamedInfo (..)
  , lmAllFuncNames
  , lmOriginalFuncName
  , lmActualFuncName
  , lmGroupByName
  , lambdaMetaL
  , mLambdaMetaL
  , stripSuffixLambdaNamedInfo
  , stripSuffixLambdaEvent
  , internalStackFrameName
  , embedFunctionNames
  , embedFunctionNameIntoLambda
  , addAppliedArg
  , getLambdaMeta
  ) where

import Control.Lens (AsEmpty (..), lens, makeLensesWith, makePrisms, non', prism)
import Data.Default (Default (..))
import Data.Singletons (SingI)
import Data.Vinyl (Rec (RNil, (:&)))
import Fmt (Buildable (..), genericF, pretty)
import Text.Interpolation.Nyan

import Morley.Michelson.Interpret (StkEl (StkEl))
import Morley.Michelson.Typed qualified as T
import Morley.Util.Lens (postfixLFields)

import Language.LIGO.Debugger.CLI
import Util

-- | Registered name of a lambda.
data LambdaName u
  = LName (Name u)
    -- ^ We know a particular name.
  | LNameUnknown
    -- ^ Name is yet unknown.
    -- It be figured out later, or this lambda may be some LIGO-internal thing.
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

deriving stock instance Eq (LambdaName 'Concise)

instance SingI u => Buildable (LambdaName u) where
  build = \case
    LName n -> build n
    LNameUnknown -> build internalStackFrameName

matchesUniqueLambdaName :: Name 'Unique -> LambdaName 'Unique -> Bool
matchesUniqueLambdaName n1 = \case
  LName n2 -> n1 `compareUniqueNames` n2
  LNameUnknown -> False

-- | An argument applied to lambda.
data LambdaArg = LambdaArg
  { laValue :: T.SomeValue
  , laType :: LigoType
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

instance ForInternalUse => Buildable LambdaArg where
  build (LambdaArg value ty) = [int||Value #{value} of type #{ty}|]

-- | Information that comes along with a lambda being named.
data LambdaNamedInfo u = LambdaNamedInfo
  { lniName :: Name u
  , lniType :: LigoType
  } deriving stock (Show, Generic)
    deriving anyclass (NFData)

deriving stock instance Eq (LambdaNamedInfo 'Concise)

instance (SingI u, ForInternalUse) => Buildable (LambdaNamedInfo u) where
  build (LambdaNamedInfo name typ) = [int||Named as "#{name}" of type #{typ}|]

stripSuffixLambdaNamedInfo :: LambdaNamedInfo 'Unique -> LambdaNamedInfo 'Concise
stripSuffixLambdaNamedInfo LambdaNamedInfo{..} = LambdaNamedInfo
  { lniName = pretty lniName, .. }

-- | An event happening to lambda.
data LambdaEvent u
  = LambdaApplied LambdaArg
    -- ^ Lambda was applied an argument.
    --
    -- Likely that produced another lambda, because if not then we will likely stop
    -- tracking this info. So this event is a case of partial application.
  | LambdaNamed (LambdaNamedInfo u)
    -- ^ We figured out a new name for a lambda, and that variable has
    -- the given type.
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

deriving stock instance Eq (LambdaEvent 'Concise)

instance (SingI u, ForInternalUse) => Buildable (LambdaEvent u) where
  build = genericF

makePrisms ''LambdaEvent

stripSuffixLambdaEvent :: LambdaEvent 'Unique -> LambdaEvent 'Concise
stripSuffixLambdaEvent = \case
  LambdaApplied arg -> LambdaApplied arg
  LambdaNamed named -> LambdaNamed (stripSuffixLambdaNamedInfo named)

-- | A meta that we carry along with lambda values when
-- interpreting a contract.
--
-- This type is pretty raw on itself, but we provide numerious smart getters.
newtype LambdaMeta = LambdaMeta
  { lmEvents :: [LambdaEvent 'Unique]
    -- ^ Events that happened to the related lambda, last event goes first.
    --
    -- Considering a general case, usually lifetime of a lambda with many
    -- arguments consists of two types of events: it gets applied those arguments
    -- one by one, and periodically it is assigned a new name (e.g. when
    -- is assigned to a variable).
    --
    -- For instance, if we have the code like this
    --
    -- @
    -- let add(a: int)(b: int) = a + b
    -- let add5(a: int) = add 5
    -- @
    --
    -- then variable named @add5@ is expected to have the following events:
    -- @[LambdaNamed "add5", LambdaApplied 5, LambdaNamed "add"]@
    --
    -- Normally the first happened event (the last in the list) should be
    -- the original function name (without partial applications), but
    -- we don't rely on this. For instance, the original function can be some
    -- LIGO-internal function that is first applied arguments and then gains
    -- a name from the user's scope; in such a situation, the arguments applied
    -- before the first naming are usually not interesting for us.
    --
    -- It both can be that several application events in a row take place,
    -- and that several namings occur (the latter case is possible for code like
    -- @op = add@).
    --
    -- Invariant: no subsequent naming events should introduce the same name
    -- (names that are same string literals but come from different scopes are
    -- treated as different here). I.e. the fact the the lambda repeatedly
    -- gets assigned the same variable name should add only one event.
  } deriving stock (Show, Generic)
    deriving anyclass (NFData)

makeLensesWith postfixLFields ''LambdaMeta

instance ForInternalUse => Buildable LambdaMeta where
  build LambdaMeta{..} =
    [int||
    LambdaMeta
      variables: #{toList lmEvents}|]

instance Default LambdaMeta where
  def = LambdaMeta []

instance AsEmpty LambdaMeta where
  _Empty = prism def \case{ LambdaMeta [] -> Right (); other -> Left other }

-- | All function names, the most recent one goes first.
lmAllFuncNames :: LambdaMeta -> [Name 'Unique]
lmAllFuncNames = fmap lniName . mapMaybe (preview _LambdaNamed) . lmEvents

-- | The last known name of the lambda.
lmActualFuncName :: LambdaMeta -> LambdaName 'Unique
lmActualFuncName = maybe LNameUnknown LName . safeHead . reverse . lmAllFuncNames

-- | The original name of the lambda.
lmOriginalFuncName :: LambdaMeta -> LambdaName 'Unique
lmOriginalFuncName = maybe LNameUnknown LName . safeHead . lmAllFuncNames

-- | Return a list where each entry contains:
--
-- * Name of the lambda (maybe partially applied) and its type.
-- * Arguments that were passed to that lambda since the time it got that name - in direct order.
--
-- In this outer list, more recently assigned names go first.
--
-- For example, in case of this code:
--
-- @
-- let add(a: int)(b: int) = a + b
-- let add5 = add 5
-- @
--
-- @lmGroupByName@ on @add5@ variable's meta should produce
-- @[(("add5", int -> int), []), (("add", int -> int -> int), [5])]@
--
-- If any applications took place before the first name was assigned to the lambda,
-- those are ignored as non-interesting.
lmGroupByName :: LambdaMeta -> [(LambdaNamedInfo 'Unique, [LambdaArg])]
lmGroupByName (LambdaMeta events) = go [] [] events
  where
  go resAcc argsAcc = \case
    LambdaApplied arg : evs -> go resAcc (arg : argsAcc) evs
    LambdaNamed info : evs -> go ((info, reverse argsAcc) : resAcc) [] evs
    [] -> reverse resAcc

-- | A lens for accessing the meta of a lambda.
--
-- Returns @Nothing@ when the lambda is unwrapped.
lambdaMetaL :: Lens' (T.Value ('T.TLambda i o)) (Maybe LambdaMeta)
lambdaMetaL = lens
  -- TODO [LIGO-986]: check LambdaCodeRec is handled properly
  do let extractMeta = \case
           T.ConcreteMeta meta _ -> Just meta
           _ -> Nothing
     \case
       (T.VLam (T.LambdaCode lam)) -> extractMeta $ T.rfAnyInstr lam
       (T.VLam (T.LambdaCodeRec lam)) -> extractMeta $ T.rfAnyInstr lam

  do let replaceMeta mMeta instr =
          let pureInstr = case instr of
                T.ConcreteMeta (_ :: LambdaMeta) i -> i
                i -> i
          in maybe id (T.Meta . T.SomeMeta) mMeta pureInstr

     \(T.VLam lamVal) mMeta -> T.VLam $ case lamVal of
       T.LambdaCode lam ->
         T.LambdaCode $ T.rfMapAnyInstr (replaceMeta mMeta) lam
       T.LambdaCodeRec lam ->
         T.LambdaCodeRec $ T.rfMapAnyInstr (replaceMeta mMeta) lam

-- | Variation of 'lambdaMetaL' that can look into arbitrary value,
-- doing nothing if it is not a lambda.
mLambdaMetaL :: Traversal' (T.Value t) (Maybe LambdaMeta)
mLambdaMetaL f = \case{ v@T.VLam{} -> lambdaMetaL f v; v -> pure v }

embedFunctionNameIntoLambda
  :: LigoVariable 'Unique
  -> LigoType
  -> T.Value t
  -> T.Value t
embedFunctionNameIntoLambda (LigoVariable newName) newType =
  mLambdaMetaL . non' _Empty . lmEventsL %~ \case
    events@(LambdaNamed namedInfo : _)
      | lniName namedInfo `compareUniqueNames` newName -> events
    events ->
      LambdaNamed (LambdaNamedInfo newName newType) : events

tryToEmbedEnvIntoLambda :: (LigoStackEntry 'Unique, StkEl t) -> StkEl t
tryToEmbedEnvIntoLambda (LigoStackEntry LigoExposedStackEntry{..}, stkEl@(StkEl val)) =
  case (leseDeclaration, leseType) of
    (Just name, typ@(LigoTypeResolved LigoTypeExpression { _lteTypeContent = LTCArrow{} })) ->
      StkEl $ embedFunctionNameIntoLambda name typ val
    _ -> stkEl
tryToEmbedEnvIntoLambda (_, stkEl) = stkEl

embedFunctionNames :: Rec StkEl t -> LigoStack 'Unique -> Rec StkEl t
embedFunctionNames (x :& xs) (y : ys) = tryToEmbedEnvIntoLambda (y, x) :& embedFunctionNames xs ys
embedFunctionNames stack [] = stack
embedFunctionNames RNil _ = RNil

addAppliedArg :: LambdaArg -> LambdaMeta -> LambdaMeta
addAppliedArg arg (LambdaMeta evs) = LambdaMeta (LambdaApplied arg : evs)

getLambdaMeta :: T.Value ('T.TLambda i o) -> LambdaMeta
getLambdaMeta = fromMaybe def . view lambdaMetaL
