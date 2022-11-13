-- This code is copypasted from Morley.Debugger.DAP.Variables
module Language.LIGO.DAP.Variables
  ( createVariables
  , insertVars
  , runBuilder
  , buildVariable
  , insertToIndex
  , buildLambdaInfo
  ) where

import Control.Lens hiding ((...))
import Data.Default (def)
import Data.HashMap.Strict qualified as HM
import Data.Map qualified as M
import Fmt (pretty)
import Text.Interpolation.Nyan

import Morley.Debugger.Core (DebugPrintMode (DpmEvaluated, DpmNormal), debugBuild)
import Morley.Debugger.Protocol.DAP (Variable)
import Morley.Debugger.Protocol.DAP qualified as DAP
import Morley.Michelson.Typed
  (Constrained (SomeValue), EntrypointCallT (..), EpAddress (..), SingI,
  SomeEntrypointCallT (SomeEpc), Value, Value' (..))
import Morley.Michelson.Untyped.Entrypoints (isDefEpName)

import Language.LIGO.AST (Lang)
import Language.LIGO.Debugger.CLI
import Language.LIGO.Debugger.Functions

-- | For a given stack generate its representation as a tree of 'DAP.Variable's.
--
-- This creates a map @varaibles references -> [variable]@, where root always has
-- largest reference.
createVariables :: Lang -> [(Text, LigoOrMichValue)] -> VariableBuilder Int
createVariables lang varsAndNames = do
  topVars <-
    forM varsAndNames \(toString -> name, ligoOrMichValue) -> do
      buildVariable lang ligoOrMichValue name
  insertVars topVars

type VariableBuilder a = State (Int, Map Int [DAP.Variable]) a

runBuilder :: VariableBuilder a -> (a, Map Int [DAP.Variable])
runBuilder act = (res, vars)
  where
    (res, (_, vars)) = usingState (1, mempty) act

insertToIndex :: Int -> [DAP.Variable] -> VariableBuilder Int
insertToIndex idx vars = do
  _2 %= M.insertWith (\added cur -> cur <> added) idx vars
  pure idx

insertVars :: [DAP.Variable] -> VariableBuilder Int
insertVars vars = do
  -- <<%= modifies state and returns previous value
  nextIdx <- _1 <<%= (+1)
  _2 %= M.insert nextIdx vars
  return nextIdx

createVariable :: String -> String -> Lang -> LigoType -> Maybe String -> Maybe String -> Variable
createVariable name varText lang typ menuContext evaluateName = DAP.defaultVariable
  { DAP.nameVariable = name
  , DAP.valueVariable = varText
  , DAP.typeVariable = pretty (buildType lang typ)
  , DAP.__vscodeVariableMenuContextVariable = menuContext
  , DAP.evaluateNameVariable = evaluateName
  }

buildVariable :: Lang -> LigoOrMichValue -> String -> VariableBuilder Variable
buildVariable lang (MichValue typ (SomeValue v@VLam{})) name =
  buildLambdaInfo lang name typ (view lambdaMetaL v ?: def)
buildVariable lang v name = do
  let
    varText = pretty $ debugBuild DpmNormal (lang, v)
    evaluatedText = pretty $ debugBuild DpmEvaluated (lang, v)
    menuContext = case v of
      LigoValue _ ligoVal -> case ligoVal of
        LVCt LCAddress{} -> Just "address"
        LVCt LCContract{} -> Just "contract"
        _ -> Nothing
      MichValue _ (SomeValue michValue) -> case michValue of
        VAddress  {} -> Just "address"
        VContract {} -> Just "contract"
        _            -> Nothing
      _ -> Nothing

    typ = getLigoType v
    var = createVariable name varText lang typ menuContext (Just evaluatedText)

  subVars <- buildSubVars lang v

  case subVars of
    [] -> return var
    _ -> do
      idx <- insertVars subVars
      return $ var
        { DAP.variablesReferenceVariable = idx
        }

getInnerTypeFromConstant :: Int -> LigoType -> LigoType
getInnerTypeFromConstant i = \case
  LigoTypeResolved LigoTypeExpression
    { _lteTypeContent = LTCConstant LigoTypeConstant{..}
    } -> LigoType (_ltcParameters ^? ix i)
  _ -> LigoType Nothing

getInnerFieldFromRecord :: Text -> LigoType -> Maybe LigoTableField
getInnerFieldFromRecord name = \case
  LigoTypeResolved LigoTypeExpression
    { _lteTypeContent = LTCRecord
        ( LigoTypeTable
            { _lttFields = hm
            }
        )
    } -> hm HM.!? name
  _ -> Nothing

getInnerTypeFromRecord :: Text -> LigoType -> LigoType
getInnerTypeFromRecord = LigoType ... fmap _ltfAssociatedType ... getInnerFieldFromRecord

getInnerTypeFromSum :: Text -> LigoType -> LigoType
getInnerTypeFromSum name = \case
  LigoTypeResolved LigoTypeExpression
    { _lteTypeContent = LTCSum
        ( LigoTypeTable
            { _lttFields = hm
            }
        )
    } -> LigoType $ _ltfAssociatedType <$> hm HM.!? name
  _ -> LigoType Nothing

getEpAddressChildren :: Lang -> EpAddress -> [Variable]
getEpAddressChildren lang EpAddress'{..} =
  if isDefEpName eaEntrypoint
  then []
  else [addr, ep]
  where
    addr = createVariable "address" (pretty eaAddress) lang (LigoType Nothing) Nothing Nothing
    ep = createVariable "entrypoint" (pretty eaEntrypoint) lang (LigoType Nothing) Nothing Nothing

buildSubVars :: Lang -> LigoOrMichValue -> VariableBuilder [Variable]
buildSubVars lang = \case
  MichValue typ (SomeValue michValue) -> case michValue of
    VOption Nothing -> return []
    VOption (Just v) -> do
      -- Inner type is wrong here. It's hard to extract it here properly.
      (:[]) <$> buildVariable lang (toLigoValue (getInnerTypeFromConstant 0 typ) v) "Some"
    VList lst -> do
      zipWithM (buildVariable lang . toLigoValue (getInnerTypeFromConstant 0 typ)) lst (show <$> [1 :: Int ..])
    VSet s -> do
      zipWithM (buildVariable lang . toLigoValue (getInnerTypeFromConstant 0 typ)) (toList s) (show <$> [1 :: Int ..])
    VMap m -> do
      forM (toPairs m) \(k, v) -> do
        let name = pretty $ debugBuild DpmNormal k
        buildVariable lang (toLigoValue (getInnerTypeFromRecord name typ) v) (toString name)
    VBigMap _id m -> do
      forM (toPairs m) \(k, v) -> do
        let name = pretty $ debugBuild DpmNormal k
        buildVariable lang (toLigoValue (getInnerTypeFromRecord name typ) v) (toString name)
    VContract eaAddress (SomeEpc EntrypointCall{ epcName = eaEntrypoint }) -> do
      pure $ getEpAddressChildren lang EpAddress'{..}
    VAddress epAddress -> pure $ getEpAddressChildren lang epAddress
    -- Other value types do not have nested structure
    _ -> return []
  LigoValue typ ligoValue -> case ligoValue of
    LVCt (LCContract LigoContract{..})
      | Just entrypoint <- lcEntrypoint -> do
          let addr = createVariable "address" (toString lcAddress) lang (LigoType Nothing) Nothing Nothing
          let ep = createVariable "entrypoint" (toString entrypoint) lang (LigoType Nothing) Nothing Nothing
          return [addr, ep]
      | otherwise -> return []
    LVList lst ->
      let innerType = getInnerTypeFromConstant 0 typ in
      zipWithM (buildVariable lang . LigoValue innerType) lst (show <$> [1 :: Int ..])
    value@(LVRecord record) -> case toTupleMaybe value of
      Just values ->
        zipWithM
          do \val n ->
              let innerType = getInnerTypeFromRecord n typ in
              buildVariable lang (LigoValue innerType val) (toString n)
          values
          (show <$> [1 :: Int ..])
      Nothing -> do
        case getRecordOrderMb typ value of
          Just order -> do
            forM order \(name, (t, v)) -> do
              buildVariable lang (LigoValue t v) (toString name)
          Nothing -> do
            forM (toPairs record) \(name, v) -> do
              let innerType = getInnerTypeFromRecord name typ
              buildVariable lang (LigoValue innerType v) (toString name)
    LVConstructor (ctor, value) ->
      let innerType = getInnerTypeFromSum ctor typ in
      (:[]) <$> buildVariable lang (LigoValue innerType value) (toString ctor)
    LVSet s ->
      let innerType = getInnerTypeFromConstant 0 typ in
      zipWithM (buildVariable lang . LigoValue innerType) s (show <$> [1 :: Int ..])
    LVMap m -> do
      forM m \(k, v) -> do
        let keyType = getInnerTypeFromConstant 0 typ
        let valueType = getInnerTypeFromConstant 1 typ

        let name = pretty @_ @String $ debugBuild DpmNormal (lang, LigoValue keyType k)
        buildVariable lang (LigoValue valueType v) name
    _ -> return []
  _ -> return []
  where
    toLigoValue :: (SingI t) => LigoType -> Value t -> LigoOrMichValue
    toLigoValue typ = MichValue typ . SomeValue

{- | Build a lambda variable.

In a trivial case this results in one variable, value of which indicates
a function in abstract way.

In the case when function is a partial application to another function or even
a cascade of partial applications (e.g. @add5 = add2 3@, @add2 = add 2@), to each
such lambda we add children variables:

* One variable for the function that we partially applied to.
  This can recursively have grand children.
* A number of applied arguments.

-}
buildLambdaInfo :: Lang -> String -> LigoType -> LambdaMeta -> VariableBuilder Variable
buildLambdaInfo lang = \name typ meta ->
  processFunc name "<lambda>" typ $ skipTrivialName name $ lmGroupByName meta
  where
    processFunc
      :: String -> String -> LigoType
      -> [(LambdaNamedInfo 'Unique, [LambdaArg])]
      -> VariableBuilder Variable
    processFunc varName varValue varType subFuncs = do
      subVarsRef <- processSubFuncs subFuncs
      return (createVariable varName varValue lang varType Nothing Nothing)
        { DAP.variablesReferenceVariable = subVarsRef ?: 0
        }

    processSubFuncs
      :: [(LambdaNamedInfo 'Unique, [LambdaArg])]
      -> VariableBuilder (Maybe Int)
    processSubFuncs = \case
      [] -> pure Nothing
      (namedInfo, args) : subFuncs' -> Just <$> do
        funVar <- processFunc (toString funConst) (pretty $ lniName namedInfo) (lniType namedInfo) subFuncs'
        argVars <- forM (zip [1 :: Word ..] args)
          \(i, LambdaArg{ laValue = val, laType = ty }) ->
            buildVariable lang (MichValue ty val) [int||arg#{i}|]
        insertVars (funVar : argVars)

    -- If I have a variable @func@, the first event there will likely be naming
    -- with @func@, with no argument applications followed.
    -- This makes sense, but it is also an unnecessary repetition in practice,
    -- and this function strips it.
    skipTrivialName varName = \case
      (namedInfo, []) : others
        | pretty (lniName namedInfo) == varName -> others
      allInfo -> allInfo

-- | Variable value shown for functions.
funConst :: Text
funConst = "<fun>"
