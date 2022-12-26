module Test.Variables
  ( test_Variables
  ) where

import Unsafe (fromJust)

import Data.Default (def)
import Data.Map qualified as M
import Fmt (pretty)
import Morley.Debugger.Protocol.DAP (Variable (..), defaultVariable)
import Morley.Michelson.Typed
  (Constrained (SomeValue), EpAddress (EpAddress'), EpName (UnsafeEpName),
  MkEntrypointCallRes (MkEntrypointCallRes), ParamNotes (pnRootAnn), SingI,
  SomeEntrypointCallT (SomeEpc), T (TUnit), Value,
  Value' (VAddress, VContract, VInt, VList, VOption, VUnit), mkEntrypointCall, sepcPrimitive,
  tyImplicitAccountParam)
import Morley.Michelson.Untyped (Annotation (UnsafeAnnotation), pattern DefEpName)
import Morley.Tezos.Address (parseAddress)
import Text.Interpolation.Nyan

import Test.Tasty (TestTree, testGroup)

import Language.LIGO.DAP.Variables (buildLambdaInfo, createVariables, runBuilder)
import Language.LIGO.Debugger.CLI
import Language.LIGO.Debugger.Functions

import Test.Util

mkDummyValue :: LigoOrMichValue -> Maybe (Name 'Concise) -> (Text, LigoOrMichValue)
mkDummyValue v nameMb =
  ( maybe unknownVariable pretty nameMb
  , v
  )

mkDummyMichValue :: (SingI t) => Value t -> Maybe (Name 'Concise) -> (Text, LigoOrMichValue)
mkDummyMichValue v = mkDummyValue (MichValue (LigoType Nothing) $ SomeValue v)

mkDummyLigoValue :: LigoValue -> Maybe (Name 'Concise) -> (Text, LigoOrMichValue)
mkDummyLigoValue v = mkDummyValue (LigoValue (LigoType Nothing) v)

-- | Create a dummy constant type.
varTy :: Text -> LigoType
varTy = LigoTypeResolved . mkSimpleConstantType

test_Variables :: TestTree
test_Variables = testGroup "variables"
  [ testOption
  , testList
  , testContracts
  , testAddresses
  , testLambdas
  ]

testAddresses :: TestTree
testAddresses = testGroup "addresses"
  [ testCase "address with entrypoint \"foo\"" do
      let epAddress = EpAddress' address (UnsafeEpName "foo")
      let addressItem = mkDummyMichValue (VAddress epAddress) (Just "addr")
      snd (runBuilder $ createVariables Caml [addressItem]) @?=
        M.fromList
          [ (1,
              [ defaultVariable
                  { nameVariable = "address"
                  , valueVariable = "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU"
                  }
              , defaultVariable
                  { nameVariable = "entrypoint"
                  , valueVariable = "foo"
                  }
              ])
          , (2,
              [ defaultVariable
                  { nameVariable = "addr"
                  , valueVariable = "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU%foo"
                  , evaluateNameVariable = Just "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU%foo"
                  , variablesReferenceVariable = 1
                  , __vscodeVariableMenuContextVariable = Just "address"
                  }
              ])
          ]
  , testCase "address without entrypoint" do
      let epAddress = EpAddress' address DefEpName
      let addressItem = mkDummyMichValue (VAddress epAddress) (Just "addr")
      snd (runBuilder $ createVariables Caml [addressItem]) @?=
        M.fromList
          [ (1,
              [ defaultVariable
                { nameVariable = "addr"
                , valueVariable = "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU"
                , evaluateNameVariable = Just "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU"
                , __vscodeVariableMenuContextVariable = Just "address"
                }
              ])
          ]
  ]
  where
    address = fromRight (error "address parse error")
      $ parseAddress "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU"

testContracts :: TestTree
testContracts = testGroup "contracts"
  [ testCase "contract with entrypoint \"foo\"" do
      let paramNotes = tyImplicitAccountParam
            { pnRootAnn = UnsafeAnnotation "foo"
            }
      let mkEntrypoint = fromJust $
            mkEntrypointCall (UnsafeEpName "foo") paramNotes
      case mkEntrypoint of
        MkEntrypointCallRes _ entrypoint -> do
          let contractItem = mkDummyMichValue (VContract address (SomeEpc entrypoint)) (Just "contract")
          snd (runBuilder $ createVariables Caml [contractItem]) @?=
            M.fromList
              [ (1,
                  [ defaultVariable
                      { nameVariable = "address"
                      , valueVariable = "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU"
                      }
                  , defaultVariable
                      { nameVariable = "entrypoint"
                      , valueVariable = "foo"
                      }
                  ])
              , (2,
                  [ defaultVariable
                      { nameVariable = "contract"
                      , valueVariable = "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU%foo"
                      , evaluateNameVariable = Just "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU%foo"
                      , variablesReferenceVariable = 1
                      , __vscodeVariableMenuContextVariable = Just "contract"
                      }
                  ])
              ]
  , testCase "contract without entrypoint" do
      let contractItem = mkDummyMichValue (VContract address (sepcPrimitive @'TUnit)) (Just "contract")
      snd (runBuilder $ createVariables Caml [contractItem]) @?=
        M.fromList
          [ (1,
              [ defaultVariable
                  { nameVariable = "contract"
                  , valueVariable = "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU"
                  , evaluateNameVariable = Just "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU"
                  , __vscodeVariableMenuContextVariable = Just "contract"
                  }
              ])
          ]
  , testCase "LIGO contract" do
      let contract = LigoContract "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU" (Just "foo")
      let contractItem = mkDummyLigoValue (LVCt $ LCContract contract) (Just "addr")
      snd (runBuilder $ createVariables Caml [contractItem]) @?=
        M.fromList
          [ (1,
              [ Variable
                  { nameVariable = "address"
                  , valueVariable = "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU"
                  , typeVariable = ""
                  , presentationHintVariable = Nothing
                  , evaluateNameVariable = Nothing
                  , variablesReferenceVariable = 0
                  , namedVariablesVariable = Nothing
                  , indexedVariablesVariable = Nothing
                  , __vscodeVariableMenuContextVariable = Nothing
                  }
              , Variable
                  { nameVariable = "entrypoint"
                  , valueVariable = "foo"
                  , typeVariable = ""
                  , presentationHintVariable = Nothing
                  , evaluateNameVariable = Nothing
                  , variablesReferenceVariable = 0
                  , namedVariablesVariable = Nothing
                  , indexedVariablesVariable = Nothing
                  , __vscodeVariableMenuContextVariable = Nothing
                  }
              ])
          , (2,
              [ Variable
                  { nameVariable = "addr"
                  , valueVariable = "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU(foo)"
                  , typeVariable = ""
                  , presentationHintVariable = Nothing
                  , evaluateNameVariable = Just "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU(foo)"
                  , variablesReferenceVariable = 1
                  , namedVariablesVariable = Nothing
                  , indexedVariablesVariable = Nothing
                  , __vscodeVariableMenuContextVariable = Just "contract"
                  }
              ])
          ]
  ]
  where
    address = fromRight (error "address parse error")
      $ parseAddress "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU"

testOption :: TestTree
testOption = testGroup "option"
  [ testCase "nothing" do
      let vNothingItem = mkDummyMichValue (VOption @'TUnit Nothing) (Just "nothingVar")
      snd (runBuilder $ createVariables Caml [vNothingItem]) @?=
        M.fromList
          [ (1,
              [ defaultVariable
                  { nameVariable = "nothingVar"
                  , valueVariable = "None"
                  , presentationHintVariable = Nothing
                  , evaluateNameVariable = Just "None"
                  }
              ]
            )
          ]
  , testCase "contains unit" do
      let vUnitItem = mkDummyMichValue (VOption $ Just VUnit) (Just "someUnit")
      snd (runBuilder $ createVariables Caml [vUnitItem]) @?=
        M.fromList
          [ (1,
              [ defaultVariable
                  { nameVariable = "Some"
                  , valueVariable = "()"
                  , evaluateNameVariable = Just "()"
                  }
                ]
              )
          , (2,
              [ defaultVariable
                  { nameVariable = "someUnit"
                  , valueVariable = "Some ()"
                  , evaluateNameVariable = Just "Some ()"
                  , variablesReferenceVariable = 1
                  }
              ]
            )
          ]
  , testCase "LIGO Some" do
      let ligoSome = mkDummyLigoValue (LVConstructor ("Some", LVCt LCUnit)) (Just "someUnit")
      snd (runBuilder $ createVariables Caml [ligoSome]) @?=
        M.fromList
          [ (1,
              [ Variable
                  { nameVariable = "Some"
                  , valueVariable = "()"
                  , typeVariable = ""
                  , presentationHintVariable = Nothing
                  , evaluateNameVariable = Just "()"
                  , variablesReferenceVariable = 0
                  , namedVariablesVariable = Nothing
                  , indexedVariablesVariable = Nothing
                  , __vscodeVariableMenuContextVariable = Nothing
                  }
                ]
              )
          , (2,
              [ Variable
                  { nameVariable = "someUnit"
                  , valueVariable = "Some (())"
                  , typeVariable = ""
                  , presentationHintVariable = Nothing
                  , evaluateNameVariable = Just "Some (())"
                  , variablesReferenceVariable = 1
                  , namedVariablesVariable = Nothing
                  , indexedVariablesVariable = Nothing
                  , __vscodeVariableMenuContextVariable = Nothing
                  }
              ]
            )
          ]
  ]

testList :: TestTree
testList = testGroup "list"
  [ testCase "empty list" do
      let vList = mkDummyMichValue (VList @'TUnit []) (Just "list")
      snd (runBuilder $ createVariables Caml [vList]) @?=
        M.fromList
          [ (1,
              [ defaultVariable
                  { nameVariable = "list"
                  , valueVariable = "[]"
                  , evaluateNameVariable = Just "[]"
                  }
              ]
            )
          ]
  , testCase "list of two units" do
      let vList = mkDummyMichValue (VList [VUnit, VUnit]) (Just "list")
      snd (runBuilder $ createVariables Caml [vList]) @?=
        M.fromList
          [ (1,
              [ defaultVariable
                  { nameVariable = "1"
                  , valueVariable = "()"
                  , evaluateNameVariable = Just "()"
                  }
              , defaultVariable
                  { nameVariable = "2"
                  , valueVariable = "()"
                  , evaluateNameVariable = Just "()"
                  }
              ]
            )
          , (2,
              [ defaultVariable
                  { nameVariable = "list"
                  , valueVariable = "[(), ()]"
                  , evaluateNameVariable = Just "[(), ()]"
                  , variablesReferenceVariable = 1
                  }
              ]
            )
          ]
  , testCase "LIGO list" do
      let ligoList = mkDummyLigoValue (LVList [LVCt LCUnit, LVCt LCUnit]) (Just "list")
      snd (runBuilder $ createVariables Caml [ligoList]) @?=
        M.fromList
          [ (1,
              [ Variable
                  { nameVariable = "1"
                  , valueVariable = "()"
                  , typeVariable = ""
                  , presentationHintVariable = Nothing
                  , evaluateNameVariable = Just "()"
                  , variablesReferenceVariable = 0
                  , namedVariablesVariable = Nothing
                  , indexedVariablesVariable = Nothing
                  , __vscodeVariableMenuContextVariable = Nothing
                  }
              , Variable
                  { nameVariable = "2"
                  , valueVariable = "()"
                  , typeVariable = ""
                  , presentationHintVariable = Nothing
                  , evaluateNameVariable = Just "()"
                  , variablesReferenceVariable = 0
                  , namedVariablesVariable = Nothing
                  , indexedVariablesVariable = Nothing
                  , __vscodeVariableMenuContextVariable = Nothing
                  }
              ]
            )
          , (2,
              [ Variable
                  { nameVariable = "list"
                  , valueVariable = "[(); ()]"
                  , typeVariable = ""
                  , presentationHintVariable = Nothing
                  , evaluateNameVariable = Just "[(); ()]"
                  , variablesReferenceVariable = 1
                  , namedVariablesVariable = Nothing
                  , indexedVariablesVariable = Nothing
                  , __vscodeVariableMenuContextVariable = Nothing
                  }
              ]
            )
          ]
  ]

testLambdas :: TestTree
testLambdas = testGroup "lambdas"
  let
    lambdaNamedEvent :: Text -> LambdaEvent u
    lambdaNamedEvent name = LambdaNamed LambdaNamedInfo
      { lniName = Name name
      , lniType = varTy [int||type of #{name}|]
      }
    lambdaAppliedNum :: Integer -> LambdaEvent u
    lambdaAppliedNum i = LambdaApplied LambdaArg
      { laValue = SomeValue (VInt i)
      , laType = varTy [int||type of #{i}|]
      }
  in
  [ testCase "complex case" do
      let (rootVar, referredVars) = runBuilder do
            buildLambdaInfo Caml "the var" (varTy "f") $
              LambdaMeta
              [ lambdaNamedEvent "add11"
              , lambdaAppliedNum 5
              , lambdaNamedEvent "addTwo"
              , lambdaNamedEvent "add2"
              , lambdaAppliedNum 3
              , lambdaAppliedNum 2
              , lambdaAppliedNum 1
              , lambdaNamedEvent "add"
              ]

      rootVar @?=
        defaultVariable
          { nameVariable = "the var"
          , valueVariable = "<lambda>"
          , typeVariable = "f"
          , variablesReferenceVariable = 4
          }
      M.lookup 5 referredVars @?= Nothing
      M.lookup 4 referredVars @?= Just
        [ defaultVariable
            { nameVariable = "<fun>"
            , valueVariable = "add11"
            , typeVariable = "type of add11"
            , variablesReferenceVariable = 3
            }
        ]
      M.lookup 3 referredVars @?= Just
        [ defaultVariable
            { nameVariable = "<fun>"
            , valueVariable = "addTwo"
            , typeVariable = "type of addTwo"
            , variablesReferenceVariable = 2
            }
        , defaultVariable
            { nameVariable = "arg1"
            , valueVariable = "5"
            , typeVariable = "type of 5"
            , evaluateNameVariable = Just "5"
            }
        ]
      M.lookup 2 referredVars @?= Just
        [ defaultVariable
            { nameVariable = "<fun>"
            , valueVariable = "add2"
            , typeVariable = "type of add2"
            , variablesReferenceVariable = 1
            }
        ]
      M.lookup 1 referredVars @?= Just
        [ defaultVariable
            { nameVariable = "<fun>"
            , valueVariable = "add"
            , typeVariable = "type of add"
            }
        , defaultVariable
            { nameVariable = "arg1"
            , valueVariable = "1"
            , typeVariable = "type of 1"
            , evaluateNameVariable = Just "1"
            }
        , defaultVariable
            { nameVariable = "arg2"
            , valueVariable = "2"
            , typeVariable = "type of 2"
            , evaluateNameVariable = Just "2"
            }
        , defaultVariable
            { nameVariable = "arg3"
            , valueVariable = "3"
            , typeVariable = "type of 3"
            , evaluateNameVariable = Just "3"
            }
        ]

  , testCase "applications before the first naming do not wreak havoc" do
      let (rootVar, referredVars) = runBuilder do
            buildLambdaInfo Caml "the var" (varTy "f") $
              LambdaMeta
              [ lambdaAppliedNum 1
              , lambdaNamedEvent "add"
              , lambdaAppliedNum (-1)
              ]

      rootVar @?=
        defaultVariable
          { nameVariable = "the var"
          , valueVariable = "<lambda>"
          , typeVariable = "f"
          , variablesReferenceVariable = 1
          }
      M.lookup 2 referredVars @?= Nothing
      M.lookup 1 referredVars @?= Just
        [ defaultVariable
            { nameVariable = "<fun>"
            , valueVariable = "add"
            , typeVariable = "type of add"
            , variablesReferenceVariable = 0
            }
        , defaultVariable
            { nameVariable = "arg1"
            , valueVariable = "1"
            , typeVariable = "type of 1"
            , evaluateNameVariable = Just "1"
            }
        ]

  , testCase "empty meta works too" do
      let (rootVar, referredVars) = runBuilder do
            buildLambdaInfo Caml "the var" (varTy "f") def

      rootVar @?=
        defaultVariable
          { nameVariable = "the var"
          , valueVariable = "<lambda>"
          , typeVariable = "f"
          }

      referredVars @?= mempty

  ]
