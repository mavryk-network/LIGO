module Test.Common.Capabilities.Find
  ( DefinitionReferenceInvariant (..)

  , localTypeOf

  , findDefinitionAndGoToReferencesCorrespondence
  , definitionOfId
  , definitionOfLeft
  , referenceOfId
  , referenceOfLeft
  , definitionOfXInWildcard
  , referenceOfXInWildcard

  , typeOfHeapArg
  , typeOfHeapConst
  , typeOfLet
  , typeOfPascaligoLambdaArg
  , pascaligoLocalType

  , contractsDir
  , invariants
  ) where

import System.Directory (makeAbsolute)
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase)
import Text.Printf (printf)

import AST.Capabilities.Find (definitionOf, findScopedDecl, referencesOf, typeDefinitionAt)
import AST.Scope (KnownScopingSystem (..), ScopingSystem (..), contractTree, lookupContract)
import AST.Scope.ScopedDecl (DeclarationSpecifics (..), ScopedDecl (..), ValueDeclSpecifics (..))
import Cli (TempDir (..), TempSettings (..))
import Range (Range (..), interval, point)

import Test.Common.Capabilities.Util qualified as Common (contractsDir)
import Test.Common.FixedExpectations (expectationFailure, shouldBe, shouldContain, shouldMatchList)
import Test.Common.Util
  (ScopeTester, parseContractsWithDependenciesScopes, readContractWithScopes, tempTemplate)

contractsDir :: FilePath
contractsDir = Common.contractsDir </> "find"

-- | Represents an invariant relation between references and a
-- definition of some LIGO entity (a variable, a type etc).
--
-- To be more precise, for a given definition 'd' and a set of
-- references 'rs', the following holds:
--
-- > ∀ r ∈ rs∪{d}. definitionOf(r) = d ∧ referencesOf(r) = rs
--
-- If a definition is not given (i.e. 'Nothing'), check that no
-- reference has a definition.
--
-- > ∀ r ∈ rs. ¬definitionOf(r)
data DefinitionReferenceInvariant = DefinitionReferenceInvariant
  { driFile :: FilePath
  -- ^ origin file
  , driDesc :: String
  -- ^ textual representation of the entity
  , driDef :: Maybe Range
  -- ^ definition range, 'Nothing' if there should be no definition
  , driRefs :: [Range]
  -- ^ references ranges
  }

-- | Check that the given @DefinitionReferenceInvariant@s hold.
checkDefinitionReferenceInvariants
  :: forall parser. ScopeTester parser => [DefinitionReferenceInvariant] -> IO [TestTree]
checkDefinitionReferenceInvariants tests = do
  let temp = TempSettings contractsDir $ GenerateDir tempTemplate
  contractsDir' <- makeAbsolute contractsDir
  graph <- parseContractsWithDependenciesScopes @parser temp contractsDir'
  pure $ tests <&> \DefinitionReferenceInvariant{..} ->
    testCase (driFile <> ": " <> driDesc) do
      driFile' <- makeAbsolute driFile
      -- a tree parser labels ranges with files, so we should too to preserve equality
      driRefs' <- traverse (label driFile') driRefs

      let Just tree = contractTree <$> lookupContract driFile' graph
      case driDef of
        Nothing ->
          for_ driRefs' \mention ->
            definitionOf mention tree `shouldBe` Nothing
        Just expectedDef -> do
          expectedDef' <- label driFile' expectedDef
          definitionOf expectedDef' tree `shouldBe` Just expectedDef'
          case referencesOf expectedDef' tree of
            Nothing -> expectationFailure $
              printf "References of '%s' from '%s' are not found." driDesc driFile'
            Just actualRefs -> actualRefs `shouldMatchList` expectedDef' : driRefs'

label :: FilePath -> Range -> IO Range
label filepath r
  | null (_rFile r) = pure r{_rFile = filepath}
  | otherwise       = fmap (\fp -> r{_rFile = fp}) (makeAbsolute $ _rFile r)

-- | Check if the given range corresponds to a definition of the given
-- entity in the given file.
checkIfDefinition :: forall parser. ScopeTester parser => FilePath -> Range -> Range -> Assertion
checkIfDefinition filepath expectedDef mention = test
  where
    test :: Assertion
    test = do
      filepath' <- makeAbsolute filepath
      expectedDef' <- label filepath' expectedDef
      mention' <- label filepath' mention
      tree <- readContractWithScopes @parser filepath'
      definitionOf mention' tree `shouldBe` Just expectedDef'

-- | Check if the given range corresponds to a reference of the given
-- entity in the given file.
checkIfReference :: forall parser. ScopeTester parser => FilePath -> Range -> Range -> Assertion
checkIfReference filepath expectedRef mention = test
  where
    test :: Assertion
    test = do
      filepath' <- makeAbsolute filepath
      expectedRef' <- label filepath' expectedRef
      mention' <- label filepath' mention
      tree <- readContractWithScopes @parser filepath'
      case referencesOf mention' tree of
        Nothing -> expectationFailure $
          printf "References in range '%s' from '%s' are not found."
            (show @String mention') filepath'
        Just references -> references `shouldContain` [expectedRef']

invariants :: [DefinitionReferenceInvariant]
invariants =
  [ DefinitionReferenceInvariant
    { driFile = contractsDir </> "id.ligo"
    , driDesc = "i, parameter"
    , driDef = Just (interval 1 20 21)
    , driRefs = [interval 1 38 39]
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "id.ligo"
    , driDesc = "id"
    , driDef = Just (interval 1 10 12)
    , driRefs = []
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "heap.ligo"
    , driDesc = "get_top"
    , driDef = Just (interval 8 10 17)
    , driRefs = [ interval 65 31 38
                , interval 23 31 38
                , interval 11 30 37
                ]
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "heap.ligo"
    , driDesc = "left, local"
    , driDef = Just (interval 73 9 13)
    , driRefs = [ interval 95 15 19
                , interval 86 13 17
                , interval 85 30 34
                , interval 83 22 26
                , interval 82 36 40
                , interval 81 10 14
                , interval 80 16 20
                , interval 79 7 11
                ]
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "params.mligo"
    , driDesc = "a, function"
    , driDef = Just (interval 1 5 6)
    , driRefs = []
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "params.mligo"
    , driDesc = "a, param"
    , driDef = Just (interval 3 11 12)
    , driRefs = [ interval 3 36 37 ]
    }

  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "type-attributes.ligo"
    , driDesc = "counter, type attribute"
    , driDef = Nothing -- type attributes don't have a declaration
    , driRefs = [interval 9 47 54, interval 13 34 41]
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "type-attributes.ligo"
    , driDesc = "counter, function"
    , driDef = Just (interval 7 10 17)
    , driRefs = []
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "type-attributes-in-rec.ligo"
    , driDesc = "counter, type attribute"
    , driDef = Nothing -- type attributes don't have a declaration
    , driRefs = [interval 9 47 54, interval 13 34 41]
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "type-attributes-in-rec.ligo"
    , driDesc = "counter, function"
    , driDef = Just (interval 7 20 27)
    , driRefs = []
    }

  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "type-attributes.mligo"
    , driDesc = "counter, type attribute"
    , driDef = Nothing  -- type attributes don't have a declaration
    , driRefs = [interval 9 3 10, interval 7 35 42]
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "type-attributes.mligo"
    , driDesc = "counter, function"
    , driDef = Just (interval 6 5 12)
    , driRefs = []
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "type-attributes-tuple.mligo"
    , driDesc = "tuple member"
    , driDef = Just (interval 2 12 15)
    , driRefs = [interval 3 3 6]
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "type-attributes-tuple.religo"
    , driDesc = "tuple member"
    , driDef = Just (interval 2 13 16)
    , driRefs = [interval 3 3 6]
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "type-attributes-tuple.jsligo"
    , driDesc = "tuple member"
    , driDef = Just (interval 2 15 18)
    , driRefs = [interval 3 10 13]
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "type-attributes-in-rec.mligo"
    , driDesc = "counter, type attribute"
    , driDef = Nothing  -- type attributes don't have a declaration
    , driRefs = [interval 9 3 10, interval 7 35 42]
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "type-attributes-in-rec.mligo"
    , driDesc = "counter, function"
    , driDef = Just (interval 6 9 16)
    , driRefs = []
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "type-attributes.religo"
    , driDesc = "counter, type attribute"
    , driDef = Nothing  -- type attributes don't have a declaration
    , driRefs = [interval 9 14 21, interval 7 35 42]
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "type-attributes.religo"
    , driDesc = "counter, function"
    , driDef = Just (interval 6 5 12)
    , driRefs = []
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "type-attributes-in-rec.religo"
    , driDesc = "counter, type attribute"
    , driDef = Nothing  -- type attributes don't have a declaration
    , driRefs = [interval 9 14 21, interval 7 35 42]
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "type-attributes-in-rec.religo"
    , driDesc = "counter, function"
    , driDef = Just (interval 6 9 16)
    , driRefs = []
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "type-attributes.jsligo"
    , driDesc = "counter, type attribute"
    , driDef = Nothing  -- type attributes don't have a declaration
    , driRefs = [interval 9 14 21, interval 7 35 42]
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "type-attributes.jsligo"
    , driDesc = "counter, function"
    , driDef = Just (interval 6 5 12)
    , driRefs = []
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "type-attributes-in-rec.jsligo"
    , driDesc = "counter, type attribute"
    , driDef = Nothing  -- type attributes don't have a declaration
    , driRefs = [interval 9 14 21, interval 7 35 42]
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "type-attributes-in-rec.jsligo"
    , driDesc = "counter, function"
    , driDef = Just (interval 6 5 12)
    , driRefs = []
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "recursion.ligo"
    , driDesc = "sum"
    , driDef = Just (interval 1 20 23)
    , driRefs = [interval 2 26 29]
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "recursion.mligo"
    , driDesc = "sum"
    , driDef = Just (interval 1 9 12)
    , driRefs = [interval 2 30 33]
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "recursion.jsligo"
    , driDesc = "sum"
    , driDef = Just (interval 1 5 8)
    , driRefs = [interval 5 16 19]
    }
  -- Test variable shadowing (including interaction between type names, parameters and variable names)
  -- that contains several declaration with the same names
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "name-shadowing.mligo"
    , driDesc = "type name"
    , driDef  = Just (interval 1 6 12)
    , driRefs = [interval 3 19 25, interval 3 29 35]
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "name-shadowing.mligo"
    , driDesc = "parameter"
    , driDef  = Just (interval 3 10 16)
    , driRefs = [interval 4 16 22]
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "name-shadowing.mligo"
    , driDesc = "first local variable"
    , driDef  = Just (interval 4 7 13)
    , driRefs = [interval 5 16 22]
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "name-shadowing.mligo"
    , driDesc = "second local variable"
    , driDef  = Just (interval 5 7 13)
    , driRefs = [interval 6 3 9]
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "recursion.religo"
    , driDesc = "sum"
    , driDef = Just (interval 1 9 12)
    , driRefs = [interval 2 29 32]
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "type-constructor.ligo"
    , driDesc = "Increment, type constructor"
    , driDef = Just (interval 2 3 12)
    , driRefs = [interval 5 20 29]
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "type-constructor.mligo"
    , driDesc = "Increment, type constructor"
    , driDef = Just (interval 2 3 12)
    , driRefs = [interval 5 18 27]
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "type-constructor.religo"
    , driDesc = "Increment, type constructor"
    , driDef = Just (interval 2 5 14)
    , driRefs = [interval 5 18 27]
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "type-constructor.jsligo"
    , driDesc = "Increment, type constructor"
    , driDef = Just (interval 2 4 15)
    , driRefs = [interval 5 18 27]
    }

  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "modules.jsligo"
    , driDesc = "Modules, B.titi"
    , driDef = Just (interval 2 17 21)
    , driRefs = [interval 6 26 30]
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "modules.jsligo"
    , driDesc = "Modules, A.add"
    , driDef = Just (interval 10 16 19)
    , driRefs = [interval 20 48 51]
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "modules.jsligo"
    , driDesc = "Modules, E.toto resolves in A.C.toto"
    , driDef = Just (interval 8 20 24)
    , driRefs = [interval 18 7 11]
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "modules.jsligo"
    , driDesc = "Modules, D.C resolves in A.C"
    , driDef = Just (interval 7 22 23)
    , driRefs = [interval 15 14 15]
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "modules.jsligo"
    , driDesc = "Modules, A.titi can be referenced within A.C"
    , driDef = Just (interval 6 17 21)
    , driRefs =
      -- FIXME (LIGO-754): Handle references in lambdas to uncomment these two
      -- intervals.
      [ interval 8 26 30, interval 10 26 30, interval 10 35 39 -- interval 10 43 47
      , interval 17 14 18, interval 20 17 21, interval 20 28 32 -- interval 20 38 42
      ]
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "modules.ligo"
    , driDesc = "Modules, B.titi"
    , driDef = Just (interval 2 10 14)
    , driRefs = [interval 6 20 24]
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "modules.ligo"
    , driDesc = "Modules, A.add"
    , driDef = Just (interval 10 14 17)
    , driRefs = [interval 19 65 68]
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "modules.ligo"
    , driDesc = "Modules, E.toto resolves in A.C.toto"
    , driDef = Just (interval 8 15 19)
    , driRefs = [interval 17 10 14]
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "modules.ligo"
    , driDesc = "Modules, D.C resolves in A.C"
    , driDef = Just (interval 7 12 13)
    , driRefs = [interval 16 19 20]
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "modules.ligo"
    , driDesc = "Modules, A.titi can be referenced within A.C"
    , driDef = Just (interval 6 10 14)
    , driRefs =
      [ interval 8 22 26, interval 10 29 33, interval 10 45 49, interval 10 53 57
      , interval 15 16 20, interval 19 27 31, interval 19 45 49, interval 19 55 59
      ]
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "modules.mligo"
    , driDesc = "Modules, B.titi"
    , driDef = Just (interval 2 10 14)
    , driRefs = [interval 6 19 23]
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "modules.mligo"
    , driDesc = "Modules, A.add"
    , driDef = Just (interval 10 9 12)
    , driRefs = [interval 19 47 50]
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "modules.mligo"
    , driDesc = "Modules, E.toto resolves in A.C.toto"
    , driDef = Just (interval 8 13 17)
    , driRefs = [interval 17 7 11]
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "modules.mligo"
    , driDesc = "Modules, D.C resolves in A.C"
    , driDef = Just (interval 7 12 13)
    , driRefs = [interval 16 18 19]
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "modules.mligo"
    , driDesc = "Modules, A.titi can be referenced within A.C"
    , driDef = Just (interval 6 10 14)
    , driRefs =
      [ interval 8 19 23, interval 10 16 20, interval 10 25 29, interval 10 33 37
      , interval 15 14 18, interval 19 19 23, interval 19 28 32, interval 19 38 42
      ]
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "modules.religo"
    , driDesc = "Modules, B.titi"
    , driDef = Just (interval 2 10 14)
    , driRefs = [interval 6 19 23]
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "modules.religo"
    , driDesc = "Modules, A.add"
    , driDef = Just (interval 10 9 12)
    , driRefs = [interval 19 52 55]
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "modules.religo"
    , driDesc = "Modules, E.toto resolves in A.C.toto"
    , driDef = Just (interval 8 13 17)
    , driRefs = [interval 17 7 11]
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "modules.religo"
    , driDesc = "Modules, D.C resolves in A.C"
    , driDef = Just (interval 7 12 13)
    , driRefs = [interval 16 18 19]
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "modules.religo"
    , driDesc = "Modules, A.titi can be referenced within A.C"
    , driDef = Just (interval 6 10 14)
    , driRefs =
      -- FIXME (LIGO-754): Handle references in lambdas to uncomment these two
      -- intervals.
      [ interval 8 19 23, interval 10 25 29, interval 10 30 34 -- interval 10 39 43
      , interval 15 14 18, interval 19 23 27, interval 19 31 35 -- interval 19 42 46
      ]
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "nested-modules.jsligo"
    , driDesc = "Modules, Cz.nested resolves in A.B.C.nested"
    , driDef = Just (interval 4 18 24)
    , driRefs = [interval 15 20 26]
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "undefined.jsligo"
    , driDesc = "Parametric types, can find references of a type variable (T)"
    , driDef = Just (interval 1 18 19)
    , driRefs = [interval 1 21 22, interval 1 50 51]
    }
  , DefinitionReferenceInvariant
    { driFile = contractsDir </> "parametric.religo"
    , driDesc = "Parametric types, can find references of a type variable (T)"
    , driDef = Just (interval 1 16 17)
    , driRefs = [interval 1 23 24, interval 1 34 35]
    }
  ]

findDefinitionAndGoToReferencesCorrespondence
  :: forall impl
   . ScopeTester impl
  => [DefinitionReferenceInvariant]
  -> IO TestTree
findDefinitionAndGoToReferencesCorrespondence =
  fmap (testGroup "Definition and References Correspondence")
  . checkDefinitionReferenceInvariants @impl

definitionOfId :: forall impl. ScopeTester impl => Assertion
definitionOfId = checkIfDefinition @impl
                        (contractsDir </> "id.ligo")
                        (interval 1 20 21)
                        (interval 1 38 39)

referenceOfId :: forall impl. ScopeTester impl => Assertion
referenceOfId = checkIfReference @impl
                       (contractsDir </> "id.ligo")
                       (interval 1 38 39)
                       (interval 1 20 21)

definitionOfLeft :: forall impl. ScopeTester impl => Assertion
definitionOfLeft = checkIfDefinition @impl
                          (contractsDir </> "heap.ligo")
                          (interval 73 9 13)
                          (interval 82 36 40)

referenceOfLeft :: forall impl. ScopeTester impl => Assertion
referenceOfLeft = checkIfReference @impl
                         (contractsDir </> "heap.ligo")
                         (interval 85 30 34)
                         (interval 73 9 13)

definitionOfXInWildcard :: forall impl. ScopeTester impl => Assertion
definitionOfXInWildcard = checkIfDefinition @impl
  (contractsDir </> "wildcard.mligo")
  (interval 1 5 6)
  (interval 2 13 14)

referenceOfXInWildcard :: forall impl. ScopeTester impl => Assertion
referenceOfXInWildcard = checkIfReference @impl
  (contractsDir </> "wildcard.mligo")
  (interval 2 13 14)
  (interval 1 5 6)

localTypeOf
  :: forall impl. ScopeTester impl
  => FilePath
  -> Range
  -> ValueDeclSpecifics
  -> Assertion
localTypeOf filepath mention typeOfMention = do
  filepath' <- makeAbsolute filepath
  mention' <- label filepath' mention
  tree <- readContractWithScopes @impl filepath'
  case findScopedDecl mention' tree of
    Nothing -> expectationFailure "Should find a declaration"
    Just decl -> do
      let ValueSpec valueDeclSpec = _sdSpec decl
      specRange <- traverse (label filepath') (_vdsInitRange valueDeclSpec)
      definition' <- traverse (label filepath') (_vdsInitRange typeOfMention)
      let typeOfMention' = typeOfMention{_vdsInitRange = definition'}
      valueDeclSpec{_vdsInitRange = specRange} `shouldBe` typeOfMention'

typeOf
  :: forall impl. ScopeTester impl
  => FilePath
  -> Range
  -> Range
  -> Assertion
typeOf filepath mention definition = do
  filepath' <- makeAbsolute filepath
  mention' <- label filepath' mention
  definition' <- label filepath' definition
  tree <- readContractWithScopes @impl filepath'
  case typeDefinitionAt mention' tree of
    Nothing -> expectationFailure "Should find type definition"
    Just range -> range{_rFile=_rFile mention'} `shouldBe` definition'

typeOfHeapConst :: forall impl. ScopeTester impl => Assertion
typeOfHeapConst = typeOf @impl (contractsDir </> "heap.ligo") (point 102 8) expected where
  expected = case knownScopingSystem @impl of
    CompilerScopes -> interval 102 15 19 -- FIXME LIGO-593
    _ -> interval 4 6 10

typeOfHeapArg :: forall impl. ScopeTester impl => Assertion
typeOfHeapArg = typeOf @impl (contractsDir </> "heap.ligo") (point 8 25) (interval 4 6 10)

typeOfLet :: forall impl. ScopeTester impl => Assertion
typeOfLet = typeOf @impl (contractsDir </> "type-attributes.mligo") (point 7 10) (interval 1 6 20)

typeOfPascaligoLambdaArg :: forall impl. ScopeTester impl => Assertion
typeOfPascaligoLambdaArg = typeOf @impl (contractsDir </> "lambda.ligo") (point 4 21) (interval 1 6 12)

pascaligoLocalType :: forall impl. ScopeTester impl => Assertion
pascaligoLocalType = typeOf @impl (contractsDir </> "local_type.ligo") (point 3 23) (interval 2 8 12)

-- See LIGO-110
-- typeOfCamligoLambdaArg :: Assertion
-- typeOfCamligoLambdaArg = typeOf (contractsDir </> "type-attributes.mligo") (point 8 52) (interval 1 6 20)
