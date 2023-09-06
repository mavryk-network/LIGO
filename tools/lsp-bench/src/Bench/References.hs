module Bench.References
  ( ReferencesRequest(..)
  , requestReferences

  , bench_simple_references
  , bench_sequence_references
  ) where

import Universum

import Criterion
import Language.LSP.Test qualified as LSP
import Language.LSP.Protocol.Types qualified as LSP

import Bench.Util

data ReferencesRequest = ReferencesRequest
  { rrProject :: FilePath
    -- ^ Absolute file path to ligo project
  , rrFile :: Doc
  -- ^ path to file (relative to project)
  , rrPos :: (LSP.UInt, LSP.UInt)
    -- ^ line/column as in VSCode (cursor is at the start of the word we want to get references)
  , rrExpectedAmount :: Int
    -- ^ number of references we expect, including definition
  } deriving stock Show

requestReferences :: ReferencesRequest -> LSP.Session [LSP.Location]
requestReferences rr@ReferencesRequest{..} = do
  doc <- getDoc rrFile
  refs <- LSP.getReferences doc (LSP.Position (line - 1) (col - 1)) True
  if length refs == rrExpectedAmount
    then return refs
    else fail $ "Request " <> show rr <> " returned unexpected amount of references: "
      <> show (length refs)
  where
    (line, col) = rrPos

bench_simple_references :: [Benchmark]
bench_simple_references =
  [ bgroup "Simple references"
    [ simpleReferencesBench rr
    | rr <- referencesOneBigFile
    ]
  ]

simpleReferencesBench :: ReferencesRequest -> Benchmark
simpleReferencesBench rr@ReferencesRequest{..} =
  benchLspSession (show rrFile) rrProject $ requestReferences rr

referencesOneBigFile :: [ReferencesRequest]
referencesOneBigFile =
  [ ReferencesRequest -- a012
      { rrProject = projectWithOneBigFile
      , rrFile = FileDoc "one_big_file.mligo"
      , rrPos = (13, 5)
      , rrExpectedAmount = 2
      }
  , ReferencesRequest -- a013
      { rrProject = projectWithOneBigFile
      , rrFile = FileDoc "one_big_file.mligo"
      , rrPos = (13, 5)
      , rrExpectedAmount = 2
      }
  , ReferencesRequest -- a014
      { rrProject = projectWithOneBigFile
      , rrFile = FileDoc "one_big_file.mligo"
      , rrPos = (13, 5)
      , rrExpectedAmount = 2
      }
  ]

-- request all references from a project in one session
-- to reduce influence of initializing costs
bench_sequence_references :: [Benchmark]
bench_sequence_references =
  [ benchLspSession
      ("sequence references " <> projectName)
      project
      (mapM requestReferences requests)
  | (projectName, project, requests) <-
    [ ("one_big_file", projectWithOneBigFile, referencesOneBigFile)
    ]
  ]
