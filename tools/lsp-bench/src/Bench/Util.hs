module Bench.Util
  (
    benchLspSession,
    getDoc,
    Doc (..),
    insertToDoc,
    insertToDocKeystrokes,
    openLigoDoc,
    projectWithOneBigFile,
    projectWithOneSmallFile,
    projectBatcher,
    withBothEditsAndKeystrokes,
  )
where

import Criterion
import Data.Row.Records
import Language.LSP.Protocol.Capabilities qualified as LSP
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Test (SessionConfig (logStdErr))
import Language.LSP.Test qualified as LSP
import System.Directory (canonicalizePath)
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)
import Universum

-- one can set LIGO_LSP_TEST_EXE env var before testing/benchmarking to test a different lsp server
-- with our requests
serverCommand :: String
serverCommand =
  unsafePerformIO $
    fromMaybe "ligo lsp"
      <$> lookupEnv "LIGO_LSP_TEST_EXE"
{-# NOINLINE serverCommand #-}

projectWithOneBigFile :: FilePath
projectWithOneBigFile = unsafePerformIO $ canonicalizePath "projects/one_big_file/"
{-# NOINLINE projectWithOneBigFile #-}

projectWithOneSmallFile :: FilePath
projectWithOneSmallFile = unsafePerformIO $ canonicalizePath "projects/one_small_file/"
{-# NOINLINE projectWithOneSmallFile #-}

projectBatcher :: FilePath
projectBatcher = unsafePerformIO $ canonicalizePath "projects/submodules/"
{-# NOINLINE projectBatcher #-}

openLigoDoc :: FilePath -> LSP.Session LSP.TextDocumentIdentifier
openLigoDoc fp = LSP.openDoc fp "ligo"

benchLspSession ::
  (NFData a) =>
  String ->
  FilePath ->
  LSP.Session a ->
  Benchmark
benchLspSession description project session =
  bench description $
    nfIO $
      LSP.runSessionWithConfig
        LSP.defaultConfig {logStdErr = True}
        serverCommand
        (LSP.capsForVersion $ LSP.LSPVersion 3 0)
        project
        session

getDoc :: Doc -> LSP.Session LSP.TextDocumentIdentifier
getDoc (OpenedDoc tdi) = pure tdi
getDoc (FileDoc path) = openLigoDoc path

data Doc
  = -- | Document on disc to be opened (path relative to project)
    FileDoc String
  | -- | Document that was already opened in this session
    OpenedDoc LSP.TextDocumentIdentifier
  deriving stock (Show)

insertToDoc, insertToDocKeystrokes :: Doc -> (LSP.UInt, LSP.UInt) -> Text -> LSP.Session ()
insertToDoc doc (line, col) text =
  getDoc doc >>= \tdi ->
    LSP.changeDoc tdi [LSP.TextDocumentContentChangeEvent $ LSP.InL $ #range .== LSP.Range pos pos .+ #rangeLength .== Nothing .+ #text .== text]
  where
    pos = LSP.Position (line - 1) (col - 1)

-- | Like insertToDoc but simulates typing
insertToDocKeystrokes doc (line, col) text =
  getDoc doc >>= \tdi ->
    for_ (Universum.zip (toString text) [col ..]) $
      \(char, activeCol) -> insertToDoc (OpenedDoc tdi) (line, activeCol) (one char)

withBothEditsAndKeystrokes :: (String -> (Doc -> (LSP.UInt, LSP.UInt) -> Text -> LSP.Session ()) -> a) -> (a,a)
withBothEditsAndKeystrokes f = (f "edits" insertToDoc, f "keystrokes" insertToDocKeystrokes)
