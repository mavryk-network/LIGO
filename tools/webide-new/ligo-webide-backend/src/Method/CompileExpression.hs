module Method.CompileExpression (compileExpression) where

import Control.Monad.Except (throwError)
import Data.Text qualified as Text
import Servant (err400, errBody)
import System.Exit (ExitCode(ExitFailure, ExitSuccess))

import Common (WebIDEM)
import Ligo (runLigo)
import Schema.CompileExpressionRequest (CompileExpressionRequest(..))
import Schema.CompilerResponse (CompilerResponse(..))
import Source (withProject)
import Types (prettyDisplayFormat)

compileExpression :: CompileExpressionRequest -> WebIDEM CompilerResponse
compileExpression request =
  withProject (cerProject request) $ \(dirPath, fullMainPath) -> do
    dialect <- case inferDialect fullMainPath of
      Nothing -> lift . throwError $ err400
        {errBody = "couldn't infer dialect from filetype"}
      Just d -> pure d

    (ec, out, err) <- runLigo dirPath $
      ["compile", "expression"]
      ++ [prettyDialect dialect, Text.unpack (cerFunction request)]
      ++ maybe []
           (\df -> ["--display-format", prettyDisplayFormat df])
           (cerDisplayFormat request)
      ++ maybe [] (\p -> ["-p", Text.unpack p]) (cerProtocol request)
      ++ ["--init-file", fullMainPath]

    case ec of
      ExitSuccess -> pure (CompilerResponse $ Text.pack out)
      ExitFailure _ -> pure (CompilerResponse $ Text.pack err)

data Dialect = CameLIGO | PascaLIGO | JsLIGO
  deriving stock (Eq, Show, Ord, Enum)

prettyDialect :: Dialect -> String
prettyDialect = \case
  CameLIGO -> "cameligo"
  PascaLIGO -> "pascaligo"
  JsLIGO -> "jsligo"

inferDialect :: FilePath -> Maybe Dialect
inferDialect filepath =
  case Text.takeWhileEnd (/= '.') (Text.pack filepath) of
    "mligo" -> Just CameLIGO
    "ligo" -> Just PascaLIGO
    "jsligo" -> Just JsLIGO
    _ -> Nothing
