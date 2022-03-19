module Extension
  ( ElimExt (..)
  , Lang (..)
  , UnsupportedExtension (..)
  , extGlobs
  , getExt
  , onExt
  , supportedExtensions
  ) where

import Control.Exception (Exception)
import Control.Monad.Except (MonadError (throwError))
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Text qualified as Text
import System.FilePath

import AST.Skeleton (Lang (..))

data ElimExt a = ElimExt
  { eePascal :: a
  , eeCaml   :: a
  , eeReason :: a
  }

newtype UnsupportedExtension = UnsupportedExtension String
  deriving stock Show
  deriving anyclass Exception

-- TODO: 'lsp' uses the 'Glob' package to deal with globs, but it doesn't
-- support braced globs such as "{,m,re}ligo" even though the LSP spec allows
-- it. Because of this, we return multiple globs instead of one single glob.
extGlobs :: [Text]
extGlobs = Text.pack . (("**" </>) . ("*" <>)) <$> supportedExtensions

getExt :: MonadError UnsupportedExtension m => FilePath -> m Lang
getExt path =
  case takeExtension path of
    ".religo" -> return Reason
    ".ligo"   -> return Pascal
    ".mligo"  -> return Caml
    ext       -> throwError $ UnsupportedExtension ext

onExt :: MonadError UnsupportedExtension m => ElimExt a -> FilePath -> m a
onExt ee path =
  getExt path <&> \case
    Pascal -> eePascal ee
    Caml   -> eeCaml   ee
    Reason -> eeReason ee

supportedExtensions :: [FilePath]
supportedExtensions = [".ligo", ".mligo", ".religo"]
