module Schema.DryRunRequest (DryRunRequest (..)) where

import Data.Aeson
  (FromJSON, ToJSON, defaultOptions, fieldLabelModifier, genericParseJSON, genericToJSON, parseJSON,
  toJSON)
import Data.Swagger.Schema
  (ToSchema, declareNamedSchema, defaultSchemaOptions, fieldLabelModifier,
  genericDeclareNamedSchema)
import Data.Text (Text)
import GHC.Generics (Generic)

import Source (Project(..))
import Types (DisplayFormat(..))
import Util (prepareField)

data DryRunRequest = DryRunRequest
  { drrProject :: Project
  , drrParameters :: Text
  , drrStorage :: Text
  , drrEntrypoint :: Maybe Text
  , drrProtocol :: Maybe Text
  , drrDisplayFormat :: Maybe DisplayFormat
  } deriving stock (Eq, Show, Ord, Generic)

instance FromJSON DryRunRequest where
  parseJSON = genericParseJSON
    defaultOptions {fieldLabelModifier = prepareField 3}

instance ToJSON DryRunRequest where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = prepareField 3}

instance ToSchema DryRunRequest where
  declareNamedSchema = genericDeclareNamedSchema
    defaultSchemaOptions {fieldLabelModifier = prepareField 3}
