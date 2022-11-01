module Schema.ListDeclarationsRequest (ListDeclarationsRequest (..)) where

import Data.Aeson
  (FromJSON, ToJSON, defaultOptions, fieldLabelModifier, genericParseJSON, genericToJSON, parseJSON,
  toJSON)
import Data.Swagger.Schema
  (ToSchema, declareNamedSchema, defaultSchemaOptions, fieldLabelModifier,
  genericDeclareNamedSchema)
import GHC.Generics (Generic)

import Source (Project)
import Util (prepareField)

data ListDeclarationsRequest = ListDeclarationsRequest
  { ldrProject :: Project
  , ldrOnlyEndpoint :: Bool
  } deriving stock (Eq, Show, Ord, Generic)

instance FromJSON ListDeclarationsRequest where
  parseJSON = genericParseJSON
    defaultOptions {fieldLabelModifier = prepareField 3}

instance ToJSON ListDeclarationsRequest where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = prepareField 3}

instance ToSchema ListDeclarationsRequest where
  declareNamedSchema = genericDeclareNamedSchema
    defaultSchemaOptions {fieldLabelModifier = prepareField 3}
