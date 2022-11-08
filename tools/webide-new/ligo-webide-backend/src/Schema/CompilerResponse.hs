module Schema.CompilerResponse (CompilerResponse (..)) where

import Data.Aeson
  (FromJSON, ToJSON, defaultOptions, genericParseJSON, genericToJSON, parseJSON, toJSON,
  unwrapUnaryRecords)
import Data.Swagger.ParamSchema (ToParamSchema)
import Data.Swagger.Schema
  (ToSchema, declareNamedSchema, defaultSchemaOptions, genericDeclareNamedSchema,
  unwrapUnaryRecords)
import Data.Text (Text)
import GHC.Generics (Generic)

newtype CompilerResponse = CompilerResponse {unCompilerResponse :: Text}
  deriving stock (Show, Generic, Eq)

instance ToJSON CompilerResponse where
  toJSON = genericToJSON
    defaultOptions {unwrapUnaryRecords = True}

instance FromJSON CompilerResponse where
  parseJSON = genericParseJSON
    defaultOptions {unwrapUnaryRecords = True}

instance ToSchema CompilerResponse where
  declareNamedSchema = genericDeclareNamedSchema
    defaultSchemaOptions {unwrapUnaryRecords = True}

instance ToParamSchema CompilerResponse
