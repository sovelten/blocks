{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Command where
import Block
import qualified Data.Aeson as A
import Data.Aeson (FromJSON, ToJSON, toJSON, parseJSON, (.=), (.:), object, withObject, genericToJSON, genericParseJSON)
import Data.Foldable (asum)
import Data.Text (Text)
import Data.Aeson.Types (emptyArray)

data QueryType = State | Heads
instance ToJSON QueryType where
   toJSON State = "state"
   toJSON Heads = "heads"
instance FromJSON QueryType where
   parseJSON (A.String s) =
     case s of
       "state" -> return State
       "heads" -> return Heads

data Command = Init Block | Submit Block | Query QueryType
instance ToJSON Command where
  toJSON (Init block) = object ["init" .= block]
  toJSON (Submit block) = object ["block" .= block]
  toJSON (Query q) = object ["query" .= q]
instance FromJSON Command where
  parseJSON = withObject "Command" $ \o -> asum
    [Init <$> o .: "init",
     Submit <$> o .: "block",
     Query <$> o .: "query"]

data Result = Ok | Error Text
instance ToJSON Result where
  toJSON Ok = object ["ok" .= emptyArray]
  toJSON (Error t) = object ["error" .= t]

runCommand :: Command -> Result
runCommand c =
  case c of
    (Init b) -> Ok
    (Submit b) -> Ok
    (Query q) -> Error "not implemented"

