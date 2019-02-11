{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Command where
import Block
import qualified Data.Aeson as A
import Data.Aeson (FromJSON, ToJSON, toJSON, parseJSON, (.=), (.:), object, withObject, genericToJSON, genericParseJSON)
import Data.Foldable (asum)
import Data.Text (Text)
import Data.Aeson.Types (emptyArray)
import BlockChain (BlockChain(..))
import qualified BlockChain as BC
import Control.Monad.State.Lazy

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

initState :: Block -> State BlockChain Result
initState b = do
  chain <- get
  case (BC.init b chain) of
    (Right new) -> do
      put new
      return Ok
    (Left message) -> return $ Error message

submitState :: Block -> State BlockChain Result
submitState b = do
  chain <- get
  case (BC.addBlock chain b) of
    (Right new) -> do
      put new
      return Ok
    (Left message) -> return $ Error message

runCommand :: Command -> State BlockChain Result
runCommand c =
  case c of
    (Init b) -> initState b
    (Submit b) -> submitState b
    (Query q) -> return $ Error "not implemented"
