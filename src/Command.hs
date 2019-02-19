{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Command where
import qualified Data.Aeson as A
import Data.Aeson (FromJSON, ToJSON, toJSON, parseJSON, (.=), (.:), object, withObject, genericToJSON, genericParseJSON)
import Data.Foldable (asum)
import Data.Text (Text)
import Data.Aeson.Types (emptyArray)
import Control.Monad.State.Lazy
import Models.BlockGraph (BlockGraph(..))
import qualified Models.ChainState as S
import qualified Models.BlockGraph as BG
import Models.Block

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

data Result = Ok | Error Text | RHeads [BG.Node] | RState S.State
instance ToJSON Result where
  toJSON Ok = object ["ok" .= emptyArray]
  toJSON (RHeads hs) = object ["heads" .= hs]
  toJSON (RState s) = object ["state" .= s]
  toJSON (Error t) = object ["error" .= t]

initState :: Block -> State BlockGraph Result
initState b = do
  g <- get
  case (BG.init b g) of
    (Right new) -> do
      put new
      return Ok
    (Left message) -> return $ Error message

submitState :: Block -> State BlockGraph Result
submitState b = do
  g <- get
  case (BG.addNode b g) of
    (Right new) -> do
      put new
      return Ok
    (Left message) -> return $ Error message

queryState :: QueryType -> State BlockGraph Result
queryState q = do
  g <- get
  case q of
    Heads -> return $ RHeads $ BG.heads g
    State -> return $ RState $ BG.longestChainState g

runCommand :: Command -> State BlockGraph Result
runCommand c =
  case c of
    (Init b) -> initState b
    (Submit b) -> submitState b
    (Query q) -> queryState q
