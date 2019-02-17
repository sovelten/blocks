{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module BlockChain where
import Block
import Data.Aeson (ToJSON, toJSON, object, (.=))
import Data.Foldable (foldl')
import Data.Text (Text)
import GHC.Generics (Generic)
import Transaction
import Operation
import Hash

data State = State { height :: Int,
                     hash :: Hash,
                     outputs :: [Operation]}
  deriving (Eq, Generic, Show)
instance ToJSON State

empty = State 0 (Hash "") []

-- | Execute transactions of a block, returning a new state
--
-- Examples:
--
-- >>> :set -XOverloadedStrings
-- >>> let s3 = (State 3 (Hash "0xabc") [Operation 1 10])
-- >>> let t1 = Transaction [Operation 1 10] [Operation 2 5, Operation 3 5]
-- >>> let block = makeBlock (Hash "0xabc") [t1]
-- >>> transactBlock s3 block == State 4 (Hash "0xd4a0b4d7fee9d163253c08423690045f2298b740b89c1a12641f2d94ad1432bd") [Operation 2 5, Operation 3 5]
-- True
transactBlock :: State -> Block -> State
transactBlock (State l h unspent) (Block _ ts h') =
  (State (l+1) h' unspent')
  where
    unspent' = foldl' transact unspent ts

calculateState :: [Block] -> State
calculateState = foldl' transactBlock empty
