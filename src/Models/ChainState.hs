{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Models.ChainState (State(..), safeTransactBlock) where
import Control.Monad (foldM)
import Data.Aeson (ToJSON, toJSON, object, (.=))
import Data.Foldable (foldl')
import Data.Text (Text)
import GHC.Generics (Generic)
import Models.Block
import Models.Hash
import Models.Operation
import Models.Transaction

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
-- >>> safeTransactBlock s3 block == Right (State 4 (Hash "0xd4a0b4d7fee9d163253c08423690045f2298b740b89c1a12641f2d94ad1432bd") [Operation 2 5, Operation 3 5])
-- True
safeTransactBlock :: State -> Block -> Either Text State
safeTransactBlock (State l h unspent) (Block _ ts h') = do
  unspent' <- foldM safeTransact unspent ts
  return (State (l+1) h' unspent')
