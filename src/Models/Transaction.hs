{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Models.Transaction (valid, safeTransact, Transaction (..)) where
import Data.Aeson (FromJSON, ToJSON)
import Data.List ((\\), intersect)
import Data.Text
import GHC.Generics (Generic)
import Models.Operation (Operation(..), sumAmounts)

data Transaction = Transaction { inputs :: [Operation],
                                 outputs :: [Operation]}
    deriving (Show, Eq, Generic)
instance ToJSON Transaction
instance FromJSON Transaction

-- | Checks if inputs and outputs of transaction have same amount
--
-- Examples:
--
-- >>> valid (Transaction [Operation 1 10] [Operation 2 10])
-- True
-- >>> valid (Transaction [Operation 1 10] [Operation 2 5])
-- False
valid :: Transaction -> Bool
valid t = amountInputs == amountOutputs
  where amountInputs = sumAmounts $ inputs t
        amountOutputs = sumAmounts $ outputs t

-- | Consumes input from unspent outputs and produces output
--
-- Examples:
--
-- >>> let t = (Transaction [Operation 1 10] [Operation 3 10])
-- >>> let unspent = [Operation 1 10, Operation 2 10]
-- >>> transact unspent t
-- [Operation {id = 2, amount = 10},Operation {id = 3, amount = 10}]
transact :: [Operation] -> Transaction -> [Operation]
transact unspent (Transaction ins outs) = (unspent \\ ins) ++ outs

-- | Same as transact, but checks if inputs are actually available before consuming
safeTransact :: [Operation] -> Transaction -> Either Text [Operation]
safeTransact unspent t@(Transaction ins _) =
  if intersect unspent ins == ins
  then Right $ transact unspent t
  else Left "invalid transaction inputs"
