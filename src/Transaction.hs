{-# LANGUAGE DeriveGeneric #-}
module Transaction where
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Operation (Operation(..), sumAmounts)

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
