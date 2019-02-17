{-# LANGUAGE DeriveGeneric #-}
module Models.Operation (sumAmounts, Operation (..)) where
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data Operation = Operation { id :: Int,
                             amount :: Int }
    deriving (Show, Eq, Generic)
instance ToJSON Operation
instance FromJSON Operation

sumAmounts :: [Operation] -> Int
sumAmounts = sum . map amount
