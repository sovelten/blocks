{-# LANGUAGE DeriveGeneric #-}
module Transaction where
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Operation (Operation)

data Transaction = Transaction { inputs :: [Operation],
                                 outputs :: [Operation]}
    deriving (Show, Eq, Generic)
instance ToJSON Transaction
instance FromJSON Transaction
