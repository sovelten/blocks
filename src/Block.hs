{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Block where
import Hash (Hash(..), hexEncode)
import Operation (Operation(..))
import Transaction
import Data.Aeson (FromJSON, ToJSON, encode)
import qualified Data.Aeson.Encode.Pretty as P
import GHC.Generics (Generic)
import qualified Crypto.Hash.SHA256 as C
import qualified Data.ByteString.Lazy.Char8 as LB

data Block = Block { predecessor :: Hash,
                     transactions :: [Transaction],
                     hash :: Hash}
  deriving (Show, Generic)
instance ToJSON Block
instance FromJSON Block

sortedEncode :: ToJSON a => a -> LB.ByteString
sortedEncode = P.encodePretty' (P.Config (P.Spaces 0) P.compare P.Generic False)

blockHash :: Hash -> [Transaction] -> Hash
blockHash p t = hexEncode $ C.hashlazy $ sortedEncode (p, t)

makeBlock :: Hash -> [Transaction] -> Block
makeBlock p t = Block p t (blockHash p t)

genesis :: Block
genesis = Block (Hash "") transactions hash
  where transactions = [Transaction [] [(Operation 73 30)]]
        hash = blockHash (Hash "") transactions


