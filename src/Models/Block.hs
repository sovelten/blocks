{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Models.Block (Block(..), makeBlock, valid, outputs) where
import qualified Crypto.Hash.SHA256 as C
import Data.Aeson (FromJSON, ToJSON, encode)
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Aeson.Encode.Pretty as P
import GHC.Generics (Generic)
import Models.Hash (Hash(..), hexEncode)
import Models.Operation (Operation(..))
import Models.Transaction (Transaction(Transaction))
import qualified Models.Transaction as T
import Data.Text (Text)

data Block = Block { predecessor :: Hash,
                     transactions :: [Transaction],
                     hash :: Hash}
  deriving (Show, Eq, Generic)
instance ToJSON Block
instance FromJSON Block

sortedEncode :: ToJSON a => a -> LB.ByteString
sortedEncode = P.encodePretty' (P.Config (P.Spaces 0) P.compare P.Generic False)

blockHash :: Hash -> [Transaction] -> Hash
blockHash p t = hexEncode $ C.hashlazy $ sortedEncode (p, t)

-- | Creates a valid block from predecessor hash and list of transactions
--
-- Examples:
--
-- >>> :set -XOverloadedStrings
-- >>> makeBlock (Hash "") []
-- Block {predecessor = Hash "", transactions = [], hash = Hash "0xaa5598c670c7f9c9ab8594d942390dad96b52631bfb78d060476b9a719365947"}
-- >>> makeBlock (Hash "0x8c9d4f1b9188e5c1a6bbfa9f1d0316f28da1153b3f68553100dc9c9e45bf6fbe") []
-- Block {predecessor = Hash "0x8c9d4f1b9188e5c1a6bbfa9f1d0316f28da1153b3f68553100dc9c9e45bf6fbe", transactions = [], hash = Hash "0x2ac26c862ac72dec16a8f47dda47634e450f7306dcdd9931fd3211060506c1c8"}
makeBlock :: Hash -> [Transaction] -> Block
makeBlock p t = Block p t (blockHash p t)

-- | Checks if all transactions in a block are valid
--
-- Examples:
--
-- >>> :set -XOverloadedStrings
-- >>> let validT = (Transaction [Operation 1 10] [Operation 2 10])
-- >>> let invalidT = (Transaction [Operation 1 10] [Operation 2 5])
-- >>> let validBlock = makeBlock (Hash "") [validT, validT]
-- >>> validTransactions validBlock == Right validBlock
-- True
-- >>> let invalidBlock = makeBlock (Hash "") [validT, invalidT]
-- >>> validTransactions invalidBlock == Left "invalid transaction"
-- True
validTransactions :: Block -> Either Text Block
validTransactions b =
  case isValid of
    True -> Right b
    False -> Left "invalid transaction"
  where
    isValid = all T.valid (transactions b)

-- | Checks if the block has a valid hash
--
-- Examples:
--
-- >>> :set -XOverloadedStrings
-- >>> let validBlock = makeBlock (Hash "") []
-- >>> validHash validBlock == Right validBlock
-- True
-- >>> let invalidBlock = Block (Hash "") [] (Hash "0xa23bcd")
-- >>> validHash invalidBlock
-- Left "invalid hash"
validHash :: Block -> Either Text Block
validHash b@(Block p ts h) =
  case isValid of
    True -> Right b
    False -> Left "invalid hash"
  where
    isValid = h == (blockHash p ts)

valid b = do
  validHash b
  validTransactions b

outputs :: Block -> [Operation]
outputs = concat . map T.outputs . transactions
