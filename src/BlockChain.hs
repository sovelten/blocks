{-# LANGUAGE OverloadedStrings #-}
module BlockChain where
import qualified Block as B
import Block (Block(Block), validHash, validTransactions)
import Hash
import Safe (headMay)
import Data.Either.Combinators (maybeToRight)
import Data.Text (Text)

data Head = Head { height :: Int,
                   hash :: Hash}
  deriving (Eq, Show)

data BlockChain = BlockChain { blocks :: [Block],
                               heads :: [Head] }
  deriving (Eq, Show)

empty = BlockChain [] []

isEmpty :: BlockChain -> Bool
isEmpty = (== empty)

hasHash :: Hash -> Head -> Bool
hasHash h ch = (hash ch) == h

updateHead :: Head -> Hash -> Head
updateHead (Head n _) h = Head (n + 1) h

-- | Update heads by appending new block
--
-- Examples:
--
-- >>> :set -XOverloadedStrings
-- >>> let initHeads = [Head 1 (Hash "0x8c9d4f1b9188e5c1a6bbfa9f1d0316f28da1153b3f68553100dc9c9e45bf6fbe")]
-- >>> let block = B.makeBlock (Hash "0x8c9d4f1b9188e5c1a6bbfa9f1d0316f28da1153b3f68553100dc9c9e45bf6fbe") []
-- >>> let result = updateHeads initHeads block
-- >>> result == Right [Head 2 (Hash "0x2ac26c862ac72dec16a8f47dda47634e450f7306dcdd9931fd3211060506c1c8")]
-- True
updateHeads :: [Head] -> Block -> Either Text [Head]
updateHeads hs (Block p _ h) =
  case (headMay end) of
    Just x -> Right $ begin ++ [updateHead x h] ++ (tail end)
    Nothing -> Left "no predecessor found"
  where
    (begin, end) = break (hasHash p) hs

--
-- Operations
--

init :: Block -> BlockChain -> Either Text BlockChain
init b c =
  if isEmpty c
  then Right $ BlockChain [b] [Head 1 (B.hash b)]
  else Left "already initialized"

addBlock :: BlockChain -> Block -> Either Text BlockChain
addBlock (BlockChain bs hs) b = do
  validHash b
  validTransactions b
  newHeads <- updateHeads hs b
  return $ BlockChain (b:bs) newHeads
