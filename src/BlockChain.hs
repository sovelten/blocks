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

data BlockChain = BlockChain { blocks :: [Block],
                               heads :: [Head] }

empty = BlockChain [] []

init :: Block -> BlockChain
init b = BlockChain [b] [Head 0 (B.hash b)]

hasHash :: Hash -> Head -> Bool
hasHash h ch = (hash ch) == h

updateHead :: Head -> Hash -> Head
updateHead (Head n _) h = Head (n + 1) h

updateHeads :: [Head] -> Block -> Either Text [Head]
updateHeads hs (Block p _ h) =
  case (headMay end) of
    Just x -> Right $ begin ++ [updateHead x h] ++ (tail end)
    Nothing -> Left "no predecessor found"
  where
    (begin, end) = span (hasHash p) hs

addBlock :: BlockChain -> Block -> Either Text BlockChain
addBlock (BlockChain bs hs) b = do
  (validHash b)
  (validTransactions b)
  newHeads <- updateHeads hs b
  return $ BlockChain (b:bs) newHeads
