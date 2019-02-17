{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module BlockGraph where
import Block
import qualified BlockChain as BC
import Data.Aeson (ToJSON, toJSON, object, (.=))
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import qualified Data.HashMap.Strict as HM
import GHC.Generics (Generic)
import Hash
import Safe (headMay)

data Node = Node { height :: Int,
                   block :: Block}
  deriving (Eq, Generic, Show)

instance ToJSON Node where
  toJSON (Node l (Block _ _ h)) = object ["height" .= l, "hash" .= h]

instance Ord Node where
  compare (Node l1 b1) (Node l2 b2) =
    case compare l1 l2 of
      LT -> LT
      GT -> GT
      EQ -> compare (hash b1) (hash b2)

data BlockGraph = BlockGraph { hashmap :: HashMap Hash Node,
                               heads :: [Node]}

-- | Find parent node of block or fail if not found
parentNode :: Block -> BlockGraph -> Either Text Node
parentNode (Block p _ _) (BlockGraph m _) =
  case HM.lookup p m of
       (Just node) -> Right node
       Nothing -> Left "no predecessor found"

-- | Find chain of blocks by following head till genesis block
-- | Note that chain order is from genesis to last block
chain :: Block -> BlockGraph -> [Block]
chain b g =
  case parentNode b g of
    Right (Node _ p) -> (chain p g) ++ [b]
    Left _ -> [b]

empty :: BlockGraph
empty = BlockGraph HM.empty []

init :: Block -> BlockGraph -> Either Text BlockGraph
init b@(Block _ _ h) (BlockGraph m hs) =
  if m == HM.empty
     then Right $ BlockGraph (HM.insert h node m) [node]
     else Left "already initialized"
  where node = Node 1 b

notDuplicated :: Block -> BlockGraph -> Either Text BlockGraph
notDuplicated (Block _ _ h) g@(BlockGraph m _) =
  case HM.lookup h m of
       (Just _) -> Left "duplicated hash"
       Nothing -> Right g

-- | Update head nodes by appending new block
-- | If parent is in the list of heads, replace parent, otherwise append to heads
updateHeads :: Node -> Node -> [Node] -> [Node]
updateHeads new parent hs =
  case (headMay end) of
    Just _ -> begin ++ [new] ++ (tail end)
    Nothing -> new:hs
  where
    (begin, end) = break (== parent) hs

-- | Include node in the hash map
updateMap n@(Node _ (Block _ _ h)) m = HM.insert h n m

-- | Add node to graph if hash not duplicated and if it has a parent
addNode :: Block -> BlockGraph -> Either Text BlockGraph
addNode b g@(BlockGraph m hs) = do
  validHash b
  validTransactions b
  notDuplicated b g
  parent@(Node l _) <- parentNode b g
  let node = (Node (l+1) b)
  return $ BlockGraph (updateMap node m) (updateHeads node parent hs)

longestHead :: BlockGraph -> Block
longestHead (BlockGraph m hs) = block $ maximum hs

longestChain :: BlockGraph -> [Block]
longestChain g = chain (longestHead g) g

state :: BlockGraph -> BC.State
state = BC.calculateState . longestChain
