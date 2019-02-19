{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Models.BlockGraph where
import Data.Aeson (ToJSON, toJSON, object, (.=))
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import qualified Data.HashMap.Strict as HM
import GHC.Generics (Generic)
import Models.Block (Block(..))
import qualified Models.Block as B
import qualified Models.ChainState as S
import Models.Hash
import Safe (headMay)

data Node = Node { state :: S.State,
                   block :: Block}
  deriving (Eq, Generic, Show)

instance ToJSON Node where
  toJSON (Node (S.State l _ _) (Block _ _ h)) = object ["height" .= l, "hash" .= h]

instance Ord Node where
  compare (Node (S.State l1 _ _) b1) (Node (S.State l2 _ _) b2) =
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

empty :: BlockGraph
empty = BlockGraph HM.empty []

-- | Creates initial block
init :: Block -> BlockGraph -> Either Text BlockGraph
init b@(Block _ _ h) (BlockGraph m hs) =
  if m == HM.empty
     then Right $ BlockGraph (updateMap node m) [node]
     else Left "already initialized"
  where node = Node (S.State 1 h (B.outputs b)) b

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

-- | Add node to graph
-- | For a node to be added, it needs to be valid, not duplicated
-- | And the transaction inputs must match unspent outputs
addNode :: Block -> BlockGraph -> Either Text BlockGraph
addNode b g@(BlockGraph m hs) = do
  B.valid b
  notDuplicated b g
  parent@(Node s _) <- parentNode b g
  s' <- (S.safeTransactBlock s b)
  let node = (Node s' b)
  return $ BlockGraph (updateMap node m) (updateHeads node parent hs)

-- | Finds head with longest height
-- | Criteria for draws is defined in Ord instance of Node
-- | Which means heads are sorted by height and then alphabetically by block hash
longestHead :: BlockGraph -> Node
longestHead (BlockGraph m hs) = maximum hs

-- | Retrieves state of the longest chain
longestChainState :: BlockGraph -> S.State
longestChainState = state . longestHead
