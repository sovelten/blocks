{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module BlockTree where
import Data.Aeson (FromJSON, ToJSON)
import Data.Tree
import GHC.Generics (Generic)
import Block
import Hash
import Data.Text (Text)

type BlockTree = Tree Block

data Head = Head { height :: Int,
                   hash :: Hash}
  deriving (Eq, Generic, Show)
instance ToJSON Head
instance FromJSON Head

emptyBlock :: Block
emptyBlock = Block (Hash "") [] (Hash "")

emptyTree = Node emptyBlock []

init :: Block -> BlockTree -> Either Text BlockTree
init b (Node root _) =
  if root == emptyBlock
  then Right $ Node b []
  else Left "already initialized"

addNode' :: Block -> Block -> [(Bool, BlockTree)] -> (Bool, BlockTree)
addNode' b root ts =
  if (not added) && (isParent root b)
    then (True, Node root ((Node b []):subTrees))
    else (False, Node root subTrees)
  where
    subTrees = map snd ts
    added = or $ map fst ts

addNode :: Block -> BlockTree -> (Bool, BlockTree)
addNode b = foldTree (addNode' b)

isLeaf :: BlockTree -> Bool
isLeaf (Node _ []) = True
isLeaf (Node _ (t:ts)) = False

makeHead :: Int -> Block -> Head
makeHead n (Block _ _ h) = Head n h

heights :: Tree Block -> [[Block]]
heights t =
    map (map rootLabel . filter isLeaf) $
        takeWhile (not . null) $
        iterate (concatMap subForest) [t]

heads :: BlockTree -> [Head]
heads = concat . zipWith (\n bs -> map (makeHead n) bs) [1..] . heights

tryAddBlock :: Block -> BlockTree -> Either Text BlockTree
tryAddBlock b t = if added
  then Right newTree
  else Left "no predecessor found"
  where
    (added, newTree) = addNode b t

addBlock :: BlockTree -> Block -> Either Text BlockTree
addBlock t b = do
  validHash b
  validTransactions b
  tryAddBlock b t
