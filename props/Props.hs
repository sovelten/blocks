{-# LANGUAGE OverloadedStrings #-}
import Data.Either (isRight)
import Data.List ((\\))
import Test.QuickCheck
import Test.QuickCheck.Instances
import Test.QuickCheck.Gen
import Command
import Models.BlockGraph
import qualified Models.ChainState as S
import Models.Operation
import Models.Transaction
import qualified Models.Transaction as T
import Models.Block
import Models.Hash
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Aeson (FromJSON, ToJSON, encode)

instance Arbitrary Operation where
    arbitrary = do
        id <- choose (0, 10000) :: Gen Int
        amount <- choose (0, 10000) :: Gen Int
        return $ Operation id amount

instance Arbitrary Transaction where
    arbitrary = do
        inputs <- arbitrary :: Gen [Operation]
        outputs <- arbitrary :: Gen [Operation]
        return $ Transaction inputs outputs

instance Arbitrary Hash where
    arbitrary = do
        s <- arbitrary :: Gen B.ByteString
        return $ hexEncode s

instance Arbitrary Block where
    arbitrary = do
        pred <- arbitrary :: Gen Hash
        transactions <- arbitrary :: Gen [Transaction]
        return $ makeBlock pred transactions

instance Arbitrary S.State where
    arbitrary = do
        l <- arbitrary :: Gen Int
        h <- arbitrary :: Gen Hash
        out <- arbitrary :: Gen [Operation]
        return $ S.State l h out

validBlock :: S.State -> Gen Block
validBlock (S.State l h ops) = do
  ins <- sublistOf ops
  let amt = sumAmounts ins
  id <- choose (0, 10000) :: Gen Int
  let out = [Operation id amt]
  return $ makeBlock h [Transaction ins out]

validSequences :: Gen (S.State, Block)
validSequences = do
  s <- arbitrary :: Gen S.State
  b <- validBlock s
  return (s, b)

initGen :: Gen Block
initGen = do
  out <- arbitrary :: Gen [Operation]
  return $ makeBlock (Hash "") [Transaction [] out]

initCommandGen :: Gen Command
initCommandGen = do
  b <- initGen
  return $ Init b

submitValidCommandGen :: BlockGraph -> Gen Command
submitValidCommandGen g = do
  (Node s _) <- elements $ heads g
  b <- validBlock s
  return $ Submit b

consistentStateUpdate :: (S.State, Block) -> Bool
consistentStateUpdate (s@(S.State l h o), b@(Block p ts h')) =
  case result of
    (Left _) -> False
    (Right (S.State l' h'' o')) ->
      (p == h) &&
      (l' == l + 1) &&
      (h'' == h') &&
      (o' == expectedOutput)
  where result = S.safeTransactBlock s b
        ins = concat $ map T.inputs ts
        outs = concat $ map T.outputs ts
        expectedOutput = (o \\ ins) ++ outs

main :: IO ()
main = do
  quickCheck (forAll validSequences consistentStateUpdate)
