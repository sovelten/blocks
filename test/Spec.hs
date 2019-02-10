{-# LANGUAGE OverloadedStrings #-}
import Test.QuickCheck
import Test.QuickCheck.Instances
import Operation
import Transaction
import Block
import Hash
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Aeson (FromJSON, ToJSON, encode)
import Test.DocTest

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

main :: IO ()
--main = do
--  t <- generate arbitrary :: IO Block
--  LB.putStrLn (sortedEncode genesis)
main = doctest ["-isrc",
                "src/Transaction.hs",
                "src/Block.hs"]
