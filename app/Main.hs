{-# LANGUAGE OverloadedStrings #-}
module Main where

import Block
import Transaction
import Operation
import Command
import Data.Aeson
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.Lazy.Char8 as B
import Hash
import Data.Maybe

block = makeBlock (Hash "") [(Transaction [] [])]

main :: IO ()
--main = B.putStrLn $ encode $ [(Operation 1 2), (Operation 3 4)]
--main = putStrLn $ show $ hexEncode (encodeUtf8 "blabla")
main = do
  init <- return $ fromJust $ decode $ encode $ Init block :: IO Command
  submit <- return $ fromJust $ decode $ encode $ Submit block :: IO Command
  query <- return $ fromJust $ decode $ encode $ Query State :: IO Command
  B.putStrLn $ encode init
  B.putStrLn $ encode submit
  B.putStrLn $ encode query
