{-# LANGUAGE OverloadedStrings #-}
module Main where
import Prelude hiding (getContents)
import Block
import Transaction
import Operation
import Command
import Data.Aeson (encode, decode)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString.Lazy (getContents)
import qualified Data.ByteString.Lazy.Char8 as B
import Hash
import Data.Maybe

block = makeBlock (Hash "") [(Transaction [] [])]

processCommand :: B.ByteString -> B.ByteString
processCommand b =
  case (decode b) of
    (Just c) -> encode $ runCommand c
    Nothing -> encode $ Error "invalid"

processCommands :: [B.ByteString] -> IO ()
processCommands bs = do
  let results = map processCommand bs
  mapM_ B.putStrLn results

main :: IO ()
main = do
  commands <- getContents >>= (return . B.lines)
  processCommands commands

--main = B.putStrLn $ encode $ [(Operation 1 2), (Operation 3 4)]
--main = putStrLn $ show $ hexEncode (encodeUtf8 "blabla")
--main = do
  --init <- return $ fromJust $ decode $ encode $ Init block :: IO Command
  --submit <- return $ fromJust $ decode $ encode $ Submit block :: IO Command
  --query <- return $ fromJust $ decode $ encode $ Query State :: IO Command
  --B.putStrLn $ encode init
  --B.putStrLn $ encode submit
  --B.putStrLn $ encode query
