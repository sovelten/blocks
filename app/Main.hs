{-# LANGUAGE OverloadedStrings #-}
module Main where
import Prelude hiding (getContents)
import BlockChain
import Command
import Data.Aeson (encode, decode)
import Data.ByteString.Lazy (getContents)
import qualified Data.ByteString.Lazy.Char8 as B
import Control.Monad.State.Lazy

processCommand :: B.ByteString -> State BlockChain B.ByteString
processCommand input =
  case (decode input) of
    (Just c) -> fmap encode (runCommand c)
    Nothing -> return $ encode $ Error "invalid"

chainState :: [B.ByteString] -> State BlockChain [B.ByteString]
chainState = sequence . map processCommand

main :: IO ()
main = do
  commands <- getContents >>= (return . B.lines)
  let (results, chain) = runState (chainState commands) empty
  mapM_ B.putStrLn results
