{-# LANGUAGE OverloadedStrings #-}
module Main where
import Prelude hiding (getContents)
import BlockTree
import Command
import Data.Aeson (encode, decode)
import Data.ByteString.Lazy (getContents)
import qualified Data.ByteString.Lazy.Char8 as B
import Control.Monad.State.Lazy

processCommand :: B.ByteString -> State BlockTree B.ByteString
processCommand input =
  case (decode input) of
    (Just c) -> fmap encode (runCommand c)
    Nothing -> return $ encode $ Error "invalid"

chainState :: [B.ByteString] -> State BlockTree [B.ByteString]
chainState = sequence . map processCommand

main :: IO ()
main = do
  commands <- getContents >>= (return . B.lines)
  let (results, chain) = runState (chainState commands) emptyTree
  mapM_ B.putStrLn results
