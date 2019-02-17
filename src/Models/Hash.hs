{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Models.Hash (Hash(..), hexEncode) where
import Data.Aeson (FromJSON, ToJSON, encode)
import GHC.Generics
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString as B
import Data.ByteString.Builder
import Data.String.Conversions (cs)
import Data.Hashable

newtype Hash = Hash T.Text
  deriving (Generic, Eq, Show, Ord)
instance ToJSON Hash
instance FromJSON Hash
instance Hashable Hash

hexEncode :: B.ByteString -> Hash
hexEncode = Hash . T.append "0x" . decodeUtf8 . cs . toLazyByteString . byteStringHex
