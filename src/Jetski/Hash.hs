{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Jetski.Hash (
    Hash(..)
  , renderHash
  , hashBytes
  , hashText
  , hashHashes
  ) where

import           Crypto.Hash (Digest, SHA1)
import qualified Crypto.Hash as Hash

import           Data.ByteArray (ByteArrayAccess, convert)
import           Data.ByteArray.Encoding (Base(..), convertToBase)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import qualified Data.Text.Encoding as T

import           P


newtype Hash =
  Hash {
      unHash :: Digest SHA1
    } deriving (Eq, Ord, Show)

renderHash :: Hash -> Text
renderHash =
  T.decodeUtf8 . takeBase16 . unHash

takeBytes :: ByteArrayAccess a => a -> ByteString
takeBytes =
  convert

takeBase16 :: ByteArrayAccess a => a -> ByteString
takeBase16 =
  convertToBase Base16

hashBytes :: ByteString -> Hash
hashBytes bytes = do
  Hash (Hash.hash bytes)

hashText :: Text -> Hash
hashText text = do
  hashBytes (T.encodeUtf8 text)

hashHashes :: [Hash] -> Hash
hashHashes =
  Hash . Hash.hashlazy . L.fromChunks . fmap (takeBytes . unHash)
