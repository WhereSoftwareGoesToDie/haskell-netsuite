{-# LANGUAGE OverloadedStrings #-}

module Netsuite.Helpers (
  bsPackedW8s,
  bs8PackedW8s,
  bytesToString,
  listToJsonArray,
  stringToJsonString
  ) where

import Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Char
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Data.Word
import GHC.Word (Word8)

bsPackedW8s :: String -> BS.ByteString
bsPackedW8s = BS.pack . strToWord8s

-- | Packs a string into an 8-bit bytestring.
bs8PackedW8s :: String -> BS.ByteString
bs8PackedW8s = BS8.pack

-- | Converts a string into a list of Word8s.
strToWord8s :: String -> [Word8]
strToWord8s = BSI.unpackBytes . Char8.pack 

-- | stolen from Language.Haskell.TH.Ppr
bytesToString :: [Word8] -> String
bytesToString = map (chr . fromIntegral)

-- | Packs a list of Aeson values into an Aeson Array
listToJsonArray :: [Value] -> Value
listToJsonArray = Array . Vector.fromList

-- | Packs a string into an Aeson String
stringToJsonString :: String -> Value
stringToJsonString = String . Text.pack
