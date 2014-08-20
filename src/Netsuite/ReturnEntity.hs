{-# LANGUAGE OverloadedStrings #-}

module Netsuite.ReturnEntity where

import Data.Aeson
import Data.ByteString (ByteString)

toEntities :: ByteString -> Maybe Value
toEntities bs = decode bs
