{-# LANGUAGE DeriveDataTypeable #-}

module Netsuite.Types.Fields.Core where

import Data.Aeson
import Data.Data
import Data.Typeable()

--------------------------------------------------------------------------------
-- | List of Netsuite fields to retrieve
newtype NsFields = NsFields [String] deriving (Data, Typeable, Show)

instance ToJSON NsFields where
      toJSON (NsFields f) = toJSON f
