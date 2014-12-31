{-# LANGUAGE DeriveDataTypeable #-}

module Netsuite.Types.Fields.Core where

import Data.Aeson
import Data.Data
import Data.Text
import Data.Typeable ()

--------------------------------------------------------------------------------
-- | List of Netsuite fields to retrieve
newtype NsFields = NsFields [Text] deriving (Data, Typeable, Show)

instance ToJSON NsFields where
      toJSON (NsFields f) = toJSON f
