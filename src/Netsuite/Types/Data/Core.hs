{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PackageImports     #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}

module Netsuite.Types.Data.Core (
    NsType (..),
    NsSubtype (..),

    IsNsType,
    toNsType,

    IsNsSubtype,
    toNsSubtype,

    typeFields,
    getTypeFromSubtype
) where

import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Char (toLower)
import Data.Data
import qualified Data.HashMap as HashMap
import qualified Data.HashMap.Strict as HMS
import Data.Maybe
import Data.Monoid
import qualified Data.Text as Text
import Data.Typeable
import qualified Data.Vector as Vector
import Data.Word

import Netsuite.Helpers
import Netsuite.Restlet.Configuration
import Netsuite.Types.Data.TypeFamily
import Netsuite.Types.Fields
import Netsuite.Types.Fields.Core

--------------------------------------------------------------------------------
-- | Netsuite record type
newtype NsType = NsType String deriving (Data, Typeable)

instance Show NsType where
    show (NsType t) = t

instance ToJSON NsType where
    toJSON (NsType s) = toJSON s

instance NsTypeFamily NsType where
    toTypeIdentList (NsType t) = [t]
    toDefaultFields = nsTypeFields . toTypeIdentList

-- | Turn simpler types into NsType
class IsNsType a where
    toNsType :: a -> NsType

instance IsNsType [Char] where
    toNsType = NsType

instance IsNsType [[Char]] where
    toNsType (a:_) = NsType a
    toNsType _     = error "Not enough arguments in list for NsType"

--------------------------------------------------------------------------------
-- | Netsuite record subtype
data NsSubtype = NsSubtype NsType String deriving (Data, Typeable)

instance Show NsSubtype where
    show (NsSubtype t s) = (show t) ++ "." ++ s

instance ToJSON NsSubtype where
    toJSON (NsSubtype _ s) = toJSON s

instance NsTypeFamily NsSubtype where
    toTypeIdentList (NsSubtype t s) = (toTypeIdentList t) ++ [s]
    toDefaultFields = nsSubtypeFields . toTypeIdentList

getTypeFromSubtype :: NsSubtype -> NsType
getTypeFromSubtype (NsSubtype t _) = t

-- | Turn simpler types into NsSubtype
class IsNsSubtype a where
    toNsSubtype :: a -> NsSubtype

instance (IsNsType a) => IsNsSubtype (a, [Char]) where
    toNsSubtype (a, b) = NsSubtype (toNsType a) b

instance IsNsSubtype [[Char]] where
    toNsSubtype (a:b:_) = NsSubtype (toNsType a) b
    toNsSubtype _       = error "Not enough arguments in list for NsSubtype"
