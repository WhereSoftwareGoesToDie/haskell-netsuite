{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}

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
import Data.Data
import Data.Text
import Data.Typeable ()

import Netsuite.Types.Data.TypeFamily
import Netsuite.Types.Fields

--------------------------------------------------------------------------------
-- | Netsuite record type
newtype NsType = NsType {
    unNSType :: Text
} deriving (Data, Typeable)

instance Show NsType where
    show NsType{..} = unpack unNSType

instance ToJSON NsType where
    toJSON = String . unNSType

instance NsTypeFamily NsType where
    toTypeIdentList NsType{..} = [unNSType]
    toDefaultFields = nsTypeFields . toTypeIdentList

-- | Turn simpler types into NsType
class IsNsType a where
    toNsType :: a -> NsType

instance IsNsType Text where
    toNsType = NsType

instance IsNsType [Text] where
    toNsType (a:_) = NsType a
    toNsType _     = error "Not enough arguments in list for NsType"

--------------------------------------------------------------------------------
-- | Netsuite record subtype
data NsSubtype = NsSubtype {
    unNSSubtypeParent :: NsType,
    unNSSubtype       :: Text
} deriving (Data, Typeable)

instance Show NsSubtype where
    show NsSubtype{..} = show unNSSubtypeParent ++ "." ++ unpack unNSSubtype

instance ToJSON NsSubtype where
    toJSON NsSubtype{..} = String unNSSubtype

instance NsTypeFamily NsSubtype where
    toTypeIdentList NsSubtype{..} = toTypeIdentList unNSSubtypeParent ++ [unNSSubtype]
    toDefaultFields = nsSubtypeFields . toTypeIdentList

getTypeFromSubtype :: NsSubtype -> NsType
getTypeFromSubtype NsSubtype{..} = unNSSubtypeParent

-- | Turn simpler types into NsSubtype
class IsNsSubtype a where
    toNsSubtype :: a -> NsSubtype

instance (IsNsType a) => IsNsSubtype (a, Text) where
    toNsSubtype (a, b) = NsSubtype (toNsType a) b

instance IsNsSubtype [Text] where
    toNsSubtype (a:b:_) = NsSubtype (NsType a) b
    toNsSubtype _       = error "Not enough arguments in list for NsSubtype"
