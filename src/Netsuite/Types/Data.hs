{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PackageImports     #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}

module Netsuite.Types.Data (
    NsAction (..),
    NsRestletCode (..),
    NsData (..),
    NsSublistData (..),
    NsFilters,
    NsFilter (..),
    NsSearchOp (..),
    NsSearchCols,
    NsSearchCol,
    toSearchCols,

    IsNsType,
    IsNsSubtype,
    IsNsId,
    IsNsDataId,
    IsNsFilter,
    IsNsSearchCol,
    IsNsData,
    toNsId,
    toNsDataId,
    toNsType,
    toNsSubtype,
    toNsFilter,
    toNsData,

    testNsDataForId,

    typeFields
) where

import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.TH
import Data.Char (toLower)
import Data.Data
import qualified Data.HashMap as HashMap
import qualified Data.HashMap.Strict as HMS
import Data.Maybe
import Data.Monoid
import Data.Typeable
import qualified Data.Text as Text

import Netsuite.Helpers
import Netsuite.Types.Data.Core
import Netsuite.Types.Data.TypeFamily
import Netsuite.Types.Fields
import Netsuite.Types.Fields.Core

--------------------------------------------------------------------------------
-- | Container for Netsuite restlet code
data NsRestletCode = NsRestletCode {
    getCode :: Text.Text
} deriving (Data, Typeable, Show)

instance ToJSON NsRestletCode where
    toJSON = toJSON . getCode

--------------------------------------------------------------------------------
-- | Netsuite entity ID, for requests that only take an ID
newtype NsId = NsId Integer deriving (Data, Typeable)

instance Show NsId where
    show (NsId _id) = show _id

instance ToJSON NsId where
    toJSON (NsId _id) = String . Text.pack . show $ _id

class IsNsId a where
    toNsId :: a -> NsId

instance IsNsId Integer where
    toNsId = NsId

instance IsNsId Int where
    toNsId = NsId . toInteger

--------------------------------------------------------------------------------
-- | Netsuite entity ID, wrapped up in a data object
newtype NsDataId = NsDataId Integer deriving (Data, Typeable)

instance Show NsDataId where
    show (NsDataId _id) = show _id

instance ToJSON NsDataId where
    toJSON (NsDataId _id) = object ["id" .= show _id]

class IsNsDataId a where
    toNsDataId :: a -> NsDataId

instance IsNsDataId Integer where
    toNsDataId = NsDataId

instance IsNsDataId Int where
    toNsDataId = NsDataId . toInteger

--------------------------------------------------------------------------------
-- | List of key/value pairs of data to send to Netsuite
newtype NsData = NsData Value deriving (Data, Typeable, Show)

instance ToJSON NsData where
    toJSON (NsData m) = toJSON m

testNsDataForId :: NsData -> Bool
testNsDataForId (NsData (Object o)) = HMS.member "id" o
testNsDataForId _                   = False

class IsNsData a where
    toNsData :: a -> NsData

instance IsNsData [Pair] where
    toNsData = NsData . object

instance IsNsData Value where
    toNsData = NsData

--------------------------------------------------------------------------------
-- | Netsuite Sublist data dictionaries
newtype NsSublistData = NsSublistData [(String, [NsData])] deriving (Data, Typeable, Show)

instance ToJSON NsSublistData where
    toJSON (NsSublistData x) = object . map (\(k, v) -> (Text.pack k) .= v) $ x

--------------------------------------------------------------------------------
-- | Types of Netsuite actions to execute
data NsAction = NsActRetrieve {
    nsactCode       :: NsRestletCode,
    nsactType       :: NsType,
    nsactDID        :: NsDataId,
    nsactFields     :: NsFields
} | NsActFetchSublist {
    nsactCode       :: NsRestletCode,
    nsactSubtype    :: NsSubtype,
    nsactID         :: NsId,
    nsactFields     :: NsFields
} | NsActRawSearch {
    nsactCode       :: NsRestletCode,
    nsactType       :: NsType,
    nsactFilters    :: NsFilters,
    nsactColumns    :: NsSearchCols
} | NsActSearch {
    nsactCode       :: NsRestletCode,
    nsactType       :: NsType,
    nsactFilters    :: NsFilters,
    nsactFields     :: NsFields
} | NsActCreate {
    nsactCode       :: NsRestletCode,
    nsactType       :: NsType,
    nsactData       :: NsData,
    nsactSubdata    :: NsSublistData,
    nsactFields     :: NsFields
} | NsActAttach {
    nsactCode       :: NsRestletCode,
    nsactTargetType :: NsType,
    nsactTargetIds  :: [NsId],
    nsactFocalType  :: NsType,
    nsactID         :: NsId,
    nsactData       :: NsData
} | NsActDetach {
    nsactCode       :: NsRestletCode,
    nsactTargetType :: NsType,
    nsactTargetIds  :: [NsId],
    nsactFocalType  :: NsType,
    nsactID         :: NsId
} | NsActUpdate {
    nsactCode       :: NsRestletCode,
    nsactType       :: NsType,
    nsactData       :: NsData,
    nsactFields     :: NsFields
} | NsActUpdateSublist {
    nsactCode       :: NsRestletCode,
    nsactSubtype    :: NsSubtype,
    nsactID         :: NsId,
    nsactSublistD   :: [NsData]
} | NsActDelete {
    nsactCode       :: NsRestletCode,
    nsactType       :: NsType,
    nsactDID        :: NsDataId
} | NsActInvoicePDF {
    nsactCode       :: NsRestletCode,
    nsactID         :: NsId
} | NsActTransform {
    nsactCode       :: NsRestletCode,
    nsactType       :: NsType,
    nsactID         :: NsId,
    nsactTargetType :: NsType,
    nsactData       :: NsData,
    nsactFields     :: NsFields
} deriving (Data, Typeable, Show)

instance ToJSON NsAction where
    toJSON (NsActRetrieve {..}) =
        object [ "code"           .= nsactCode
               , "action"         .= String "retrieve"
               , "type_id"        .= nsactType
               , "data"           .= nsactDID
               , "fields"         .= nsactFields
                 ]
    toJSON (NsActFetchSublist {..}) =
        object [ "code"           .= nsactCode
               , "action"         .= String "fetch_sublist"
               , "type_id"        .= (getTypeFromSubtype nsactSubtype)
               , "sublist_id"     .= nsactSubtype
               , "parent_id"      .= nsactID
               , "fields"         .= nsactFields
                 ]
    toJSON (NsActRawSearch {..}) =
        object [ "code"           .= nsactCode
               , "action"         .= String "raw_search"
               , "type_id"        .= nsactType
               , "data"           .= object ["filters" .= nsactFilters, "columns" .= nsactColumns]
                 ]
    toJSON (NsActSearch {..}) =
        object [ "code"           .= nsactCode
               , "action"         .= String "search"
               , "type_id"        .= nsactType
               , "data"           .= object ["filters" .= nsactFilters]
               , "fields"         .= nsactFields
                 ]
    toJSON (NsActCreate {..}) =
        object [ "code"           .= nsactCode
               , "action"         .= String "create"
               , "type_id"        .= nsactType
               , "data"           .= nsactData
               , "sublists"       .= nsactSubdata
               , "fields"         .= nsactFields
                 ]
    toJSON (NsActAttach {..}) =
        object [ "code"           .= nsactCode
               , "action"         .= String "attach"
               , "target_type_id" .= nsactTargetType
               , "type_id"        .= nsactFocalType
               , "attachee_id"    .= nsactID
               , "data"           .= (listToJsonArray $ map toJSON nsactTargetIds)
               , "attributes"     .= nsactData
                 ]
    toJSON (NsActDetach {..}) =
        object [ "code"           .= nsactCode
               , "action"         .= String "detach"
               , "target_type_id" .= nsactTargetType
               , "type_id"        .= nsactFocalType
               , "attachee_id"    .= nsactID
               , "data"           .= (listToJsonArray $ map toJSON nsactTargetIds)
                ]
    toJSON (NsActUpdate {..}) =
        object [ "code"           .= nsactCode
               , "action"         .= String "update"
               , "type_id"        .= nsactType
               , "data"           .= nsactData
               , "fields"         .= nsactFields
                 ]
    toJSON (NsActUpdateSublist {..}) =
        object [ "code"           .= nsactCode
               , "action"         .= String "update_sublist"
               , "type_id"        .= (getTypeFromSubtype nsactSubtype)
               , "parent_id"      .= nsactID
               , "sublist_id"     .= nsactSubtype
               , "data"           .= (listToJsonArray $ map toJSON nsactSublistD)
                 ]
    toJSON (NsActDelete {..}) =
        object [ "code"           .= nsactCode
               , "action"         .= String "delete"
               , "type_id"        .= nsactType
               , "data"           .= nsactDID
                 ]
    toJSON (NsActInvoicePDF {..}) =
        object [ "code"           .= nsactCode
               , "action"         .= String "invoice_pdf"
               , "invoice_id"     .= nsactID
                 ]
    toJSON (NsActTransform {..}) =
        object [ "code"           .= nsactCode
               , "action"         .= String "transform"
               , "source_type_id" .= nsactType
               , "source_id"      .= nsactID
               , "target_type_id" .= nsactTargetType
               , "data"           .= nsactData
               , "fields"         .= nsactFields
                 ]

--------------------------------------------------------------------------------
-- | List of search columns
type NsSearchCols = [NsSearchCol]

-- | Search column definition
-- | May be a single field for the current table
-- | May also reach across table joins
data NsSearchCol = NsSearchCol String (Maybe String) deriving (Data, Typeable, Show)

instance ToJSON NsSearchCol where
    toJSON (NsSearchCol colName joinName) =
        listToJsonArray . maybe base ((++) base . take 1 . repeat . stringToJsonString) $ joinName
      where
        base = [stringToJsonString colName]

-- | Converts lists of lists of strings into search columns
toSearchCols :: (IsNsSearchCol c) => [c] -> NsSearchCols
toSearchCols = Prelude.map toNsSearchCol

class IsNsSearchCol a where
    toNsSearchCol :: a -> NsSearchCol

instance IsNsSearchCol [[Char]] where
    toNsSearchCol (n:j:_) = NsSearchCol n (Just j)
    toNsSearchCol (n:_)   = NsSearchCol n Nothing
    toNsSearchCol _       = error "Not enough items in list to describe NsSearchCol"

--------------------------------------------------------------------------------
-- | List of search filters
type NsFilters = [NsFilter]

-- | Constructing a search filter
-- | eg. NsFilter "lastmodifieddate" Nothing OnOrAfter "2014-08-12"
data NsFilter = NsFilter String (Maybe String) NsSearchOp (Maybe String) (Maybe String) deriving (Data, Typeable, Show)

instance ToJSON NsFilter where
    toJSON = listToJsonArray . buildFilterArr
      where
        buildFilterArr (NsFilter name join op value1 value2) = maybe base ((++) base . v2) value2
            where
                base = [
                    stringToJsonString name,
                    maybe Null stringToJsonString join,
                    toJSON op,
                    maybe Null stringToJsonString value1]
                v2 x = [stringToJsonString x]

class IsNsFilter a where
    toNsFilter :: a -> NsFilter

instance IsNsFilter ([Char], NsSearchOp) where
    toNsFilter (f, op) = NsFilter f Nothing op Nothing Nothing

instance IsNsFilter ([Char], NsSearchOp, [Char]) where
    toNsFilter (f, op, v) = NsFilter f Nothing op (Just v) Nothing

instance IsNsFilter ([Char], [Char], NsSearchOp, [Char]) where
    toNsFilter (f, join, op, v) = NsFilter f (Just join) op (Just v) Nothing

instance IsNsFilter ([Char], [Char], NsSearchOp, [Char], [Char]) where
    toNsFilter (f, join, op, v, v2) = NsFilter f (Just join) op (Just v) (Just v2)

instance IsNsFilter ([Char], Maybe [Char], NsSearchOp, Maybe [Char], Maybe [Char]) where
    toNsFilter (f, join, op, v, v2) = NsFilter f join op v v2

--------------------------------------------------------------------------------
-- | All the types of search operators we have
data NsSearchOp = After                   |
                  AnyOf                   |
                  Before                  |
                  Between                 |
                  Contains                |
                  DoesNotContain          |
                  DoesNotStartWith        |
                  EqualTo                 |
                  GreaterThan             |
                  GreaterThanOrEqualTo    |
                  HasKeywords             |
                  Is                      |
                  IsEmpty                 |
                  IsNot                   |
                  IsNotEmpty              |
                  LessThan                |
                  LessThanOrEqualTo       |
                  NoneOf                  |
                  NotAfter                |
                  NotBefore               |
                  NotBetween              |
                  NotEqualTo              |
                  NotGreaterThan          |
                  NotGreaterThanOrEqualTo |
                  NotLessThan             |
                  NotLessThanOrEqualTo    |
                  NotOn                   |
                  NotOnOrAfter            |
                  NotOnOrBefore           |
                  NotWithin               |
                  On                      |
                  OnOrAfter               |
                  OnOrBefore              |
                  StartWith               |
                  Within
                  deriving (Data, Typeable, Show)

instance ToJSON NsSearchOp where
    toJSON = String . Text.pack . (Prelude.map toLower) . show
