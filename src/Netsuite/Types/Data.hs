{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedStrings  #-}
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
    toNsSearchCol,

    IsNsType,
    IsNsSubtype,
    IsNsId,
    IsNsDataId,
    IsNsFilter,
    IsNsSearchCol,
    IsNsData,
    IsNsSublistData,
    toNsId,
    toNsDataId,
    toNsType,
    toNsSubtype,
    toNsFilter,
    toNsData,
    toNsSublistData,

    testNsDataForId,

    typeFields
) where

import Data.Aeson
import Data.Aeson.Types
import Data.Char (toLower)
import Data.Data
import qualified Data.HashMap.Strict as HMS
import qualified Data.Text as Text

import Netsuite.Helpers
import Netsuite.Types.Data.Core
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
newtype NsSublistData = NsSublistData [(Text.Text, [NsData])] deriving (Data, Typeable, Show)

instance ToJSON NsSublistData where
    toJSON (NsSublistData x) = object . map (\(k, v) -> k .= v) $ x

class IsNsSublistData a where
    toNsSublistData :: a -> NsSublistData

instance (IsNsData a) => IsNsSublistData [(Text.Text, [a])] where
    toNsSublistData = NsSublistData . Prelude.map x
      where
        x (a, b) = (a, Prelude.map toNsData b)

--------------------------------------------------------------------------------
-- | Types of Netsuite actions to execute
data NsAction = NsActRetrieve {
    nsactType   :: NsType,
    nsactDID    :: NsDataId,
    nsactFields :: NsFields,
    nsactCode   :: NsRestletCode
} | NsActFetchSublist {
    nsactSubtype :: NsSubtype,
    nsactID      :: NsId,
    nsactFields  :: NsFields,
    nsactCode    :: NsRestletCode
} | NsActRawSearch {
    nsactType    :: NsType,
    nsactFilters :: NsFilters,
    nsactColumns :: NsSearchCols,
    nsactCode    :: NsRestletCode
} | NsActSearch {
    nsactType    :: NsType,
    nsactFilters :: NsFilters,
    nsactFields  :: NsFields,
    nsactCode    :: NsRestletCode
} | NsActCreate {
    nsactType    :: NsType,
    nsactData    :: NsData,
    nsactSubdata :: NsSublistData,
    nsactFields  :: NsFields,
    nsactCode    :: NsRestletCode
} | NsActAttach {
    nsactTargetType :: NsType,
    nsactTargetIds  :: [NsId],
    nsactFocalType  :: NsType,
    nsactID         :: NsId,
    nsactData       :: NsData,
    nsactCode       :: NsRestletCode
} | NsActDetach {
    nsactTargetType :: NsType,
    nsactTargetIds  :: [NsId],
    nsactFocalType  :: NsType,
    nsactID         :: NsId,
    nsactCode       :: NsRestletCode
} | NsActUpdate {
    nsactType   :: NsType,
    nsactData   :: NsData,
    nsactFields :: NsFields,
    nsactCode   :: NsRestletCode
} | NsActUpdateSublist {
    nsactSubtype  :: NsSubtype,
    nsactID       :: NsId,
    nsactSublistD :: [NsData],
    nsactCode     :: NsRestletCode
} | NsActDelete {
    nsactType :: NsType,
    nsactDID  :: NsDataId,
    nsactCode :: NsRestletCode
} | NsActInvoicePDF {
    nsactID   :: NsId,
    nsactCode :: NsRestletCode
} | NsActTransform {
    nsactType       :: NsType,
    nsactID         :: NsId,
    nsactTargetType :: NsType,
    nsactData       :: NsData,
    nsactFields     :: NsFields,
    nsactCode       :: NsRestletCode
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
               , "type_id"        .= getTypeFromSubtype nsactSubtype
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
               , "data"           .= listToJsonArray (map toJSON nsactTargetIds)
               , "attributes"     .= nsactData
                 ]
    toJSON (NsActDetach {..}) =
        object [ "code"           .= nsactCode
               , "action"         .= String "detach"
               , "target_type_id" .= nsactTargetType
               , "type_id"        .= nsactFocalType
               , "attachee_id"    .= nsactID
               , "data"           .= listToJsonArray (map toJSON nsactTargetIds)
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
               , "type_id"        .= getTypeFromSubtype nsactSubtype
               , "parent_id"      .= nsactID
               , "sublist_id"     .= nsactSubtype
               , "data"           .= listToJsonArray (map toJSON nsactSublistD)
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
data NsSearchCol = NsSearchCol Text.Text (Maybe Text.Text) deriving (Data, Typeable, Show)

instance ToJSON NsSearchCol where
    toJSON (NsSearchCol col_name join_name) =
        listToJsonArray . maybe base ((++) base . replicate 1 . String) $ join_name
      where
        base = [String col_name]

class IsNsSearchCol a where
    toNsSearchCol :: a -> NsSearchCol

instance IsNsSearchCol [Text.Text] where
    toNsSearchCol (n:j:_) = NsSearchCol n (Just j)
    toNsSearchCol (n:_)   = NsSearchCol n Nothing
    toNsSearchCol _       = error "Not enough items in list to describe NsSearchCol"

--------------------------------------------------------------------------------
-- | List of search filters
type NsFilters = [NsFilter]

-- | Constructing a search filter
-- | eg. NsFilter "lastmodifieddate" Nothing OnOrAfter "2014-08-12"
data NsFilter = NsFilter {
    filterField  :: Text.Text,
    filterJoin   :: Maybe Text.Text,
    filterOp     :: NsSearchOp,
    filterValue  :: Maybe Text.Text,
    filterValue2 :: Maybe Text.Text
} deriving (Data, Typeable, Show)

instance ToJSON NsFilter where
    toJSON = listToJsonArray . buildFilterArr
      where
        buildFilterArr (NsFilter name join op value1 value2) = maybe base ((++) base . v2) value2
            where
                base = [
                    String name,
                    maybe Null String join,
                    toJSON op,
                    maybe Null String value1]
                v2 x = [String x]

-- | Specifies whether we have an object of some kind that can be
-- converted into an NsFilter.
class IsNsFilter a where
    toNsFilter :: a -> NsFilter

-- | A filter composed of Field Name and Filter Operation only.
-- Most useful when working with Filter Operation like IsEmpty and IsNotEmpty.
instance IsNsFilter (Text.Text, NsSearchOp) where
    toNsFilter (f, op) = NsFilter f Nothing op Nothing Nothing

-- | A filter composed of Field Name, Filter Operation and Filter Value.
-- Use this when comparing the value of a particular Field to the Filter Value.
instance IsNsFilter (Text.Text, NsSearchOp, Text.Text) where
    toNsFilter (f, op, v) = NsFilter f Nothing op (Just v) Nothing

-- | A filter composed of Field Name, Field Entity Join, and Filter Operation.
instance IsNsFilter (Text.Text, Text.Text, NsSearchOp) where
    toNsFilter (f, join, op) = NsFilter f (Just join) op Nothing Nothing

-- | A filter composed of Field Name, Field Entity Join, Filter Operation and Filter Value.
instance IsNsFilter (Text.Text, Text.Text, NsSearchOp, Text.Text) where
    toNsFilter (f, join, op, v) = NsFilter f (Just join) op (Just v) Nothing

-- | A filter composed of Field Name, Filter Operation, Filter Value and Filter Value 2.
-- Useful when using Filter Operations like Between, NotBetween, Within, NotWithin, etc.
instance IsNsFilter (Text.Text, NsSearchOp, Text.Text, Text.Text) where
    toNsFilter (f, op, v, v2) = NsFilter f Nothing op (Just v) (Just v2)

-- | A filter composed of Field Name, Filter Join, Filter Operation, Filter Value and Filter Value 2.
instance IsNsFilter (Text.Text, Text.Text, NsSearchOp, Text.Text, Text.Text) where
    toNsFilter (f, join, op, v, v2) = NsFilter f (Just join) op (Just v) (Just v2)

-- | A filter composed of all the raw parameters that can compose a NsFilter more or less directly,
-- with Maybe used to dictate optional parameters.
instance IsNsFilter (Text.Text, Maybe Text.Text, NsSearchOp, Maybe Text.Text, Maybe Text.Text) where
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
    toJSON = String . Text.pack . Prelude.map toLower . show
