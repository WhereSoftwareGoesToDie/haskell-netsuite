{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PackageImports     #-}
{-# LANGUAGE RecordWildCards    #-}

module Netsuite.Types.Data where

import Data.Aeson
import Data.Aeson.Types
import Data.Char (toLower)
import Data.Data
import qualified Data.HashMap.Strict as HMS
import qualified Data.Text as Text

import Netsuite.Helpers

--------------------------------------------------------------------------------
-- | Container for Netsuite restlet code
newtype NsRestletCode = NsRestletCode Text.Text deriving (Data, Typeable, Show)

getCode :: NsRestletCode -> Text.Text
getCode (NsRestletCode x) = x

instance ToJSON NsRestletCode where
  toJSON (NsRestletCode s) = toJSON s

--------------------------------------------------------------------------------
-- | Netsuite record type
newtype NsType = NsType String deriving (Data, Typeable, Show)

instance ToJSON NsType where
  toJSON (NsType s) = toJSON s

--------------------------------------------------------------------------------
-- | Netsuite record subtype
data NsSubtype = NsSubtype NsType String deriving (Data, Typeable, Show)

instance ToJSON NsSubtype where
  toJSON (NsSubtype _ s) = toJSON s

getTypeFromSubtype :: NsSubtype -> NsType
getTypeFromSubtype (NsSubtype t _) = t

--------------------------------------------------------------------------------
-- | Netsuite entity ID, for requests that only take an ID
newtype NsId = NsId Integer deriving (Data, Typeable, Show)

instance ToJSON NsId where
  toJSON (NsId _id) = String . Text.pack . show $ _id

--------------------------------------------------------------------------------
-- | Netsuite entity ID, wrapped up in a data object
newtype NsDataId = NsDataId Integer deriving (Data, Typeable, Show)

instance ToJSON NsDataId where
  toJSON (NsDataId _id) = object ["id" .= show _id]

--------------------------------------------------------------------------------
-- | List of Netsuite fields to retrieve
newtype NsFields = NsFields [String] deriving (Data, Typeable, Show)

instance ToJSON NsFields where
  toJSON (NsFields f) = toJSON f

--------------------------------------------------------------------------------
-- | List of key/value pairs of data to send to Netsuite
newtype NsData = NsData Value deriving (Data, Typeable, Show)

instance ToJSON NsData where
  toJSON (NsData m) = toJSON m

newNsData :: [Pair] -> NsData
newNsData = NsData . object

testNsDataForId :: NsData -> Bool
testNsDataForId (NsData (Object o)) = HMS.member "id" o
testNsDataForId _                   = False

--------------------------------------------------------------------------------
-- | Netsuite Sublist data dictionaries
newtype NsSublistData = NsSublistData [(String, [NsData])] deriving (Data, Typeable, Show)

instance ToJSON NsSublistData where
  toJSON (NsSublistData x) = object . map (\(k, v) -> (Text.pack k) .= v) $ x

--------------------------------------------------------------------------------
-- | Types of Netsuite actions to execute
data NsAction = NsActRetrieve {
    nsarCode   :: NsRestletCode,
    nsarType   :: NsType,
    nsarId     :: NsDataId,
    nsarFields :: NsFields
  } |
  NsActFetchSublist {
    nsafsCode     :: NsRestletCode,
    nsafsSubtype  :: NsSubtype,
    nsafsParentId :: NsId,
    nsafsFields   :: NsFields
  } |
  NsActRawSearch {
    nsrwsCode    :: NsRestletCode,
    nsrwsType    :: NsType,
    nsrwsFilters :: NsFilters,
    nsrwsColumns :: NsSearchCols
  } |
  NsActSearch {
    nssCode    :: NsRestletCode,
    nssType    :: NsType,
    nssFilters :: NsFilters,
    nssFields  :: NsFields
  } |
  NsActCreate {
    nscCode     :: NsRestletCode,
    nscType     :: NsType,
    nscData     :: NsData,
    nscSublists :: NsSublistData,
    nscFields   :: NsFields
  } |
  NsActAttach {
    nsaCode       :: NsRestletCode,
    nsaTargetType :: NsType,
    nsaTargetIds  :: [NsId],
    nsaAttachType :: NsType,
    nsaAttachId   :: NsId,
    nsaAttributes :: NsData
  } |
  NsActDetach {
    nsdCode       :: NsRestletCode,
    nsdTargetType :: NsType,
    nsdTargetIds  :: [NsId],
    nsdDetachType :: NsType,
    nsdDetachId   :: NsId
  } |
  NsActUpdate {
    nsuCode   :: NsRestletCode,
    nsuType   :: NsType,
    nsuData   :: NsData,
    nsuFields :: NsFields
  } |
  NsActUpdateSublist {
    nsusCode     :: NsRestletCode,
    nsusSubtype  :: NsSubtype,
    nsusParentId :: NsId,
    nsusData     :: [NsData]
  } |
  NsActDelete {
    nsdlCode :: NsRestletCode,
    nsdlType :: NsType,
    nsdlId   :: NsDataId
  } |
  NsActInvoicePDF {
    nsinvCode      :: NsRestletCode,
    nsinvInvoiceId :: NsId
  } |
  NsActTransform {
    nstrCode       :: NsRestletCode,
    nstrSourceType :: NsType,
    nstrSourceId   :: NsId,
    nstrTargetType :: NsType,
    nstrData       :: NsData,
    nstrFields     :: NsFields
  } deriving (Data, Typeable, Show)

instance ToJSON NsAction where
  toJSON (NsActRetrieve {..}) =
    object [ "code"           .= nsarCode
           , "action"         .= String "retrieve"
           , "type_id"        .= nsarType
           , "data"           .= nsarId
           , "fields"         .= nsarFields
             ]
  toJSON (NsActFetchSublist {..}) =
    object [ "code"           .= nsafsCode
           , "action"         .= String "fetch_sublist"
           , "type_id"        .= (getTypeFromSubtype nsafsSubtype)
           , "sublist_id"     .= nsafsSubtype
           , "parent_id"      .= nsafsParentId
           , "fields"         .= nsafsFields
             ]
  toJSON (NsActRawSearch {..}) =
    object [ "code"           .= nsrwsCode
           , "action"         .= String "raw_search"
           , "type_id"        .= nsrwsType
           , "data"           .= object ["filters" .= nsrwsFilters, "columns" .= nsrwsColumns]
             ]
  toJSON (NsActSearch {..}) =
    object [ "code"           .= nssCode
           , "action"         .= String "search"
           , "type_id"        .= nssType
           , "data"           .= object ["filters" .= nssFilters]
           , "fields"         .= nssFields
             ]
  toJSON (NsActCreate {..}) =
    object [ "code"           .= nscCode
           , "action"         .= String "create"
           , "type_id"        .= nscType
           , "data"           .= nscData
           , "sublists"       .= nscSublists
           , "fields"         .= nscFields
             ]
  toJSON (NsActAttach {..}) =
    object [ "code"           .= nsaCode
           , "action"         .= String "attach"
           , "target_type_id" .= nsaTargetType
           , "type_id"        .= nsaAttachType
           , "attachee_id"    .= nsaAttachId
           , "data"           .= (listToJsonArray $ map toJSON nsaTargetIds)
           , "attributes"     .= nsaAttributes
             ]
  toJSON (NsActDetach {..}) =
    object [ "code"           .= nsdCode
           , "action"         .= String "detach"
           , "target_type_id" .= nsdTargetType
           , "type_id"        .= nsdDetachType
           , "attachee_id"    .= nsdDetachId
           , "data"           .= (listToJsonArray $ map toJSON nsdTargetIds)
             ]
  toJSON (NsActUpdate {..}) =
    object [ "code"           .= nsuCode
           , "action"         .= String "update"
           , "type_id"        .= nsuType
           , "data"           .= nsuData
           , "fields"         .= nsuFields
             ]
  toJSON (NsActUpdateSublist {..}) =
    object [ "code"           .= nsusCode
           , "action"         .= String "update_sublist"
           , "type_id"        .= (getTypeFromSubtype nsusSubtype)
           , "parent_id"      .= nsusParentId
           , "sublist_id"     .= nsusSubtype
           , "data"           .= (listToJsonArray $ map toJSON nsusData)
             ]
  toJSON (NsActDelete {..}) =
    object [ "code"           .= nsdlCode
           , "action"         .= String "delete"
           , "type_id"        .= nsdlType
           , "data"           .= nsdlId
             ]
  toJSON (NsActInvoicePDF {..}) =
    object [ "code"           .= nsinvCode
           , "action"         .= String "invoice_pdf"
           , "invoice_id"     .= nsinvInvoiceId
             ]
  toJSON (NsActTransform {..}) =
    object [ "code"           .= nstrCode
           , "action"         .= String "transform"
           , "source_type_id" .= nstrSourceType
           , "source_id"      .= nstrSourceId
           , "target_type_id" .= nstrTargetType
           , "data"           .= nstrData
           , "fields"         .= nstrFields
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
toSearchCols :: [[String]] -> NsSearchCols
toSearchCols l = Prelude.map toSearchCol l
  where
    toSearchCol (n:[]) = NsSearchCol n Nothing
    toSearchCol (n:j:_) = NsSearchCol n (Just j)

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
