{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Netsuite.Types.Search (
  NsFilters,
  NsFilter (..),
  NsSearchOp (..),
  NsSearchCols,
  NsSearchCol,
  toSearchCols
  ) where

import Data.Aeson
import Data.Aeson.TH
import Data.Char
import Data.Data
import Data.Typeable
import qualified Data.Text as Text

import Netsuite.Helpers

-- | List of search columns
type NsSearchCols = [NsSearchCol]

-- | Search column definition
-- | May be a single field for the current table
-- | May also reach across table joins

data NsSearchCol = NsSearchCol String (Maybe String) deriving (Data, Typeable, Show)

instance ToJSON NsSearchCol where
  toJSON (NsSearchCol colName joinName) =
    case joinName of
      Nothing -> listToJsonArray [stringToJsonString colName]
      Just j  -> listToJsonArray [stringToJsonString colName, stringToJsonString j]

-- | Converts lists of lists of strings into search columns
toSearchCols :: [[String]] -> NsSearchCols
toSearchCols l = Prelude.map toSearchCol l

toSearchCol :: [String] -> NsSearchCol
toSearchCol (n:[]) = NsSearchCol n Nothing
toSearchCol (n:j:_) = NsSearchCol n (Just j)

-- | List of search filters
type NsFilters = [NsFilter]

-- | Constructing a search filter
-- | eg. NsFilter "lastmodifieddate" Nothing OnOrAfter "2014-08-12"
data NsFilter = NsFilter String (Maybe String) NsSearchOp (Maybe String) (Maybe String) deriving (Data, Typeable, Show)

instance ToJSON NsFilter where
  toJSON = listToJsonArray . buildFilterArr

buildFilterArr :: NsFilter -> [Value]
buildFilterArr (NsFilter name join op value1 value2) =
  case value2 of
    Nothing -> [n', j, o', v1]
    Just x  -> [n', j, o', v1, (v2 x)]
  where
    n' = stringToJsonString name
    o' = toJSON op
    j = case join of
      Nothing -> Null
      Just x  -> stringToJsonString x
    v1 = case value1 of
      Nothing -> Null
      Just x  -> stringToJsonString x
    v2 x = stringToJsonString x

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
