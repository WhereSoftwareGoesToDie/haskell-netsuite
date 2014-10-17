{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}

module Netsuite.Connect (
  retrieveNS,
  fetchSublistNS,
  rawSearchNS,
  searchNS,
  createNS,
  attachNS,
  detachNS,
  updateNS,
  updateSublistNS,
  deleteNS,
  invoicePdfNS,
  transformNS,
  newNsData,
  NsRestletConfig (..),
  NsType (..),
  NsSubtype (..),
  NsData (..),
  NsSublistData (..),
  NsDataId (..),
  NsId (..),
  NsFilters,
  NsFilter,
  NsSearchOp,
  NsSearchCols,
  NsSearchCol,
  RestletError(..),
  toSearchCols
  ) where

import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO

import "network-uri" Network.URI

import Paths_netsuite (getDataFileName)

import Netsuite.Helpers
import Netsuite.Restlet
import Netsuite.Restlet.Configuration
import Netsuite.Restlet.Response
import Netsuite.Types.Compile
import Netsuite.Types.Data
import Netsuite.Types.Fields
import Netsuite.Types.Search

import Network.Http.Client (HttpClientError)

-- | Retrieves an object from Netsuite.
retrieveNS :: NsRestletConfig -> NsType -> NsDataId -> IO (Either RestletError Value)
retrieveNS cfg t i = do
  code <- restletCode
  doNS cfg (NsActRetrieve (NsRestletCode code) t i f)
  where
    f = nsTypeFields t

-- | Retrieves an object's sublists from Netsuite.
fetchSublistNS :: NsRestletConfig -> NsSubtype -> NsId -> IO (Either RestletError Value)
fetchSublistNS cfg st i = do
  code <- restletCode
  doNS cfg (NsActFetchSublist (NsRestletCode code) st i f)
  where
    f = nsSubtypeFields st

-- | Does a raw search in Netsuite.
rawSearchNS :: NsRestletConfig -> NsType -> NsFilters -> NsSearchCols -> IO (Either RestletError Value)
rawSearchNS cfg t fil f = do
  code <- restletCode
  doNS cfg (NsActRawSearch (NsRestletCode code) t fil f)

-- | Does an object search in Netsuite.
searchNS :: NsRestletConfig -> NsType -> NsFilters -> IO (Either RestletError Value)
searchNS cfg t fil = do
  code <- restletCode
  doChunkableNS cfg (NsActSearch (NsRestletCode code) t fil f)
  where
    f = nsTypeFields t

-- | Creates an object in Netsuite.
createNS :: NsRestletConfig -> NsType -> NsData -> NsSublistData -> IO (Either RestletError Value)
createNS cfg t d sd = do
  code <- restletCode
  doNS cfg (NsActCreate (NsRestletCode code) t d sd f)
  where
    f = nsTypeFields t

-- | Attaches an object to another in Netsuite.
attachNS :: NsRestletConfig -> NsType -> [NsId] -> NsType -> NsId -> NsData -> IO (Either RestletError Value)
attachNS cfg targetType targetIDs attType attID attrs = do
  code <- restletCode
  doNS cfg (NsActAttach (NsRestletCode code) targetType targetIDs attType attID attrs)

-- | Detaches an object from another in Netsuite.
detachNS :: NsRestletConfig -> NsType -> [NsId] -> NsType -> NsId -> IO (Either RestletError Value)
detachNS cfg targetType targetIDs detType detID = do
  code <- restletCode
  doNS cfg (NsActDetach (NsRestletCode code) targetType targetIDs detType detID)

-- | Updates an object in Netsuite.
updateNS :: NsRestletConfig -> NsType -> NsData -> IO (Either RestletError Value)
updateNS cfg t d = 
  if testNsDataForId d
  then do
    code <- restletCode
    doNS cfg (NsActUpdate (NsRestletCode code) t d f)
  else error "Update data does not contain ID."
  where
      f = nsTypeFields t

-- | Updates an object's sublist in Netsuite.
updateSublistNS :: NsRestletConfig -> NsSubtype -> NsId -> [NsData] -> IO (Either RestletError Value)
updateSublistNS cfg st i d = do
  code <- restletCode
  doNS cfg (NsActUpdateSublist (NsRestletCode code) st i d)

-- | Deletes an object from Netsuite.
deleteNS :: NsRestletConfig -> NsType -> NsDataId -> IO (Either RestletError Value)
deleteNS cfg t i = do
  code <- restletCode
  doNS cfg (NsActDelete (NsRestletCode code) t i)

-- | Fetches an Invoice PDF by ID.
invoicePdfNS :: NsRestletConfig -> NsId -> IO (Either RestletError Value)
invoicePdfNS cfg i = do
  code <- restletCode
  doNS cfg (NsActInvoicePDF (NsRestletCode code) i)

-- | Transforms a Netsuite record to another type.
transformNS :: NsRestletConfig -> NsType -> NsId -> NsType -> NsData -> IO (Either RestletError Value)
transformNS cfg st sid tt d = do
  code <- restletCode
  doNS cfg (NsActTransform (NsRestletCode code) st sid tt d f)
  where
    f = nsTypeFields tt

-- | Performs a Netsuite restlet action.
doNS :: NsRestletConfig -> NsAction -> IO (Either RestletError Value)
doNS cfg act = do
  result <- restletExecute (reqJSON act) cfg
  case result of
    x@(RestletErrorResp _) -> return $ Left $ interpretError x
    y                      -> return $ Right $ responseToAeson y
  where
    reqJSON = bytesToString . BSL.unpack . restletJSON

-- | Performs a Netsuite restlet action.
doChunkableNS :: NsRestletConfig -> NsAction -> IO (Either RestletError Value)
doChunkableNS cfg act = do
  result <- chunkableRestletExecute (reqJSON act) cfg
  case result of
    x@(RestletErrorResp _) -> return $ Left $ interpretError x
    y                      -> return $ Right $ responseToAeson y
  where
    reqJSON = bytesToString . BSL.unpack . restletJSON

-- | Loads the Netsuite restlet code.
restletCode :: IO Text.Text
restletCode = do
  file <- getDataFileName "Support/Restlet.js"
  x <- TextIO.readFile file
  return x
