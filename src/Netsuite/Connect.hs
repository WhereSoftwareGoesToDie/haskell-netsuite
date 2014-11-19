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
    NsRestletConfig (..),
    NsFilters,
    NsFilter (..),
    NsSearchOp (..),
    RestletError(..),

    IsNsType,
    IsNsSubtype,
    IsNsId,
    IsNsDataId,
    IsNsFilter,
    IsNsData,
    IsNsSublistData,
    toNsFilter
) where

import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO

import Paths_netsuite (getDataFileName)

import Netsuite.Helpers
import Netsuite.Restlet
import Netsuite.Restlet.Configuration
import Netsuite.Restlet.Response
import Netsuite.Types.Compile
import Netsuite.Types.Data
import Netsuite.Types.Fields

import Network.Http.Client (HttpClientError)

-- | Retrieves an object from Netsuite.
retrieveNS
    :: (IsNsType t, IsNsDataId a)
    => NsRestletConfig
    -> t
    -> a
    -> IO (Either RestletError Value)
retrieveNS cfg t i = do
    code <- restletCode
    doNS cfg (NsActRetrieve (NsRestletCode code) t' (toNsDataId i) f)
  where
    t' = toNsType t
    f  = typeFields cfg t'

-- | Retrieves an object's sublists from Netsuite.
fetchSublistNS
    :: (IsNsSubtype st, IsNsId a)
    => NsRestletConfig
    -> st
    -> a
    -> IO (Either RestletError Value)
fetchSublistNS cfg st i = do
    code <- restletCode
    doNS cfg (NsActFetchSublist (NsRestletCode code) st' (toNsId i) f)
  where
    st' = toNsSubtype st
    f   = typeFields cfg st'

-- | Does a raw search in Netsuite.
rawSearchNS
    :: (IsNsType t, IsNsSearchCol c)
    => NsRestletConfig
    -> t
    -> NsFilters
    -> [c]
    -> IO (Either RestletError Value)
rawSearchNS cfg t fil col = do
    code <- restletCode
    doNS cfg (NsActRawSearch (NsRestletCode code) (toNsType t) fil (toSearchCols col))

-- | Does an object search in Netsuite.
searchNS
    :: (IsNsType t)
    => NsRestletConfig
    -> t
    -> NsFilters
    -> IO (Either RestletError Value)
searchNS cfg t fil = do
    code <- restletCode
    doChunkableNS cfg (NsActSearch (NsRestletCode code) (toNsType t) fil f)
  where
    f = typeFields cfg (toNsType t)

-- | Creates an object in Netsuite.
createNS
    :: (IsNsType t, IsNsData d, IsNsSublistData sd)
    => NsRestletConfig
    -> t
    -> d
    -> sd
    -> IO (Either RestletError Value)
createNS cfg t d sd = do
    code <- restletCode
    doNS cfg (NsActCreate (NsRestletCode code) (toNsType t) (toNsData d) (toNsSublistData sd) f)
  where
    f = typeFields cfg (toNsType t)

-- | Attaches an object to another in Netsuite.
attachNS
    :: (IsNsType t, IsNsId a, IsNsData d)
    => NsRestletConfig
    -> t
    -> [a]
    -> t
    -> a
    -> d
    -> IO (Either RestletError Value)
attachNS cfg targetType targetIDs attType attID attrs = do
    code <- restletCode
    doNS cfg (NsActAttach (NsRestletCode code) (toNsType targetType) (map toNsId targetIDs) (toNsType attType) (toNsId attID) (toNsData attrs))

-- | Detaches an object from another in Netsuite.
detachNS
    :: (IsNsType t, IsNsId a)
    => NsRestletConfig
    -> t
    -> [a]
    -> t
    -> a
    -> IO (Either RestletError Value)
detachNS cfg targetType targetIDs detType detID = do
    code <- restletCode
    doNS cfg (NsActDetach (NsRestletCode code) (toNsType targetType) (map toNsId targetIDs) (toNsType detType) (toNsId detID))

-- | Updates an object in Netsuite.
updateNS
    :: (IsNsType t, IsNsData d)
    => NsRestletConfig
    -> t
    -> d
    -> IO (Either RestletError Value)
updateNS cfg t d = 
    if testNsDataForId d'
    then do
        code <- restletCode
        doNS cfg (NsActUpdate (NsRestletCode code) (toNsType t) d' f)
    else error "Update data does not contain ID."
  where
    f  = typeFields cfg (toNsType t)
    d' = toNsData d

-- | Updates an object's sublist in Netsuite.
updateSublistNS
    :: (IsNsSubtype st, IsNsId a, IsNsData d)
    => NsRestletConfig
    -> st
    -> a
    -> [d]
    -> IO (Either RestletError Value)
updateSublistNS cfg st i d = do
    code <- restletCode
    doNS cfg (NsActUpdateSublist (NsRestletCode code) (toNsSubtype st) (toNsId i) (map toNsData d))

-- | Deletes an object from Netsuite.
deleteNS
    :: (IsNsType t, IsNsDataId a)
    => NsRestletConfig
    -> t
    -> a
    -> IO (Either RestletError Value)
deleteNS cfg t i = do
    code <- restletCode
    doNS cfg (NsActDelete (NsRestletCode code) (toNsType t) (toNsDataId i))

-- | Fetches an Invoice PDF by ID.
invoicePdfNS
    :: (IsNsId a)
    => NsRestletConfig
    -> a
    -> IO (Either RestletError Value)
invoicePdfNS cfg i = do
    code <- restletCode
    doNS cfg (NsActInvoicePDF (NsRestletCode code) (toNsId i))

-- | Transforms a Netsuite record to another type.
transformNS
    :: (IsNsType t, IsNsId a, IsNsData d)
    => NsRestletConfig
    -> t
    -> a
    -> t
    -> d
    -> IO (Either RestletError Value)
transformNS cfg st sid tt d = do
    code <- restletCode
    doNS cfg (NsActTransform (NsRestletCode code) (toNsType st) (toNsId sid) tt' (toNsData d) f)
  where
    tt' = toNsType tt
    f   = typeFields cfg tt'

-- | Performs a Netsuite restlet action.
doNS
    :: NsRestletConfig
    -> NsAction
    -> IO (Either RestletError Value)
doNS = runAction restletExecute

-- | Performs a Netsuite restlet action.
doChunkableNS
    :: NsRestletConfig
    -> NsAction
    -> IO (Either RestletError Value)
doChunkableNS = runAction chunkableRestletExecute

-- | Runs a generic action
runAction
    :: (String -> NsRestletConfig -> IO RestletResponse)
    -> NsRestletConfig
    -> NsAction
    -> IO (Either RestletError Value)
runAction runner cfg act = do
    result <- runner (reqJSON act) cfg
    case result of
        x@(RestletErrorResp _) -> return $ Left $ interpretError x
        y                      -> return $ Right $ responseToAeson y
  where
    reqJSON = bytesToString . BSL.unpack . encode

-- | Loads the Netsuite restlet code.
restletCode :: IO Text.Text
restletCode = getDataFileName "Support/Restlet.js" >>= TextIO.readFile
