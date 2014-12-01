{-# LANGUAGE OverloadedStrings #-}

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
    NsFilters,
    NsFilter (..),
    NsSearchOp (..),
    RestletError (..),

    IsNsType,
    IsNsSubtype,
    IsNsId,
    IsNsDataId,
    IsNsFilter,
    IsNsData,
    IsNsSublistData,
    IsNsRestletConfig,
    toNsFilter
) where

import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.IO as TextIO

import Paths_netsuite (getDataFileName)

import Netsuite.Helpers
import Netsuite.Restlet
import Netsuite.Restlet.Configuration
import Netsuite.Restlet.Response
import Netsuite.Types.Compile
import Netsuite.Types.Data

-- | Retrieves an object from Netsuite.
retrieveNS
    :: (IsNsRestletConfig cfg, IsNsType t, IsNsDataId a)
    => cfg
    -> t
    -> a
    -> IO (Either RestletError Value)
retrieveNS cfg t i = doNS cfg' $ NsActRetrieve t' (toNsDataId i) f
  where
    t'   = toNsType t
    f    = typeFields cfg' t'
    cfg' = toNsRestletConfig cfg

-- | Retrieves an object's sublists from Netsuite.
fetchSublistNS
    :: (IsNsRestletConfig cfg, IsNsSubtype st, IsNsId a)
    => cfg
    -> st
    -> a
    -> IO (Either RestletError Value)
fetchSublistNS cfg st i = doNS cfg' $ NsActFetchSublist st' (toNsId i) f
  where
    st'  = toNsSubtype st
    f    = typeFields cfg' st'
    cfg' = toNsRestletConfig cfg

-- | Does a raw search in Netsuite.
rawSearchNS
    :: (IsNsRestletConfig cfg, IsNsType t, IsNsSearchCol c)
    => cfg
    -> t
    -> NsFilters
    -> [c]
    -> IO (Either RestletError Value)
rawSearchNS cfg t fil col = doNS (toNsRestletConfig cfg) $ NsActRawSearch (toNsType t) fil (map toNsSearchCol col)

-- | Does an object search in Netsuite.
searchNS
    :: (IsNsRestletConfig cfg, IsNsType t)
    => cfg
    -> t
    -> NsFilters
    -> IO (Either RestletError Value)
searchNS cfg t fil = doChunkableNS cfg' $ NsActSearch (toNsType t) fil f
  where
    f    = typeFields cfg' (toNsType t)
    cfg' = toNsRestletConfig cfg

-- | Creates an object in Netsuite.
createNS
    :: (IsNsRestletConfig cfg, IsNsType t, IsNsData d, IsNsSublistData sd)
    => cfg
    -> t
    -> d
    -> sd
    -> IO (Either RestletError Value)
createNS cfg t d sd = doNS cfg' $ NsActCreate (toNsType t) (toNsData d) (toNsSublistData sd) f
  where
    f    = typeFields cfg' (toNsType t)
    cfg' = toNsRestletConfig cfg

-- | Attaches an object to another in Netsuite.
attachNS
    :: (IsNsRestletConfig cfg, IsNsType t, IsNsId a, IsNsData d)
    => cfg
    -> t
    -> [a]
    -> t
    -> a
    -> d
    -> IO (Either RestletError Value)
attachNS cfg targetType targetIDs attType attID attrs = doNS (toNsRestletConfig cfg) $ NsActAttach (toNsType targetType) (map toNsId targetIDs) (toNsType attType) (toNsId attID) (toNsData attrs)

-- | Detaches an object from another in Netsuite.
detachNS
    :: (IsNsRestletConfig cfg, IsNsType t, IsNsId a)
    => cfg
    -> t
    -> [a]
    -> t
    -> a
    -> IO (Either RestletError Value)
detachNS cfg targetType targetIDs detType detID = doNS (toNsRestletConfig cfg) $ NsActDetach (toNsType targetType) (map toNsId targetIDs) (toNsType detType) (toNsId detID)

-- | Updates an object in Netsuite.
updateNS
    :: (IsNsRestletConfig cfg, IsNsType t, IsNsData d)
    => cfg
    -> t
    -> d
    -> IO (Either RestletError Value)
updateNS cfg t d =
    if testNsDataForId d'
    then doNS cfg' $ NsActUpdate t' d' f
    else error "Update data does not contain ID."
  where
    f    = typeFields cfg' t'
    d'   = toNsData d
    t'   = toNsType t
    cfg' = toNsRestletConfig cfg

-- | Updates an object's sublist in Netsuite.
updateSublistNS
    :: (IsNsRestletConfig cfg, IsNsSubtype st, IsNsId a, IsNsData d)
    => cfg
    -> st
    -> a
    -> [d]
    -> IO (Either RestletError Value)
updateSublistNS cfg st i d = doNS (toNsRestletConfig cfg) $ NsActUpdateSublist (toNsSubtype st) (toNsId i) (map toNsData d)

-- | Deletes an object from Netsuite.
deleteNS
    :: (IsNsRestletConfig cfg, IsNsType t, IsNsDataId a)
    => cfg
    -> t
    -> a
    -> IO (Either RestletError Value)
deleteNS cfg t i = doNS (toNsRestletConfig cfg) $ NsActDelete (toNsType t) (toNsDataId i)

-- | Fetches an Invoice PDF by ID.
invoicePdfNS
    :: (IsNsRestletConfig cfg, IsNsId a)
    => cfg
    -> a
    -> IO (Either RestletError Value)
invoicePdfNS cfg i = doNS (toNsRestletConfig cfg) $ NsActInvoicePDF (toNsId i)

-- | Transforms a Netsuite record to another type.
transformNS
    :: (IsNsRestletConfig cfg, IsNsType t, IsNsId a, IsNsData d)
    => cfg
    -> t
    -> a
    -> t
    -> d
    -> IO (Either RestletError Value)
transformNS cfg st sid tt d = doNS cfg' $ NsActTransform (toNsType st) (toNsId sid) tt' (toNsData d) f
  where
    tt'  = toNsType tt
    f    = typeFields cfg' tt'
    cfg' = toNsRestletConfig cfg

-- | Performs a Netsuite restlet action.
doNS
    :: NsRestletConfig
    -> (NsRestletCode -> NsAction)
    -> IO (Either RestletError Value)
doNS = runAction restletExecute

-- | Performs a Netsuite restlet action.
doChunkableNS
    :: NsRestletConfig
    -> (NsRestletCode -> NsAction)
    -> IO (Either RestletError Value)
doChunkableNS = runAction chunkableRestletExecute

-- | Runs a generic action
runAction
    :: (String -> NsRestletConfig -> IO RestletResponse)
    -> NsRestletConfig
    -> (NsRestletCode -> NsAction)
    -> IO (Either RestletError Value)
runAction runner cfg act = do
    code   <- restletCode
    let actJSON = reqJSON . act . NsRestletCode $ code
    result <- runner actJSON cfg
    case result of
        x@(RestletErrorResp _) -> return $ Left $ interpretError x
        y                      -> return $ Right $ responseToAeson y
  where
    reqJSON = bytesToString . BSL.unpack . encode
    restletCode = getDataFileName "Support/Restlet.js" >>= TextIO.readFile
