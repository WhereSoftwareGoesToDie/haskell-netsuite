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
    => cfg -- ^ Restlet configuration
    -> t -- ^ NetSuite entity type
    -> a -- ^ Entity ID
    -> IO (Either RestletError Value)
retrieveNS cfg t i =
    doNS (toNsRestletConfig cfg) $
        NsActRetrieve (toNsType t)
                      (toNsDataId i)
                      (typeFields (toNsRestletConfig cfg) $ toNsType t)

-- | Retrieves an object's sublists from Netsuite.
fetchSublistNS
    :: (IsNsRestletConfig cfg, IsNsSubtype st, IsNsId a)
    => cfg -- ^ Restlet configuration
    -> st -- ^ NetSuite entity subtype
    -> a -- ^ Entity ID
    -> IO (Either RestletError Value)
fetchSublistNS cfg st i =
    doNS (toNsRestletConfig cfg) $
        NsActFetchSublist (toNsSubtype st)
                          (toNsId i)
                          (typeFields (toNsRestletConfig cfg) $ toNsSubtype st)

-- | Does a raw search in Netsuite.
rawSearchNS
    :: (IsNsRestletConfig cfg, IsNsType t, IsNsSearchCol c)
    => cfg -- ^ Restlet configuration
    -> t -- ^ NetSuite entity type
    -> NsFilters -- ^ Search filters
    -> [c] -- ^ Columns to return
    -> IO (Either RestletError Value)
rawSearchNS cfg t fil col =
    doNS (toNsRestletConfig cfg) $
        NsActRawSearch (toNsType t) fil (map toNsSearchCol col)

-- | Does an object search in Netsuite.
searchNS
    :: (IsNsRestletConfig cfg, IsNsType t)
    => cfg -- ^ Restlet configuration
    -> t -- ^ NetSuite entity type
    -> NsFilters -- ^ Search filters
    -> IO (Either RestletError Value)
searchNS cfg t fil =
    doChunkableNS (toNsRestletConfig cfg) $
        NsActSearch (toNsType t)
                    fil
                    (typeFields (toNsRestletConfig cfg) $ toNsType t)

-- | Creates an object in Netsuite.
createNS
    :: (IsNsRestletConfig cfg, IsNsType t, IsNsData d, IsNsSublistData sd)
    => cfg -- ^ Restlet configuration
    -> t -- ^ NetSuite entity type
    -> d -- ^ Data to assign to new entity
    -> sd -- ^ Sublist data to assign to new entity
    -> IO (Either RestletError Value)
createNS cfg t d sd =
    doNS (toNsRestletConfig cfg) $
        NsActCreate (toNsType t)
                    (toNsData d)
                    (toNsSublistData sd)
                    (typeFields (toNsRestletConfig cfg) $ toNsType t)

-- | Attaches an object to another in Netsuite.
attachNS
    :: (IsNsRestletConfig cfg, IsNsType t, IsNsId a, IsNsData d)
    => cfg -- ^ Restlet configuration
    -> t -- ^ Type of entities to attach to
    -> [a] -- ^ Target entity IDs to attach to
    -> t -- ^ Type of entity to attach
    -> a -- ^ Entity ID to attach
    -> d -- ^ Data to assign to attachment
    -> IO (Either RestletError Value)
attachNS cfg targetType targetIDs attType attID attrs =
    doNS (toNsRestletConfig cfg) $
        NsActAttach (toNsType targetType)
                    (map toNsId targetIDs)
                    (toNsType attType)
                    (toNsId attID)
                    (toNsData attrs)

-- | Detaches an object from another in Netsuite.
detachNS
    :: (IsNsRestletConfig cfg, IsNsType t, IsNsId a)
    => cfg -- ^ Restlet configuration
    -> t -- ^ Type of entities to detach from
    -> [a] -- ^ Target entity IDs to detach from
    -> t -- ^ Type of entity to detach
    -> a -- ^ Entity ID to detach
    -> IO (Either RestletError Value)
detachNS cfg targetType targetIDs detType detID =
    doNS (toNsRestletConfig cfg) $
        NsActDetach (toNsType targetType)
                    (map toNsId targetIDs)
                    (toNsType detType)
                    (toNsId detID)

-- | Updates an object in Netsuite.
updateNS
    :: (IsNsRestletConfig cfg, IsNsType t, IsNsData d)
    => cfg -- ^ Restlet configuration
    -> t -- ^ NetSuite entity type
    -> d -- ^ Data to update entity with (must include ID)
    -> IO (Either RestletError Value)
updateNS cfg t d =
    if testNsDataForId $ toNsData d
    then doNS (toNsRestletConfig cfg) $
        NsActUpdate (toNsType t)
                    (toNsData d)
                    (typeFields (toNsRestletConfig cfg) $ toNsType t)
    else error "Update data does not contain ID."

-- | Updates an object's sublist in Netsuite.
updateSublistNS
    :: (IsNsRestletConfig cfg, IsNsSubtype st, IsNsId a, IsNsData d)
    => cfg -- ^ Restlet configuration
    -> st -- ^ NetSuite entity subtype
    -> a -- ^ Entity ID
    -> [d] -- ^ List of sublist items to update sublist with
    -> IO (Either RestletError Value)
updateSublistNS cfg st i d =
    doNS (toNsRestletConfig cfg) $
        NsActUpdateSublist (toNsSubtype st) (toNsId i) (map toNsData d)

-- | Deletes an object from Netsuite.
deleteNS
    :: (IsNsRestletConfig cfg, IsNsType t, IsNsDataId a)
    => cfg -- ^ Restlet configuration
    -> t -- ^ NetSuite entity type
    -> a -- ^ Entity ID to delete
    -> IO (Either RestletError Value)
deleteNS cfg t i =
    doNS (toNsRestletConfig cfg) $
        NsActDelete (toNsType t) (toNsDataId i)

-- | Fetches an Invoice PDF by ID.
invoicePdfNS
    :: (IsNsRestletConfig cfg, IsNsId a)
    => cfg -- ^ Restlet configuration
    -> a -- ^ Invoice entity ID
    -> IO (Either RestletError Value)
invoicePdfNS cfg i =
    doNS (toNsRestletConfig cfg) $
        NsActInvoicePDF (toNsId i)

-- | Transforms a Netsuite record to another type.
transformNS
    :: (IsNsRestletConfig cfg, IsNsType t, IsNsId a, IsNsData d)
    => cfg -- ^ Restlet configuration
    -> t -- ^ NetSuite entity source type (what you're transforming from)
    -> a -- ^ Entity ID
    -> t -- ^ NetSuite entity target type (what you're transforming into)
    -> d -- ^ Data to be used in this transformation
    -> IO (Either RestletError Value)
transformNS cfg st sid tt d =
    doNS (toNsRestletConfig cfg) $
        NsActTransform (toNsType st)
                       (toNsId sid)
                       (toNsType tt)
                       (toNsData d)
                       (typeFields (toNsRestletConfig cfg) $ toNsType tt)

-- | Performs a Netsuite restlet action with the normal runner.
doNS
    :: NsRestletConfig -- ^ Restlet configuration
    -> (NsRestletCode -> NsAction)
    -> IO (Either RestletError Value)
doNS = runAction restletExecute

-- | Performs a Netsuite restlet action with the chunkable runner.
doChunkableNS
    :: NsRestletConfig -- ^ Restlet configuration
    -> (NsRestletCode -> NsAction) -- ^ Partial action that requires
                                   -- restlet code
    -> IO (Either RestletError Value)
doChunkableNS = runAction chunkableRestletExecute

-- | Runs a generic action
runAction
    :: (BSL.ByteString -> NsRestletConfig -> IO RestletResponse) -- Restlet runner
    -> NsRestletConfig -- ^ Restlet configuration
    -> (NsRestletCode -> NsAction) -- ^ Partial action that requires
                                   -- restlet code
    -> IO (Either RestletError Value)
runAction runner cfg act = do
    code   <- restletCode
    let actJSON = reqJSON . act . NsRestletCode $ code
    result <- runner actJSON cfg
    return $ case result of
        RestletErrorResp{} -> Left $ interpretError result
        _                  -> Right $ responseToAeson result
  where
    reqJSON = encode
    restletCode = getDataFileName "Support/Restlet.js" >>= TextIO.readFile
