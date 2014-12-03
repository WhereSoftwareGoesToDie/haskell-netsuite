{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE PackageImports       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Netsuite.Restlet.Configuration (
    NsRestletConfig (..),
    IsNsRestletConfig,
    toNsRestletConfig
) where

import Data.List
import Data.Map
import Data.Maybe
import Network.URI

data NsRestletConfig = NsRestletConfig {
  restletURI          :: URI,
  restletAccountID    :: Integer,
  restletRole         :: Integer,
  restletIdent        :: String,
  restletPassword     :: String,
  restletUA           :: Maybe String,
  restletCustomFields :: Maybe (Map [String] [String])
} deriving (Eq)

instance Show NsRestletConfig where
    show (NsRestletConfig {..}) = intercalate ", " [
        show restletURI,
        show restletAccountID,
        show restletRole,
        restletIdent,
        restletPassword,
        maybe "(default user agent)" show restletUA]

-- | Configuration for Netsuite Restlet endpoint.
-- Requires the following information to be able to connect:
-- RESTlet Endpoint URL
-- Account ID
-- Role ID
-- User identifier
-- User password
-- Also optionally takes a custom user agent, and a mapping of entity types/subtypes to custom fields.
class IsNsRestletConfig a where
    toNsRestletConfig :: a -> NsRestletConfig

-- | Configuration containing:
-- RESTlet Endpoint URL
-- Account ID
-- Role ID
-- User identifier
-- User password
instance (Integral a) => IsNsRestletConfig (String, a, a, String, String) where
    toNsRestletConfig (u, a, r, i, p) = NsRestletConfig (justParseURI u) (toInteger a) (toInteger r) i p Nothing Nothing

-- | Configuration containing:
-- RESTlet Endpoint URL
-- Account ID
-- Role ID
-- User identifier
-- User password
-- Custom User Agent
instance (Integral a) => IsNsRestletConfig (String, a, a, String, String, String) where
    toNsRestletConfig (u, a, r, i, p, ua) = NsRestletConfig (justParseURI u) (toInteger a) (toInteger r) i p (Just ua) Nothing

-- | Configuration containing:
-- RESTlet Endpoint URL
-- Account ID
-- Role ID
-- User identifier
-- User password
-- Mapping of entity types/subtypes to custom fields to retrieve
instance (Integral a) => IsNsRestletConfig (String, a, a, String, String, Map [String] [String]) where
    toNsRestletConfig (u, a, r, i, p, cf) = NsRestletConfig (justParseURI u) (toInteger a) (toInteger r) i p Nothing (Just cf)

-- | Configuration containing:
-- RESTlet Endpoint URL
-- Account ID
-- Role ID
-- User identifier
-- User password
-- Custom User Agent
-- Mapping of entity types/subtypes to custom fields to retrieve
instance (Integral a) => IsNsRestletConfig (String, a, a, String, String, String, Map [String] [String]) where
    toNsRestletConfig (u, a, r, i, p, ua, cf) = NsRestletConfig (justParseURI u) (toInteger a) (toInteger r) i p (Just ua) (Just cf)

justParseURI :: String -> URI
justParseURI = fromJust . parseURI
