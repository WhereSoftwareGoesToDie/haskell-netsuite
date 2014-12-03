{-# LANGUAGE PackageImports #-}

module Netsuite.Restlet.Configuration where

import Network.URI

-- | Configuration for Netsuite Restlet
data NsRestletConfig = NsRestletConfig {
  restletURI       :: URI,
  restletAccountID :: Integer,
  restletRole      :: Integer,
  restletIdent     :: String,
  restletPassword  :: String,
  restletUA        :: Maybe String
} deriving (Show)
