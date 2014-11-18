{-# LANGUAGE PackageImports  #-}
{-# LANGUAGE RecordWildCards #-}

module Netsuite.Restlet.Configuration where

import Data.HashMap
import Data.List
import Data.Maybe
import "network-uri" Network.URI

-- | Configuration for Netsuite Restlet
data NsRestletConfig = NsRestletConfig {
  restletURI          :: URI,
  restletAccountID    :: Integer,
  restletRole         :: Integer,
  restletIdent        :: String,
  restletPassword     :: String,
  restletUA           :: Maybe String,
  restletCustomFields :: Maybe (HashMap [String] [String])
}

instance Show NsRestletConfig where
	show (NsRestletConfig {..}) = intercalate ", " [
		show restletURI,
		show restletAccountID,
		show restletRole,
		restletIdent,
		restletPassword,
		maybe "(default user agent)" show restletUA]
