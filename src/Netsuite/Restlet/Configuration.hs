{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

module Netsuite.Restlet.Configuration (
	NsRestletConfig (..),
	IsNsRestletConfig,
	toNsRestletConfig
) where

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

class IsNsRestletConfig a where
	toNsRestletConfig :: a -> NsRestletConfig

instance (Integral a) => IsNsRestletConfig ([Char], a, a, [Char], [Char]) where
	toNsRestletConfig (u, a, r, i, p) = NsRestletConfig (justParseURI u) (toInteger a) (toInteger r) i p Nothing Nothing

instance (Integral a) => IsNsRestletConfig ([Char], a, a, [Char], [Char], [Char]) where
	toNsRestletConfig (u, a, r, i, p, ua) = NsRestletConfig (justParseURI u) (toInteger a) (toInteger r) i p (Just ua) Nothing

instance (Integral a) => IsNsRestletConfig ([Char], a, a, [Char], [Char], HashMap [[Char]] [[Char]]) where
	toNsRestletConfig (u, a, r, i, p, cf) = NsRestletConfig (justParseURI u) (toInteger a) (toInteger r) i p Nothing (Just cf)

instance (Integral a) => IsNsRestletConfig ([Char], a, a, [Char], [Char], [Char], HashMap [[Char]] [[Char]]) where
	toNsRestletConfig (u, a, r, i, p, ua, cf) = NsRestletConfig (justParseURI u) (toInteger a) (toInteger r) i p (Just ua) (Just cf)

justParseURI :: String -> URI
justParseURI = fromJust . parseURI
