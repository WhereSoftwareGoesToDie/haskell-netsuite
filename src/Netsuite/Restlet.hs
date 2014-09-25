{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}

module Netsuite.Restlet (
  restletExecute,
  chunkableRestletExecute,
  RestletResponse
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8

import Blaze.ByteString.Builder (Builder)

import Control.Applicative
import Control.Exception
import Data.Char
import qualified Data.List as List
import Data.Maybe
import Data.Monoid
import Data.Word
import "network-uri" Network.URI

import System.IO.Streams (InputStream, OutputStream, stdout)
import qualified System.IO.Streams as Streams
import Network.Http.Client

import Control.Exception (Exception, bracket, throw)

import OpenSSL (withOpenSSL)
import OpenSSL.Session (SSLContext)
import qualified OpenSSL.Session as SSL

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.IO.Unsafe (unsafePerformIO)

import Netsuite.Helpers
import Netsuite.Restlet.Configuration
import Netsuite.Restlet.Response

-- | External restlet execution function, with quick config check.
restletExecute :: String -> NsRestletConfig -> IO RestletResponse
restletExecute s cfg = do
  case (configOK cfg) of
    Just bd -> restletExecute' s cfg bd
    Nothing -> error "Configuration not valid"

-- | Chunked restlet execution function.
chunkableRestletExecute :: String -> NsRestletConfig -> IO RestletResponse
chunkableRestletExecute s cfg = chunkableRestletExecute' mempty False s cfg

-- | Chunked restlet execution function internal.
chunkableRestletExecute' :: RestletResponse -> Bool -> String -> NsRestletConfig -> IO RestletResponse
chunkableRestletExecute' lastR isChunking s cfg =
  case (configOK cfg) of
    Just bd -> do
      resp <- restletExecute' s cfg bd
      case resp of
        err@(RestletErrorResp _) ->
          case (interpretError err) of
            BeginChunking _ -> runAgain mempty -- Begin chunking
            EndChunking _   -> return lastR -- End chunking
            _               -> return err -- Just return the error
        ok                       ->
          case isChunking of
            True -> runAgain (mappend lastR ok) -- Read another chunk
            False -> return ok -- We have enough already
    Nothing -> error "Configuration not valid"
  where
    runAgain newResp = chunkableRestletExecute' newResp True s cfg

-- | Checks to see if our URL config is valid.
-- @TODO: Add more checks.
configOK :: NsRestletConfig -> Maybe (String, Integer, String)
configOK cfg =
  case (breakdownURI $ restletURI cfg) of
    (Just a, Just b, Just c) -> Just (a, b, c)
    (_, _, _)                -> Nothing

-- | Internal restlet execution function.
restletExecute' :: String -> NsRestletConfig -> (String, Integer, String) -> IO RestletResponse
restletExecute' s cfg (_hostname, _port, _path) = do
  bracket est teardown process
  where
    est = establish (uriScheme $ restletURI cfg) _hostname _port
    teardown = closeConnection
    process c = do
      q <- buildRequest $ do
        http POST (bsPackedW8s _path)
        setContentType "application/json"
        setAccept "application/json"
        setNsAuth cfg
        setHeader "User-Agent" "NsRestlet"
      putStrLn $ show q
      is <- Streams.fromByteString (bsPackedW8s s)
      putStrLn $ show s
      _ <- sendRequest c q (inputStreamBody is)
      catch (RestletOk . (\s -> [s]) <$> (receiveResponse c concatHandler')) (return . RestletErrorResp)

-- | Establish HTTP or HTTPS connection.
establish :: String -> String -> Integer -> IO (Connection)
establish scheme h p =
  case scheme of
    "http:"  -> do
      openConnection host port
    "https:" -> withOpenSSL $ do
      ctx <- readIORef global
      openConnectionSSL ctx host port
    _        -> error ("Unknown URI scheme " ++ scheme)
  where
    host = bsPackedW8s h
    port = fromInteger p :: Word16

-- | Chop up a URI into a tuple of three maybes; hostname, port and path.
breakdownURI :: URI -> (Maybe String, Maybe Integer, Maybe String)
breakdownURI u = (hostname, port, path)
  where
    hostname =
      case (uriAuthority u) of
        Nothing -> Nothing
        Just au -> Just (uriRegName au)
    port =
      case (uriAuthority u) of
        Nothing -> Nothing
        Just au ->
          case (filter isDigit $ uriPort au) of
            "" ->
              case (uriScheme u) of
                "http:"  -> Just 80
                "https:" -> Just 443
            x  -> Just (read x :: Integer)
    path = Just $ (uriPath u) ++ (uriQuery u) ++ (uriFragment u)

-- | Set NLAuth auth header to be sent in the HTTP request.
setNsAuth :: NsRestletConfig -> RequestBuilder ()
setNsAuth cfg = do
  setHeader "Authorization" (nsAuth cfg)

-- | Build NLAuth header.
nsAuth :: NsRestletConfig -> BS.ByteString
nsAuth cfg = BS8.append plna (sig cfg)
  where
    plna = BS8.pack "NLAuth "
    sig = BS8.concat . BS8.lines . bs8PackedW8s . pcfg
    pcfg = List.intercalate "," . map (\(k, v) -> concat [k, "=", v]) . nsAuthPairs

-- | Basic pairs for NLAuth header.
nsAuthPairs :: NsRestletConfig -> [(String, String)]
nsAuthPairs cfg = [
  ("nlauth_account", show $ restletAccountID cfg),
  ("nlauth_email", restletIdent cfg),
  ("nlauth_role", show $ restletRole cfg),
  ("nlauth_signature", normalizeEscape $ restletPassword cfg)]

-- | Stolen from http-streams.
-- This makes the global IO context for HTTPS work.
global :: IORef SSLContext
global = unsafePerformIO $ do
  ctx <- baselineContextSSL
  newIORef ctx
