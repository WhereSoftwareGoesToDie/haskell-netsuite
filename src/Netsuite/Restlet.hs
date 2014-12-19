{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}

module Netsuite.Restlet (
    restletExecute,
    chunkableRestletExecute,
    makeRequest,
    RestletResponse
) where

import qualified Data.ByteString.Char8 as BS8

import Control.Applicative
import Control.Exception
import Data.Char
import qualified Data.List as List
import Data.Monoid
import Data.Tuple.Sequence (sequenceT)
import Network.URI

import Network.Http.Client
import qualified System.IO.Streams as Streams

import OpenSSL (withOpenSSL)

import Data.IORef (newIORef, readIORef)
import System.IO.Unsafe (unsafePerformIO)

import Netsuite.Helpers
import Netsuite.Restlet.Configuration
import Netsuite.Restlet.Response
import Netsuite.Restlet.ResponseHandler

-- | External restlet execution function, with quick config check.
restletExecute :: String -> NsRestletConfig -> IO RestletResponse
restletExecute s cfg =
    case configOK cfg of
        Just bd -> restletExecute' s cfg bd
        Nothing -> error "Configuration not valid"

-- | Chunked restlet execution function.
chunkableRestletExecute :: String -> NsRestletConfig -> IO RestletResponse
chunkableRestletExecute s cfg = chunkableRestletExecute' mempty False s cfg
  where
    -- | Chunked restlet execution function internal.
    -- chunkableRestletExecute' :: RestletResponse -> Bool -> String -> NsRestletConfig -> IO RestletResponse
    chunkableRestletExecute' lastR isChunking s' cfg' =
        case configOK cfg' of
            Just bd -> do
                resp <- restletExecute' s' cfg' bd
                case resp of
                    RestletErrorResp {} ->
                        case interpretError resp of
                            BeginChunking _ -> runAgain mempty -- Begin chunking
                            EndChunking _   -> return lastR -- End chunking
                            _               -> return resp -- Just return the error
                    _                   ->
                        if isChunking
                            then runAgain (mappend lastR resp) -- Read another chunk
                            else return resp -- We have enough already
            Nothing -> error "Configuration not valid"
    runAgain newResp = chunkableRestletExecute' newResp True s cfg

-- | Checks to see if our URL config is valid.
-- @TODO: Add more checks.
configOK :: NsRestletConfig -> Maybe (String, Integer, String)
configOK = sequenceT . breakdownURI . restletURI
  where
    -- | Chop up a URI into a tuple of three maybes; hostname, port and path.
    breakdownURI u = (cfg_host, cfg_port, cfg_path)
      where
        cfg_host = uriRegName <$> uriAuthority u
        cfg_port = getPort <$> uriAuthority u
        cfg_path = Just $ uriPath u ++ uriQuery u ++ uriFragment u
        getPort au = case filter isDigit $ uriPort au of
            "" ->
                case uriScheme u of
                    "http:"  -> 80
                    "https:" -> 443
            x  -> read x

-- | Internal restlet execution function.
restletExecute' :: String -> NsRestletConfig -> (String, Integer, String) -> IO RestletResponse
restletExecute' s cfg (_hostname, _port, _path) = bracket est teardown process
  where
    est = establish (uriScheme $ restletURI cfg) _hostname _port
    teardown = closeConnection
    process c = do
        q  <- makeRequest cfg _path
        is <- Streams.fromByteString (bsPackedW8s s)
        -- putStrLn $ show s
        _ <- sendRequest c q (inputStreamBody is)
        catch (RestletOk . (: []) <$> receiveResponse c restletResponseHandler) debugError

    debugError e = return . RestletErrorResp $ e
    -- putStrLn $ show e

    -- | Establish HTTP or HTTPS connection.
    establish rl_scheme h p =
        case rl_scheme of
            "http:"  -> openConnection host port
            "https:" -> withOpenSSL $ do
                ctx <- readIORef global
                openConnectionSSL ctx host port
            _ -> error ("Unknown URI scheme " ++ rl_scheme)
      where
        host = bsPackedW8s h
        port = fromInteger p

        -- | Stolen from http-streams.
        -- This makes the global IO context for HTTPS work.
        global = unsafePerformIO $ do
            ctx <- baselineContextSSL
            newIORef ctx

-- | Construct the HTTP headers to send.
makeRequest :: NsRestletConfig -> String -> IO Request
makeRequest c p = buildRequest $ do
    http POST (bsPackedW8s p)
    setContentType "application/json"
    setAccept "application/json"
    setNsAuth c
    setHeader "User-Agent" "NsRestlet"
  where
    setNsAuth = setHeader "Authorization" . nsAuth

    nsAuth = BS8.append (BS8.pack "NLAuth ") . sig
    sig = BS8.concat . BS8.lines . bs8PackedW8s . pcfg
    pcfg = List.intercalate "," . map (\(k, v) -> concat [k, "=", v]) . nsAuthPairs

    nsAuthPairs c' = [ ("nlauth_account",   show $ restletAccountID c')
                     , ("nlauth_email",     restletIdent c')
                     , ("nlauth_role",      show $ restletRole c')
                     , ("nlauth_signature", normalizeEscape $ restletPassword c')]
