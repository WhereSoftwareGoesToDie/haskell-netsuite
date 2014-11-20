{-# LANGUAGE DeriveDataTypeable #-}

module Netsuite.Restlet.ResponseHandler (
    HttpRestletError (..),
    restletResponseHandler
) where

import qualified Blaze.ByteString.Builder as Builder
import Control.Exception
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import Data.Monoid
import Data.Typeable
import Network.Http.Client (concatHandler)
import Network.Http.Internal (pHeaders)
import Network.Http.Types
import qualified System.IO.Streams as Streams

data HttpRestletError = HttpRestletError {
    httpErrCode    :: Int,
    httpErrMsg     :: ByteString,
    httpErrHeaders :: Headers,
    httpErrBody    :: ByteString
} deriving (Typeable)

-- | Custom response handler that actually catches everything in the HTTP response
-- instead of just the HTTP Status Code and HTTP Status Message.
restletResponseHandler
    :: Response
    -> Streams.InputStream ByteString
    -> IO ByteString
restletResponseHandler p i = do
    b <- body
    if s >= 300
        then throw (HttpRestletError s m h b)
        else return b
  where
    body = concatHandler p i >>= return
    s = getStatusCode p
    m = getStatusMessage p
    h = pHeaders p

instance Exception HttpRestletError

instance Show HttpRestletError where
    show (HttpRestletError s msg h b) = Prelude.show s ++ " " ++ S.unpack msg ++ " " ++ S.unpack b
