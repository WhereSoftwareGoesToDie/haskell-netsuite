{-# LANGUAGE OverloadedStrings #-}

module Netsuite.Restlet.Response (
  RestletResponse(..),
  RestletError(..),
  interpretError
  ) where

import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import Data.Monoid
import qualified Data.Text as Text

import Network.Http.Client

-- | Response type
data RestletResponse = RestletOk [BS.ByteString]
                     | RestletErrorResp HttpClientError deriving (Show)

instance Monoid RestletResponse where
    mappend (RestletOk x) (RestletOk y)        = RestletOk (x ++ y)
    mappend (RestletOk x) (RestletErrorResp _) = RestletOk x
    mappend (RestletErrorResp x) _             = RestletErrorResp x
    mempty  = RestletOk []

-- | Response error

data RestletError = NotFound Int String
                  | ResourceConflict Int String
                  | InvalidSearchFilter Int String
                  | CCProcessorError Int String
                  | BeginChunking Int
                  | EndChunking Int
                  | UnknownError Int String
                  | GibberishError Int String BS.ByteString deriving (Eq, Show)

interpretError :: RestletResponse -> RestletError
interpretError (RestletErrorResp e) = interpretError' (fst e') (snd e')
  where
    e' = httpClientErrorCodeBody e
interpretError _ = error "We should not be here"

httpClientErrorCodeBody :: HttpClientError -> (Int, BS.ByteString)
httpClientErrorCodeBody (HttpClientError x y) = (x, y)

interpretError' :: Int -> BS.ByteString -> RestletError
interpretError' httpCode es = case mightValue of
    Nothing -> GibberishError httpCode "Unparseable response, expecting JSON." es
    Just jv  ->
        case jv of
            Object v ->
                case HM.lookup (Text.pack "code") v of
                    Just "RCRD_DOESNT_EXIST"       -> NotFound httpCode (getErrorMessage v)
                    Just "SSS_INVALID_SRCH_FILTER" -> InvalidSearchFilter httpCode (getErrorMessage v)
                    Just "CC_PROCESSOR_ERROR"      -> CCProcessorError httpCode (getErrorMessage v)
                    Nothing                        -> interpretErrorMsg httpCode (getErrorMessage v)
                    Just x                         ->
                        case x of
                            String x' ->
                                case Text.isSuffixOf (Text.pack "_ALREADY_EXISTS") x' of
                                    True  -> ResourceConflict httpCode (getErrorMessage v)
                                    False -> interpretErrorMsg httpCode (getErrorMessage v)
                            _         -> error "'message' value is not a string we can use"
            _        -> GibberishError httpCode "Couldn't extract meaningful error object." es
    where
        mightValue = decode (BSL.fromStrict es) :: Maybe Value

getErrorMessage :: Object -> String
getErrorMessage v = case HM.lookup (Text.pack "message") v of
    Nothing -> ""
    Just z  ->
        case z of
            String z' -> Text.unpack z'
            _         -> error "'message' value is not a string we can use"

interpretErrorMsg :: Int -> String -> RestletError
interpretErrorMsg httpCode msg =
    case msg of
        "CHUNKY_MONKEY"  -> BeginChunking httpCode
        "NO_MORE_CHUNKS" -> EndChunking httpCode
        y                -> UnknownError httpCode y
