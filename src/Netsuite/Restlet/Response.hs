{-# LANGUAGE OverloadedStrings #-}

module Netsuite.Restlet.Response (
    RestletResponse (..),
    RestletError (..),
    interpretError
) where

import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import Data.Monoid
import qualified Data.Text as Text
import Netsuite.Restlet.ResponseHandler

-- | Response type
data RestletResponse = RestletOk [BS.ByteString]
                     | RestletErrorResp HttpRestletError deriving (Show)

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
                  | UnknownError Int String BS.ByteString
                  | GibberishError Int String BS.ByteString deriving (Eq, Show)

interpretError :: RestletResponse -> RestletError
interpretError (RestletErrorResp e) = uncurry interpretError' e'
  where
    e' = httpClientErrorCodeBody e
interpretError _ = error "We should not be here"

httpClientErrorCodeBody :: HttpRestletError -> (Int, BS.ByteString)
httpClientErrorCodeBody (HttpRestletError code _ _ body) = (code, body)

-- | Interpret error message as a data type
interpretError' :: Int -> BS.ByteString -> RestletError
interpretError' http_code es = case mightValue of
    Nothing -> GibberishError http_code "Unparseable response, expecting JSON." es
    Just jv@Object{} ->
        let
            em = getErrorMessage jv
            in case getVal jv ["error", "code"] of
                Just "RCRD_DOESNT_EXIST"       -> NotFound http_code em
                Just "SSS_INVALID_SRCH_FILTER" -> InvalidSearchFilter http_code em
                Just "CC_PROCESSOR_ERROR"      -> CCProcessorError http_code em
                Just x                         ->
                    if Text.isSuffixOf (Text.pack "_ALREADY_EXISTS") x
                        then ResourceConflict http_code em
                        else interpretErrorMsg http_code em es
                Nothing                        -> interpretErrorMsg http_code em es
    Just _ -> GibberishError http_code "Couldn't extract meaningful error object." es
    where
        mightValue = decode (BSL.fromStrict es) :: Maybe Value

-- | Get textual error message
getErrorMessage :: Value -> String
getErrorMessage v = maybe "" Text.unpack $ getVal v ["error", "message"]

-- | Get item from deep down in object tree
getVal :: Value -> [String] -> Maybe Text.Text
getVal (Object v) (key:xs) = case HM.lookup (Text.pack key) v of
    Nothing -> Nothing
    Just v' -> case length xs of
        0 -> case v' of
            String x -> Just x
            y        -> Just . Text.pack . show $ y
        _ -> getVal v' xs
getVal _ [] = error "Netsuite.Restlet.Response.getVal: Tried to get a key that wasn't there."

-- | Get special error message meaning
interpretErrorMsg :: Int -> String -> BS.ByteString -> RestletError
interpretErrorMsg http_code msg body =
    case msg of
        "CHUNKY_MONKEY"  -> BeginChunking http_code
        "NO_MORE_CHUNKS" -> EndChunking http_code
        y                -> UnknownError http_code y body
