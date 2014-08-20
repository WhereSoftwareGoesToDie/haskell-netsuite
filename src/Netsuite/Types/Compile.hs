{-# LANGUAGE OverloadedStrings #-}

module Netsuite.Types.Compile (responseToAeson) where

import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap as HashMap
import Data.Monoid
import qualified Data.Vector as Vector

import Netsuite.Helpers
import Netsuite.Restlet.Response

-- | Parse all objects and mconcat them.
responseToAeson :: RestletResponse -> Value
responseToAeson (RestletOk strings)  = mconcat $ map singleResponseToAeson strings
responseToAeson (RestletErrorResp _) = error "Nope"

-- | Parse a single object.
singleResponseToAeson :: BS.ByteString -> Value
singleResponseToAeson x =
  case maybeVal of
    Nothing -> error ("Could not decode response " ++ (bytesToString $ BS.unpack x))
    Just y  -> y
  where
    maybeVal = decode (BSL.fromStrict x) :: Maybe Value

-- | Define a monoid instance so we can get mconcat for free.
instance Monoid Value where
    mempty  = object []
    mappend = concatResults

-- | Append arrays, or take the first object of any other kind.
concatResults :: Value -> Value -> Value
concatResults (Array x) (Array y)   = Array $ (Vector.++) x y
concatResults x _                   = x
