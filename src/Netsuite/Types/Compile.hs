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
responseToAeson (RestletErrorResp _) = error "Netsuite.Types.Compile.responseToAeson: Received a RestletErrorResp when we were expecting a RestletOk."
responseToAeson (RestletOk strings)  = mconcat $ map singleResponseToAeson strings
  where
    -- | Parse a single object.
    singleResponseToAeson x =
      case maybeVal x of
        Nothing -> error ("Could not decode response " ++ (bytesToString $ BS.unpack x))
        Just y  -> y
    maybeVal = decode . BSL.fromStrict

-- | Define a monoid instance so we can get mconcat for free.
instance Monoid Value where
    mempty  = object []
    mappend = concatResults
      where
        -- | Append arrays, or take the first object of any other kind.
        concatResults (Array x) (Array y)   = Array $ (Vector.++) x y
        concatResults x _                   = x
