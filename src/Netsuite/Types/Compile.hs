{-# LANGUAGE OverloadedStrings #-}

module Netsuite.Types.Compile (responseToAeson) where

import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe
import Data.Monoid
import qualified Data.Vector as Vector
import qualified Data.Text as Text

import Netsuite.Helpers
import Netsuite.Restlet.Response

newtype ConcatValue = ConcatValue { unConcatValue :: Value }

-- | Parse all objects and mconcat them.
responseToAeson :: RestletResponse -> Value
responseToAeson (RestletErrorResp _) = error "Netsuite.Types.Compile.responseToAeson: Received a RestletErrorResp when we were expecting a RestletOk."
responseToAeson (RestletOk strings)  = unConcatValue . mconcat . map (ConcatValue . singleResponseToAeson) $ strings
  where
    -- | Parse a single object.
    singleResponseToAeson x = String . Text.pack $
        fromMaybe ("Could not decode response " ++ bytesToString (BS.unpack x))
                  (decode . BSL.fromStrict $ x)

-- | Define a monoid instance so we can get mconcat for free.
instance Monoid ConcatValue where
    mempty  = ConcatValue (object [])
    mappend = concatResults
      where
        -- | Append arrays, or take the first object of any other kind.
        concatResults (ConcatValue (Array x)) (ConcatValue (Array y)) = ConcatValue . Array $ (Vector.++) x y
        concatResults x _                   = x
