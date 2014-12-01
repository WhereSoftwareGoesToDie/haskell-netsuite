{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Aeson
import Data.Text hiding (map)
import Data.Typeable
import Netsuite.Connect
import Netsuite.Types.Data
import Netsuite.Types.Data.Core
import Netsuite.Types.Data.TypeFamily
import Netsuite.Types.Fields.Core
import Test.Hspec

-- | Actual test suite
suite :: Spec
suite = do
    describe "Requests" $ do
        it "can marshal NsAction objects as valid JSON" $ do
            case map toJSON exampleNsActions of
                (x :: [Value]) -> pass
                x              -> error $ (show x) ++ " should be a list of valid JSON objects"

-- | Run everything
main :: IO ()
main = hspec suite

-- | Just pass
pass :: Expectation
pass = return ()

-- | Example NsActions
exampleNsActions :: [NsAction]
exampleNsActions = [
    NsActRetrieve (NsType "customer") (toNsDataId (12345 :: Int)) (NsFields ["id", "companyname"]) exampleCode,
    NsActFetchSublist (NsSubtype (NsType "customer") "addressbook") (toNsId (12345 :: Int)) (NsFields ["phone", "email"]) exampleCode]
  where
    exampleCode = NsRestletCode $ pack "alert(\"Hello world!\");"
