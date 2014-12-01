{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Aeson
import Data.Maybe
import Data.Text hiding (length, map)
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
        it "can marshal all NsAction types as valid JSON" $ do
            case catMaybes . map (decode . encode) $ exampleNsActions of
                (x :: [Value]) -> (length x) `shouldBe` (length exampleNsActions)
                y              -> error $ (show y) ++ " should be a list of valid JSON objects"

-- | Run everything
main :: IO ()
main = hspec suite

-- | Just pass
pass :: Expectation
pass = return ()

-- | Example NsActions
exampleNsActions :: [NsAction]
exampleNsActions = [
    NsActRetrieve type1 (toNsDataId (12345 :: Int)) (NsFields ["id", "companyname"]) exampleCode,
    NsActFetchSublist subtype1 (toNsId (12345 :: Int)) (NsFields ["phone", "email"]) exampleCode,
    NsActRawSearch type1 filters1 cols1 exampleCode ]
  where
    type1 = (toNsType ("customer"))
    subtype1 = (toNsSubtype ("customer","addressbook"))
    filters1 = [toNsFilter ("foo", IsEmpty),
                toNsFilter ("bar", Is, "1"),
                toNsFilter ("baz", "beep", EqualTo, "1"),
                toNsFilter ("fing", Contains, "fang", "foom"),
                toNsFilter ("person", "location", Between, "rock", "hard place")]
    cols1 = map toNsSearchCol [["foo"], ["bar"], ["baz", "beep"], ["a column"]]
    exampleCode = NsRestletCode $ pack "alert(\"Hello world!\");"
