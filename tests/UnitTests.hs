{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception (evaluate)
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString.Char8 as C hiding (length, map)
import Data.Maybe
import Data.Text hiding (length, map)
import Data.Typeable
import Netsuite.Connect
import Netsuite.Restlet
import Netsuite.Restlet.Configuration
import Netsuite.Types.Data
import Netsuite.Types.Data.Core
import Netsuite.Types.Data.TypeFamily
import Netsuite.Types.Fields.Core
import "network-uri" Network.URI
import Network.Http.Internal
import System.Exit
import Test.Hspec

-- | Actual test suite
suite :: Spec
suite = do
    describe "NsAction" $
        it "can marshal all NsAction types as valid JSON" $
            case mapMaybe (decode . encode) exampleNsActions of
                (x :: [Value]) -> length x `shouldBe` length exampleNsActions
                y              -> error $ show y ++ " should be a list of valid JSON objects"
    describe "NsRestletConfig" $ do
        it "can create identical NsRestletConfig object from dissimilar origins" $ do
            let a = ("http://example.com:8080", 12345 :: Int, 1000 :: Int, "foo@example.com", "bar")
            let b = NsRestletConfig (fromJust . parseURI $ "http://example.com:8080")
                                    (12345 :: Integer)
                                    (1000 :: Integer)
                                    "foo@example.com"
                                    "bar"
                                    Nothing
                                    Nothing
            toNsRestletConfig a `shouldBe` b
        it "does not accept invalid URLs" $ do
            let a = ("obviously incorrect ha ha", 12345 :: Int, 1000 :: Int, "foo@example.com", "bar")
            evaluate (show $ toNsRestletConfig a) `shouldThrow` errorCall "Maybe.fromJust: Nothing"

    describe "Restlet Requests" $
        it "creates the expected headers" $ do
            let (nsid, nsrole, nsident, nspwd) = (12345 :: Int, 1000 :: Int, "foo@example.com", "bar")

            let a = ("http://example.com:8080", nsid, nsrole, nsident, nspwd)
            let p = "/test"
            r <- makeRequest (toNsRestletConfig a) p
            qPath r `shouldBe` C.pack p
            let hdrs = retrieveHeaders . qHeaders $ r
            liftIO $ putStrLn $ show hdrs
            tryBsMatch "Content-Type" "application/json" hdrs
            tryBsMatch "Accept" "application/json" hdrs
            tryBsMatch "User-Agent" "NsRestlet" hdrs
            tryBsMatch "Authorization" (Prelude.concat ["NLAuth nlauth_account=",
                                                        show nsid,
                                                        ",nlauth_email=",
                                                        nsident,
                                                        ",nlauth_role=",
                                                        show nsrole,
                                                        ",nlauth_signature=",
                                                        nspwd]) hdrs
          where
            tryBsMatch a b h = shouldBe (lookup (C.pack a) h) (Just . C.pack $ b)

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
    NsActRawSearch type1 filters1 cols1 exampleCode,
    NsActSearch type1 filters1 (NsFields ["id", "companyName"]) exampleCode,
    NsActCreate type1 data1 subdata1 (NsFields ["id", "companyName"]) exampleCode ]
  where
    type1    = toNsType "customer"
    subtype1 = toNsSubtype ("customer","addressbook")
    filters1 = [toNsFilter ("foo", IsEmpty),
                toNsFilter ("bar", Is, "1"),
                toNsFilter ("baz", "beep", EqualTo, "1"),
                toNsFilter ("fing", Contains, "fang", "foom"),
                toNsFilter ("person", "location", Between, "rock", "hard place")]
    cols1 = map toNsSearchCol [["foo"], ["bar"], ["baz", "beep"], ["a column"]]
    data1 = toNsData [(pack "foo")         .= (String . pack $ "bar"),
                      (pack "baz")         .= (String . pack $ "frob"),
                      (pack "companyname") .= (String . pack $ "Sturm und Drang Inc.")]
    subdata1 = toNsSublistData [("addressbook", [ [(pack "address1") .= (String . pack $ "1 Boog Street"),
                                                           (pack "city")     .= (String . pack $ "Sydney")]])]
    exampleCode = NsRestletCode $ pack "alert(\"Hello world!\");"
