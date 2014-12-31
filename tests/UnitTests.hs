{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception (evaluate)
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C hiding (length, map)
import qualified Data.CaseInsensitive as CI
import Data.Maybe
import Data.Text hiding (head, length, map)
import Data.Typeable
import Netsuite.Connect
import Netsuite.Restlet
import Netsuite.Restlet.Configuration
import Netsuite.Restlet.Response
import Netsuite.Restlet.ResponseHandler
import Netsuite.Types.Data
import Netsuite.Types.Data.Core
import Netsuite.Types.Data.TypeFamily
import Netsuite.Types.Fields.Core
import Network.URI
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
            let a = (pack "http://example.com:8080", 12345 :: Int, 1000 :: Int, pack "foo@example.com", pack "bar")
            let b = NsRestletConfig (fromJust . parseURI $ "http://example.com:8080")
                                    (12345 :: Integer)
                                    (1000 :: Integer)
                                    (pack "foo@example.com")
                                    (pack "bar")
                                    Nothing
                                    Nothing
            toNsRestletConfig a `shouldBe` b
        it "does not accept invalid URLs" $ do
            let a = (pack "obviously incorrect ha ha", 12345 :: Int, 1000 :: Int, pack "foo@example.com", pack "bar")
            evaluate (show $ toNsRestletConfig a) `shouldThrow` errorCall "Maybe.fromJust: Nothing"

    describe "Restlet Requests" $
        it "creates the expected headers" $ do
            let (nsid, nsrole, nsident, nspwd) = (12345 :: Int, 1000 :: Int, pack "foo@example.com", pack "bar")

            let a = (pack "http://example.com:8080", nsid, nsrole, nsident, nspwd)
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
                                                        (unpack nsident),
                                                        ",nlauth_role=",
                                                        show nsrole,
                                                        ",nlauth_signature=",
                                                        (unpack nspwd)]) hdrs

    describe "Restlet Errors" $ do
        it "should parse GibberishError correctly" $
            interpretError (head exampleRestletErrors) `shouldBe` GibberishError 400 "Unparseable response, expecting JSON." (C.pack "Bad Request")
        it "should parse BeginChunking correctly" $
            interpretError (exampleRestletErrors !! 1) `shouldBe` BeginChunking 400
        it "should parse EndChunking correctly" $
            interpretError (exampleRestletErrors !! 2) `shouldBe` EndChunking 400
        it "should parse NotFound correctly" $
            interpretError (exampleRestletErrors !! 3) `shouldBe` NotFound 404 ""
        it "should parse InvalidSearchFilter correctly" $
            interpretError (exampleRestletErrors !! 4) `shouldBe` InvalidSearchFilter 400 ""
        it "should parse CCProcessorError correctly" $
            interpretError (exampleRestletErrors !! 5) `shouldBe` CCProcessorError 400 ""
        it "should parse ResourceConflict correctly" $
            interpretError (exampleRestletErrors !! 6) `shouldBe` ResourceConflict 400 ""
        it "should parse fall back to UnknownError correctly" $
            interpretError (exampleRestletErrors !! 7) `shouldBe` UnknownError 400 "IDK_LOL" (C.pack "{\"error\": {\"message\": \"IDK_LOL\"}}")

-- | Tries to match header key values
tryBsMatch :: String -> String -> [(C.ByteString, C.ByteString)] -> Expectation
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
    NsActRetrieve type1 (toNsDataId (12345 :: Int)) fields1 exampleCode,
    NsActFetchSublist subtype1 (toNsId (12345 :: Int)) fields2 exampleCode,
    NsActRawSearch type1 filters1 cols1 exampleCode,
    NsActSearch type1 filters1 fields1 exampleCode,
    NsActCreate type1 data1 subdata1 fields1 exampleCode,
    NsActAttach type1 multiId type2 (toNsId (4 :: Int)) data1 exampleCode,
    NsActDetach type1 multiId type2 (toNsId (4 :: Int)) exampleCode,
    NsActUpdate type1 data2 fields1 exampleCode,
    NsActUpdateSublist subtype1 (toNsId (12334 :: Int)) [data1, data2] exampleCode,
    NsActDelete type1 (toNsDataId (12356 :: Int)) exampleCode,
    NsActInvoicePDF (toNsId (9999 :: Int)) exampleCode,
    NsActTransform type1 (toNsId (12345 :: Int)) type2 data1 fields2 exampleCode ]
  where
    type1    = toNsType $ pack "customer"
    type2    = toNsType $ pack "contact"
    subtype1 = toNsSubtype (pack "customer",pack "addressbook")
    filters1 = [toNsFilter (pack "foo", IsEmpty),
                toNsFilter (pack "bar", Is, pack "1"),
                toNsFilter (pack "baz", pack "beep", EqualTo, pack "1"),
                toNsFilter (pack "fing", Contains, pack "fang", pack "foom"),
                toNsFilter (pack "person", pack "location", Between, pack "rock", pack "hard place")]
    cols1 = map toNsSearchCol [[pack "foo"], [pack "bar"], [pack "baz", pack "beep"], [pack "a column"]]
    data1 = toNsData [(pack "foo")         .= (String . pack $ "bar"),
                      (pack "baz")         .= (String . pack $ "frob"),
                      (pack "companyname") .= (String . pack $ "Sturm und Drang Inc.")]
    data2 = toNsData [(pack "id")  .= (String . pack $ "1234"),
                      (pack "foo") .= (String . pack $ "baz")]
    subdata1 = toNsSublistData [(pack "addressbook", [ [(pack "address1") .= (String . pack $ "1 Boog Street"),
                                                        (pack "city")     .= (String . pack $ "Sydney")] ] )]
    multiId = map toNsId [1 :: Int, 2 :: Int, 3 :: Int]
    fields1 = NsFields [pack "id", pack "companyname"]
    fields2 = NsFields [pack "phone", pack "email"]
    exampleCode = NsRestletCode $ pack "alert(\"Hello world!\");"

-- | Test Restlet errors
exampleRestletErrors :: [RestletResponse]
exampleRestletErrors = map RestletErrorResp [ HttpRestletError 400 txBadRequest hdrs1 txBadRequest
                                            , HttpRestletError 400 txBadRequest hdrs2 txChunky
                                            , HttpRestletError 400 txBadRequest hdrs2 txEndChunk
                                            , HttpRestletError 404 txNotFound hdrs2 txDoesntExist
                                            , HttpRestletError 400 txBadRequest hdrs2 txInvalidSearch
                                            , HttpRestletError 400 txBadRequest hdrs2 txCcProcessor
                                            , HttpRestletError 400 txBadRequest hdrs2 txAlreadyExists
                                            , HttpRestletError 400 txBadRequest hdrs2 txUnknown ]
  where
    txBadRequest = C.pack "Bad Request"
    txNotFound = C.pack "Not Found"
    txChunky = C.pack "{\"error\": {\"message\": \"CHUNKY_MONKEY\"}}"
    txEndChunk = C.pack "{\"error\": {\"message\": \"NO_MORE_CHUNKS\"}}"
    txDoesntExist = C.pack "{\"error\": {\"code\": \"RCRD_DOESNT_EXIST\"}}"
    txInvalidSearch = C.pack "{\"error\": {\"code\": \"SSS_INVALID_SRCH_FILTER\"}}"
    txCcProcessor = C.pack "{\"error\": {\"code\": \"CC_PROCESSOR_ERROR\"}}"
    txAlreadyExists = C.pack "{\"error\": {\"code\": \"FOO_ALREADY_EXISTS\"}}"
    txUnknown = C.pack "{\"error\": {\"message\": \"IDK_LOL\"}}"
    hdrs1 = updateHeader emptyHeaders (C.pack "Content-Type") (C.pack "text/plain")
    hdrs2 = updateHeader emptyHeaders (C.pack "Content-Type") (C.pack "application/json")
