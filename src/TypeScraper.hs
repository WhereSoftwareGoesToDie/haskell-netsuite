{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.IORef (newIORef, readIORef)
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Network.Http.Client
import OpenSSL (withOpenSSL)
import Safe
import System.Environment
import System.Exit
import System.IO.Unsafe (unsafePerformIO)
import Text.HTML.TagSoup
import Text.StringLike

data RecordBlob = RecordBlob
    { _rbName :: [Text]
    , _rbFields :: [Text]
    }

instance Show RecordBlob where
    show (RecordBlob (a:b:_) f) = "-- | " <> T.unpack a <> " > " <> T.unpack b
        <> "\n" <> "nsSubtypeFields (\""
        <> T.unpack a <> "\":\"" <> T.unpack b <> "\":_) = ["
        <> showRemainder f
    show (RecordBlob (a:_) f) = "-- | " <> T.unpack a
        <> "\n" <> "nsTypeFields (\""
        <> T.unpack a <> "\":_) = ["
        <> showRemainder f

showRemainder :: [Text] -> String
showRemainder f = (concat . fmap showField $ zip [0..] f) <> " ]\n\n"

showField :: (Int, Text) -> String
showField (0,f) = "\n    \"" <> (T.unpack f) <> "\""
showField (_,f) = "\n  , \"" <> (T.unpack f) <> "\""

main :: IO ()
main = do
    args <- getArgs
    case args of
        (t:_) -> do
            r <- req t
            let tags = parseTags $ BS8.unpack r
            let b = getBlobs tags
            mapM_ print b
        _     -> putStrLn "USAGE: netsuite-type-scraper <type name>" >> exitFailure

-- | Get Record Blobs from HTML
getBlobs :: (Show s, StringLike s) => [Tag s] -> [RecordBlob]
getBlobs t =
    [RecordBlob [baseType] baseFields] <> fmap sublistBlob sublistTables
  where
    -- generic
    pk = T.pack . toString . stringHead itemInnerValue
    -- base
    baseType = T.replace (T.pack "Internal ID: ") (T.pack "") .
               pk .
               sections (~== ("<div class='record_id'>" :: String)) $
               t
    recordTables = tagsBetween "<table class='record_table'>" "</table>" t
    rtRows = tagsBetween "<tr>" "</tr>" . fromMaybe [] .
             headMay $ recordTables
    baseFields = fmap linkInner . catMaybes .
                 fmap (headMay . tagsBetween "<td>" "</td>") . fromMaybe [] .
                 tailMay $ rtRows
    linkInner  = T.pack . toString . stringHead itemInnerValue .
                 sections (~== ("<a>" :: String))
    -- sublists
    sublists      = tagsBetween "<div class='sublist'>" "</table>" t
    sublistTables = fmap (fromMaybe [] . tailMay . tagsBetween "<tr>" "</tr>") $ sublists
    sublistBlob r = RecordBlob [baseType, sublistName r] (sublistCnames r)
    sublistName r = pk .
                    sections (~== ("</a>" :: String)) .
                    fromMaybe [] . headMay .
                    tagsBetween "<td>" "</td>" . fromMaybe [] $
                    headMay r
    sublistCnames r = [headCname r] <> tailCname r
    headCname = pk .
                tagsBetween "<div>" "</div>" .
                fromMaybe [] . headMay .
                fromMaybe [] . tailMay .
                tagsBetween "<td>" "</td>" . head
    tailCname = fmap (pk .
                    tagsBetween "<div>" "</div>" .
                    fromMaybe [] . headMay .
                    tagsBetween "<td>" "</td>") . tail

-- | Fetch the Netsuite docs page.
req :: String -> IO ByteString
req nsType = bracket est teardown process
  where
    est = establish "system.netsuite.com" 443
    teardown = closeConnection
    process c = do

        q <- buildRequest $ do
                    http GET (BS8.pack $ "/help/helpcenter/en_US/RecordsBrowser/2013_1/Records/" <> nsType <> ".html")
                    setAccept "text/html"
                    setHeader "User-Agent" "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_8_5) AppleWebKit/537.73.11 (KHTML, like Gecko) Version/6.1.1 Safari/537.73.11"

        sendRequest c q emptyBody
        r <- receiveResponse c concatHandler
        return r

    -- | Establish HTTP or HTTPS connection.
    establish h p = withOpenSSL $ do
        ctx <- readIORef global
        openConnectionSSL ctx (BS8.pack h) (fromInteger p)
      where
        host = BS8.pack h

        -- | Stolen from http-streams.
        -- This makes the global IO context for HTTPS work.
        global = unsafePerformIO $ do
            ctx <- baselineContextSSL
            newIORef ctx

-- | Grab everything between two tags
tagsBetween :: (Show s, StringLike s) => String -> String -> [Tag s] -> [[Tag s]]
tagsBetween s e t = fmap (takeWhile (~/= e)) .
                    sections (~== s) $
                    t

-- | Get a string out of something
stringHead :: (Show s, StringLike s) => (a -> s) -> [a] -> s
stringHead fn = maybe (fromString ("" :: String)) fn . headMay

itemInnerValue :: (Show s, StringLike s) => [Tag s] -> s
itemInnerValue x = case x !! 1 of
    t@(TagText _) -> fromTagText t
    _             -> fromString ("" :: String)
