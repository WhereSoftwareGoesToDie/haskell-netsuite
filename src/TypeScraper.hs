{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.IORef (newIORef, readIORef)
import Data.Monoid
import Network.Http.Client
import OpenSSL (withOpenSSL)
import Safe
import System.Environment
import System.Exit
import System.IO.Unsafe (unsafePerformIO)
import Text.HTML.TagSoup
import Text.StringLike

data RecordBlob = RecordBlob
    { _rbName :: [String]
    , _rbFields :: [String]
    } deriving (Show)

main :: IO ()
main = do
    args <- getArgs
    case args of
        (t:_) -> do
            r <- req t
            let tags = parseTags $ BS8.unpack r
            print r
            print $ getBlobs tags
        _     -> putStrLn "USAGE: netsuite-type-scraper <type name>" >> exitFailure

-- | Get Record Blobs from HTML
getBlobs :: (Show s, StringLike s) => [Tag s] -> String -- [RecordBlob]
getBlobs t = toString $
    stringHead itemInnerValue $
    sections (\a -> a ~== ("<div class='record_id'>" :: String)) t

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

-- | Get a string out of something
stringHead :: (Show s, StringLike s) => (a -> s) -> [a] -> s
stringHead fn = maybe (fromString ("" :: String)) fn . headMay

itemInnerValue :: (Show s, StringLike s) => [Tag s] -> s
itemInnerValue x = case x !! 1 of
    t@(TagText _) -> fromTagText t
    _             -> fromString ("" :: String)
