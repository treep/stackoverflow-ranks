{-# LANGUAGE TypeSynonymInstances #-}

import Data.Ord
import Data.Char
import Data.Ratio
import Data.Maybe
import Data.List
import qualified Data.ByteString.Lazy.Char8 as L
import Text.Printf
import Text.JSON
import Codec.Compression.GZip
import Control.Applicative
import Control.Monad
import Control.Arrow
import Control.Concurrent
import Control.Concurrent.STM
import System.IO
import System.Environment
import Network.URI
import Network.HTTP
import Network.Browser

type Rank = (String, Integer)

type Ranks = [Rank]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> readTags file >>= stackoverflowRanks >>= printRanks
    _  -> hPutStrLn stderr "usage: ./ranks <file-with-tags> 1>/dev/null"
  where
    readTags :: FilePath -> IO [String]
    readTags file = parseTags <$> readFile file

    parseTags :: String -> [String]
    parseTags = map stringToLower . concatMap words . lines

    printRanks :: Ranks -> IO ()
    printRanks = mapM_ $
      \(name, rank) -> hPutStrLn stderr $ printf "%-20s%-40i" name rank

stackoverflowRanks :: [String] -> IO Ranks
stackoverflowRanks things = do
  nTags <- extractNTags
  let pages = [1 .. nTags `div` 100 + 1]
  chan <- newTChanIO
  -- map
  forM_ pages $ forkIO . worker chan
  -- reduce
  atomically $ reverse . sortBy (comparing snd) . concat <$>
    forM pages (const $ readTChan chan)
  where
    extractNTags :: IO Integer
    extractNTags = fromResult . fmap
      ( numerator
      . fromResult . readJSON . snd . fromJust
      . find (("total" ==) . fst)
      . fromJSObject . readJSON'
      ) <$> json 1

    worker ch i = pairs i >>= atomically . writeTChan ch

    pairs :: Integer -> IO Ranks
    pairs n = extractPairs <$> json n

    extractPairs :: Result JSValue -> Ranks
    extractPairs = fromResult . fmap
      ( filter (flip elem things . fst)
      . map (makePair . fromJSObject . readJSON')
      . fromResult . readJSON . snd . fromJust
      . find (("tags" ==) . fst)
      . fromJSObject . readJSON'
      )

    makePair :: [(String, JSValue)] -> Rank
    makePair = 
      fromResult . readJSON . fromJust . lookup "name" 
      &&&
      numerator . fromResult . readJSON . fromJust . lookup "count"

    json :: Integer -> IO (Result JSValue)
    json n = decode . L.unpack <$> answer n

    answer :: Integer -> IO L.ByteString
    answer n = rspBody . snd <$> browse (get $ tagsURI n)

    tagsURI :: Integer -> String
    tagsURI = ("http://api.stackoverflow.com/1.1/tags?pagesize=100&page=" ++) . show

-- -----------------------------------------------------------------------------
-- * JSON utils

-- ** Orphan instance.

instance JSON Rational where
  readJSON (JSRational _ x) = Ok x
  readJSON _ = Error "Unable to read Rational"
  showJSON = JSRational False

-- ** Partial functions.

fromResult :: Result t -> t
fromResult (Ok x) = x
fromResult (Error e) = error e

readJSON' :: JSON t => JSValue -> t
readJSON' = fromResult . readJSON

-- -----------------------------------------------------------------------------
-- * HTTP utils

-- | Perform a GET request.
get :: String -> BrowserAction (HandleStream L.ByteString) (URI, Response L.ByteString)
get = fmap (second maybeUnzip)
  . request
  . replaceHeader HdrAcceptEncoding "gzip"
  . defaultGETRequest_
  . fromJust
  . parseURI
  where
    maybeUnzip rsp = if isGz rsp then rsp { rspBody = decompress $ rspBody rsp } else rsp
    isGz = maybe False (== "gzip") . findHeader HdrContentEncoding

-- -----------------------------------------------------------------------------
-- * Common utils

stringToLower :: String -> String
stringToLower = map toLower
