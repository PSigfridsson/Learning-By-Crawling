{-# LANGUAGE OverloadedStrings,DeriveGeneric,DuplicateRecordFields #-}

module FetchGate where

import Network.HTTP
import Control.Lens
import qualified Data.ByteString as B
import GHC.Generics
import Network.Wreq
import Control.Concurrent (threadDelay)
import Network.HTTP.Types.Header
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC


{-getContent
PRE: Existing url
RETURN: The responseBody of the url
SIDE-EFFECTS: Fetches the responseHead, responseBody and statusCode from an url
-}
getContent :: String -> IO BL.ByteString
getContent url = do
         r <- get url

         statusCode <- return (r ^. responseStatus . statusCode)
         if (statusCode == 503) || (statusCode == 500)
            then do
                   print "503 was found, fetching again"
                   threadDelay 60000000
                   getContent url
            else do
                   body <- return (r^. responseBody)
                   list <- return (r ^. responseHeaders)
                   if (Prelude.read(BC.unpack(drop4 (limitCount list) 0 )) :: Int) > 99 then threadDelay 80000000 else threadDelay 0
                   return body

{- limitCount ((hName, hValue):xs)
Gives the current fetch count
PRE: The header exists
RETURN: The header value where the header name is "X-App-Rate-Limit-Count"
EXAMPLE: limitCount [("X-App-Rate-Limit-Count","1:1,1:120"),("X-App-Rate-Limit","20:1,100:120")] == "1"

-}
limitCount :: ResponseHeaders -> B.ByteString
limitCount [] = ""
limitCount ((hName, hValue):xs) | hName == "X-App-Rate-Limit-Count" = hValue
limitCount ((hName, hValue):xs) | otherwise = limitCount xs

{-drop4 value n
Drops the first and last 4 characters in a bytestring
PRE: 0 <= n < 4
RETURN: value without the first and last 4 characters
EXAMPLE: drop 4 "1:1,5:120" 0 == "5"

-}
drop4 :: B.ByteString -> Int -> B.ByteString
drop4 value n | n == 4 = value
drop4 value n | otherwise = drop4 ((B.tail . B.init) value) (n+1)
