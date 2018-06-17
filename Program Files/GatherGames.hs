module GatherGames where

import FetchGate
import GameFetch
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as B
import DataFile
import System.Random

{- initiateGather str accountId
used to start a match gathering session.
PRE: accountId is a valid account id from a RIOT account on EU-West.
Also that the account is atleast level 30 and has played atleast 1 ranked 5v5 solo game.
RETURNS: Null
SIDE EFFECTS: gather matches and printing them to a JSON file, in the form of a Match data type (declared in JSONGameFetch).
EXAMPLE: initiateGather "START" 200094231 = the following object being written to GameStorage.json
[{
  "team1": {
    "player1": {
      "playerChampWin": 23,
      "playerChampLoss": 11,
      "playerRank": "SILVER"
    },
    "player5": {...
    },
    "player4": { ...
    },
    "player3": {...
    },
    "player2": {...
    }
  },
  "team2": { ...
  }
}

initiateGather "RESUME" 200094231 = The result is the same as above but
without the first bracket, "[".

(Note that these examples show the first match gathered and more matches
will be added later on.
Also that the example shows what eventually will be added to the json file not
all the steps inbetween.)
-}
initiateGather :: String -> Int -> IO ()
initiateGather str accountId | str == "START" = do
  B.appendFile "GameStorage.json" (B.cons 91 B.empty)
  gatherMatchSession accountId
                             | otherwise = do
  gatherMatchSession accountId

{- gatherMatchSession accountid
used to start the gathering sessions of matches from accountid then resumes
the gathering sessions with a new account id.
PRE: accountId is a valid account id from a RIOT account on EU-West.
Also that the account is atleast level 30 and has played atleast 1 ranked 5v5 solo game.
RETURNS: Null
SIDE EFFECTS: fetches against riot's api and writes to a JSON file (GameStorage.json)
EXAMPLES: see the example for initiateGather, the eventual result will be the same.

Note: The decoding code snippet are taken and inspired by "school of haskell".
URL: https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/json
-}
gatherMatchSession :: Int -> IO ()
gatherMatchSession accountId = do
  -- Get JSON data and decode it
  d <- (eitherDecode <$> matchListByteString accountId) :: IO (Either String MatchList)
  -- If d is Left, the JSON was malformed.
  -- In that case, we report the error.
  -- Otherwise, we perform the operation of
  -- our choice..
  case d of
     Left err -> print err
     Right matchList -> do
       createMatches $ createMatchUrl $ gatherMatchIds matchList
       resumeGather $ getNewMatchId $ matches matchList

{- matchListByteString accountid
used to create a valid URL from accountid, which can then be used to fetch data
from riot's api by the getContent function.
PRE: accountId is a valid account id from a RIOT account on EU-West.
RETURNS: The JSON retrieved from the url (created from accountid) request as a IO B.ByteString.
SIDE EFFECTS: Fetches the responseHead, responseBody and statusCode from an url
EXAMPLES: matchListByteString 200094231 = returns the encoded json data on the url below.
https://euw1.api.riotgames.com/lol/match/v3/matchlists/by-account/200094231?queue=420&api_key=RGAPI-799a380d-61bc-45bd-ae45-cc0503aeadef
-}
matchListByteString :: Int -> IO B.ByteString
matchListByteString accountId = getContent url
  where
    url = "https://euw1.api.riotgames.com/lol/match/v3/matchlists/by-account/" ++ (show accountId) ++ "?queue=420&api_key=" ++ riotKey

{- gatherMatchIds matchList
used to gather all the match id's from matchList.
PRE: True
RETURNS: A list of all game id's from the matchList
SIDE EFFECTS: None
EXAMPLES: gatherMatchIds (MatchList { matches = [MatchDetails { gameId = 344
                                                                , champion = ...
                                                                , queue = 420
                                                                } ....
                                                   ]}) = [344, .....]
-}
gatherMatchIds :: MatchList -> [Int]
gatherMatchIds matchList = gatherMatchIdsAux $ matches matchList

{- gatherMatchIdsAux listOfMatchDetails
used to gather all game id's from the listOfMatchDetails.
PRE: True
RETURNS: A list of all game id's from the matchList
SIDE EFFECTS: None
EXAMPLES: gatherMatchIdsAux [MatchDetails { gameId = 344
                                          , champion = ...
                                          , queue = 420
                                          },
                            MatchDetails { gameId = 434
                                         , champion = ...
                                         , queue = 420}, ....
                            ] = [344, 434]
VARIANT: Length of listOfMatchDetails
-}
gatherMatchIdsAux :: [MatchDetails] -> [Int]
gatherMatchIdsAux [] = []
gatherMatchIdsAux (x:xs) = [gameId x] ++ gatherMatchIdsAux xs

{- createMatchUrl listOfGameIDs
used to create a list of valid url's from listOfGameIDs, which can be used to
do several match fetches.
PRE: True
RETURNS: a list of url's created from the listOfGameIDs.
SIDE EFFECTS: None
EXAMPLES: createMatchUrl [3529427714,3538759617,3538685683] =
["https://euw1.api.riotgames.com/lol/match/v3/matches/3529427714?api_key=RGAPI-40f1478e-df7d-4277-8a8e-635024167716",
"https://euw1.api.riotgames.com/lol/match/v3/matches/3538759617?api_key=RGAPI-40f1478e-df7d-4277-8a8e-635024167716"
,"https://euw1.api.riotgames.com/lol/match/v3/matches/3538685683?api_key=RGAPI-40f1478e-df7d-4277-8a8e-635024167716"]
VARIANT: Length of listOfGameIDs
-}
createMatchUrl :: [Int] -> [String]
createMatchUrl [] = []
createMatchUrl (x:xs) = [url] ++ createMatchUrl xs
  where url = "https://euw1.api.riotgames.com/lol/match/v3/matches/" ++ (show x) ++ "?api_key=" ++ riotKey

{- createMatches listOfUrls
used to fetch the data from all the URL's in listOfUrls and pass on the data
retrieved from the fetch to createMatchesAux.
PRE: all of the URL's in listOfUrls must be a valid url to fetch match and contain
a valid match id.
RETURNS: Null
SIDE EFFECTS: Writes to GameStorage.json and prints.
EXAMPLES: see the example for initiateGather, the eventual result will be the same.
VARIANT: Length of listOfUrls
-}
createMatches :: [String] -> IO ()
createMatches [] = do
  print "all matches created from the given account id"
  print "initiating gather with a new account id"
createMatches (x:xs) = do
  createMatchesAux $ getContent x
  createMatches xs

{- createMatchesAux jsonMatch
used to decode jsonMatch and bypass the decoded information on to createGameDataObject.
Also adds a coma to the GameStorage.json file after a GameData type has been added.
PRE: True
RETURNS: Null
SIDE EFFECTS: Writes to GameStorage.json and prints.
EXAMPLES: see the example for initiateGather, the eventual result will be the same.

Note: The decoding code snippet are taken and inspired by "school of haskell".
URL: https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/json
-}
createMatchesAux :: IO B.ByteString -> IO ()
createMatchesAux jsonMatch = do
  d <- (eitherDecode <$> jsonMatch) :: IO (Either String Match)
  case d of
    Left err -> print err
    Right match -> do
      createGameDataObject match
      B.appendFile "GameStorage.json" (B.cons 44 B.empty)
      print "A match has been added to the JSON file."

{- getNewMatchId listOfMatchDetails
used to get a game id from the first game of listOfMatchDetails.
PRE: listOfMatchDetails is none empty.
RETURNS: a valid game id from listOfMatchDetails.
SIDE EFFECTS: None
EXAMPLES: getNewMatchId (MatchList { matches = [MatchDetails { gameId = 344
                                                                , champion = ...
                                                                , queue = 420
                                                                } ....
                                                   ]}) = [344]
-}
getNewMatchId :: [MatchDetails] -> Int
getNewMatchId (x:xs) = gameId x

{- resumeGather gameId
used to resume a gathering session
PRE: gameId is a valid game id from a EU-west game.
RETURNS: Null
SIDE EFFECTS: Fetches the responseHead, responseBody and statusCode from an url
EXAMPLES: see the example for initiateGather, the eventual result will be the same.

Note: The decoding code snippet are taken and inspired by "school of haskell".
URL: https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/json
-}
resumeGather :: Int -> IO ()
resumeGather gameId = do
  -- Get JSON data and decode it
  d <- (eitherDecode <$> gameByteString gameId) :: IO (Either String Match)
  -- If d is Left, the JSON was malformed.
  -- In that case, we report the error.
  -- Otherwise, we perform the operation of
  -- our choice. In this case, just print it.
  case d of
     Left err -> print err
     Right match -> do
       newAccountId <- getNewAccountId (participantIdentities match) randomNumberOneToTen
       initiateGather "RESUME" newAccountId

{- getNewAccountId listOfParIds randomNumber
used to select a random account id from listOfParIds
PRE: listOfParIds are valid data extracted from a game from the EU-west server region.
RETURNS: returns a random account id from listOfParIds.
SIDE EFFECTS: Comes from the newStdGen.
EXAMPLES: getNewAccountId ([ParticipantIdentities { participantId = 1, currentAccountId = 214287791, ..}, ...]) 1 = 214287791
          getNewAccountId ([ParticipantIdentities { ....,  participantId = 3, currentAccountId = 210550628, ..}, ...]) 3 = 210550628
VARIANT: Length of listOfParIds
-}
getNewAccountId :: [ParticipantIdentities] -> IO Int -> IO Int
getNewAccountId ((ParticipantIdentities pId player):xs) ioInt = do
  newId <- ioInt
  if newId == pId then return $ currentAccountId player
    else getNewAccountId xs ioInt

{- randomNumberOneToTen
used to generate a number between 1 and 10.
PRE: True
RETURNS: a number between 1 and 10.
SIDE EFFECTS: Comes from the newStdGen.
EXAMPLES: randomNumberOneToTen = 3
          randomNumberOneToTen = 6
          randomNumberOneToTen = 2
-}
randomNumberOneToTen :: IO Int
randomNumberOneToTen = do
          g <- newStdGen
          let rNumber = randomR (1,10) g
          return $ fst rNumber

{- gameByteString gameId
used to fetch json data from a url based on gameId.
PRE: gameId is a valid game id from a EU-west game.
RETURNS: The JSON retrieved from the url (created from gameId) request as a IO B.ByteString.
SIDE EFFECTS: Fetches the responseHead, responseBody and statusCode from an url
EXAMPLES: gameByteString 3538728731 = returns the encoded json data on the url below.
https://euw1.api.riotgames.com/lol/match/v3/matches/3538728731?api_key=RGAPI-799a380d-61bc-45bd-ae45-cc0503aeadef
-}
gameByteString :: Int -> IO B.ByteString
gameByteString gameId = getContent url
  where
    url = "https://euw1.api.riotgames.com/lol/match/v3/matches/" ++ (show gameId) ++ "?api_key=" ++ riotKey
