{-# LANGUAGE OverloadedStrings, DeriveGeneric, DuplicateRecordFields #-}

module GameFetch where

import FetchGate
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import GHC.Generics
import DataFile
riotKey = "RGAPI-90c3a35b-000c-461d-818a-83db85b697c0"


{- createGameDataObject m
   Temporary stores players and their stats in storePlayers.json to then clear after writing them permanently as
   GameData types to GameStorage.json
   PRE:  TRUE
   RETURNS: NULL
   SIDE EFFECTS: Writes an object to GameStorage.json based on given Match that can be decoded to GameData object
                 Exceptions occur if given faulty Match info. Clears storePlayers.json file.
   EXAMPLES: createGameDataObject "GamesData.json" = the following object being written to GameStorage.json
   {
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

Note: The decoding code snippet are taken and inspired by "school of haskell".
URL: https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/json
-}
createGameDataObject :: Match -> IO ()
createGameDataObject match = do
  createGameDataObjectTeams match
  c <- (eitherDecode <$> getJSONPlayers) :: IO (Either String [PlayerData]) -- Read file of players
  case c of
    Left err -> do
     print err
     B.writeFile "storePlayers.json" B.empty
    Right players -> insGameDataFile players
  B.writeFile "storePlayers.json" B.empty
  -- If d is Left, the JSON was malformed.
  -- In that case, we report the error.
  -- Otherwise, we perform the operation of
  -- our choice.

{- insGameDataFile pd
   Writes list of PlayerData to GameStorage file in a format that can be decoded to GameData type
   PRE:  pd contains 10 elements
   RETURNS: NULL
   SIDE EFFECTS: Writes list of PlayerData to GameStorage
   EXAMPLES: insGameDataFile [PlayerData{playerChampWin = 23, playerChampLoss = 29, playerRank = "SILVER"}, ...] =
   the following object being written to GameStorage.json:
   {
     "team1": {
       "player1": {
         "playerChampWin": 23
         "playerChampLoss": 29,
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

-}
insGameDataFile :: [PlayerData] -> IO ()
insGameDataFile p = B.appendFile "GameStorage.json" (encode $ createGameDataTypes p)

{- createGameDataTypes pd
  Creates GameData data type based on PlayerData list
  PRE: TRUE
  RETURNS: GameData object based on list pd of PlayerData objects
  EXAMPLES: [PlayerData{playerChampWin = 23, playerChampLoss = 29, playerRank = "SILVER"}, ...] =
  GameData { team1 = TeamData { player1 = PlayerData { playerChampWin = 23
                                                     , playerChampLoss = 29
                                                     , playerRank = "SILVER"
                                                      },
                                player2 = ... ,
                                player3 = ... ,
                                player4 = ... ,
                                player5 = ... ,
                              }
            team2 = ...
          }
-}
createGameDataTypes :: [PlayerData] -> GameData
createGameDataTypes (p0:p1:p2:p3:p4:p5:p6:p7:p8:p9:[]) =  GameData { team1 = TeamData { player1 = p0,
                                                                                        player2 = p1,
                                                                                        player3 = p2,
                                                                                        player4 = p3,
                                                                                        player5 = p4
                                                                                      },
                                                                     team2 = TeamData { player1 = p5,
                                                                                        player2 = p6,
                                                                                        player3 = p7,
                                                                                        player4 = p8,
                                                                                        player5 = p9
                                                                                       }
                                                                    }
{- createGameDataObjectTeams m
   Writes PlayerData objects to storePlayers.json file
   in a format that is decodable into PlayerData objects
   PRE:  TRUE
   RETURNS: NULL
   SIDE EFFECTS: Writes all 10 participants in given match m to file storePlayers.json
   EXAMPLES: createGameDataObjectTeams "GamesData.json" = the following object being written to GameStorage.json:
   {
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
-}
createGameDataObjectTeams :: Match -> IO ()
createGameDataObjectTeams (Match ((Team _ wol):ts) (p:ps) pIds) | wol == "Win" = do
    (formatFile 91)
    createGameDataObjectTeamsAux (p:ps) pIds
    (formatFile 93)
                                                                | otherwise = do
    (formatFile 91)
    createGameDataObjectTeamsAux  (Prelude.reverse (p:ps)) pIds
    (formatFile 93)
      where
        -- createGameDataObjectTeamsAux pars parsId
        -- VARIANT: length of pars.
        createGameDataObjectTeamsAux :: [Participant] -> [ParticipantIdentities] -> IO ()
        createGameDataObjectTeamsAux [p] pIds = createPlayerDataObject p pIds
        createGameDataObjectTeamsAux (p:ps) pIds = do
          createPlayerDataObject p pIds
          print "A player has been added to storePlayers.json"
          (formatFile 44)
          createGameDataObjectTeamsAux ps pIds

{- formatFile c
   Takes int that describes which character to append to file storePlayers.json
   PRE: TRUE
   RETURNS: NULL
   SIDE EFFECTS: writes character (B.cons c) based on int c to file storePlayers.json
   EXAMPLES: formatFile 44 = will write "," to storePlayers.json file,
             formatFile 91 = will write "[" to storePlayers.json file,
             formaFile 93 = will write "]" to storePlayers.json file
-}
formatFile :: Int -> IO ()
formatFile c | c == 91 = B.appendFile "storePlayers.json" (B.cons 91 B.empty)
             | c == 44 = B.appendFile "storePlayers.json" (B.cons 44 B.empty)
             | otherwise =     B.appendFile "storePlayers.json" (B.snoc B.empty 93)

{- createPlayerDataObject p pId
   decodes matchlist of given p pId and uses decoded matchlist, p and pId to create and write
   player to storePlayers.json
   PRE: TRUE
   RETURNS : NULL
   SIDE EFFECTS: writes participant p to file storePlayers.json
   EXAMPLES: createPlayerDataObject (Participant{participantId = 1, championId = 15, highestAchievedSeasonTier = "UNRANKED" ...}) ([ParticipantIdentities { participantId = 1, player = "Sprittiiy", ..}, ...])
                                   =  the following object being written to storePlayers.json:
                                            "player1": {
                                            "playerChampWin": 0,
                                            "playerChampLoss": 1,
                                            "playerRank": "UNRANKED"
                                          }

Note: The decoding code snippet are taken and inspired by "school of haskell".
URL: https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/json
-}
createPlayerDataObject :: Participant -> [ParticipantIdentities] -> IO ()
createPlayerDataObject p pId = do
  g <- (eitherDecode <$> (getJSONmatchList (fst (analyzePlayerGetAccId p pId)) (analyzePlayerGetChampId p))) :: IO (Either String MatchList)
  print g
  case g of
    Left err -> print err
    Right matchList -> calculatePlayerStats (analyzePlayerGetChampId p) (gatherChampGames matchList) p pId []

{- calculatePlayerStats i gameIds p pIds matchList
  calculates champion winrate of played champion in given match based on given match history
  PRE: TRUE
  RETURNS: NULL
  SIDE EFFECTS: writes participant p and his champion winrate based on match history gameIds to
                file storePlayers.json
  EXAMPLES: calculatePlayerStats 15
            (MatchList{[MatchDetails{gameId = 3342, champion = 15, queue = 420, ..},...]})
            (Participant{participantId = 1, championId = 15, highestAchievedSeasonTier = "UNRANKED" ...})
            ([ParticipantIdentities { participantId = 1, player = "Sprittiiy", ...}, ...])
            [] =
            the following object being written to storePlayers.json:
                     "player1": {
                     "playerChampWin": 0,
                     "playerChampLoss": 1,
                     "playerRank": "UNRANKED"
                   }

Note: The decoding code snippet are taken and inspired by "school of haskell".
URL: https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/json
-}
--VARIANT: length of gameIds
calculatePlayerStats :: Int -> [Int] -> Participant -> [ParticipantIdentities] -> [Match] -> IO ()
calculatePlayerStats cId [] p pId listOfMatches = do
  B.appendFile "storePlayers.json" (encode $ constructPlayer ( analyzePlayer p pId (gatherStats listOfMatches (analyzePlayerGetChampId p) (0,0))))
calculatePlayerStats cId (x:xs) p pId listOfMatches = do
  print (x:xs)
  x <- (eitherDecode <$> (getJSONGame x)) :: IO (Either String Match)
  case x of
    Left err -> do print err
    Right match -> calculatePlayerStats cId xs p pId (match:listOfMatches)

{- analyzePlayer p pId wl
   creates 2-tuple containing participants champion win/loss and highest achieved rank
   PRE: wl is tuple where fst element is total wins and snd element is total losses on played champ
   RETURNS: 2-tuple (wl, hRank) where wl contains total champ wins and total champ losses. hRank
   is the highest achieved rank for given player p in list of participant identities pId
   EXAMPLES: analyzePlayer
     (Participant{participantId = 1, championId = 15, highestAchievedSeasonTier = "UNRANKED" ...})
     ([ParticipantIdentities { participantId = 1, player = "Sprittiiy", ...}, ...])
     (0,0) =
     ((0,1) "UNRANKED")

-}
analyzePlayer :: Participant -> [ParticipantIdentities] -> (Int, Int) -> ((Int,Int), String)
analyzePlayer p@(Participant _ _ cId _) pId cWL = let aId = analyzePlayerGetAccId p pId in (cWL , snd aId)

{- analyzePlayerGetAccId p pId
   get account Id for player to use for searching match histories and their highest achieved rank
   PRE: TRUE
   RETURNS: (accId, hRank) where accId is the accountId of given participant p in pId
   EXAMPLES: analyzePlayerGetAccId
            (Participant{participantId = 1, championId = 15, highestAchievedSeasonTier = "UNRANKED" ...})
            ([ParticipantIdentities { participantId = 1, player = "Sprittiiy", ...}, ...]) =
            (200094231, "UNRANKED")
   VARIANT: length of pId
-}
analyzePlayerGetAccId :: Participant -> [ParticipantIdentities] -> (Int, String)
analyzePlayerGetAccId (player@(Participant participantId _ _ hRank)) ((ParticipantIdentities matchingId playerId):xs)
                        | participantId == matchingId = (getAccountId playerId, hRank)
                        | otherwise = analyzePlayerGetAccId player xs

{- analyzePlayerGetChampId p
  extract champId from given participant
  PRE: TRUE
  RETURNS: champion id based of given participant p
  EXAMPLES:  analyzePlayerGetChampId (Participant { participantId :: 7
                                                  , teamId :: 100
                                                  , championId :: 40
                                                  , highestAchievedSeasonTier :: "DIAMOND"
                                                  } ) = 40
-}
analyzePlayerGetChampId :: Participant -> Int
analyzePlayerGetChampId (Participant _ _ cId _) = cId

{- gatherChampGames (ml mDetails)
  adds all the gameIds in ml and puts them in a list
  PRE: TRUE
  RETURNS: list of gameIds based on given MatchList ml
  EXAMPLES: gatherChampGames (MatchList { matches = [MatchDetails { gameId = 344
                                                                  , champion = 12
                                                                  , queue = 420
                                                                  }
                                                     ]}) = [344]
 VARIANT: length of mDetails
-}
gatherChampGames :: MatchList -> [Int]
gatherChampGames (MatchList []) = []
gatherChampGames (MatchList (x:xs)) = (gatherChampGamesAux x []) ++ (gatherChampGames (MatchList xs))
                                                  where
                                                    gatherChampGamesAux :: MatchDetails-> [Int] -> [Int]
                                                    gatherChampGamesAux (MatchDetails gameId _ q) acc = gameId:acc

{- gatherStats m cId acc
  based on given list of matches calculates amount of win losses on a champ
  PRE: TRUE
  RETURNS: tuple (w, l) where w is total losses on champion with id cId
  calculated from list of matches m
  EXAMPLE: gatherStats [Match{[(Team{teamId = 1234, win = "Win"}), (Team{teamId = 3421, win = "Fail"})]
                      [Participant{participantId = 1,teamId = 3421,championId = 15, highestAchievedSeasonTier ="UNRANKED"...}, ..]
                      [ParticipantIdentities{...}, ..]}, ..] 15 = (0,1)
  VARIANT: length of m
-}
gatherStats :: [Match] -> Int -> (Int, Int) -> (Int, Int)
gatherStats [] cId wl     = wl
gatherStats (m:ms) cId wl =  gatherStats ms cId (gatherStatsAcc wl (winOrLoss m (gatherChampsWinLossAux m cId)))
        where
            gatherStatsAcc :: (Int,Int) -> Bool -> (Int, Int)
            gatherStatsAcc (w, l) b
                      | b = (w+1, l)
                      | otherwise  = (w, l+1)


{- gatherChampsWinLossAux (m t (listOfP)) cId
   retrieves the teamId of the team containing a certain champion
   PRE: TRUE
   RETURNS: teamId of a team in m containing champ with id cId
   EXAMPLES: gatherChampsWinLossAux (Match{[(Team{...}), (Team{teamId = 3421,...})]
                       [Participant{participantId = 1,teamId = 3421,championId = 15, highestAchievedSeasonTier ="UNRANKED"...}, ..]
                       [ParticipantIdentities{...}, ..]}) 15 = 3421
   VARIANT: length of listOfP
-}
gatherChampsWinLossAux :: Match -> Int ->  Int
gatherChampsWinLossAux (Match t ((Participant _ teamId matchingId _):xs) pId) cId
                                    | cId == matchingId = teamId
                                    | otherwise = gatherChampsWinLossAux (Match t xs pId) cId
{- winOrLoss m tId
  checks to see if team won or lost
  PRE: TRUE
  RETURNS: True if win-status is true for given m and teamId, otherwise false
  EXAMPLES: winOrLoss (Match{[(Team{teamId = 1234, win = "Win"}), (Team{teamId = 3421, win = "Fail"})]
                      [Participant{...}, ..] [ParticipantIdentities{...}, ..]}) 1234
                       = True
-}
winOrLoss :: Match -> Int -> Bool
winOrLoss (Match [(Team matchingId wl), (Team matchingId2 wl2)] _ _) teamId
                      | matchingId == teamId && wl == "Win" = True
                      | matchingId2 == teamId && wl2 == "Win" = True
                      | otherwise = False

{- constructPlayer ((w, l), hrank)
   constructs PlayerData object
   PRE: w are wins, l are losses
   RETURNS: PlayerData object with based on w, l and hrank
   EXAMPLE: constructPlayer ((100, 31) "BRONZE") =
            PlayerData { playerRank = "BRONZE",
                         playerChampWin = 100,
                         playerChamploss = 31
                     }
-}
constructPlayer :: ((Int, Int), String) ->  PlayerData
constructPlayer ((w,l), hRank) = PlayerData { playerRank = hRank
                                             , playerChampWin = w
                                             , playerChampLoss = l
                                             }
{- getAccountId p
   retrieves players account id
   PRE: TRUE
   RETURNS: account id based on given player p
   EXAMPLES: getAccountId (Player { currentAccountId = 344,
                                    summonerName = "TestSummoner",
                                    summonerId = 1923}) = 344
-}
getAccountId :: Player -> Int
getAccountId (Player aId _ _) = aId


{- matchListURL aId cId
   retrieves matchlist of games where a certain account id played a specific champion
   in ranked solo queue, in season 11
   PRE: TRUE
   RETURNS: url as string containing aId and cId
   EXAMPLES: matchlistURL 200094231 15 = "https://euw1.api.riotgames.com/lol/match/v3/matchlists/by-account/200094231?champion=15&season=11&queue=420&api_keyRGAPI-318f56b8-0d6d-4d61-bd5e-26351e6da2c5
-}
matchListURL :: Int -> Int -> String
matchListURL aId cId = "https://euw1.api.riotgames.com/lol/match/v3/matchlists/by-account/" ++ (show aId) ++ "?" ++ "champion=" ++ (show cId)++"&season=11"++"&queue=420"++"&api_key=" ++ riotKey

{- getJSONmatchList aId cId
   retrieves a json file as IO B.ByteString
   PRE: TRUE
   RETURNS: The JSON retrieved from the url request containing aId & cId as a IO B.ByteString
   SIDE EFFECT: Fetches the responseHead, responseBody and statusCode from an url
   EXAMPLES: getJSONmatchList 200094231 15 = returns the encoded json data on the url below.
   https://euw1.api.riotgames.com/lol/match/v3/matchlists/by-account/200094321?champion=15&season=11&queue=420&api_key=RGAPI-318f56b8-0d6d-4d61-bd5e-26351e6da2c5
-}
getJSONmatchList :: Int -> Int -> IO B.ByteString
getJSONmatchList aId cId = getContent $ matchListURL aId cId

{- getJSONGame gId
  PRE: TRUE
  RETURNS: The JSON retrieved from the url request containing gId as a IO B.ByteString
  SIDE EFFECTS: Fetches the responseHead, responseBody and statusCode from an url
  EXAMPLES: getJSONGame 3535466692 = returns the encoded json data on the url below.
  https://euw1.api.riotgames.com/lol/match/v3/matches/3535466692
-}
getJSONGame :: Int -> IO B.ByteString
getJSONGame gameId = getContent url
  where
    url = "https://euw1.api.riotgames.com/lol/match/v3/matches/" ++ (show gameId) ++ "?api_key=" ++ riotKey

jsonPlayersFile :: FilePath
jsonPlayersFile = "storePlayers.json"

getJSONPlayers :: IO B.ByteString
getJSONPlayers = B.readFile jsonPlayersFile
