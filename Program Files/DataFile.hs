{-# LANGUAGE OverloadedStrings, DeriveGeneric, DuplicateRecordFields #-}
module DataFile where

import GHC.Generics
import Data.Aeson

{- Match
Represents a match containing data such as a list of teams, participants and participant identities
INVARIANT: must contain valid data from the EU-West region. Eg the list of
participantIdentities must contain valid player data types.
-}
data Match =
  Match { teams :: [Team]
        , participants :: [Participant]
        , participantIdentities :: [ParticipantIdentities]
  } deriving (Show, Generic)

instance FromJSON Match
instance ToJSON Match

{- Team
Represents a team with data such as teamId and win.
INVARIANT: teamId == 100 or teamId == 200. win == "Win" or win == "Fail"
-}
data Team =
  Team { teamId :: Int
        , win :: String
} deriving (Show, Generic)

instance FromJSON Team
instance ToJSON Team

{- Participant
Represents a participant which contains participant id, team id, champion played and
the rank.
INVARIANT: participantId, teamId, championId and the rank must all be valid data
retrieved from the EU west server region from Riot Games API.
-}
data Participant =
  Participant { participantId :: Int
              , teamId :: Int
              , championId :: Int
              , highestAchievedSeasonTier :: String
} deriving (Show, Generic)

instance FromJSON Participant
instance ToJSON Participant

{- ParticipantIdentities
Represents a participants identity. Contains a participant id and a player object.
INVARIANT: The participantId and the player must be be valid data
retrieved from the EU west server region from Riot Games API
-}
data ParticipantIdentities =
  ParticipantIdentities { participantId :: Int
                        , player :: Player
} deriving (Show, Generic, Eq)

instance FromJSON ParticipantIdentities
instance ToJSON ParticipantIdentities

{- Player
Represents a player object which contain data such as a currently used accountId,
summoner name and a summoner Id.
INVARIANT: currentAccountId, summonerName and summonerId must all be valid data
retrieved from the EU west server region from Riot Games API.
-}
data Player =
  Player { currentAccountId :: Int
         , summonerName :: String
         , summonerId :: Int
} deriving (Show, Generic, Eq)

instance FromJSON Player
instance ToJSON Player

{- MatchList
Represents a players match list and the game details of all the matches.
INVARIANT: MatchList must contain valid data retreived from the EU west server
region from Riot Games API.
-}
data MatchList =
    MatchList { matches :: [MatchDetails]
              } deriving(Show, Generic, Eq)

instance FromJSON MatchList
instance ToJSON MatchList

{- MatchDetails
Represents all details of a match such as game id, champion played and queue.
INVARIANT: MatchDetails must contain valid data retreived from the EU west server
region from Riot Games API.
-}
data MatchDetails =
    MatchDetails { gameId :: Int
                 , champion :: Int
                 , queue :: Int
                 } deriving(Show, Generic, Eq)
instance FromJSON MatchDetails
instance ToJSON MatchDetails

{- GameData
Represents a match of two teams and the players individual statistics (Champion win ratio).
INVARIANT: team1 and team2 must contain valid player objects retreived from the
EU west server region from Riot Games API. team1 is always the winner.
-}
data GameData =
  GameData { team1 :: TeamData
            ,team2 :: TeamData
            } deriving (Show, Generic, Eq)

instance FromJSON GameData
instance ToJSON GameData

{- TeamData
Represents a team of five players with their individual statistics.
INVARIANT: player1, player2, player3, player4 and player5 are all valid and
contains data retreived from the EU west server region from Riot Games API.
-}
data TeamData =
  TeamData { player1 :: PlayerData
            ,player2 :: PlayerData
            ,player3 :: PlayerData
            ,player4 :: PlayerData
            ,player5 :: PlayerData
            } deriving (Show, Generic, Eq)

instance FromJSON TeamData
instance ToJSON TeamData

{- PlayerData
Represents a player object with their name and individual statistics
INVARIANT: playerRank, playerChampLoss and playerChampWin must all be valid and
retreived from the EU west server region from Riot Games API.
-}
data PlayerData =
  PlayerData { playerRank :: String
             , playerChampLoss :: Int
             , playerChampWin :: Int
             } deriving (Show, Generic, Eq)

instance FromJSON PlayerData
instance ToJSON PlayerData
