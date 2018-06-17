{-# LANGUAGE OverloadedStrings,DeriveGeneric,DuplicateRecordFields #-}
module TestDataType where
import DataFile

testMatch = Match {
  teams = [
    Team {
      teamId = 100,
      win = "Fail"
    },
    Team {
      teamId = 200,
      win = "Win"
    }
  ],
  participants= [
    Participant{
      participantId= 1,
      teamId= 100,
      championId= 15,
      highestAchievedSeasonTier= "UNRANKED"
      }
    ,
    Participant{
      participantId= 2,
      teamId= 100,
      championId= 24,
      highestAchievedSeasonTier= "BRONZE"
      }
    ,
    Participant{
      participantId= 3,
      teamId= 100,
      championId= 43,
      highestAchievedSeasonTier= "UNRANKED"
      }
    ,
    Participant{
      participantId= 4,
      teamId= 100,
      championId= 142,
      highestAchievedSeasonTier= "BRONZE"
    }
    ,
    Participant{
      participantId= 5,
      teamId= 100,
      championId= 28,
      highestAchievedSeasonTier= "SILVER"
      }
    ,
    Participant{
      participantId= 6,
      teamId= 200,
      championId= 161,
      highestAchievedSeasonTier= "BRONZE"
      }
    ,
    Participant{
      participantId= 7,
      teamId= 200,
      championId= 18,
      highestAchievedSeasonTier= "BRONZE"
      }
    ,
    Participant{
      participantId= 8,
      teamId= 200,
      championId= 67,
      highestAchievedSeasonTier= "SILVER"
      }
    ,
    Participant{
      participantId= 9,
      teamId= 200,
      championId= 37,
      highestAchievedSeasonTier= "SILVER"
      }
    ,
    Participant{
      participantId= 10,
      teamId= 200,
      championId= 53,
      highestAchievedSeasonTier= "SILVER"
      }

  ],
  participantIdentities= [
    ParticipantIdentities{
      participantId= 1,
      player= Player{
        currentAccountId= 200094231,
        summonerName= "Sprittiiy",
        summonerId= 40619666
      }
    }
    ,
    ParticipantIdentities{
      participantId= 2,
      player= Player{
        currentAccountId= 214287791,
        summonerName= "oulghost54",
        summonerId= 63677784

      }
    }
    ,
    ParticipantIdentities{
      participantId= 3,
      player= Player{
        currentAccountId= 210550628,
        summonerName= "SOG Hiroshi",
        summonerId= 58182677
      }
    }
    ,
    ParticipantIdentities{
      participantId= 4,
      player= Player{
        currentAccountId= 205099704,
        summonerName= "beryu",
        summonerId= 47339012

      }
    }
    ,
    ParticipantIdentities{
      participantId= 5,
      player= Player{
        currentAccountId= 35403577,
        summonerName= "TheLightBuster",
        summonerId= 31767239

      }
    }
    ,
    ParticipantIdentities{
      participantId= 6,
      player= Player{
        currentAccountId= 225584965,
        summonerName= "Schlombert",
        summonerId= 81967011


      }
    }
    ,
    ParticipantIdentities{
      participantId= 7,
      player= Player{
        currentAccountId= 31355548,
        summonerName= "ProStroma",
        summonerId= 27494164

      }
    }
    ,
    ParticipantIdentities{
      participantId= 8,
      player= Player{
        currentAccountId= 33644275,
        summonerName= "GuMmÝ",
        summonerId= 29839525


      }
    }
    ,
    ParticipantIdentities{
      participantId= 9,
      player= Player{
        currentAccountId= 33216622,
        summonerName= "MaximusCranus",
        summonerId= 29544037

      }
    }
    ,
    ParticipantIdentities{
      participantId= 10,
      player= Player{
        currentAccountId= 40183602,
        summonerName= "TiTo PhReNeTiC",
        summonerId= 37559272

      }
    }
  ]
}


testGames = [GameData{
  team1 = TeamData{
    player1 = PlayerData{
      playerChampWin= 2,
      playerChampLoss =  3,
      playerRank= "PLATINUM"
    },
    player5 =PlayerData{
      playerChampWin= 3,
      playerChampLoss= 8,
      playerRank= "GOLD"
    },
    player4 = PlayerData{
      playerChampWin= 4,
      playerChampLoss= 1,
      playerRank= "PLATINUM"
    },
    player3 = PlayerData{
      playerChampWin= 2,
      playerChampLoss= 1,
      playerRank= "PLATINUM"
    },
    player2 =PlayerData{
      playerChampWin = 2,
      playerChampLoss = 1,
      playerRank= "PLATINUM"
    }
  },
  team2 = TeamData {
    player1 = PlayerData{
      playerChampWin= 10,
      playerChampLoss= 13,
      playerRank= "PLATINUM"
    },
    player5 = PlayerData{
      playerChampWin= 8,
      playerChampLoss= 5,
      playerRank= "GOLD"
    },
    player4 = PlayerData{
      playerChampWin= 16,
      playerChampLoss= 12,
      playerRank= "PLATINUM"
    },
    player3 = PlayerData{
      playerChampWin= 1,
      playerChampLoss= 1,
      playerRank= "UNRANKED"
    },
    player2 = PlayerData{
      playerChampWin= 3,
      playerChampLoss= 5,
      playerRank= "DIAMOND"
    }
  }
}]

playersExample = ([PlayerData {playerRank = "PLATINUM", playerChampLoss = 3, playerChampWin = 2},PlayerData {playerRank = "PLATINUM", playerChampLoss = 1, playerChampWin = 2},PlayerData {playerRank = "PLATINUM", playerChampLoss = 1, playerChampWin = 2},PlayerData {playerRank = "PLATINUM", playerChampLoss = 1, playerChampWin = 4},PlayerData {playerRank = "GOLD", playerChampLoss = 8, playerChampWin = 3}],[PlayerData {playerRank = "PLATINUM", playerChampLoss = 13, playerChampWin = 10},PlayerData {playerRank = "DIAMOND", playerChampLoss = 5, playerChampWin = 3},PlayerData {playerRank = "UNRANKED", playerChampLoss = 1, playerChampWin = 1},PlayerData {playerRank = "PLATINUM", playerChampLoss = 12, playerChampWin = 16},PlayerData {playerRank = "GOLD", playerChampLoss = 5, playerChampWin = 8}])

listOfplayerExample = [PlayerData {playerRank = "PLATINUM", playerChampLoss = 3, playerChampWin = 2},PlayerData {playerRank = "PLATINUM", playerChampLoss = 1, playerChampWin = 2},PlayerData {playerRank = "PLATINUM", playerChampLoss = 1, playerChampWin = 2},PlayerData {playerRank = "PLATINUM", playerChampLoss = 1, playerChampWin = 4},PlayerData {playerRank = "GOLD", playerChampLoss = 8, playerChampWin = 3}, PlayerData {playerRank = "PLATINUM", playerChampLoss = 13, playerChampWin = 10},PlayerData {playerRank = "DIAMOND", playerChampLoss = 5, playerChampWin = 3},PlayerData {playerRank = "UNRANKED", playerChampLoss = 1, playerChampWin = 1},PlayerData {playerRank = "PLATINUM", playerChampLoss = 12, playerChampWin = 16},PlayerData {playerRank = "GOLD", playerChampLoss = 5, playerChampWin = 8}]

testGame = GameData {team1 = TeamData {player1 = PlayerData {playerRank = "PLATINUM", playerChampLoss = 3, playerChampWin = 2}, player2 = PlayerData {playerRank = "PLATINUM", playerChampLoss = 1, playerChampWin = 2}, player3 = PlayerData {playerRank = "PLATINUM", playerChampLoss = 1, playerChampWin = 2}, player4 = PlayerData {playerRank = "PLATINUM", playerChampLoss = 1, playerChampWin = 4}, player5 = PlayerData {playerRank = "GOLD", playerChampLoss = 8, playerChampWin = 3}}, team2 = TeamData {player1 = PlayerData {playerRank = "PLATINUM", playerChampLoss = 13, playerChampWin = 10}, player2 = PlayerData {playerRank = "DIAMOND", playerChampLoss = 5, playerChampWin = 3}, player3 = PlayerData {playerRank = "UNRANKED", playerChampLoss = 1, playerChampWin = 1}, player4 = PlayerData {playerRank = "PLATINUM", playerChampLoss = 12, playerChampWin = 16}, player5 = PlayerData {playerRank = "GOLD", playerChampLoss = 5, playerChampWin = 8}}}

matchListEx = MatchList {
  matches = matchDetailsEx
}

matchDetailsEx = [
 MatchDetails {
  gameId = 20001,
  champion = 76,
  queue = 420

},
 MatchDetails{
  gameId = 30001,
  champion = 34,
  queue = 420

},
 MatchDetails{
  gameId = 40001,
  champion = 54,
  queue = 420
},MatchDetails {
  gameId = 50001,
  champion = 86,
  queue = 420

} ]

playerEx = Player{
              currentAccountId = 2004361,
              summonerName = "TestName",
              summonerId = 2000312

}

participantIdListEx = [ParticipantIdentities {
                        participantId = 1,
                        player = playerEx
                        },
                       ParticipantIdentities {
                        participantId = 2,
                        player = playerEx
                        },
                       ParticipantIdentities {
                       participantId = 3,
                        player = playerEx
                        },
                       ParticipantIdentities {
                       participantId = 4,
                        player = playerEx
                        },
                       ParticipantIdentities {
                       participantId = 5,
                        player = playerEx
                        },
                       ParticipantIdentities {
                       participantId = 6,
                        player = playerEx
                        },
                       ParticipantIdentities {
                       participantId = 7,
                        player = playerEx
                        },
                       ParticipantIdentities {
                       participantId = 8,
                        player = playerEx
                        },
                       ParticipantIdentities {
                       participantId = 9 ,
                        player = playerEx
                        },
                       ParticipantIdentities {
                       participantId = 10,
                        player = playerEx
                        }]


participantListEx = [ Participant{
                       participantId = 1,
                       teamId = 100,
                       championId = 42,
                       highestAchievedSeasonTier = "BRONZE"

                      },
                      Participant{
                      participantId = 2,
                       teamId = 100,
                       championId = 42,
                       highestAchievedSeasonTier = "BRONZE"



                      },Participant{
                      participantId = 3,
                       teamId = 100,
                       championId = 42,
                       highestAchievedSeasonTier = "BRONZE"

                      },
                      Participant{
                      participantId = 4,
                       teamId = 100,
                       championId = 42,
                       highestAchievedSeasonTier = "BRONZE"

                      },
                      Participant{
                      participantId = 5,
                       teamId = 100,
                       championId = 42,
                       highestAchievedSeasonTier = "BRONZE"

                      },
                      Participant{
                      participantId = 6,
                       teamId = 200,
                       championId = 42,
                       highestAchievedSeasonTier = "BRONZE"

                      },
                      Participant{
                      participantId = 7,
                       teamId = 200,
                       championId = 42,
                       highestAchievedSeasonTier = "BRONZE"

                      },
                      Participant{
                      participantId = 8,
                       teamId = 200,
                       championId = 42,
                       highestAchievedSeasonTier = "BRONZE"

                      },
                      Participant{
                      participantId = 9,
                       teamId = 200,
                       championId = 42,
                       highestAchievedSeasonTier = "BRONZE"

                      },
                      Participant{
                      participantId = 10,
                       teamId = 200,
                       championId = 42,
                       highestAchievedSeasonTier = "BRONZE"

                      }]

teamListEx = [Team{
                teamId = 100,
                win = "Win"

                   },
              Team{
                teamId = 200,
                win = "Fail"


                   }]
accid1 = 37841420
accid2 = 40399723
accid3 = 39456538
accid4 = 230204172
accid5 = 210570575
accid6 = 40308716
accid7 = 26959586
accid8 = 25352954
accid9 = 227240661
accid10 = 41190138
