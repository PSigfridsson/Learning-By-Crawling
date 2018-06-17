{-# LANGUAGE OverloadedStrings, DeriveGeneric, DuplicateRecordFields#-}
module Converter where
  
import DataFile
import LogisticRegression
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import GHC.Generics


{-startML
Takes all the games from a JSON file and calculates the accuracy using logistic regression
PRE: File existing
RETURN: The total accuracy from the training set
SIDE-EFFECTS: Reading from JSON file
EXAMPLE: startML = "The program predicted correct in 84.433% of the matches"

Note: The decoding code snippet are taken and inspired by "school of haskell".
URL: https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/json
-}
startML :: IO ()
startML = do
  -- Get JSON data and decode it
  d <- (eitherDecode <$> getJSONLG) :: IO (Either String [GameData])
  -- If d is Left, the JSON was malformed.
  -- In that case, we report the error.
  -- Otherwise, we perform the operation of
  -- our choice. In this case, just print it.
  case d of
     Left err -> print err
     Right match -> do
                       trainingList <- return(Prelude.take (trainingTestDivider(fromIntegral(Prelude.length match))) match)
                       testList <- return(Prelude.drop (trainingTestDivider(fromIntegral(Prelude.length match))) match)
                       print ("Creating trainingset of " ++ show(Prelude.length trainingList) ++ " games")
                       print ("Creating testset of " ++ show(Prelude.length testList) ++ " games")
                       print ("The program predicted correct in " ++ (show( mainFunction (comparison(getStatistics testList)) (mainPredictor (comparison (getStatistics trainingList)))) ++ "% of the matches"))

{-trainingTestDivider n
Calculates 80% of the input value, used for splitting up a data set
PRE:TRUE
RETURNS: 80% of n
EXAMPLE: trainingTestDivider 56 == 44
 -}

trainingTestDivider :: Double -> Int
trainingTestDivider n = floor(n * 0.8) :: Int


{- comparison l@((t1,t2):ls)
Calculates the ratio in the two teams wins and losses and alternates between games if the winning team is team 1 or team 2, so the
logistic regression can see two different outcomes (If we don't alternate the result will always be 0, since team 1 is the winner, and the
regression wont work)
PRE: TRUE
RETURNS: A list of tuples where each tuple contains the ratio between t1 and t2 and the result of the match, either 1 or 0
EXAMPLE: comparison [(1.4,1.2),(2.54,0.43),(1,1),(3.4,0),(0,5.3)] == [(1.1666666666666667,0.0),(0.16929133858267717,1.0),(1.0,0.0),(0.0,1.0),(0.0,0.0)]

-}
comparison :: [(Double,Double)] -> [(Double,Double)]
comparison [] = []
comparison [(t1, t2)] = ((t1 / t2, 0) : comparison [])
comparison ((t1G1, 0):(0,t2G2):ls) = ((t1G1 / 1, 0) : (t2G2 / 1, 1) : comparison ls)
comparison ((t1G1, t2G1):(0,t2G2):ls) = ((t1G1 / t2G1, 0) : (t2G2 / 1, 1) : comparison ls)
comparison ((t1G1, 0):(t1G2,t2G2):ls) = ((t1G1 / 1, 0) : (t2G2 / t1G2, 1) : comparison ls)
comparison ((t1G1, t2G1):(t1G2,t2G2):ls) = ((t1G1 / t2G1, 0) : (t2G2 / t1G2, 1) : comparison ls)


{- myStatistics l@(x:xs)
Creates a list of tuples where each tuple has the statistics which will be used in the logistic regression
PRE: TRUE
RETURN: A list of tuples containing the values in l
EXAMPLE: getStatistics games = [(1.3,0.5)(4.0,2.2)(1.4343,2.0)(2.33,0.44)(1.0,1.0)]
where the length of games = length of the result
games = [ GameData
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
   }, ...]
-}
--VARIANT: length l
getStatistics :: [GameData] -> [(Double, Double)]
getStatistics [] = []
getStatistics (x:xs) = ((statisticsAux x) : (getStatistics xs))

{- statistics Aux game
Creates the statistics used in the logistic regression from a game
PRE: TRUE
RETURN: A tuple containing the statistics taken from game
EXAMPLE: statisticsAux game = (1,2,3.43

where game = GameData {
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
statisticsAux :: GameData -> (Double, Double)
statisticsAux game = let (t1,t2) = toPlayers game
                     in (champWinLoss t1 (0 ,0), champWinLoss t2 (0,0))

{- toPlayers game
Takes all the players out of the game
PRE: TRUE
RETURN: A Tuple containing two lists where each list is all players in a team from game
EXAMPLE: toPlayers game == (["player1": {
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
       }],
       ["player1": {
         "playerChampWin": 13,
         "playerChampLoss": 5,
         "playerRank": "BRONZE"
       },
       "player5": {...
       },
       "player4": { ...
       },
       "player3": {...
       },
       "player2": {...
       }])
where game = GameData {
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
toPlayers :: GameData -> ([PlayerData], [PlayerData])
toPlayers game = ([ player1 (team1 game),
                    player2 (team1 game),
                    player3 (team1 game),
                    player4 (team1 game),
                    player5 (team1 game)
                    ] ,
                  [ player1 (team2 game),
                    player2 (team2 game),
                    player3 (team2 game),
                    player4 (team2 game),
                    player5 (team2 game)
                    ])



{-champWin l@(x:xs) (cW, cL)
Calculates the ratio in a teams total wins and losses
PRE: cW and cL >= 0
RETURN: The ratio in a teams total wins and losses
EXAMPLE: champWinLoss players (0,0) == 3.2
where players = [ "player1": {
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
       }]
-}
champWinLoss :: [PlayerData] -> (Double,Double) -> Double
champWinLoss [] (cW,0) = cW / 1
champWinLoss [] (cW,cL) = cW / cL
champWinLoss (x:xs) (cW,cL) = champWinLoss xs (cW + fromIntegral(playerChampWin x), cL + fromIntegral(playerChampLoss x))


{-
Reads the information from a JSON file
PRE: File exists
SIDE-EFFECTS: Reading from file
-}
getJSONLG :: IO B.ByteString
getJSONLG = B.readFile jsonFileGS

{- Change the FilePath to "GameStorage.json" to use newly farmed games. -}
jsonFileGS :: FilePath
jsonFileGS = "TestMachineLearningGames.json"
