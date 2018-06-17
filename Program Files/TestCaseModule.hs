module TestCaseModule where

import Converter
import GatherGames
import GameFetch
import LogisticRegression
import TestDataType
import Test.HUnit

logPredTest = TestCase (assertEqual "logPredictor when input is 0: " (0.5) (logPredictor(helpFunction 0 0 1)))
updateTest = TestCase (assertEqual "updater: " (0.05)  (updater 0.5 1 0 1))
mainPredTest = TestCase (assertEqual "mainPredictor: " (8.131340328106973e-2,-0.296633933183218) (mainPredictor [(0.1,1.0),(4.0,0.0),((-0.5),1.0),((-2.0),1.0)]))
totalPredTest = TestCase (assertEqual "totalPredicts" ([(0.5719386000675216,1.0),(0.9237590113124382,0.0),(0.4876442659589688,1.0),(0.28957933079893683,1.0)]) (totalPredicts [(0.1,1.0),(4.0,0.0),((-0.5),1.0),((-2.0),1.0)] (0.233232,0.56533)))
convertTest = TestCase (assertEqual "converter: " ([(0.0,1.0),(1.0,0.0),(0.0,1.0),(1.0,1.0)]) (converter [(0.2,1.0),(0.5,0.0),(0.342,1.0),(0.999,1.0)]))
correctTest = TestCase (assertEqual "correctPredicts: " (4.0) (correctPredicts [(1.0,1.0),(0.0,1.0),(0.0,0.0),(1.0,1.0),(1.0,0.0),(0.0,0.0)]))
accuracyTest = TestCase (assertEqual "accuracy: " (66.66666666666666) (accuracy [(1.0,1.0),(0.0,1.0),(0.0,0.0),(1.0,1.0),(1.0,0.0),(0.0,0.0)] 4.0))
mainFunc = TestCase (assertEqual "mainFunction: " (25.0) (mainFunction [(0.1,1.0),(4.0,0.0),((-0.5),1.0),((-2.0),1.0)] (0.233232,0.56533)))

ttdTest = TestCase ( assertEqual "trainingTestDivider: " (44) (trainingTestDivider 56))
compTest = TestCase (assertEqual "comparison" ([(1.1666666666666667,0.0),(0.16929133858267717,1.0),(1.0,0.0),(0.0,1.0),(0.0,0.0)]) ( comparison [(1.4,1.2),(2.54,0.43),(1,1),(3.4,0),(0,5.3)]))
getStatsTest = TestCase (assertEqual "getStatistics" ([(0.9285714285714286,1.0555555555555556)]) (getStatistics testGames))
statAuxTest = TestCase (assertEqual "statisticsAux" ((0.9285714285714286,1.0555555555555556)) (statisticsAux(Prelude.head testGames)))
toPlayerTest = TestCase (assertEqual "toPlayers" (playersExample) ( toPlayers(Prelude.head testGames)))
champWinLossTest = TestCase (assertEqual "champWinLoss" (0.9285714285714286) (champWinLoss(fst playersExample) (0,0)))

test1 = TestCase (assertEqual "gatherMatchIds" ([20001, 30001, 40001, 50001]) (gatherMatchIds matchListEx))
test2 = TestCase (assertEqual "createMatchUrl" (["https://euw1.api.riotgames.com/lol/match/v3/matches/3529427714?api_key=" ++ riotKey,
  "https://euw1.api.riotgames.com/lol/match/v3/matches/3538759617?api_key=" ++ riotKey ,
  "https://euw1.api.riotgames.com/lol/match/v3/matches/3538685683?api_key=" ++ riotKey]) (createMatchUrl [3529427714,3538759617,3538685683]))


tests = TestList[TestLabel "logPredTest" logPredTest,
                 TestLabel "updateTest" updateTest,
                 TestLabel "mainPredTest" mainPredTest,
                 TestLabel "totalPredTest" totalPredTest,
                 TestLabel "convertTest " convertTest ,
                 TestLabel "correctTest" correctTest,
                 TestLabel "accuracyTest" accuracyTest,
                 TestLabel "mainFunc" mainFunc,
                 TestLabel "ttdTest" ttdTest,
                 TestLabel "compTest" compTest,
                 TestLabel "getStatsTest" getStatsTest,
                 TestLabel "statAuxTest" statAuxTest,
                 TestLabel "toPlayerTest" toPlayerTest,
                 TestLabel "champWinLossTest" champWinLossTest,
                 TestLabel "gatherMatchIds" test1,
                 TestLabel "createMatchUrl" test2
                 ]
