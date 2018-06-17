module LogisticRegression where

--All the math functions come from this guide: https://machinelearningmastery.com/logistic-regression-tutorial-for-machine-learning/

{-logPredictor func 

The sigmoid function which will calculate the prediction and output a result between 0 and 1
RETURNS: A number, if func > 0 the number is > 0.5, if func < 0  the number is < 0.5
PRE: TRUE
EXAMPLE: logPredictor (-2.0) == 0.11920292202211755
-}
logPredictor :: Double -> Double
logPredictor func = 1 / (1 + exp (-func))

{-helpFunction b0 b1 x1

Calculates the value which will be the input of the logistic function using weights and chosen parameters
RETURNS: A number calculated by an equation with b0 b1 b2 x1 x2  
PRE: TRUE
EXAMPLE: helpFunction 0.2 0.3 2  == 0.8
-}
helpFunction :: Double -> Double -> Double -> Double
helpFunction b0 b1 x1  = b0 + b1*x1 

{-updater prediction x update0 result
Calculates the new weights based upon old weights, the algorithms prediction and the actual result

PRE: 0 < prediction < 1
RETURNS: A number calculated by an equation with prediction x updater0 and result
EXAMPLE: updater 0.334 (-0.43) 0.5432 1.0 == 0.536829648728
-}
updater :: Double -> Double -> Double -> Double -> Double
updater prediction x update0 result = update0 + 0.4 * (result - prediction ) * prediction * (1 - prediction ) * x

{-mainPredictor list@((l1, result): ls)
The function which calculates the final weights based on the training set
PRE: TRUE
RETURNS: A tuple containing values calculated by inputting l1 and result recursively into the logistic, help and update functions 
EXAMPLE: mainPredictor [(0.1,1.0),(4.0,0.0),((-0.5),1.0),((-2.0),1.0)] == (8.131340328106973e-2,-0.296633933183218)
-}
--VARIANT: length list
mainPredictor :: [(Double,Double)] -> (Double,Double)
mainPredictor list1 = mainPredAux list1 0 0  

mainPredAux :: [(Double,Double)] -> Double -> Double -> (Double,Double)
mainPredAux [] b0 b1 = (b0,b1)
mainPredAux ((wL,result):ls) b0 b1 =
                                        let predict = (logPredictor (helpFunction b0 b1 wL)) 
                                        in mainPredAux ls (updater predict 1 b0 result) (updater predict wL b1 result) 

{-totalPredicts list@((l1,l2):result) b0 b1 
Makes a list of all the predictions in a testset by inputting a test set and the weights from the training 
PRE: TRUE
RETURNS: A list of values calculated by inputting l1 and result recursively into the logistic and help function
EXAMPLE: totalPredicts [(0.1,1.0),(4.0,0.0),((-0.5),1.0),((-2.0),1.0)] (0.233232,0.56533) == [(0.5719386000675216,1.0),(0.9237590113124382,0.0),(0.4876442659589688,1.0),(0.28957933079893683,1.0)]
-}
--VARIANT: length list
totalPredicts :: [(Double,Double)] -> (Double, Double) -> [(Double,Double)]
totalPredicts [] _  = []
totalPredicts ((wL, result):ls) (b0,b1) = ((logPredictor (helpFunction b0 b1 wL), result) : (totalPredicts ls (b0,b1) ))                                                                          

{-converter ((wL, res):xs)
Converts numbers in a list to either 1 or 0 depending on if they are > or < than 0.5
PRE: 0 < x < 1
RETURNS: A List of tuples values where the first value is 1 if wL >= 0.5 and 0 if wL < 0.5
EXAMPLE: converter [(0.2,1.0),(0.5,0.0),(0.342,1.0),(0.999,1.0)] == [(0.0,1.0),(1.0,0.0),(0.0,1.0),(1.0,1.0)]
-}
converter :: [(Double,Double)] -> [(Double,Double)]
converter [] = []
converter ((wL,res):xs) | wL >= 0.5 = ((1,res) : (converter xs))
converter ((wL,res):xs) | wL < 0.5 = ((0,res) : (converter xs)) 

{-correctPredicts pred@((wL, res):predictsT)

Determines how many correct predicts you have
PRE: predicts == 0 or predicts == 1
RETURNS: A value indicating how many values in pred were correct
EXAMPLE: correctPredicts [(1.0,1.0),(0.0,1.0),(0.0,0.0),(1.0,1.0),(1.0,0.0),(0.0,0.0)]  == 4.0
-}
--VARIANT: length pred
correctPredicts :: [(Double,Double)] -> Double
correctPredicts [] = 0.0
correctPredicts ((wl,res):predictsT) | wl == res = 1.0 + correctPredicts predictsT
correctPredicts (predicts:predictsT) | otherwise = correctPredicts predictsT

{-accuracy predictions correct
Gives a procentage of how many of the predicts were correct
PRE: correct > 0
RETURNS: A procentage calculated by predictions and correct
EXAMPLE: accuracy [(1.0,1.0),(0.0,1.0),(0.0,0.0),(1.0,1.0),(1.0,0.0),(0.0,0.0)] 4.0 == 66.66666666666666
-}
accuracy :: [(Double,Double)] -> Double -> Double
accuracy predictions correct = (correct / (fromIntegral(length predictions))) * 100

{-mainFunction l b0 b1 
Calculates the accuracy on a test set with given weights
PRE: l not empty
RETURNS: The accuracy calculated using the weights b0 b1 on the list l
EXAMPLE: mainFunction [(0.1,1.0),(4.0,0.0),((-0.5),1.0),((-2.0),1.0)] (0.233232,0.56533) == 25.0
-}
mainFunction :: [(Double,Double)] -> (Double,Double) -> Double
mainFunction l (b0,b1) = let predictions = (totalPredicts l (b0,b1) ) 
                         in accuracy predictions (correctPredicts (converter predictions))



