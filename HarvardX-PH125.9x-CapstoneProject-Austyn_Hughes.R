#Setting working directory to desktop
setwd("C:/Users/alhughes/OneDrive - USDA/Desktop")

#Installing needed packages
install.packages("tidyverse")
install.packages("Metrics")
install.packages("Car")
library(tidyverse)
library(Metrics)
library(car)

#Importing test data into R. Data from Baseball-Reference, 1961 through he 2020 season.
TestData <- read.csv("MLBData1961to2020.csv")
TestData

#Creating linear model to predict winning percentage (WL.) as related to bWAR.
ModelOne <- lm(WL. ~ bWAR, data = TestData)

#Determining the significance of bWAR as related to WL.
summary(ModelOne)

#Plotting actual winning percentage v ModelOne predicted winning percentage
ModelOnePlot <- plot(ValData$WL., ValData$M1pWL., xlab = "Actual Winning Percentage",ylab = "ModelOne Prediction", main = "Actual v ModelOne")

#Printing above plot
ModelOnePlot

#Creating linear model to predict winning percentage (WL.) as related to pWAR.
ModelTwo <- lm(WL. ~ pWAR, data = TestData)

#Determining the significance of pWAR as related to WL.
summary(ModelTwo)

#Plotting actual winning percentage v ModelOne predicted winning percentage
ModelTwoPlot <- plot(ValData$WL., ValData$M2pWL., xlab = "Actual Winning Percentage", ylab = "ModelTwo Prediction", main = "Actual v ModelTwo")

#Printing above plot
ModelTwoPlot

#Creating a linear model to predict winning percentage (WL.) as related to bWAR and pWAR. 
ModelThree <- lm(WL. ~ bWAR + pWAR, data = TestData)

#Variance inflation factor (VIF) of ModelThree.
vif(ModelThree)

#Determining the significance of bWAR + pWAR as related to WL. 
summary(ModelThree)

#Plotting actual winning percentage v ModelOne predicted winning percentage
ModelThreePlot <- plot(ValData$WL., ValData$M3pWL., xlab = "Actual Winning Percentage", ylab = "ModelThree Prediction", main = "Actual v ModelThree")

#Printing above plot
ModelThreePlot

#Importing validation data into R. Data from Baseball-Reference, 2021 season.
ValData <- read.csv("MLBData2021.csv")
ValData

#Creating a new variable, M1pWL., and pipping it into ValData dataframe. Based on linear model 
#created above (ModelOne).
ValData <- ValData %>% mutate(M1pWL. = 0.38692995 + 0.0059124*bWAR)

#Creating a new variable, M2pWL., and pipping it into ValData dataframe. Based on linear model 
#created above (ModelTwo).
ValData <- ValData %>% mutate(M2pWL. = 0.4278563 + 0.0054142*pWAR)

#Creating a new variable, M3pWL., and pipping it into ValData dataframe. Based on linear model 
#created above (ModelThree).
ValData <- ValData %>% mutate(M3pWL. = 0.3217205 + 0.0057590*bWAR + 0.0051148*pWAR)

#Creating a new variable, M1diffWL., and pipping it into ValData dataframe. This calculates
#the difference between the M1 predicted and acutal winning percentage. 
ValData <- ValData %>% mutate(M1diffWL. = M1pWL. - WL.)

#Creating a new variable, M2diffWL., and pipping it into ValData dataframe. This calculates
#the difference between the M1 predicted and acutal winning percentage. 
ValData <- ValData %>% mutate(M2diffWL. = M2pWL. - WL.)

#Creating a new variable, M3diffWL., and pipping it into ValData dataframe. This calculates
#the difference between the M1 predicted and acutal winning percentage. 
ValData <- ValData %>% mutate(M3diffWL. = M3pWL. - WL.)

#Making sure differences have been added to ValData set.
ValData

#Creating a new variable, pytWL., the standard prediction formula, and pipping it into the ValData dataframe.
ValData <- ValData %>% mutate(pytWL. = (R^1.83)/((R^1.83)+(RA^1.83)))
ValData <- ValData %>% mutate(pytdiffWL. = pytWL. - WL.)
ValData

#Table showing actual winning percentage, models and differences
ValData %>% select('WL.', 'M1pWL.', 'M1diffWL.', 'M2pWL.', 'M2diffWL.', 'M3pWL.', 'M3diffWL.')
ValData

#Plotting using ggplot, for each model. 
ModelOne_ggp <- ggplot(data = ValData, aes(x = WL., y = M1pWL.))+ geom_point()+labs(x = 'Actual Winning Percentage', y = 'Model 1 Predicted Winning Percentage', title = 'Actual vs Model 1 Predicted')+ geom_smooth(method = 'lm')
ModelOne_ggp

ModelTwo_ggp <- ggplot(data = ValData, aes(x = WL., y = M2pWL.))+ geom_point()+labs(x = 'Actual Winning Percentage', y = 'Model 2 Predicted Winning Percentage', title = 'Actual vs Model 2 Predicted')+ geom_smooth(method = 'lm')
ModelTwo_ggp

ModelThree_ggp <- ggplot(data = ValData, aes(x = WL., y = M3pWL.))+ geom_point()+labs(x = 'Actual Winning Percentage', y = 'Model 3 Predicted Winning Percentage', title = 'Actual vs Model 3 Predicted')+ geom_smooth(method = 'lm')
ModelThree_ggp

pytWL._ggp <- ggplot(data = ValData, aes(x = WL., y = pytWL.))+ geom_point()+labs(x = 'Actual Winning Percentage', y = 'Pythagorean Predicted Winning Percentage', title = 'Actual vs Pythagorean Predicted')+ geom_smooth(method = 'lm')
pytWL._ggp

#RMSE for the standard model pytWL and each model created for pWL.
rmse_pytWL. <- rmse(ValData$WL.,ValData$pytWL.)
rmse_ModelOne <- rmse(ValData$WL., ValData$M1pWL.)
rmse_ModelTwo <- rmse(ValData$WL., ValData$M2pWL.)
rmse_ModelThree <- rmse(ValData$WL., ValData$M3pWL.)

#Printing RMSE for pytWL. and the three models.
rmse_pytWL.
rmse_ModelOne
rmse_ModelTwo
rmse_ModelThree
