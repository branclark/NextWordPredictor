#setwd("C:/Users/bclar/OneDrive/Documents/Data Science Specialization/10 - Capstone Project")

library(plotly)

nGramWeightData <- read.csv('optimizeWeights.csv')

nGramWeightData <- nGramWeightData[order(-nGramWeightData$score),]

nGramWeightData <- nGramWeightData[which(nGramWeightData$n1 == 0.05),]

plot_ly(nGramWeightData, x = ~n3, y = ~n2, z = ~score, color = ~n4)

