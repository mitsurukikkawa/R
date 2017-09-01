library(quantmod)
library(RSNNS)
library(DMwR)

# Import the csv file
quotes <- read.csv("USDJPY.csv", header=FALSE)

#number of rows in the dataframe
x <-nrow(quotes)

#convert the data frame into an xts object
quotes <- as.ts(quotes)

#convert time series into a zoo object
quotes1 <- as.zoo(quotes)

# lag the data
x1 <- lag(quotes1, k=-1, na.pad=TRUE)
x2 <- lag(quotes1, k=-2, na.pad=TRUE)
x3 <- lag(quotes1, k=-3, na.pad=TRUE)
x4 <- lag(quotes1, k=-4, na.pad=TRUE)
x5 <- lag(quotes1, k=-5, na.pad=TRUE)


#Neural Network for the Close Price Time Series
# combine all the above matrices into one matrix having close prices
CQuotes <- cbind (x1[ ,5], x2[ ,5], x3[ ,5], x4[ ,5], x5[ ,5],quotes1[ ,5])

#scale the data
CQuotes <- scale(CQuotes, center=T, scale=T)

# create data for training
inputs_trainC  <- CQuotes[100:x, 1:5]
outputs_trainC <- CQuotes[100:x, 6]

#build the Elman Neural Network
modelC <- elman (inputs_trainC, outputs_trainC, size =c(20,20), learnFuncParams =c(0.1), maxit =10000)

#create testing data
inputs_testC <- CQuotes[x+1, 1:5]

#make predictions
predC <- predict(modelC , inputs_testC)

CQuotes[x+1,6] <- predC[1]

predC1 <- unscale(CQuotes[x+1, 6], CQuotes)


#Neural Network for the Low Price Time Series

# combine all the above matrices into one matrix having close prices
LQuotes <- cbind (x1[ ,4], x2[ ,4], x3[ ,4], x4[ ,4], x5[ ,4],quotes1[ ,4])

#scale the data
LQuotes <- scale(LQuotes, center=T, scale=T)

# create data for training
inputs_trainL  <- LQuotes[100:x, 1:5]
outputs_trainL <- LQuotes[100:x, 6]

#build the Elman Neural Network
modelL <- elman (inputs_trainL, outputs_trainL, size =c(20,20), learnFuncParams =c(0.1), maxit =10000)

#create testing data
inputs_testL <- LQuotes[x+1, 1:5]

#make predictions
predL <- predict(modelL , inputs_testL)

LQuotes[x+1,6] <- predL[1]

predL1 <- unscale(LQuotes[x+1, 6], LQuotes)


#Neural Network for the High Price Time Series

# combine all the above matrices into one matrix having close prices
HQuotes <- cbind (x1[ ,3], x2[ ,3], x3[ ,3], x4[ ,3], x5[ ,3],quotes1[ ,3])

#scale the data
HQuotes <- scale(HQuotes, center=T, scale=T)

# create data for training
inputs_trainH  <- HQuotes[100:x, 1:5]
outputs_trainH <- HQuotes[100:x, 6]

#build the Elman Neural Network
modelH <- elman (inputs_trainH, outputs_trainH, size =c(20,20), learnFuncParams =c(0.1), maxit =10000)

#create testing data
inputs_testH <- HQuotes[x+1, 1:5]

#make predictions
predH <- predict(modelH , inputs_testH)

HQuotes[x+1,6] <- predH[1]

predH1 <- unscale(HQuotes[x+1, 6], HQuotes)

#show predicted each price
#predH1
#predC1
#predL1

