library(data.table)
library(reshape2)
library(Matrix)
library(nnet)

rm(list = ls())
gc()

setwd("D:/self/Kaggle")
source("./NeuralNetworkClassifier.R")
### Reading Data
load("./trainingMatrix_X.Rda")
load("./trainingMatrix_y.Rda")
trainingData_X <- copy(trainingmatrix_X)
trainingData_y <- copy(trainingmatrix_y)
rm(trainingmatrix_X,trainingmatrix_y)
gc()

## Sourcing all the functions


m <<- dim(trainingData_X)[1]
n <<- dim(trainingData_X)[2]

trainingData_y <<- trainingData_y + 1

## Initialize the Neural Network

hiddenLayer <<- 100
inputLayer <<- n
numLabels <<- 2

## Modyfing the data format so as to use in Neural Network

y <<-ModifyY(trainingData_y, numLabels, 3)$y
training_y <-ModifyY(trainingData_y, numLabels, 3)$training_y
x <<- cBind(matrix(rep(1,m)), trainingData_X)
gc()
theta <<- c( ThetaInitialize(inputLayer, hiddenLayer),ThetaInitialize(hiddenLayer, numLabels))

### Step 1 - Training Neural Network for classifying between neutral and non-neutral
yTot = copy(y)
xTot = copy(x)
training_yTot = copy(training_y)
size = m*.6
x = xTot[1:size,]
y = yTot[1:size]
training_y = training_yTot[1:size]
i = 0
m<<- dim(x)[1]
predAccuracy = 0
load("./Theta100HiddenUnit_stage1.Rda")
pred = Neuralpredict(x, theta)
predAccuracy = mean(pred == training_y)
print(predAccuracy)
while(predAccuracy < 0.9 | i < 50){
  par <- optim(theta, NeuralCost, NeuralGrad, method = "CG")$par
  theta <- par
  pred = Neuralpredict(x, theta)
  predAccuracy = mean(pred == training_y)
  rm(par)
  gc()
  i = i + 1
  print(i)
  print(predAccuracy)
}

save(theta, "./Theta100HiddenUnit_stage1_v2.Rda")
load("./Theta100HiddenUnit_stage1.Rda")

predTrain = Neuralpredict(x, theta)
mean(predTrain == training_y)
##===================================== Stage 1 Completed ====================================================




### === 2nd Stage of Neural Network Training=================================
## At this stage we have y values which are 1,2,4,5

hiddenLayer <<- 100
inputLayer <<- n
numLabels <<- 4

theta <<- c( ThetaInitialize(numLabels, hiddenLayer),ThetaInitialize(numLabels, outputLayer))
index = c(seq(1,dim(x)[1]))
yInt = trainingData_y[1:size]
index2 = index[predTrain ==2 & yInt != 3 ]
xInt = x
x <<- xInt[index2,]
yInt2 = yInt[index2]
yInt2[yInt2 == 4] = 3
yInt2[yInt2 == 5] = 4
y <<-ModifyY(yInt2, numLabels, 3)$y
training_y <-ModifyY(yInt2, numLabels, 3)$training_y

rm(predTrain, trainingData_X)
load("./SecondStageTheta100Hidden.Rda")
predTrain = Neuralpredict(x,theta)
predAccuracy = mean(predTrain == training_y)
print(predAccuracy)
gc()
i = 0
m <<- dim(x)[1]
predAccuracy = 0
while(predAccuracy < 0.9){
  par <- optim(theta, NeuralCost, NeuralGrad, method = "CG")$par
  theta <- par
  pred = Neuralpredict(x, theta)
  predAccuracy = mean(pred == training_y)
  rm(par)
  gc()
  i = i + 1
  print(i)
  print(predAccuracy)
}

#### ========= Overall Prediction for entire Training and Testing



rm(predtrain, predTest)
rm(theta)
load("./SecondStageTheta100Hidden.Rda")
predTrain = Neuralpredict(x,theta)
trainAccuracy = mean(predTrain == training_y)
# predTest = predict(xTest,theta)
# testAccuracy = mean(predTest ==yTest)
predTrain[predTrain ==4] = 5
predTrain[predTrain ==3] = 4

yPred = c(rep(3,NROW(yInt)))
yPred[index2] = predTrain
mean(yPred == yInt)
###=== Prediction on Test Data



rm(list = ls())

gc()
load("./TestingMatrix_X.Rda")

trainingData_X <- copy(testingmatrix_X)

rm(testingmatrix_X)
gc()

m <<- dim(trainingData_X)[1]
n <<- dim(trainingData_X)[2]



## Initialize the Neural Network

hiddenLayer <<- 100
inputLayer <<- n
outputLayer <<- 2
outLayer <<-2

## Modyfing the data format so as to use in Neural Network
x <<- cBind(matrix(rep(1,m)), trainingData_X)
xTrain <<- x



### First Step Prediction

load("./Theta100HiddenUnit_stage1.Rda")
predTrain = predict(xTrain,theta)
index = c(seq(1,NROW(xTrain)))

index2 = index[predTrain ==2]

## At this stage we have y values which are 1,2,4,5

hiddenLayer <<- 100
inputLayer <<- n
outputLayer <<- 4
outLayer <<-4
#theta <<- c( ThetaInitialize(inputLayer, hiddenLayer),ThetaInitialize(hiddenLayer, outputLayer))
xTrain2 = xTrain
xTrain = xTrain[predTrain == 2 & xTrain,]





rm(trainingData_y)


rm(predtrain, predTest)
rm(theta)
load("./SecondStageTheta100Hidden.Rda")
predTrain = predict(xTrain,theta)
#trainAccuracy = mean(predTrain == yTrain)
# predTest = predict(xTest,theta)
# testAccuracy = mean(predTest ==yTest)
predTrain[predTrain ==4] = 5
predTrain[predTrain ==3] = 4

yPred = c(rep(3,NROW(xTrain2)))
for(i in 1:NROW(predTrain))
{
  yPred[index2[i]] = predTrain[i]
}

yPred = yPred - 1
