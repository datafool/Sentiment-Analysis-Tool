library(data.table)
library(reshape2)
library(Matrix)
library(nnet)
rm(list = ls())
gc()
#trainingData_x <- copy(trainingmatrix_X)
#trainingData_y <- copy(trainingmatrix_y)
#rm(trainingmatrix_X, trainingmatrix_y)
#gc()
setwd("D:/self/Kaggle")
## Functions 
## Implementing prediction 
predict <- function(x, theta1, theta2)
{
  z1 <- x%*%t(theta1)
  a2 <- 1/(1+exp(-z1)) 
  a2 <- cBind(rep(1,nrow(a2)),a2)
  z2 <- a2%*%t(theta2)
  h <- 1/(1+ exp(-z2))
  predict = max.col(h)
  return(predict)
}


  

### Reading Data
load("./trainingMatrix_X.Rda")
load("./trainingMatrix_y.Rda")
trainingData_X <- copy(trainingmatrix_X)
trainingData_y <- copy(trainingmatrix_y)
rm(trainingmatrix_X,trainingmatrix_y)
gc()
#load("./trainingData_X.Rda")
#load("./trainingData_y.Rda")
m <- dim(trainingData_X)[1]
n <- dim(trainingData_X)[2]
trainingData_y <- trainingData_y + 1
train_y <- matrix(rep(0,m*2), nrow=m)
for (i in 1:m)
{
  if(trainingData_y[i] == 3){
  train_y[i,1] =1  
  trainingData_y[i] = 1
  }else{ train_y[i,2]  = 1
  trainingData_y[i] =2}
  #train_y[i,trainingData_y[i]]= 1

}  


#Delta1 = rep(0,nrow(theta1)*ncol(theta1), nrow = nrow(theta1))

## Loading initial value of parameters
#theta1 <- as.matrix(read.csv("./theta1.csv", stringsAsFactors = F, header = F))
#theta2 <- as.matrix(read.csv("./theta2.csv", stringsAsFactors = F, header = F))
hiddenLayerUnit = 299
inputLayerUnit = dim(trainingData_X)[2]
epsilonInit1 = sqrt(6)/(sqrt(hiddenLayerUnit + inputLayerUnit))
num_lables = 2
epsilonInit2 = sqrt(6)/sqrt(hiddenLayerUnit + num_lables)
set.seed(hiddenLayerUnit*(n+1))
theta1 <- matrix(runif(hiddenLayerUnit*(inputLayerUnit+1)), nrow = hiddenLayerUnit)*epsilonInit1 - epsilonInit1
theta1_v <- as.vector(theta1)

#0.000001
set.seed(num_lables*(hiddenLayerUnit+1))
theta2 <- matrix(runif(num_lables*(hiddenLayerUnit + 1)), nrow = num_lables)*epsilonInit2 - epsilonInit2
theta2_v <- as.vector(theta2)
theta_v <- c(theta1_v, theta2_v)
#*0.000001

### Implementing forward propogation
x <- cBind(matrix(rep(1,m)), trainingData_X)
#ir1 <- nnet(trainingData_X[1:100,],train_y[1:100,], size = 1, rang = 0.1, decay = 5e-4, maxit = 200)
rm(trainingData_X)
gc()
cost <- c(rep(0, 150000))
a1 = x
#for(i in 1:1500)
i <- 1
j <- 1
alpha <- 0.00001
predAccuracy <- 0
while(predAccuracy < 0.97 & i < 5000)
   {
    z1 <- x%*%t(theta1)
    a2 <- 1/(1+exp(-z1))
    a2 <- cBind(rep(1,nrow(a2)),a2)
    z2 <- a2%*%t(theta2)
    h <- 1/(1+ exp(-z2))
    rm(z1,z2)
    gc()
    
    ### Calculating Cost for all training example
    cost[i] <- (1/m)*(sum(-train_y*log(h) - (1-train_y)*log(1-h)))
    
        
    if(i!=1){
     if(cost[i-1] - cost[i] < 0.01) {
       if(j < 500){
         alpha <- alpha*5/i
         j <- j +1
       }
       else{break}
     } 
}
    print("cost")
    print(cost[i])
    i <- i + 1
    print("i")
    print(i)
    print("j")
    print(j)
    
    ### Gradient Computation using back propogation
    
    delta3 <- h - train_y
    delta2 <- delta3%*%theta2*(a2*(1-a2))
    Delta2 <- (t(delta3)%*%a2)/m
    Delta1 <- (t(delta2[,2:ncol(delta2)])%*%a1)/m
    theta1 <- theta1 - alpha*Delta1
    theta2 <- theta2 - alpha*Delta2
    
    pred   <- predict(x,theta1, theta2)
    predAccuracy <-mean(pred == trainingData_y)
    print("Accuracy")
    print(predAccuracy)
    gc()
    alpha <- alpha/i;
}



#write.csv(predict, file = "./predict.csv", row.names =F)

