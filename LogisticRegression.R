###==== Functions to Implement Logistic Regression
### Functions which are present in this scrpit are
## 1. ModifyY()
## 2. LogisticThetaInitialize()
## 3. LogisticCost()
## 4. LogisticGrad()
## 5. LogisticPredict()

## 1. ModifyY() Function to Modify "Y" into matrix which has "0" and "1" 
### ModifyY - this function takes vector Y training data and transforms it this into matrices depedning upon the 
### Number of Lables in Output
### Please note -
###     -  number of lable being 1 is not possible
###     -  if number of lable is two then it will create a vector of 1 and 0, 
###        where 1 denotes one case and other is denoted by 0
###     -  if number of lables are more than two, then we create matrix having as many column as number of lables
###        in this case matrix would be of size (number of rows in y * number of lables)

ModifyY <- function(trainingData_y, numLables, lableAsOne)
{
  for (i in 1:dim(trainingData_y)[1])
  {
    if(numLables == 2)
    {
      if(trainingData_y[i] == lableAsOne){
        y[i] = 1
        trainingData_y[i] = 1                         
      }else{
        y[i] = 0
        trainingData_y[i] = 2}
    }else{
      y[i,trainingData_y[i]]= 1  
    }
    
  }
  return(list(y = y, training_y = trainingData_y))
}



### LogisticThetaInitialize() - Provide initial set of theta which using which we start training Logistic Regression

LogisticThetaInitialize <- function(numLables, epsilon)
{
  if(numLables == 2){
  set.seed((numLables-1)*(n+1))
  theta <- matrix(runif((numLables-1)*(n+1)), nrow = numLables-1, ncol = n+1)*2*epsilon - epsilon
  }else{
    set.seed((numLables)*(n+1))
    theta <- matrix(runif((numLables)*(n+1)), nrow = numLables, ncol = n+1)*2*epsilon - epsilon    
  }
  return(as.vector(theta))
}

### LogisticCost() - implementation of Logistics Regression, this function calculates cost and gradient
###                  cost is returned whcih is used by "optim" function, whereas gradient is made globale variable

LogisticCost <- function(theta)
{
  if(numLables == 2){
  theta <- matrix(theta, nrow = numLables - 1, ncol = n + 1)
  }else{theta <- matrix(theta, nrow = numLables, ncol = n + 1)
  }
  
  z = xTrain%*%t(theta)
  h = 1/(1+exp(-z))
  cost = (1/m)*sum(-yTrain*log(h) - (1-yTrain)*log(1-h))
  LogGrad <<- as.vector((1/m)*(t(h-yTrain)%*%xTrain))
  return(cost)
}

LogisticGrad <- function(theta)
{
  return(LogGrad)
}

LogisticPredict <- function(xTest, theta)
{
  if(numLables == 2){
  theta <- matrix(thetaL, nrow = numLables - 1, ncol = n + 1)
  }else{theta <- matrix(thetaL, nrow = numLables, ncol = n + 1)}
  z = xTest%*%t(theta)
  h = 1/(1+exp(-z))
  predict = as.numeric(h > .5)
  return(predict)
}
