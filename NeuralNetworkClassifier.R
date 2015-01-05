## 1. ModifyY() Function to Modify "Y" into matrix which has "0" and "1" 
### ModifyY - this function takes vector Y training data and transforms it this into matrices depedning upon the 
### Number of Lables in Output
### Please note -
###     -  number of lable being 1 is not possible
###     -  if number of lable is two then it will create a vector of 1 and 0, 
###        where 1 denotes one case and other is denoted by 0
###     -  if number of lables are more than two, then we create matrix having as many column as number of lables
###        in this case matrix would be of size (number of rows in y * number of lables)

ModifyY <- function(trainingData_y, numLabels, lableAsOne)
{
    y = matrix(rep(0),nrow= NROW(trainingData_y), ncol = numLabels)
  for (i in 1:dim(x)[1])
  {
    if(numLabels == 2)
    {
      if(trainingData_y[i] == lableAsOne){
        y[i,1] = 1
        trainingData_y[i] = 1                         
      }else{
        y[i,2] = 1
        trainingData_y[i] = 2}
    }else{
      y[i,trainingData_y[i]]= 1  
    }
    
  }
  return(list(y = y, training_y = trainingData_y))
}


##== Functions for Neural Network Based Classifer====
ThetaInitialize <- function(inputLayer, numLabels)
{
  epsilonInit = sqrt(6)/(sqrt(inputLayer + numLabels))
  
  if(numLabels == 2){
    numLabels = 1
  }
  set.seed(numLabels*(inputLayer+1))
  theta1 <- matrix(runif(numLabels*(inputLayer+1)), nrow = numLabels)*2*epsilonInit - epsilonInit
  theta11 <- as.vector(theta1)
  return(theta11)
}

Neuralpredict <- function(x, theta)
{

    theta1Element = hiddenLayer*(inputLayer+1)
    theta2Element = numLabels*(hiddenLayer +1)
    start2 = theta1Element + 1
    end2   = theta1Element + theta2Element
    theta1 <- matrix(theta[1:theta1Element], nrow = hiddenLayer, ncol = (inputLayer + 1))
    theta2 <- matrix(theta[start2: end2], nrow = numLabels, ncol = (hiddenLayer + 1))
    rm(theta)
    gc()
    z1 <- x%*%t(theta1)
    a2 <- 1/(1+exp(-z1)) 
    a2 <- cBind(rep(1,nrow(a2)),a2)
    z2 <- a2%*%t(theta2)
    h  <- 1/(1+ exp(-z2))
    predict = max.col(h)  
  
  
  return(predict)
}


NeuralCost <- function(theta) 
{  
  
    theta1Element = hiddenLayer*(inputLayer+1)
    theta2Element = numLabels*(hiddenLayer +1)
    start2 = theta1Element + 1
    end2   = theta1Element + theta2Element
    theta1 <- matrix(theta[1:theta1Element], nrow = hiddenLayer, ncol = (inputLayer + 1))
    theta2 <- matrix(theta[start2: end2], nrow = numLabels, ncol = (hiddenLayer + 1))
  
  rm(theta)
  gc()
  a1 = x
  
  z1 <- a1%*%t(theta1)
  a2 <- 1/(1+exp(-z1))
  a2 <- cBind(rep(1,nrow(a2)),a2)
  z2 <- a2%*%t(theta2)
  h <- 1/(1+ exp(-z2))
  rm(z1,z2)
  gc()
  
  ### Calculating Cost for all training example
  cost =(1/m)*(sum(-y*log(h) - (1- y)*log(1-h)))
  
  
  ### Gradient Computation using back propogation  
  delta3 <- h - y
  delta2 <- delta3%*%theta2*(a2*(1-a2))
  Delta2 <- (t(delta3)%*%a2)/m
  Delta1 <- (t(delta2[,2:ncol(delta2)])%*%a1)/m
  
  
  grad <<- c(as.vector(Delta1), as.vector(Delta2))
  return(cost)
}

NeuralGrad <- function(theta){
  return(grad)
}





