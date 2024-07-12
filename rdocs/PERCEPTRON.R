
### TOPICOS 2 - RECONHECIMENTO DE PADROES
### PERCEPTRON               
### PROFESSOR: GEORGE VON BORRIES


### PACOTES

rm(list=ls())
pkgs = installed.packages()

if (!("mlpack" %in% pkgs)) install.packages("mlpack"); 
library(mlpack)

#### Faiyaz (2019) ####
# Fonte: https://rpubs.com/FaiHas/197581

x1 <- rnorm(50,0,1)
x2 <- rnorm(50,0,1)
x  <- cbind(x1,x2)
y  <- ifelse(x2 > 1.5*x1 + 0.2, +1,-1)
train <- as.data.frame(cbind(x1,x2,y))

library(ggplot2)
ggplot(train, aes(x = x1, y = x2)) + 
  geom_point(aes(colour=as.character(y), 
                 shape=as.character(y)), size = 3) +
  xlab("x1") + 
  ylab("x2") + 
  ggtitle("Teste")

x <- train[,c(1,2)]
y <- train[,3]

head(x)
head(y)

perceptron <- function(x, y, eta, niter) {
  
  # initialize weight vector
  weight <- rep(0, dim(x)[2] + 1)
  errors <- rep(0, niter)
  
  
  # loop over number of epochs niter
  for (jj in 1:niter) {
    
    # loop through training data set
    for (ii in 1:length(y)) {
      
      # Predict binary label using Heaviside activation 
      # function
      z <- sum(weight[2:length(weight)] * 
                 as.numeric(x[ii, ])) + weight[1]
      if(z < 0) {
        ypred <- -1
      } else {
        ypred <- 1
      }
      
      # Change weight - the formula doesn't do anything 
      # if the predicted value is correct
      weightdiff <- eta * (y[ii] - ypred) * 
        c(1, as.numeric(x[ii, ]))
      weight <- weight + weightdiff
      
      # Update error function
      if ((y[ii] - ypred) != 0.0) {
        errors[jj] <- errors[jj] + 1
      }
      
    }
  }
  
  # weight to decide between the two species 
  print(weight)
  return(errors)
}

err <- perceptron(x, y, 0.01, 20)
err 

plot(1:10, err[1:10], type="l", lwd=2, 
     col="red", xlab="epoch #", ylab="errors")
title("Errors vs epoch - learning rate eta = 1")


#### mlpack::perceptron() ####


xm <- as.matrix(x[1:45,])
ym <- as.matrix(y[1:45])

output <- mlpack::perceptron(training = xm,
                             labels=ym)
perceptron_model <- output$output_model

# Utilizei para teste o mesmo conjunto de treinamento
# Não deve ser feito

xt <- as.matrix(x[46:50,])

output <- perceptron(input_model = perceptron_model,
                     test = xt)

yt <- as.matrix(y[46:50])

cbind(yt,output$predictions)
cbind(yt,
      (round(output$predictions/1e+19,0)-1)*(-1))
