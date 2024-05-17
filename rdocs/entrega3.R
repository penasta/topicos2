# q 6) ----
# Sobre o gradiente descendente:
"
Kneusel traz uma visão prática do gradiente descendente. Este, seria a implementação computacional para encontrar mínimos de funções, de difícil cálculo analítico.
Trazendo um exemplo unidimensional, traz o cálculo analítico da derivada que minimiza a função. Após, traz uma implementação computacional em Python que converge para o resultado analítico. Logo após, traz a implementação para o caso bidimensional, em que o cálculo das derivadas parciais analiticamente ainda é possível, porém consideravelmente mais complicado. Já computacionalmente, o procedimento não difere muito do caso unidimensional.
Destes, é possível expandir para qualquer situação p-dimensional, onde o cálculo analítico das derivadas é impossível, mas o computacional é trivial.
O autor argumenta que sobre a importância de definir bem o tamanho do passo \eta, também o chamando de 'learning rate',assim como elucida que este não necessariamente precisa ser uma constante. Argumenta ainda que, por mais que este seja importante, o formato da função será ainda mais determinante para o bem funcionamento do algoritmo de gradiente descendente.
Argumenta ainda sobre casos em que existem mínimos locais. Neste caso, o sucesso do algoritmo dependerá do ponto inicial, visto que pode ficar 'preso' em um mínimo local caso passe por um. Entretando, para casos práticos de aprendizado de máquina, existem diversos mínimos locais e parecidos entre sí, portanto, para muitos casos, encontrar um mínimo local será suficiente para endereçar o problema, não necessitando necessariamente atingir o mínimo global.
"

# Implementação do gradiente descendente em Kneusel (2022)
library(tidyverse)
f = function(x){
  return(6*x^2 - 12*x + 3)
}

d = function(x){
  return(12*x - 12)
}

x <- seq(-1, 3, length.out = 1000)

p <- ggplot(data.frame(x), aes(x)) + 
  stat_function(fun = f, geom = "line")

p

x <- -0.9 # Ponto inicial de avaliação da função
eta <- 0.03 # tamanho de cada passo

pontos <- data.frame(x = numeric(), y = numeric())

for(i in 1:15) {
  pontos <- rbind(pontos, data.frame(x = x, y = f(x)))
  x <- x - eta * d(x) # Fórmula de atualização do gradiente descendente
}

p <- p + geom_point(data = pontos, aes(x, y), color = "red")
p

# Mudando ponto inicial; e aumentando tamanho do passo

x <- seq(-1, 3, length.out = 1000)

p <- ggplot(data.frame(x), aes(x)) + 
  stat_function(fun = f, geom = "line") +
  ylim(-3.001, -2.8) + 
  xlim(.8, 1.2)
p
x <- .75 # Ponto inicial de avaliação da função
eta <- 0.15 # tamanho de cada passo

pontos <- data.frame(x = numeric(), y = numeric())

for(i in 1:15) {
  pontos <- rbind(pontos, data.frame(x = x, y = f(x)))
  x <- x - eta * d(x) # Fórmula de atualização do gradiente descendente
}

p <- p + geom_point(data = pontos, aes(x, y), color = "red")
p

"
Gradiente descendente estocástico

O gradiente descendente estocástico é uma variação do gradiente descendente que, ao invés de calcular o gradiente da função em relação a todos os exemplos de treinamento, calcula o gradiente em relação a um 'minibatch' de treinamento por vez. Isso torna o processo de treinamento muito mais rápido, especialmente para conjuntos de dados muito grandes, além de fornecer por vezes estimativas melhores, evitando o algoritmo de cair em mínimos locais
O tamanho deste 'minibatch' é um hiperparâmetro do algoritmo, e deve ser ajustado de acordo com o problema em questão. 
O autor define epoch, ou época, para definir uma passagem completa pelo conjunto de dados de treinamento. Quando separamos o conjunto de dados originais em 'minibatches', o número de amostras dividido pelo tamanho dos minibatches irá determinar o número de minibatches por época. Ou seja, será reamostrado diversas vezes o conjunto de dados original, e cada vez que o conjunto dessas reamostras for formado, contendo todos os dados originais, teremos completado uma época; e isto se repetirá por k épocas.
"

"
Momentos
A ideia dos momentos é de que o gradiente descendente estocástico pode ser melhorado ao considerar a direção e a magnitude dos passos. O autor argumenta que, ao invés de considerar apenas o gradiente da função, podemos considerar um peso relacionado ao gradiente anterior. Este peso é um hiperparâmetro do algoritmo, e deve ser ajustado de acordo com o problema em questão.
Na aplicação, é mais fácil entender a ideia de adicionar este peso. A descida do gradiante é feita em direção ao mínimo local, porém, ao adicionar o peso, o algoritmo considera a direção e a magnitude do passo anterior. Isto é, o algoritmo considera a direção e a magnitude do passo anterior para determinar o passo atual, possibilitando assim 'fugir' de um mínimo local.
Traz também o conceito de 'momento de Nesterov', que é uma variação do momento tradicional, que considera o gradiente da função em um ponto adiantado, e não no ponto atual. Isto é, o algoritmo considera a direção e a magnitude do passo anterior, porém, considera o gradiente da função em um ponto adiantado, possibilitando assim 'fugir' de um mínimo local em menos passos, bem como acertar o mínimo mais rapidamente.

O artigo traz ainda propostas de otimização.
"

#q 7) ----


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

err <- perceptron(x, y, .01, 5)
err 

plot(1:10, err[1:10], type="l", lwd=2, 
     col="red", xlab="epoch #", ylab="errors")
title("Errors vs epoch - learning rate eta = 0.01")


#####
library(ISLR)
Default

ggplot(Default, aes(x = balance, y = income)) + 
  geom_point(aes(colour=as.character(default), 
                 shape=as.character(default)), size = 3) +
  xlab("x1") + 
  ylab("x2") + 
  ggtitle("Teste")
x_treino = Default[,3:4]
y_treino = ifelse(Default[,1]=="No",-1,1)


err <- perceptron(x_treino, y_treino, .01, 100)
err 

plot(1:100, err[1:100], type="l", lwd=2, 
     col="red", xlab="epoch #", ylab="errors")
title("Errors vs epoch - learning rate eta = 0.01")

library(gclus)
data("bank")
bank

x_train = bank[,2:7]
x_train
y_train = bank[,1]
y_train = ifelse(y_train==0,-1,1)
y_train

ggplot(Default, aes(x = balance, y = income)) + 
  geom_point(aes(colour=as.character(default), 
                 shape=as.character(default)), size = 3) +
  xlab("x1") + 
  ylab("x2") + 
  ggtitle("Teste")
x_treino = Default[,3:4]
y_treino = ifelse(Default[,1]=="No",-1,1)


err <- perceptron(x_train, y_train, .01, 10)
err 

plot(1:10, err[1:10], type="l", lwd=2, 
     col="red", xlab="epoch #", ylab="errors")
title("Errors vs epoch - learning rate eta = 0.01")

### MLPACK:
#8) a) ----
# separando os dados 'bank' em treino teste:
set.seed(150167636)
data("bank")
index <- sample(1:nrow(bank), 0.7*nrow(bank))
training_data <- bank[index,]
test_data <- bank[-index,]
training_labels = ifelse(training_data[,1]==0,1,2) # Para o pacote mlpack, os labels devem ser 1 e 2
test_labels = ifelse(test_data[,1]==0,1,2)
training_data = training_data[,2:7]
test_data = test_data[,2:7]
training_labels <- as.matrix(training_labels) # O input dos labels deve ser uma matriz para o pacote
test_labels <- as.matrix(test_labels)
output <- mlpack::perceptron(training=training_data, labels=training_labels)
perceptron_model <- output$output_model
output <- mlpack::perceptron(input_model=perceptron_model, test=test_data)
predictions <- output$predictions
setdiff(test_labels,predictions)

# 8) b) ----
library(rgl)
Random.Unit <-function(n, dim, threshold) {
  points <- runif(n * dim)
  points <- matrix(points, ncol = dim)
  label <- ifelse(apply(points, 1, sum) < threshold, -1, 1)
  return(cbind(label, x0 = rep(1, n), points))
}

Classify <- function(x, weights) {
  return(sign(x %*% weights))
}

Perceptron <- function(data, threshold) {
  w <- c(-threshold, runif(ncol(data) - 2))
  n <- nrow(data)
  label <- data[ , 1]
  obs <- data[ , 2:ncol(data)]
  misclassfied <- TRUE
  while (misclassfied) {
    misclassfied <- FALSE
    for (i in 1:n) {
      if (label[i] * Classify(obs[i , ], w) <= 0) {
        w <- w + label[i] * obs[i , ]
        misclassfied <- TRUE
      }
    }
  }
  return(w)
}

Plot3D <- function(points, a, b, c, d) {
  plot3d(points[, 3:5], xlab = "X", ylab = "Y", zlab = "Z",
         pch = ifelse(points[, 1] == 1, 2, 8),
         col = ifelse(points[, 1] == 1, "blue", "red"))
  planes3d(a, b, c, d)
}

Plot2D <- function(points, a, b) {
  plot(points[, 3:4], xlab = "X", ylab = "Y",
       pch = ifelse(points[, 1] == 1, 2, 8),
       col = ifelse(points[, 1] == 1, "blue", "red"))
  abline(a, b)
}

THRESHOLD <- 1.5
pts <- Random.Unit(1000, 3, THRESHOLD)
Plot3D(pts, 1, 1, 1, -THRESHOLD)
w <- Perceptron(pts, THRESHOLD)
Plot3D(pts, w[4], w[3], w[2], w[1])

THRESHOLD <- 0.75
pts <- Random.Unit(1000, 2, THRESHOLD)
Plot2D(pts, THRESHOLD, -1)
w <- Perceptron(pts, THRESHOLD)
Plot2D(pts, -w[1]/w[3], -w[2]/ w[3])

# 8) c) ----
# OBS: Este é um código Julia, portanto, não irá funcionar aqui no R!
# Copiar e colar num compilador de Julia para observar seu funcionamento.
# É necessário installar os pacotes listados na primeira linha (Após a declaração Using).
# A instalação pode ser feita via terminal utilizando o comando 'using Pkg; Pkg.add("NomeDoPacote")'.
# Versão Julia utilizada: 1.10.3

using Perceptrons, RDatasets, DataFrames, Plots, Random, StatsBase, Random, MLUtils

iris = dataset("datasets", "iris")

iris = iris[iris.Species .!= "virginica", :]

iris.Species = iris.Species .== "setosa"  

iris = iris[shuffle(1:end), :]

train_proportion = 0.7
train_size = Int(floor(train_proportion * nrow(iris)))
train = iris[1:train_size, :]
test = iris[train_size+1:end, :]

X_train = Matrix(train[:, 1:4])
Y_train = Vector(train[:, 5])

X_test = Matrix(test[:, 1:4])
Y_test = Vector(test[:, 5])
Y_train = convert(Array{Float64}, Y_train)

model = Perceptrons.fit(X_train,Y_train,centralize=true,mode="voted")
Y_pred = Perceptrons.predict(model,X_test)

# 9) ----

# 10) ----


