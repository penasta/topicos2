#8 ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,caret,MASS,gridExtra,car,mlpack,gclus)

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



