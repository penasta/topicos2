# Tópicos 2

# Prof. George von Borries

# Grupo 4

# Alunos:
# Bruno Gondim Toledo
# Stefan Zurman Gonçalves
# João Pedro Almeida Santos
# João Alberto de Rezende Alvares

# UnB, 1º/2024

# Random Forest

options(repos='http://cran.rstudio.org')
have.packages <- installed.packages()
cran.packages <- c('devtools','plotrix','randomForest','tree')
to.install <- setdiff(cran.packages, have.packages[,1])
if(length(to.install)>0) install.packages(to.install)

library(devtools)
if(!('reprtree' %in% installed.packages())){
  install_github('munoztd0/reprtree')
}
for(p in c(cran.packages, 'reprtree')) eval(substitute(library(pkg), list(pkg=p)))

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, randomForest,randomForest,reprtree,
               reshape2,latex2exp,caret,e1071,VGAM)

data <- iris

data$Species <- as.factor(data$Species)

set.seed(150167636)
ind <- sample(2, nrow(data), replace = TRUE,
              prob = c(0.7, 0.3))
train <- data[ind==1,]
test <- data[ind==2,]

rf <- randomForest(Species~., data=train, proximity=TRUE)

rf

p1 <- predict(rf, train)
confusionMatrix(p1, train$ Species)

p2 <- predict(rf, test)
confusionMatrix(p2, test$ Species)

t <- tuneRF(train[,-5], train[,5],
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 150,
            trace = TRUE,
            improve = 0.05)

hist(treesize(rf),
     main = "No. of Nodes for the Trees",
     col = "green")

varImpPlot(rf,
           sort = T,
           n.var = 10,
           main = "Top 10 - Variable Importance")

importance(rf)

partialPlot(rf, train, Petal.Width, "setosa")

MDSplot(rf, train$Species)

# Python:

{
# library(reticulate)
# import pandas as pd
# import numpy as np
# from sklearn.model_selection import train_test_split
# from sklearn.metrics import confusion_matrix
# from sklearn.ensemble import RandomForestClassifier
# from sklearn import datasets
# 
# iris = datasets.load_iris()
# dados = pd.DataFrame(data=iris.data, columns=iris.feature_names)
# 
# X = dados
# y = iris.target
# X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3)
# 
# model = RandomForestClassifier()
# model.fit(X_train, y_train)
# y_pred = model.predict(X_test)
# confusion_matrix(y_test, y_pred)
}

data = read.csv('https://raw.githubusercontent.com/penasta/topicos2/main/arquivos/data.csv', header = TRUE)

set.seed(123)
splitIndex <- createDataPartition(data$class, p = 0.8, list = FALSE)

train_data <- data[splitIndex, ]  %>% dplyr::select(-`ID`)
test_data <- data[-splitIndex, ]  %>% dplyr::select(-`ID`)

train_data$class <- as.factor(train_data$class)
test_data$class <- as.factor(test_data$class)

set.seed(123)

rf_model <- randomForest(
  class ~ ., data = train_data, 
  ntree = 500, 
  importance = TRUE
)

# Printa a matriz de confusão
print(rf_model$confusion)

# Prever com o modelo
predictions <- predict(rf_model, test_data)

# Avaliar a precisão
accuracy <- mean(predictions == test_data$class)
print(paste0("Test error:", round((1-accuracy)*100,2),"%"))

# Definir os valores de mtry que queremos avaliar
mtry_values <- seq(1, ncol(train_data) - 1)  # de 1 até o número de variáveis - 1

# Vetor para armazenar as acurácias
accuracy <- numeric(length(mtry_values))

# Loop para ajustar o modelo e calcular a acurácia para cada valor de mtry
for (i in seq_along(mtry_values)) {
  rf_model <- randomForest(class ~ ., data = train_data, mtry = mtry_values[i], ntree = 500, importance = TRUE)
  predictions <- predict(rf_model, test_data)
  accuracy[i] <- mean(predictions == test_data$class)
}

# Criar um dataframe com os resultados
results <- data.frame(mtry = mtry_values, accuracy = accuracy)

# Identificar o índice do ponto com maior acurácia
best_index <- which.max(results$accuracy)
max_accuracy <- max(results$accuracy)
sqrt_accuracy <- results[round(sqrt(450)),]$accuracy
sqrt_index <- results[round(sqrt(450)),]$mtry

# Plotar a acurácia em função de mtry
ggplot(results, aes(x = mtry, y = accuracy)) +
  geom_smooth() +
  geom_point(alpha = 0.1) +
  geom_point(data = results[best_index, ], aes(x = mtry, y = accuracy), color = "red", size = 3) +
  geom_point(data = results[sqrt_index, ], aes(x = mtry, y = accuracy), color = "blue", size = 3) +
  annotate("text", x = best_index+100 , y = max_accuracy+0.03, label = "Maior Acurácia", color = "red", hjust = 1.5) +
  annotate("text", x = sqrt_index+180 , y = sqrt_accuracy+0.02, label = TeX("Acurácia com  $p = \\sqrt{m}$"), color = "blue", hjust = 1.5) +
  labs(title = "Acurácia no banco de teste em função de p",
       x = "Número de Variáveis (p) em cada divisão",
       y = "Acurácia") +
  ylim(.7,1) +
  theme_minimal()

# Definir os valores de ntree que queremos avaliar
ntree_values <- c(1, 50, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)

# Vetor para armazenar as acurácias
accuracy <- numeric(length(ntree_values))

# Loop para ajustar o modelo e calcular a acurácia para cada valor de ntree
for (i in seq_along(ntree_values)) {
  rf_model <- randomForest(class ~ ., data = train_data, ntree = ntree_values[i])
  predictions <- predict(rf_model, test_data)
  accuracy[i] <- mean(predictions == test_data$class)
}

# Criar um dataframe com os resultados
results <- data.frame(ntree = ntree_values, accuracy = accuracy)

# Plotar a acurácia em função de ntree
ggplot(results, aes(x = ntree, y = accuracy)) +
  geom_line() +
  geom_point() +
  labs(title = "Acurácia do Random Forest em função de ntree",
       x = "Número de Árvores (ntree)",
       y = "Acurácia") +
  theme_minimal() +
  ylim(0,1)

iris <- iris %>%
  mutate(cor = ifelse(Species == "setosa",1,ifelse(Species == "versicolor",2,3)))

set.seed(150167636)
ind <- sample(2, nrow(iris), replace = TRUE,
              prob = c(0.7, 0.3))
train <- iris[ind==1,]
test <- iris[ind==2,]

i=4 #Número de pontos contaminados
dadosPoluidos1 <- train[train$cor==1,]
dadosPoluidos1 <- dadosPoluidos1[sample(1:nrow(dadosPoluidos1),i,replace = F),]

dadosPoluidos2 <- train[train$cor==3,]
dadosPoluidos2 <- dadosPoluidos2[sample(1:nrow(dadosPoluidos2),i,replace = F),]

dadosPoluidos1$Petal.Length <- dadosPoluidos1$Petal.Length + 5
dadosPoluidos1$Petal.Width <- dadosPoluidos1$Petal.Width + 1.7

dadosPoluidos2$Petal.Length <- dadosPoluidos2$Petal.Length - 4
dadosPoluidos2$Petal.Width <- dadosPoluidos2$Petal.Width - 1.2

DadosExempOutTreino <- rbind(train,dadosPoluidos1,dadosPoluidos2)
#DadosExempOutTreino <- DadosExempOutTreino[,-6]

ggplot(train) +
  aes(x = Petal.Length, y =Petal.Width) +
  geom_point(colour = train$cor, size = 3) +
  labs(
    x = "Petal.Length",
    y = "Petal.Width"
  ) +
  theme_minimal()

ggplot(DadosExempOutTreino) +
  aes(x = Petal.Length, y =Petal.Width) +
  geom_point(colour = DadosExempOutTreino$cor, size = 3) +
  labs(
    x = "Petal.Length",
    y = "Petal.Width"
  ) +
  theme_minimal()

DadosExempOutTreino <- DadosExempOutTreino[,-6]
rm(dadosPoluidos1,dadosPoluidos2,train,ind)

#Árvore de decisão
Outliersrf <- randomForest(Species~., data=DadosExempOutTreino, proximity=TRUE)
PredOutliersrf <- predict(Outliersrf, test)
confusionMatrix(PredOutliersrf, test$Species)

#Reg Logística
OutliersRegLog <- VGAM::vglm(Species ~., family=VGAM::multinomial(refLevel="setosa"), 
                             data=DadosExempOutTreino) 
PredOutliersRegLog <- predict(OutliersRegLog,type="response",newdata = test)
PredOutliersRegLog <- ifelse(PredOutliersRegLog[,1]>PredOutliersRegLog[,2] &
                               PredOutliersRegLog[,1]>PredOutliersRegLog[,3],"setosa",
                             ifelse(PredOutliersRegLog[,2]>PredOutliersRegLog[,1] &
                                      PredOutliersRegLog[,2]>PredOutliersRegLog[,3],"versicolor",
                                    "virginica"))
PredOutliersRegLog <- factor(PredOutliersRegLog)
confusionMatrix(PredOutliersRegLog, test$Species)

#SVM
#linear
OutliersSVMlin <- svm(Species~., data=DadosExempOutTreino, kernel="linear")
PredOutliersSVMlin <- predict(OutliersSVMlin, test)
confusionMatrix(PredOutliersSVMlin, test$Species)

#SVM
#radial
OutliersSVMrad <- e1071::svm(Species~., data=DadosExempOutTreino, kernel="radial")
PredOutliersSVMrad <- predict(OutliersSVMrad, test)
confusionMatrix(PredOutliersSVMrad, test$Species)

Robust <- function(rep,ncont){
  AccRF <- numeric()
  AccSVMLin <- numeric()
  AccSVMRad <- numeric()
  AccRegLog <- numeric()
  for (j in 1:rep){
    ind <- sample(2, nrow(iris), replace = TRUE,
                  prob = c(0.7, 0.3))
    trainRobust <- iris[ind==1,]
    testRobust <- iris[ind==2,]
    
    
    trainRobust <- trainRobust %>%
      mutate(cor = ifelse(Species == "setosa",1,ifelse(Species == "versicolor",2,3)))
    
    i=ncont #Número de pontos contaminados
    dadosPoluidos1 <- trainRobust[trainRobust$cor==1,]
    dadosPoluidos1 <- dadosPoluidos1[sample(1:nrow(dadosPoluidos1),i,replace = F),]
    
    dadosPoluidos2 <- trainRobust[trainRobust$cor==3,]
    dadosPoluidos2 <- dadosPoluidos2[sample(1:nrow(dadosPoluidos2),i,replace = F),]
    
    dadosPoluidos1$Petal.Length <- dadosPoluidos1$Petal.Length + 5
    dadosPoluidos1$Petal.Width <- dadosPoluidos1$Petal.Width + 1.7
    
    dadosPoluidos2$Petal.Length <- dadosPoluidos2$Petal.Length - 4
    dadosPoluidos2$Petal.Width <- dadosPoluidos2$Petal.Width - 1.2
    
    DadosExempOutTreino <- rbind(trainRobust,dadosPoluidos1,dadosPoluidos2)
    
    DadosExempOutTreino <- DadosExempOutTreino[,-6]
    rm(dadosPoluidos1,dadosPoluidos2,trainRobust,ind)
    
    #Árvore de decisão
    Outliersrf <- randomForest(Species~., data=DadosExempOutTreino, proximity=TRUE)
    PredOutliersrf <- predict(Outliersrf, testRobust)
    AccRF[j] <- confusionMatrix(PredOutliersrf, testRobust$Species)$overall[1]
    
    #SVM
    #linear
    OutliersSVMlin <- svm(Species~., data=DadosExempOutTreino, kernel="linear")
    PredOutliersSVMlin <- predict(OutliersSVMlin, testRobust)
    AccSVMLin[j] <- confusionMatrix(PredOutliersSVMlin, testRobust$Species)$overall[1]
    
    #radial
    OutliersSVMrad <- svm(Species~., data=DadosExempOutTreino, kernel="radial")
    PredOutliersSVMrad <- predict(OutliersSVMrad, testRobust)
    AccSVMRad[j] <- confusionMatrix(PredOutliersSVMrad, testRobust$Species)$overall[1]
    
    #Reg Logística
    OutliersRegLog <- vglm(Species ~., family=multinomial(refLevel="setosa"), 
                           data=DadosExempOutTreino) 
    PredOutliersRegLog <- predict(OutliersRegLog,type="response",newdata = testRobust)
    PredOutliersRegLog <- ifelse(PredOutliersRegLog[,1]>PredOutliersRegLog[,2] &
                                   PredOutliersRegLog[,1]>PredOutliersRegLog[,3],"setosa",
                                 ifelse(PredOutliersRegLog[,2]>PredOutliersRegLog[,1] &
                                          PredOutliersRegLog[,2]>PredOutliersRegLog[,3],"versicolor",
                                        "virginica"))
    PredOutliersRegLog <- factor(PredOutliersRegLog)
    AccRegLog[j] <- confusionMatrix(PredOutliersRegLog, testRobust$Species)$overall[1]
  }
  return(data.frame(AccRF,AccSVMLin,AccSVMRad,AccRegLog))
}

Simulações0 <- Robust(rep = 100,ncont = 0)

boxplot(Simulações0)

Simulações2 <- Robust(rep = 100,ncont = 2)

boxplot(Simulações2)

Simulações4 <- Robust(rep = 100,ncont = 4)

boxplot(Simulações4)

Simulações6 <- Robust(rep = 100,ncont = 6)

boxplot(Simulações6)

Simulações8 <- Robust(rep = 100,ncont = 8)

boxplot(Simulações8)
