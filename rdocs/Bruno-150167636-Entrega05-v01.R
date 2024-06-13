if (!require("pacman")) install.packages("pacman")
pacman::p_load(ISLR,e1071,
               caTools,ISLR2,
               gridExtra,JuliaCall)
# 7a ----
Auto = Auto
median(Auto$mpg)

Auto$mpg01 = factor(ifelse(Auto$mpg > median(Auto$mpg), 1, 0))

set.seed(150167636) 
sample = sample.split(Auto, SplitRatio = .75)
train = subset(Auto, sample == TRUE)
test  = subset(Auto, sample == FALSE)

x <- subset(test, select = -mpg01)
y <- test$mpg01

# 7b ----
fit1 = svm(mpg01 ~ origin + year + acceleration + weight + horsepower + displacement + cylinders,
           data = train,
           kernel = "linear",
           cost = 1,
           cross = 5,
)

summary(fit1)

pred1 <- predict(fit1, x)
table(pred1, y)

fit2 = svm(mpg01 ~ origin + year + acceleration + weight + horsepower + displacement + cylinders,
           data = train,
           kernel = "linear",
           cost = 2,
           cross = 5,
)

summary(fit2)

pred2 <- predict(fit2, x)
table(pred2, y)

fit3 = svm(mpg01 ~ origin + year + acceleration + weight + horsepower + displacement + cylinders,
           data = train,
           kernel = "linear",
           cost = 10,
           cross = 5,
)

summary(fit3)

pred3 <- predict(fit3, x)
table(pred3, y)

#7c ----

ft = tune.svm(x=Auto[,2:8],y=Auto$mpg01,cost=1:10,gamma=seq(0,5,0.1))
ft$best.parameters


fit4 = svm(mpg01 ~ origin + year + acceleration + weight + horsepower + displacement + cylinders,
           data = train,
           kernel = "polynomial",
           cost = 1,
           cross = 5,
)

summary(fit4)

pred4 <- predict(fit4, x)
table(pred4, y)


fit5 = svm(mpg01 ~ origin + year + acceleration + weight + horsepower + displacement + cylinders,
           data = train,
           kernel = "polynomial",
           gamma = 0.7,
           cost = 2,
           cross = 5,
)

summary(fit5)

pred5 <- predict(fit5, x)
table(pred5, y)


fit6 = svm(mpg01 ~ origin + year + acceleration + weight + horsepower + displacement + cylinders,
           data = train,
           kernel = "radial",
           cost = 1,
           cross = 5,
)

summary(fit6)

pred6 <- predict(fit6, x)
table(pred6, y)


fit7 = svm(mpg01 ~ origin + year + acceleration + weight + horsepower + displacement + cylinders,
           data = train,
           kernel = "radial",
           gamma = 0.7,
           cost = 2,
           cross = 5,
)

summary(fit7)

pred7 <- predict(fit7, x)
table(pred7, y)

# 7d ----
p1 = ggplot(x, aes(x = weight, y = horsepower, color = y)) +
  geom_point() +
  theme_minimal() +
  ggtitle("Real")

p2 = ggplot(x, aes(x = weight, y = horsepower, color = pred1)) +
  geom_point() +
  theme_minimal() + 
  ggtitle("Predito")

grid.arrange(p1, p2, nrow = 1)

p1 = ggplot(x, aes(x = weight, y = horsepower, color = y)) +
  geom_point() +
  theme_minimal() +
  ggtitle("Real")

p2 = ggplot(x, aes(x = weight, y = horsepower, color = pred2)) +
  geom_point() +
  theme_minimal() + 
  ggtitle("Predito")

grid.arrange(p1, p2, nrow = 1)

p1 = ggplot(x, aes(x = weight, y = horsepower, color = y)) +
  geom_point() +
  theme_minimal() +
  ggtitle("Real")

p2 = ggplot(x, aes(x = weight, y = horsepower, color = pred3)) +
  geom_point() +
  theme_minimal() + 
  ggtitle("Predito")

grid.arrange(p1, p2, nrow = 1)

p1 = ggplot(x, aes(x = weight, y = horsepower, color = y)) +
  geom_point() +
  theme_minimal() +
  ggtitle("Real")

p2 = ggplot(x, aes(x = weight, y = horsepower, color = pred4)) +
  geom_point() +
  theme_minimal() + 
  ggtitle("Predito")

grid.arrange(p1, p2, nrow = 1)

p1 = ggplot(x, aes(x = weight, y = horsepower, color = y)) +
  geom_point() +
  theme_minimal() +
  ggtitle("Real")

p2 = ggplot(x, aes(x = weight, y = horsepower, color = pred5)) +
  geom_point() +
  theme_minimal() + 
  ggtitle("Predito")

grid.arrange(p1, p2, nrow = 1)

p1 = ggplot(x, aes(x = weight, y = horsepower, color = y)) +
  geom_point() +
  theme_minimal() +
  ggtitle("Real")

p2 = ggplot(x, aes(x = weight, y = horsepower, color = pred6)) +
  geom_point() +
  theme_minimal() + 
  ggtitle("Predito")

grid.arrange(p1, p2, nrow = 1)

p1 = ggplot(x, aes(x = weight, y = horsepower, color = y)) +
  geom_point() +
  theme_minimal() +
  ggtitle("Real")

p2 = ggplot(x, aes(x = weight, y = horsepower, color = pred7)) +
  geom_point() +
  theme_minimal() + 
  ggtitle("Predito")

grid.arrange(p1, p2, nrow = 1)

# 8a ----
oj = OJ

oj$id = 1:nrow(oj)
set.seed(150167636) 
amostra = sample(1:nrow(oj),800)
train = oj |> filter(id %in% amostra) |> select(-id)
test = oj |> filter(!id %in% amostra) |> select(-id)

x <- subset(test, select = -Purchase)
y <- test$Purchase

# 8b ----
fit8 = svm(Purchase ~ ., data = train, kernel="linear", cost = 0.01)
summary(fit8)

# 8c ----
pred8_1 <- predict(fit8, subset(train, select = -Purchase))
table(pred8_1, train$Purchase)

pred8_2 <- predict(fit8, x)
table(pred8_2, y)

# 8d ----
obj <- tune.svm(Purchase~., data = oj, cost = seq(0.1,10,0.1))

knitr::kable(obj$best.parameters,row.names = FALSE)
plot(obj)

# 8e ----
fit9 = svm(Purchase ~ ., data = train, kernel="linear", cost = 0.7)

pred9_1 <- predict(fit9, subset(train, select = -Purchase))
table(pred9_1, train$Purchase)

pred9_2 <- predict(fit9, x)
table(pred9_2, y)

# 8f ----
fit10 = svm(Purchase ~ ., data = train, kernel="radial", cost = 0.01)

pred10_1 <- predict(fit10, subset(train, select = -Purchase))
table(pred10_1, train$Purchase)

pred10_2 <- predict(fit10, x)
table(pred10_2, y)

fit11 = svm(Purchase ~ ., data = train, kernel="radial", cost = 0.7)

pred11_1 <- predict(fit11, subset(train, select = -Purchase))
table(pred11_1, train$Purchase)

pred11_2 <- predict(fit11, x)
table(pred11_2, y)

# 8g ----
fit12 = svm(Purchase ~ ., data = train, kernel="polynomial",degree = 2, cost = 0.01)

pred12_1 <- predict(fit12, subset(train, select = -Purchase))
table(pred12_1, train$Purchase)

pred12_2 <- predict(fit12, x)
table(pred12_2, y)

fit13 = svm(Purchase ~ ., data = train, kernel="radial",degree = 2, cost = 0.7)

pred13_1 <- predict(fit13, subset(train, select = -Purchase))
table(pred13_1, train$Purchase)

pred13_2 <- predict(fit13, x)
table(pred13_2, y)

# Questão 13 ----
## Kernel linear ----
obj <- tune.svm(Species~., data = iris, cost = seq(0.1,10,0.1))
cost = obj$best.parameters |> pull()


set.seed(150167636) 
sample = sample.split(iris, SplitRatio = .7)
train = subset(iris, sample == TRUE)
test  = subset(iris, sample == FALSE)

x <- subset(test, select = -Species)
y <- test$Species

fit_linear = svm(Species ~ .,
                 data = train,
                 kernel = "linear",
                 cost = cost,
                 cross = 5,
)

summary(fit_linear)

pred <- predict(fit_linear, x)
table(pred, y)

## Kernel não linear ----

obj = tune.svm(Species~., data = iris, cost = seq(0.1,10,0.1),gamma=seq(0,5,0.1))
cost = obj$best.parameters


fit_nl = svm(Species ~ .,
             data = train,
             kernel = "radial",
             cost = cost |> select(cost) |> pull(),
             gamma = cost |> select(gamma) |> pull(),
             cross = 5,
)

summary(fit_nl)

pred <- predict(fit_nl, x)
table(pred, y)

## KERNEL LINEAR EM JULIA: ----
# OBS: O CÓDIGO ABAIXO É UM CÓDIGO JULIA. NÃO IRÁ FUNCIONAR EM R
# COPIE-O E EXECUTE-O EM UM COMPILADOR JULIA

using LIBSVM
using RDatasets
using Printf
using Statistics

iris = dataset("datasets","iris")

X = Matrix(iris[:, 1:4])'

y = iris.Species

Xtrain = X[:, 1:2:end]
Xtest = X[:, 2:2:end]
ytrain = y[1:2:end]
ytest = y[2:2:end]

model = svmtrain(Xtrain, ytrain, kernel = Kernel.Linear, cost = 1.0)

yh , decision_values = svmpredict(model, Xtest);

@printf "Accuracy: %.2f%%\n" mean(yh .== ytest) * 100
