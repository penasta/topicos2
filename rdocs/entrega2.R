pacman::p_load(tidyverse,caret,MASS,gridExtra,car,heplots)

mu1 <- c(1, 0)
mu2 <- c(-1, 0)

sigma <- matrix(c(1, 0,
                  0, 1),2)
df = data.frame(MASS::mvrnorm(100, mu1, sigma))
df = rbind(df,data.frame(MASS::mvrnorm(100, mu2, sigma)))
df$grupo = factor(c(rep(1,100),rep(2,100)))
df

shapiro.test(df$X1)
shapiro.test(df$X2)

x1 = df |> filter(grupo == 1) |> dplyr::select(X1) |> pull()
shapiro.test(x1)
x2 = df |> filter(grupo == 1) |> dplyr::select(X2) |> pull()
shapiro.test(x2)

mu <- t(matrix(c(mean(x1),mean(x2)),1,2))
S <- matrix(c(var(x1),cov(x1,x2),
              cov(x1,x2),var(x2)),2,2)
Sinv <- solve(S)

distancias <- vector("numeric", length(x1))
for (i in 1:length(x1)) {
  xjx <- c(x1[i], x2[i]) - mu
  distancia <- t(xjx) %*% Sinv %*% xjx
  distancias[i] <- distancia
}
#distancias

limite <- qchisq(.5, df = 2) 
prop1 <- sum(distancias < limite)/ length(distancias)

t <- sort(distancias)
car::qqPlot(t, dist="chisq", df=2)
heplots::cqplot(data.frame(x1,x2))

x1 = df |> filter(grupo == 2) |> dplyr::select(X1) |> pull()
shapiro.test(x1)
x2 = df |> filter(grupo == 2) |> dplyr::select(X2) |> pull()
shapiro.test(x2)

mu <- t(matrix(c(mean(x1),mean(x2)),1,2))
S <- matrix(c(var(x1),cov(x1,x2),
              cov(x1,x2),var(x2)),2,2)
Sinv <- solve(S)

distancias <- vector("numeric", length(x1))
for (i in 1:length(x1)) {
  xjx <- c(x1[i], x2[i]) - mu
  distancia <- t(xjx) %*% Sinv %*% xjx
  distancias[i] <- distancia
}
#distancias

prop2 <- sum(distancias < limite)/ length(distancias)

t <- sort(distancias)
car::qqPlot(t, dist="chisq", df=2)
heplots::cqplot(data.frame(x1,x2))

x1 = df |> filter(grupo == 1) |> dplyr::select(X1) |> pull()
x2 = df |> filter(grupo == 1) |> dplyr::select(X2) |> pull()
knitr::kable(MVN::mvn(data.frame(x1,x2))$multivariateNormality)
qqplot(x=x1,y=x2)

x1 = df |> filter(grupo == 2) |> dplyr::select(X1) |> pull()
x2 = df |> filter(grupo == 2) |> dplyr::select(X2) |> pull()
knitr::kable(MVN::mvn(data.frame(x1,x2))$multivariateNormality)
qqplot(x=x1,y=x2)

v = numeric()
p = 2
S = sigma
for (i in 1:200){
  x = c(df$X1[i], df$X2[i])
  v = append(v,(1/(2*pi)^(p/2)*det(S)^(1/20)*exp((-t((x-mu1)) %*% solve(S) %*% (x-mu1))/2)) / (1/(2*pi)^(p/2)*det(S)^(1/20)*exp((-t((x-mu2)) %*% solve(S) %*% (x-mu2))/2)))
}
threshold = quantile(v, probs = 0.5)
df$v = v
df$grupo_NP <- ifelse(df$v > threshold, 1, 2)

df |> mutate(acerto = grupo == grupo_NP) |> summarise(acertos = sum(acerto), porcentagem = acertos/200)

plot1 <- ggplot(df, aes(x = X1, y = X2, color = grupo)) +
  geom_point() +
  scale_color_manual(values = c("blue", "red")) +
  labs(title = "Grupos reais", x = "X1", y = "X2") +
  theme_minimal()
plot2 <- ggplot(df, aes(x = X1, y = X2, color = factor(grupo_NP))) +
  geom_point() +
  scale_color_manual(values = c("blue", "red")) +
  labs(title = "Grupos preditos", x = "X1", y = "X2") +
  theme_minimal()

grid.arrange(plot1, plot2, nrow = 2)

for(i in 1:10){
  x = rnorm(2)
  print(x)
  v = (1/(2*pi)^(p/2)*det(S)^(1/20)*exp((-t((x-mu1)) %*% solve(S) %*% (x-mu1))/2)) / (1/(2*pi)^(p/2)*det(S)^(1/20)*exp((-t((x-mu2)) %*% solve(S) %*% (x-mu2))/2))
  if (v > threshold) {
    print("A coordenada x pertence a Omega_1")
  } else {
    print("A coordenada x pertence a Omega_2")
  }
}

preproc.param <- iris %>% 
  preProcess(method = c("center", "scale"))

iris.transformed <- preproc.param %>%
  predict(iris)

model <- lda(Species~., data = iris.transformed)

lda.data <- cbind(iris.transformed, predict(model)$x)
lda.data$LD1 <- lda.data$LD1 * -1

ggplot(lda.data, aes(LD1, LD2)) +
  geom_point(aes(color = Species)) +
  scale_color_manual(values = c("setosa" = "#1F77B4",
                                "versicolor" = "#FF7F0E",
                                "virginica" = "#2CA02C")) +
  theme_minimal()


# Daqui para baixo, o código está em Julia. Utilizado Julia na versão 1.10.2.

using MultivariateStats, RDatasets, Plots

iris = dataset("datasets", "iris")

X = Matrix(iris[1:end,1:4])'
X_labels = Vector(iris[1:end,5])

lda = fit(MulticlassLDA, X, X_labels; outdim=2)
Ylda = predict(lda, X)

p = plot(size=(400,300))

for s in ["setosa", "versicolor", "virginica"]
    points = Ylda[:,X_labels.==s]
    scatter!(p, points[1,:],points[2,:], label=s, legend=:bottomleft)
end