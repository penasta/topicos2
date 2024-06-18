if (!require("pacman")) install.packages("pacman")
p_load(klaR,janitor,knitr,caTools,
       ks,MASS)

# 16 ----

# Definir uma seed para as amostras
set.seed(150167636)

# Definir tamanho da amostra
samp <- 200 

# Definindo a matriz dos vetores de médias das normais
mus <- rbind(c(-2,2), c(0,0), c(2,-2))

# Definindo as matrizes de variância-covariância
Sigmas <- rbind(diag(2), matrix(c(0.8, -0.72, -0.72, 0.8), nrow=2), diag(2)) 

# Definindo as proporções das misturas
cwt <- 3/11
props <- c((1-cwt)/2, cwt, (1-cwt)/2)

# Gerando a amostra de uma mistura de normais com parâmetros definidos anteriormente
x <- rmvnorm.mixt(n=samp, mus=mus, Sigmas=Sigmas, props=props) 

ggplot(as.data.frame(x), aes(x=V1, y=V2))+
  geom_point(alpha = .5) +
  geom_density_2d()+
  theme_classic()+
  ylim(-5,5.5)+
  xlim(-5,5.5)

Hpi1 <- Hpi(x=x)
Hpi2 <- Hpi.diag(x=x)

fhat.pi1 <- kde(x=x, H=Hpi1) # Estima a densidade Kernel
fhat.pi2 <- kde(x=x, H=Hpi2)

par(mfrow = c(1, 2))
plot(fhat.pi1,main="Parâmetro irrestrito")
plot(fhat.pi2,main="Parâmetro de matriz diagonal")

Hscv1 = Hscv(x=x)
Hscv2 = Hscv.diag(x=x)

fhat.pi3 <- kde(x=x, H=Hscv1)
fhat.pi4 <- kde(x=x, H=Hscv2)
par(mfrow = c(1, 2))
plot(fhat.pi3,main="Parâmetro irrestrito",sub="com validação cruzada")
plot(fhat.pi4,main="Parâmetro de matriz diagonal",sub="com validação cruzada")

# 17 ----
## LDA ----

dados = iris %>% janitor::clean_names()
dados$species = factor(dados$species)

LDA <- lda(species~., data = dados)
LDAp1 <- predict(LDA)
LDAtable1 <- table(dados$species, LDAp1$class)
prop <- (diag(prop.table(LDAtable1,1))) # prop de classif. correta no grupo
propt <- (sum(diag(prop.table(LDAtable1)))) # prop total de classf. correta 

klaR::partimat(species~., data=dados, method="lda", 
         plot.matrix = F, imageplot = T,prec=100)

# Validação hold-out
set.seed(150167636)
split <- sample.split(dados$species, SplitRatio = 0.3) 
train <- subset(dados, split==T)
test <- subset(dados, split==F)

lda1 <- lda(species~., data = train)

PT <- predict(lda1, newdata = test, type = "response")

pred <- LDA |>
  predict(dados)

glctable <- table(dados$species, pred$class)

## QDA ----

dados = iris %>% janitor::clean_names()
dados$species = factor(dados$species)
gqda <- qda(species~., data = dados)

gqdap1 <- predict(gqda)
gqctable1 <- table(dados$species, gqdap1$class)

klaR::partimat(species~., data=dados, method="qda", 
               plot.matrix = F, imageplot = T,prec=100)

# Com validação cruzada
gqdaVC <- qda(species~., data = dados,CV=T)

# Matrizes de confusão:
M <- table(dados$species, gqdap1$class) 
MCV <- table(dados$species, gqdaVC$class) 

# APER e \hat{E}APR:
APER <- (sum(M)-sum(diag(M)))/sum(M) # APER x_1,x_2
E_APR <- (sum(MCV)-sum(diag(MCV)))/sum(MCV) # \hat{E} APR x_1,x_2
