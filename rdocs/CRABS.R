
### TOPICOS 2 - RECONHECIMENTO DE PADROES
### REGRESSAO LOGISTICA               
### PROFESSOR: GEORGE VON BORRIES


### PACOTES

rm(list=ls())
pkgs = installed.packages()

if (!("ggplot2" %in% pkgs)) install.packages("ggplot2"); 
library(ggplot2)
if (!("car" %in% pkgs)) install.packages("car"); 
library(car) 
if (!("lmtest" %in% pkgs)) install.packages("lmtest"); 
library(lmtest) 
if (!("MASS" %in% pkgs)) install.packages("MASS"); 
library(MASS) 
if (!("ResourceSelection" %in% pkgs)) 
  install.packages("ResourceSelection"); 
library(ResourceSelection)


### EXEMPLO: HORSESHOE CRAB DATA 
#### CRABS ####
###   RESPOSTA BINARIA - PREDITORES QUANTITATIVOS
### ### ### ### ### ### ### ### ### ### ### ### ###

#### CRABS - DADOS #### 


dadospath <- c("c:/001-DADOS/cursos/recpad/")

crabs <- read.table(paste0(dadospath,"Crabs.dat"),header = T)
head(crabs, n=5)

### Ajuste

# M0: somente intercepto

M0 <- glm(y ~ 1, family=binomial(link="logit"),
            data=crabs)

plot(jitter(y,0.01) ~ width, xlab="Comprimento", 
     ylab="Satelites Presentes", 
     xlim=c(18,34), data=crabs, pch=16)

curve(predict(M0, data.frame(width=x),type="resp"),
      add=T, col="blue", lwd=2)

plot(jitter(y,0.01) ~ weight, xlab="Peso", 
     ylab="Satelites Presentes", 
     xlim=c(0.5,4.5), data=crabs, pch=16)

curve(predict(M0, data.frame(weight=x),
              type="resp"),
      add=T, col="blue", lwd=2)

# M1: somente comprimento

M1 <- glm(y ~ width, family = binomial, data=crabs)

plot(jitter(y,0.01) ~ width, xlab="Comprimento", 
     ylab="Satelites Presentes", 
     xlim=c(18,34), data=crabs, pch=16)

curve(predict(M1, data.frame(width=x),type="resp"),
      add=T, col="blue", lwd=2)

# M2: somente peso

M2 <- glm(y ~ weight, family = binomial, data=crabs)

plot(jitter(y,0.01) ~ weight, xlab="Peso", 
     ylab="Satelites Presentes", 
     xlim=c(0.5,4.5), data=crabs, pch=16)

curve(predict(M2, data.frame(weight=x),type="resp"),
      add=T, col="blue", lwd=2)

# M3: comprimento e peso

crabs$resp <- factor(crabs$y,
                     levels = c(0,1),
                     labels = c("Ausente",
                                "Presente"),
                             ordered = T)  

cbind(crabs$resp,crabs$y)

ggplot(crabs, aes(width,weight)) +
  geom_point(aes(color = resp)) +
  scale_color_manual(values=c("cyan","darkblue"),
                     name = "Satélites") +
  labs(x = "Comprimento", y="Peso") +
  theme(legend.position = c(0.2,0.7)) 


M3 <- glm(y ~ width + weight, 
            family = binomial, data=crabs)

ggplot(crabs, aes(width,weight)) +
  geom_point(aes(color = predict(M3,
                         type = 'resp'))) +
  scale_color_gradient(low="cyan", 
                       high="darkblue") +
  labs(x = "Comprimento", y="Peso") +
  theme(legend.position = 'none') 

### Analise 1

summary(M1); confint(M1)
summary(M2); confint(M2)
summary(M3); confint(M3)

cbind(-2*logLik(M1),-2*logLik(M2),
      -2*logLik(M3))  # Verossimlihanca

MASS::stepAIC(M3, direction = c("backward")) # padrao
stepAIC(M0, direction = c("forward"),
        scope = list(lower=M0,
                     upper=M3))
stepAIC(M3, direction = c("both"))


### Analise 2

ResourceSelection::hoslem.test(M1$y,M1$fitted.values,g=10)
hoslem.test(M2$y,M2$fitted.values,g=10)
hoslem.test(M3$y,M3$fitted.values,g=10)

# M3 melhor que M0?

lmtest::lrtest(M0,M3) 

# M3 melhor que M1?

-2*(logLik(M1) - logLik(M3))
1-pchisq(1.560777,1)

lrtest(M1,M3)

# M3 melhor que M2?

lrtest(M2,M3)

# M1 melhor que M0?

lrtest(M0,M1) # ou
car::Anova(M1)

# M2 melhor que M0?

lrtest(M0,M2) # ou
car::Anova(M2)

### Analise 3

stats::influence.measures(M1)

