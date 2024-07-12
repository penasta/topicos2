
### TOPICOS 2 - RECONHECIMENTO DE PADROES
### REGRESSAO LOGISTICA               
### PROFESSOR: GEORGE VON BORRIES


### PACOTES

rm(list=ls())
pkgs = installed.packages()

if (!("ggplot2" %in% pkgs)) install.packages("ggplot2"); 
library(ggplot2)
if (!("VGAM" %in% pkgs)) install.packages("VGAM"); 
library(VGAM) 


### EXEMPLO: ALLIGATORS FOOD CHOICE 
#### ALLIGATORS ####
###   RESPOSTA POLITOMICA - PREDITORES QUANTITATIVOS
### ### ### ### ### ### ### ### ### ### ### ### ###

#### ALLIGATORS - DADOS #### 


dadospath <- c("c:/001-DADOS/cursos/recpad/")

Gators <- read.table(paste0(dadospath,"Alligators.dat"),
                     header = T)

head(Gators, n=5)

# pacote para GLMs multivariados, 
# como modelos multinomiais
# vglm = vector GLM

fitO <- vglm(y ~ x, family=multinomial(refLevel="O"), 
             data=Gators) 
coef(fitO, matrix = TRUE)
summary(fitO)

# ou 

fitI <- vglm(y ~ x, family=multinomial(refLevel="I"), 
             data=Gators) 
coef(fitI, matrix = TRUE)
summary(fitI)

# ou 

fitF <- vglm(y ~ x, family=multinomial(refLevel="F"), 
             data=Gators) 
coef(fitF, matrix = TRUE)
summary(fitI)

fitnull <- vglm(y ~ 1, family=multinomial, data=Gators) 
deviance(fitO) 
deviance(fitnull) 

VGAM::lrtest(fitO, fitnull) 

fitted <- fitted(fitO) 
head(fitted)

plot(Gators$x,fitted[,1], 
     main = 'Alligator Food Choice',
     xlab = 'Length',
     ylab = 'Adjusted Prob.', ylim = c(0,1),
     font=2, font.lab=2, pch=20, col='blue',type='l', lwd=1.5)
lines(Gators$x,fitted[,2],type='l',col='red', lwd=1.5)
lines(Gators$x,fitted[,3],type='l',col='darkgreen',lwd=1.5)
text(3.01,0.25,'Choice Other', col='darkgreen')
text(3.14,0.1,'Choice Invertebrates', col='red')
text(3,0.8,'Choice Fish', col='blue')
