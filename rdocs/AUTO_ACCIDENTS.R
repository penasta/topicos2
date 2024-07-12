### TOPICOS 2 - RECONHECIMENTO DE PADROES
### REGRESSAO LOGISTICA               
### PROFESSOR: GEORGE VON BORRIES


### PACOTES

rm(list=ls())
pkgs = installed.packages()

if (!("ggplot2" %in% pkgs)) install.packages("ggplot2"); 
library(ggplot2)
if (!("MASS" %in% pkgs)) install.packages("MASS"); 
library(MASS) 
if (!("car" %in% pkgs)) install.packages("car"); 
library(car) 
if (!("effects" %in% pkgs)) install.packages("effects"); 
library(effects) 
if (!("erer" %in% pkgs)) install.packages("erer"); 
library(erer)
if (!("VGAM" %in% pkgs)) install.packages("VGAM"); 
library(VGAM)


### EXEMPLO: AUTO ACCIDENTS 
#### AGRESTI (2019) - EXERCICIO 6.19 ####
###   RESPOSTA POLITOMICA - CATEGORIAS ORDENADAS
### ### ### ### ### ### ### ### ### ### ### ### ###


### ENTRADA DE DADOS

vec.l <- c(7287,175,720,91,10,
           11587,126,577,48,8,
           3246,73,710,159,31,
           6134,94,564,82,17,
           10381,136,566,96,14,
           10969,83,259, 37,1,
           6123,141,710,188,45,
           6693,74,353,74,12)


#### DADOS NO FORMATO LONGO ####

accidents.l <- data.frame(gender = rep(c("F","M"), each=20), 
                        location = rep(c("urban", "rural"),2,each=10),
                        seatbelt = rep(c("no","yes"),each=5, times=4) ,
                        injury = as.factor(rep( c("i1","i2","i3","i4","i5"),
                                                times=8)), 
                        count = vec.l)

accidents.l

## ESTABELECENDO CATEGORIAS DE REFERENCIA

accidents.l$injury <- factor(accidents.l$injury)
accidents.l$gender <- relevel(factor(accidents.l$gender), "M")
accidents.l$location <- relevel(factor(accidents.l$location), "urban")
accidents.l$seatbelt <- relevel(factor(accidents.l$seatbelt), "yes")

head(accidents.l)

## AJUSTE COM POLR

# library(MASS)

fitpa <- polr(injury ~ seatbelt + location,
              weights = count, Hess = T, 
              method="logistic", data=accidents.l)
summary(fitpa)
exp(cbind(OR = coef(fitpa), confint(fitpa)))


car::Anova(fitpa)

effects::allEffects(fitpa)
plot(allEffects(fitpa))

pnr <- predict(fitpa, data.frame(seatbelt="no",location="rural"), 
        type = "probs")
pyr <- predict(fitpa, data.frame(seatbelt="yes",location="rural"), 
        type = "probs")
pnu <- predict(fitpa, data.frame(seatbelt="no",location="urban"), 
        type = "probs")
pyu <- predict(fitpa, data.frame(seatbelt="yes",location="urban"), 
        type = "probs")

rbind(pnr,pyr,pnu,pyu)

pyr - pnr # efeito de usar o cinto e nao usar em area rural
pyu - pnu # efeito de usar o cinto e nao usar em area urbana


library(erer)
resumo<-erer::ocME(fitpa)
resumo$out

## TESTANDO OUTROS MODELOS

fitpb <- polr(injury ~ seatbelt,
              weights = count, Hess = T, 
              method="logistic", data=accidents.l)

fitpc <- polr(injury ~ location,
              weights = count, Hess = T, 
              method="logistic", data=accidents.l)

fitpd <- polr(injury ~ 1,
              weights = count, Hess = T, 
              method="logistic", data=accidents.l)

fitpe <- polr(injury ~ seatbelt*location,
              weights = count, Hess = T,
              method="logistic", data=accidents.l)

summary(fitpe)

# RAZAO DE VEROSSIMILHANCA

lmtest::lrtest(fitpa,fitpb)
lmtest::lrtest(fitpa,fitpc)
lmtest::lrtest(fitpa,fitpd)

lmtest::lrtest(fitpe,fitpa)

# SELECAO DE VARIAVEIS COM AIC 

fitpt <- polr(injury ~ seatbelt * location * gender,
              weights = count, Hess = T, 
              method="logistic", data=accidents.l)

stepAIC(fitpt, direction = c("backward"))


#### DADOS NO FORMATO CURTO ####

vec.s <- matrix(vec.l,8,5,byrow=TRUE)

accidents.s <- data.frame(gender = rep(c("F","M"),each=2),
                          location = rep(c("urban", "rural"), times = 2),
                          seatbelt = rep(c("no","yes"), times=4),
                          i1 = vec.s[,1], i2 = vec.s[,2], i3 = vec.s[,3],
                          i4 = vec.s[,4], i5 = vec.s[,5])

accidents.s$gender <- relevel(factor(accidents.s$gender), "F")
accidents.s$location <- relevel(factor(accidents.s$location), "rural")
accidents.s$seatbelt <- relevel(factor(accidents.s$seatbelt), "no")

accidents.s

# OBS. note que invertemos as referencias para que os resultados
#      sajam os mesmos dos obtidos com polr


### AJUSTE COM VGLM - NAO CONSEGUE AJUSTAR COM COVARIAVEIS
  # SEATBELT E LOCATION


fitva <- VGAM::vglm(cbind(i1,i2,i3,i4,i5) ~ seatbelt + location, 
              maxit = 100,
              family=cumulative(parallel=TRUE), data=accidents.s)



fitvb <- vglm(cbind(i1,i2,i3,i4,i5) ~ seatbelt, maxit = 100,
              family=cumulative(parallel=TRUE), data=accidents.s)
summary(fitvb)
confint(fitvb)

# comparing with polr

fitpe <- polr(injury ~ seatbelt, weights = count, 
              Hess = T, method="logistic", data=accidents.l)
summary(fitpe)
exp(c(OR = coef(fitpe), confint(fitpe)))
