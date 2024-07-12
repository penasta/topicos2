
### TOPICOS 2 - RECONHECIMENTO DE PADROES
### REGRESSAO LOGISTICA               
### PROFESSOR: GEORGE VON BORRIES


### PACOTES

rm(list=ls())
pkgs = installed.packages()

if (!("ggplot2" %in% pkgs)) install.packages("ggplot2"); 
library(ggplot2)
if (!("cowplot" %in% pkgs)) install.packages("cowplot"); 
library(cowplot) # plot_grid
if (!("mdscore" %in% pkgs)) install.packages("mdscore"); 
library(mdscore) # funcao lr.test 
if (!("AICcmodavg" %in% pkgs)) install.packages("AICcmodavg"); 
library(AICcmodavg) # AIC
if (!("questionr" %in% pkgs)) install.packages("questionr"); 
library(questionr) # odds.ratio
if (!("mlpack" %in% pkgs)) install.packages("mlpack"); 
library(mlpack) # discriminante linear logistico


### EXEMPLO: CHALLENGER SPACE SHUTTLE O-RING FAILURES
#### CHALLENGER ####
### ### ### ### ### ### ### ### ### ### ### ### ###  

#### CHALLENGER - DADOS #### 

falhas <- c(2,1,1,1,0,0,0,0,0,0,0,0,1,1,0,0,0,2,0,0,0,0,0, rep(NA,12))	
temperatura <- c(53,57,58,63,66,67,67,67,68,69,70,70,70,70,72,73,75,
                 75,76,76,78,79,80,31,30,35,40,45,50,55,60,65,70,75,80)
orings <- rep(6,length(falhas))
dados <- as.data.frame(cbind(falhas,temperatura,orings))

plot(jitter(falhas,0.01) ~ temperatura, xlab="Temperatura", 
     ylab="Falhas", 
     xlim=c(50,80), data=dados, pch=16,
     yaxt="n")
axis(side=2, at = seq(0,2,1), las=2)


#### CHALLENGER - RL SEM ZEROS ####
#  ANALISE INCORRETA 1 - REGRESSAO LINEAR SEM ZEROS  

dados1 <- dados[!dados$falhas==0,]

plot(jitter(falhas,0.01) ~ temperatura, xlab="Temperatura", 
     ylab="Falhas", 
     xlim=c(50,80), data=dados1,  pch=16,
     yaxt="n")
axis(side=2, at = seq(1,2,1), las=2)

pfalhas <- dados1$falhas/dados1$orings
dados1[,4] <- pfalhas
names(dados1)[4] <- 'prop.falhas'

mod1 <- lm(prop.falhas~temperatura, data = dados1, 
           na.action = na.omit)
anova(mod1)
summary(mod1)

names(mod1)

(ime1 <- influence.measures(mod1))
(res1 <- mod1$residuals)
(fit1 <- mod1$fitted.values)
(dat1 <- na.omit(dados1))

(ajuste1 <- as.data.frame(cbind(
  Obs=c(1:7),
  Prop.falhas=dat1$prop.falhas,
  Temperatura=dat1$temperatura,
  Residuos=res1,
  Preditos=fit1,
  CooksD=ime1[[1]][,5],
  RStud=rstudent(mod1),
  Leverage=ime1[[1]][,6])))

g1 <- ggplot(ajuste1,aes(Preditos,Residuos)) + 
  geom_point(na.rm = T) + geom_hline(yintercept=0)+ 
  # theme_classic()+
  labs(x = "Valores Preditos", y="Residuos") 
g1

g2 <- ggplot(ajuste1,aes(Preditos,RStud)) + geom_point(na.rm = T) +
  # theme_classic()+
  labs(x = "Valores Ajustados", y="RStudent") + 
  scale_y_continuous(limits=c(-2,3))
g2

g3 <- ggplot(ajuste1,aes(Leverage,RStud)) + geom_point(na.rm = T) +
  # theme_classic()+
  labs(x = "Leverage", y="RStudent") + 
  scale_y_continuous(limits=c(-2,3))
g3

g4 <- ggplot(ajuste1,aes(Preditos,Prop.falhas)) +
  geom_point(na.rm = T) + 
  #theme_classic() +
  labs(x = "Valores Preditos", y="Prop. Falhas") + 
  scale_x_continuous(limits = c(.1,0.5)) +
  scale_y_continuous(limits = c(.1,.5)) +
  geom_abline(slope = 1, intercept = 0)
g4

g5 <- ggplot(ajuste1,aes(Obs,CooksD)) + 
  geom_point(na.rm = T) +
  geom_line(na.rm = T)+ 
  # theme_classic()+ 
  labs(x = "Observacao", y="Distancia de Cook") + 
  scale_x_discrete(limits = c(1,2,3,4,5,6,7)) + 
  scale_y_continuous(limits=c(-1,2))
g5


g6 <- ggplot(ajuste1, aes(sample=Residuos)) + 
  stat_qq() + stat_qq_line() +
  labs(y="qqplot")
g6

plot_grid(g1,g2,g3,g4,g5,g6, ncol=3)


ggplot(ajuste1, aes(Temperatura,Residuos)) + geom_point(na.rm = T) + 
  theme_classic()+labs(x = "Temperatura", y="Residuo") + 
  geom_hline(yintercept = 0)


# Inserindo intervalo de confianca

(varcov1 <- vcov(mod1))
(x <- model.matrix(~ Temperatura,data=ajuste1))
(se.fit1 <- sqrt(diag(x %*% varcov1 %*% t(x))))

ajuste1$lwr <- fit1 - qnorm(0.975)*se.fit1
ajuste1$upr <- fit1 + qnorm(0.975)*se.fit1

ggplot(ajuste1,aes(Temperatura,Preditos)) + geom_line() + 
  scale_y_continuous(limits = c(0,.5)) + theme_classic() +
  geom_point(aes(Temperatura,Prop.falhas)) + 
  geom_ribbon(data=ajuste1,aes(ymin=as.numeric(lwr),
                               ymax=as.numeric(upr)),
              fill= "slateblue",alpha=0.3) + 
  labs(x = "Temperatura", y="Valores Preditos")


#### CHALLENGER - RL COM ZEROS ####
#  ANALISE INCORRETA 2 - REGRESSAO LINEAR COM ZEROS  

dados2 <- dados

(pfalhas2 <- dados2$falhas/dados2$orings)
(dados2[,4] <- pfalhas2)
names(dados2)[4] <- 'prop.falhas'

mod2 <- lm(prop.falhas ~ temperatura, data = dados2)
anova(mod2)
summary(mod2)

names(mod2)

(ime2 <- influence.measures(mod2))
(res2 <- mod2$residuals)
(fit2 <- mod2$fitted.values)
(dat2 <- na.omit(dados2))

ajuste2 <- as.data.frame(cbind(
  Obs=c(1:23),
  Prop.falhas=dat2$prop.falhas,
  Temperatura=dat2$temperatura,
  Residuos=res2,
  Preditos=fit2,
  CooksD=ime2[[1]][,5],
  RStud=rstudent(mod2),
  Leverage=ime2[[1]][,6]))

ajuste2


g7 <- ggplot(ajuste2,aes(Preditos,Residuos)) + 
  geom_point(na.rm = T) + geom_hline(yintercept=0)+ 
  # theme_classic()+
  labs(x = "Valores Preditos", y="Residuos") 
g7

g8 <- ggplot(ajuste2,aes(Preditos,RStud)) + geom_point(na.rm = T) +
  # theme_classic()+
  labs(x = "Valores Ajustados", y="RStudent") + 
  scale_y_continuous(limits=c(-2,3))
g8

g9 <- ggplot(ajuste2,aes(Leverage,RStud)) + geom_point(na.rm = T) +
  # theme_classic()+
  labs(x = "Leverage", y="RStudent") + 
  scale_y_continuous(limits=c(-2,3))
g9

g10 <- ggplot(ajuste2,aes(Preditos,Prop.falhas)) +
  geom_point(na.rm = T) + 
  #theme_classic() +
  labs(x = "Valores Preditos", y="Prop. Falhas") + 
  scale_x_continuous(limits = c(.1,0.5)) +
  scale_y_continuous(limits = c(.1,.5)) +
  geom_abline(slope = 1, intercept = 0)
g10

g11 <- ggplot(ajuste2,aes(Obs,CooksD)) + 
  geom_point(na.rm = T) +
  geom_line(na.rm = T)+ 
  # theme_classic()+ 
  labs(x = "Observacao", y="Distancia de Cook") + 
  scale_x_discrete(limits = c(1,2,3,4,5,6,7)) + 
  scale_y_continuous(limits=c(-1,2))
g11


g12 <- ggplot(ajuste2, aes(sample=Residuos)) + 
  stat_qq() + stat_qq_line() +
  labs(y="qqplot")
g12

plot_grid(g7,g8,g9,g10,g11,g12, ncol=3)


ggplot(ajuste2, aes(Temperatura,Residuos)) + geom_point(na.rm = T) + 
  theme_classic()+labs(x = "Temperatura", y="Residuo") + 
  geom_hline(yintercept = 0)


# Inserindo intervalo de confianca

(varcov2 <- vcov(mod2))
(x <- model.matrix(~ Temperatura,data=ajuste2))
(se.fit2 <- sqrt(diag(x %*% varcov2 %*% t(x))))

ajuste2$lwr <- fit2 - qnorm(0.975)*se.fit2
ajuste2$upr <- fit2 + qnorm(0.975)*se.fit2

ggplot(ajuste2,aes(Temperatura,Preditos)) + geom_line() + 
  scale_y_continuous(limits = c(-.2,.5)) + theme_classic() +
  geom_point(aes(Temperatura,Prop.falhas)) + 
  geom_ribbon(data=ajuste2,aes(ymin=as.numeric(lwr),
                               ymax=as.numeric(upr)),
              fill= "slateblue",alpha=0.3) + 
  labs(x = "Temperatura", y="Valores Preditos")


#### CHALLENGER - RLOG COM ZEROS ####
# ANALISE CORRETA - REGRESSAO LOGISTICA COM ZEROS

(pfalhas <- dados$falhas/dados$orings)
(dados[,4] <- pfalhas)
names(dados)[4] <- 'prop.falhas'
mod3 <- glm(prop.falhas ~ temperatura, 
            family=binomial(link=logit), 
            weights = orings,
            data=dados)
summary(mod3)

# teste verossimilhanca

mod0 <- glm(prop.falhas ~ 1, 
            family=binomial(link=logit),
            weights = orings,
            data=dados)
t <- lr.test(mod0,mod3)


# teste de wald

thetahat <- mod3$coefficients
vcov3 <- vcov(mod3)
LL <- rbind(c(0,1))

WaldTest = function(L,thetahat,Vn,h=0) {
  WaldTest = numeric(3)
  names(WaldTest) = c("W","df","p-value")
  r = dim(L)[1]
  W = t(L%*%thetahat-h) %*% solve(L%*%Vn%*%t(L)) %*%
    (L%*%thetahat-h)
  W = as.numeric(W)
  pval = 1-pchisq(W,r)
  WaldTest[1] = W; WaldTest[2] = r; WaldTest[3] = pval
  WaldTest}


# teste score

(score<-anova(mod0,mod3, test="Rao"))
resultados <- cbind(t$LR,1,t$pvalue)
resultados2 <- matrix(WaldTest(LL,thetahat,vcov3), ncol=3)
resultados3 <- cbind(anova(mod0,mod3, test="Rao")[2,4],
                     anova(mod0,mod3, test="Rao")[2,3],
                     anova(mod0,mod3, test="Rao")[2,6])

testes <- rbind(resultados,resultados2,resultados3)
rownames(testes) <- c("Razao de Verossimilhanca","Wald","Score")
colnames(testes) <- c("Estatistica","GL","P-valor")

testes

coef3 <- summary(mod3)$coefficients

colnames(coef3) <- c("Estimativa","Erro Padrao","Valor Z","Pr(>|z|)")
rownames(coef3) <- c("Intercepto","Temperatura")

coef3

odds.ratio(mod3)[2,]

# Medidas de qualidade de ajuste

medidas3 <- as.data.frame(cbind(mod3$deviance,mod3$aic, BIC(mod3),
                                logLik(mod3)[1],
                                AIC(mod3)))
colnames(medidas3) <- c("Deviance","AIC","BIC","Log Likelihood","AIC")
medidas3

temp.data <- data.frame(dados$temperatura)

predicted.data <- as.data.frame(predict(mod3, newdata = temp.data, 
                                        type="link", se=TRUE))

(new.data <- cbind(temp.data, predicted.data))


new.data$yci <- mod3$family$linkinv(new.data$fit - qnorm(0.975) * new.data$se)
new.data$ycs <- mod3$family$linkinv(new.data$fit + qnorm(0.975) * new.data$se)
new.data$fit <- mod3$family$linkinv(new.data$fit)  # Rescale to 0-1

p <- ggplot(dados, aes(x=temperatura, y=prop.falhas))  
p + geom_point(na.rm = T) + 
  geom_ribbon(data=new.data, aes(y=fit, ymin=yci, ymax=ycs),
              fill="slateblue", alpha=0.3) +
  geom_line(data=new.data, aes(y=fit)) + 
  labs(x="Temperatura", y="Probabilidade") 


#### CHALLENGER - DISC. LINEAR LOGISTICO #### 
#### DLL BINARIO (w1 = 1 e 2, w2 = 0) ####

(dados)
(dadostrainb <- dados[1:23,c(1,2)])
(dadostestb  <- c(30:80))

(dadostrainb[,1] <- ifelse(dadostrainb[,1] > 0, 1, 2)) # 1 = falha, 2 = não falha
## Obs. Manual mlpack tem um erro. As classes devem ser numeradas como
##      inteiros de 1 ao numero de classes.

outlr <- logistic_regression(training = as.matrix(dadostrainb[,2]),
                             labels = as.matrix(dadostrainb[,1]),
                             lambda = 0)

lr_model <- outlr$output_model
output <- logistic_regression(input_model = lr_model,
                              test = as.matrix(dadostestb))
output$predictions
cbind(dadostestb,output$predictions,output$probabilities)

#### DLL BINARIO (w1 = 2, w2 = 0 e 1) ####

(dados)
(dadostrainb <- dados[1:23,c(1,2)])
(dadostestb  <- c(30:80))

(dadostrainb[,1] <- ifelse(dadostrainb[,1] > 1, 1, 2))  # 1 = 2 falhas , 2 = não falha ou 1 falha

outlr <- logistic_regression(training = as.matrix(dadostrainb[,2]),
                             labels = as.matrix(dadostrainb[,1]),
                             lambda = 0)

lr_model <- outlr$output_model
output <- logistic_regression(input_model = lr_model,
                              test = as.matrix(dadostestb))
output$predictions
cbind(dadostestb,output$predictions,output$probabilities)
