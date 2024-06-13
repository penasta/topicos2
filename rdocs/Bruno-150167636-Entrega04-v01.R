pacman::p_load(tidyverse,caret,mlpack,tidymodels,readr,knitr,
               haven,gtsummary,broom,here,funModeling,plotly)
chd <- read_delim("https://github.com/Smolski/livroavancado/raw/master/cdh.csv")
chd$CHD <- factor(chd$CHD)

head(chd)
knitr::kable(summary(chd))

kable(chd |>
        mutate(Grupo = ifelse(CHD == 1, "Apresenta doenca", "Nao apresenta doenca")) |>
        group_by(Grupo) |>
        summarise(`Idade média do grupo` = round(mean(AGE))))

ggplot(chd, aes(x=AGE, y=CHD)) + 
  geom_point() + theme_classic()

m1=glm(CHD~AGE, family = binomial(link="logit"), data = chd)
summary(m1)

IDADE<-chd[,1]  
chd$PRED=predict(m1, newdata=IDADE, type="response")
ggplot(chd, aes(x=AGE, y=PRED)) + 
  geom_point()

ggplot(chd, aes(x=AGE, y=PRED)) + 
  geom_point()+
  geom_vline(xintercept = 47.5, linetype = 2)

fit_glm=glm(has_heart_disease ~ age, data=heart_disease,
            family = binomial(link = 'logit'))
kable(tidy(fit_glm, conf.int = TRUE))

heart_disease %>%
  mutate(
    has_heart_disease_numeric = case_when(
      has_heart_disease == "yes" ~ 1,
      has_heart_disease == "no" ~ 0 ))%>%
  ggplot(aes(y = has_heart_disease_numeric,
             x = age)) +
  geom_point(alpha = .4) +
  labs(y = "Tem a doenca?",
       x = "Idade",
       title = "Doenca cardiaca por idade") +
  theme_classic()

heart_disease$lr_log_odds = predict(fit_glm)
heart_disease$logistic_predictions = predict(fit_glm, type = "response")
heart_disease %>%
  ggplot(aes(x = age,
             y = logistic_predictions)) +
  geom_point() +
  labs(y = "p(doenca)",
       title = "p(doenca) ~ idade") +
  theme_minimal()+
  geom_vline(xintercept = 57.5, linetype = 2)

fails <- c(2, 0, 0, 1, 0, 0, 1, 0, 0, 1, 2, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0)
temp <- c(53, 66, 68, 70, 75, 78, 57, 67, 69, 70, 75,
          79, 58, 67, 70, 72, 76, 80, 63, 67, 70, 73, 76)

challenger <- tibble::tibble(fails, temp)

orings = 6
challenger <- challenger %>%
  dplyr::mutate(resp = fails/orings)

model_fit <- glm(resp ~ temp, 
                 data = challenger, 
                 weights = rep(6, nrow(challenger)),
                 family=binomial(link="logit"))

x_limits <- challenger %>%
  dplyr::summarise(min = 0, max = max(temp)+10)

x <- seq(x_limits[[1]], x_limits[[2]], by=0.5)

temp.data = data.frame(temp = x)

predicted.data <- as.data.frame(
  predict(model_fit, 
          newdata = temp.data, 
          type="link", se=TRUE))

new.data <- cbind(temp.data, predicted.data)

std <- qnorm(0.95 / 2 + 0.5)
new.data$ymin <- model_fit$family$linkinv(new.data$fit - std * new.data$se)
new.data$ymax <- model_fit$family$linkinv(new.data$fit + std * new.data$se)
new.data$fit <- model_fit$family$linkinv(new.data$fit) 

ggplot(challenger, aes(x=temp, y=resp))+ 
  geom_point(colour = "darkblue")+ 
  geom_ribbon(data=new.data, 
              aes(y=fit, ymin=ymin, ymax=ymax), 
              alpha = 0.5, 
              fill = 'lightblue')+
  geom_line(data=new.data, aes(y=fit), colour = "blue") + 
  labs(x="Temperatura", y="Probabilidade de falha estimada")+
  ggtitle("Probabilidades preditas para falha dos Orings com I.C. 95%")+
  theme_classic()+
  theme(panel.border = element_blank(), plot.title = element_text(hjust=0.5))

fit_glm_caret <- train(has_heart_disease ~ age,
                       data = heart_disease,
                       method = "glm",
                       family = binomial(link = 'logit'))
kable(tidy(fit_glm_caret$finalModel, conf.int = TRUE))

heart_disease$lr_log_odds_caret = predict(fit_glm_caret, newdata = heart_disease)
heart_disease$logistic_predictions_caret = predict(fit_glm_caret,
                                                   newdata = heart_disease,
                                                   type = "prob")[,2]
heart_disease %>%
  ggplot(aes(x = age,
             y = logistic_predictions_caret)) +
  geom_point() +
  labs(y = "p(doenca)",
       title = "p(doenca) ~ idade") +
  theme_minimal()+
  geom_vline(xintercept = 57.5, linetype = 2)

heart_disease_mlpack = heart_disease %>%
  mutate(has_heart_disease = ifelse(has_heart_disease == "yes", 2, 1))

index <- sample(1:nrow(heart_disease_mlpack), 0.7*nrow(heart_disease_mlpack))
training_data <- matrix(heart_disease_mlpack[index,]$age)
training_label <- matrix(heart_disease_mlpack[index,]$has_heart_disease)

test_data <- matrix(heart_disease_mlpack[-index,]$age)
test_labels <- matrix(heart_disease_mlpack[-index,]$has_heart_disease)

output <- logistic_regression(training=training_data, labels=training_label,
                              lambda=0.1)
lr_model <- output$output_model

output <- logistic_regression(input_model=lr_model, test=test_data)
predictions <- output$predictions

kable(head(data.frame(predictions,test_labels)))
kable(tail(data.frame(predictions,test_labels)))
kable(table(test_labels == predictions),
      col.names = c("Acertou?","Quantidade"))
