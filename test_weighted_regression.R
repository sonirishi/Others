rm(list = ls(all=T))

library(car)

data("Prestige")

######## regression normal

lm1 <- lm(income ~ women + education + prestige + census, data = Prestige)

lm2 <- lm(income/education ~ women + prestige + census, data = Prestige)  ## Multiple R-squared:  0.5461

Prestige$education_wt <- Prestige$education/sum(Prestige$education)

lm3 <- lm(income ~ women + prestige + census, data = Prestige, weights = education_wt)  #Multiple R-squared:  0.6283

lm4 <- lm(income ~ women + prestige + census, data = Prestige, weights = (1/education_wt))  #Multiple R-squared:  0.6488

lm5 <- lm(income ~ women + prestige + census, data = Prestige, weights = (1/education_wt)^2)  #Multiple R-squared:  0.6528

######### Test the step wise vs. original regression ############

summary(lm1)  ### R squared - 64.36%

yhat <- rep(0,102)

y <- Prestige$income - yhat

for(i in c("women","education","prestige","census")){
  model <- lm(y~Prestige[,i])
  y <- model$residuals
}

yhat <- Prestige$income - y

R2 <- 1 - (sum((Prestige$income-yhat )^2)/sum((Prestige$income-mean(Prestige$income))^2))

### Full on regression gives higher R2 compared to step wise regression

