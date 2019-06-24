rm(list = ls(all=T))

setwd("E:/Documents/Practice")

set.seed(2019)

library(dplyr)
library(readxl)
library(randomForest)
library(MLmetrics)
library(tseries)

df <- as.data.frame(read_excel('AirQualityUCI.xlsx'))

for(i in 3:15){
  df[which(df[,i] == -200),i] <- mean(df[setdiff(1:nrow(df),which(df[,i] == -200)),i])
}

train_df <- df[1:8500,]

test_df <- df[8501:nrow(df),]

tree <- 50

rf_orig <- randomForest(train_df[,4:15],train_df[,3],ntree=tree)

print(rf_orig$importance)

MLmetrics::R2_Score(predict(rf_orig,test_df[,4:15]),test_df[,3])  ## 83.4%

print(MLmetrics::R2_Score(predict(rf_orig,train_df[,4:15]),train_df[,3])) ## 96.8%

block <- 25  ## can be bigger

crow <- nrow(train_df); row_test <- nrow(test_df)

predict_train <- as.data.frame(matrix(rep(crow*tree,1),crow,tree))
predict_test <- as.data.frame(matrix(rep(row_test*tree,1),row_test,tree))

for(i in 1:tree){
  train_new <- as.data.frame(matrix(rep(0,13),1,13))
  train_new <- train_new[-1,]
  colnames(train_new) <- colnames(train_df[3:15])
  j <- 0
  while(j < crow){
    ind <- as.numeric(sample(1:crow,1,replace = T))
    if(crow-j > block){
      if(ind+block-1 <= crow){
        data <- train_df[seq(ind,ind+block-1),3:15]
        train_new <- rbind(train_new,data)
      }else{
        data <- train_df[seq(ind,crow),3:15]
        train_new <- rbind(train_new,data)
      }
    }else{
      if(ind+crow-j>crow){
        data <- train_df[seq(ind,crow),3:15]
        train_new <- rbind(train_new,data)
      }else{
        data <- train_df[seq(ind,ind+crow-j-1),3:15]
        train_new <- rbind(train_new,data)
      }
    }
    j <- nrow(train_new)
  }
  rf_block <- randomForest(train_new[,2:13],train_new[,1],ntree=1,replace=F)
  predict_train[,i] <- predict(rf_block,train_df[4:15])
  predict_test[,i] <- predict(rf_block,test_df[4:15])
}

train_pred <- apply(predict_train,1,mean)

test_pred <- apply(predict_test,1,mean)

MLmetrics::R2_Score(test_pred,test_df[,3])  ## 84%

print(MLmetrics::R2_Score(train_pred,train_df[,3])) ## 94.2%

