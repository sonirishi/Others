temp = readMat("C:\\Users\\rsoni106\\Documents\\Work\\Methodology Work\\Seizure Detection\\Data\\sample_clip.mat")

eeg_data = as.data.frame(temp[[1]])

eeg_data_1 = as.numeric(eeg_data[1,])

colnames(eeg_data_1) = NULL

IMF = emd(eeg_data_1, tt=seq(1:400),boundary="wave", sm = "spline", check=FALSE, spar=0.1,
          max.imf=5, plot.imf=FALSE, interm=NULL)()

x = seq(1:400)  

y = eeg_data_1

spars <- seq(0.1,2,length.out=20)          
dat <- data.frame(
  spar= as.factor(rep(spars,each=20)),    
  x = seq(1:400),                          
  y = eeg_data_1)
xyplot(y~x|spar,data =dat, type=c('p'), pch=19,groups=spar,
       panel =function(x,y,groups,...)
       {
         s2  <- smooth.spline(y,spar=spars[panel.number()])
         panel.lines(s2)
         panel.xyplot(x,y,groups,...)
       })

#################### Model Dry run using the variables created in R ####################

data = read.csv("C:/Users/rsoni106/Documents/Work/Methodology Work/Seizure Detection/DataPreparation/Dog_data/eeg_data_R.csv")

colnames(data) = c("event","max_max_imf","max_kurt_imf","max_skew_imf","max_std_imf","std_max_imf",
                   "std_kurt_imf","std_std_imf","std_skew_imf","dfa_mean","dfa_std","dfa_max",
                   "ami_mean","ami_std","ami_max","sam_entr_mean","sam_entr_std","sam_entr_max",
                   "mean_rqa_rec","mean_rqa_entr","mean_rqa_vmean","mean_rqa_ratio","std_rqa_rec",
                   "std_rqa_entr","std_rqa_vmean","std_rqa_ratio")

vars_r = setdiff(colnames(data),"event")

data_matrix = xgb.DMatrix(data = as.matrix(data[,vars_r]), label = as.matrix(data[,"event"]))

set.seed(1234)

model_cv = xgb.cv(data =  data_matrix, nrounds = 1200, nfold = 5, showsd = TRUE,
       eval_metric = "auc", verbose = TRUE, early.stop.round = 5,
       objective = "binary:logistic", max.depth = 8, "eta" = 0.01,                               
       subsample = 0.8, colsample_bytree = 0.85, maximize = T,
       min_child_weight = 6,nthread = 4, alpha = 0.001, gamma = 1.5)

#############Model run full data #####################

data = read.csv("C:/Users/rsoni106/Documents/Work/Methodology Work/Seizure Detection/DataPreparation/Dog_data/final_eeg_data_nokalman_lyp.csv")

col_list = c("max_power_b1","median_power_b1","max_power_b2","median_power_b2","max_power_b3",
         "median_power_b3","max_power_bd1","median_power_bd1","max_power_bd2","median_power_bd2","max_power_bd3","median_power_bd3",
         "mean_power_b1","mean_power_bd1","mean_power_b3","min_power_b1","std_power_b1","min_power_b2","std_power_b2","min_power_b3",
         "std_power_b3","min_power_bd1","std_power_bd1","min_power_bd2","std_power_bd2","min_power_bd3","std_power_bd3",
         "mean_power_b2","mean_power_bd3","mean_power_bd2")

for (i in col_list){
  data[,i] = data[,i]/1000
}

vars_r = setdiff(colnames(data),"event")

library(xgboost)

data_matrix = xgb.DMatrix(data = as.matrix(data[,vars_r]), label = as.matrix(data[,"event"]))

set.seed(1234)

model_cv = xgb.cv(data =  data_matrix, nrounds = 1200, nfold = 5, showsd = TRUE,
                  eval_metric = "auc", verbose = TRUE, early.stop.round = 5,
                  objective = "binary:logistic", max.depth = 8, "eta" = 0.01,                               
                  subsample = 0.8, colsample_bytree = 0.85, maximize = T,
                  min_child_weight = 6,nthread = 4, alpha = 0.001, gamma = 1.5)


model_full = xgboost(data =  data_matrix, nrounds = 468, showsd = TRUE,
                  eval_metric = "logloss", verbose = TRUE,
                  objective = "binary:logistic", max.depth = 8, "eta" = 0.01,                               
                  subsample = 0.8, colsample_bytree = 0.85, maximize = F,
                  min_child_weight = 6,nthread = 4, alpha = 0.001, gamma = 1.5)

model_dump = xgb.dump(model_full, with.stats = T)

names = dimnames(as.matrix(data[,-1]))[[2]]

importance_matrix = xgb.importance(names, model = model_full)

xgb.plot.importance(importance_matrix[1:50,])

#############################################################

vars_r1 = setdiff(colnames(data),c("event","dtw_mean","dtw_median"))

data_matrix = xgb.DMatrix(data = as.matrix(data[,vars_r1]), label = as.matrix(data[,"event"]))

set.seed(1234)

model_full = xgboost(data =  data_matrix, nrounds = 500, showsd = TRUE,
                     eval_metric = "logloss", verbose = TRUE, early.stop.round = 5,
                     objective = "binary:logistic", max.depth = 8, "eta" = 0.01,                               
                     subsample = 0.8, colsample_bytree = 0.85, maximize = F,
                     min_child_weight = 6,nthread = 4, alpha = 0.001, gamma = 1.5)

model_dump = xgb.dump(model_full, with.stats = T)

names = dimnames(as.matrix(data[,vars_r1]))[[2]]

importance_matrix = xgb.importance(names, model = model_full)

xgb.plot.importance(importance_matrix[1:20,])
