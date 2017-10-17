
library(tidyverse)
library(caret)
library(e1071)
library(xgboost)
library(mlbench)
library(MLmetrics)

mydata <- read.csv('Data_set.csv', stringsAsFactors = T, fileEncoding = 'euc-kr')

# Split train and test datat
train_ind <- createDataPartition(1:nrow(mydata), p = 0.7, list = F)
train <- mydata[train_ind, ]
test <- mydata[-train_ind, ]

train_target1 <- train %>% filter(TARGET == 1)
train_target0 <- train %>% filter(TARGET == 0)

# Shuffle the data
train_target0 <- train_target0[sample(nrow(train_target0)), ]

# Make folds
folds <- cut(seq(1, nrow(train_target0)), breaks = 10, labels = F)

list_names <- c('fold1', 'fold2', 'fold3', 'fold4', 'fold5', 'fold6', 'fold7', 'fold8', 'fold9', 'fold10')
train_target0_folds <- vector('list', length(list_names))
names(train_target0_folds) <- list_names

for(i in 1:10){
    folds_idx <- which(folds == i)
    target0_folds <- train_target0[folds_idx, ]
    train_target0_folds[[i]] <- rbind(target0_folds, train_target1)
}

str(train_target0_folds)

sample_1 <- train_target0_folds[[1]]

sample_1$TARGET <- as.factor(sample_1$TARGET)

# XGBoost Model fitting
xgb_model_1 <- train(TARGET~.,
                  data = sample_1,
                  method = 'xgbTree',
                  trControl = trainControl(method = 'cv', number = 5, verboseIter = F))

# Predict TARGET Probability
pred <- round(predict(xgb_model, newdata = test, type = 'prob'), 2) %>% select(`1`)
names(pred) <- 'prob'

# Define TARGET Result by thresholds
threshold <- seq(0.1, 0.9, 0.1)

pred <- pred %>% mutate(answer_0.1 = ifelse(prob <= threshold[1], 0, 1))
pred <- pred %>% mutate(answer_0.2 = ifelse(prob <= threshold[2], 0, 1))
pred <- pred %>% mutate(answer_0.3 = ifelse(prob <= threshold[3], 0, 1))
pred <- pred %>% mutate(answer_0.4 = ifelse(prob <= threshold[4], 0, 1))
pred <- pred %>% mutate(answer_0.5 = ifelse(prob <= threshold[5], 0, 1))
pred <- pred %>% mutate(answer_0.6 = ifelse(prob <= threshold[6], 0, 1))
pred <- pred %>% mutate(answer_0.7 = ifelse(prob <= threshold[7], 0, 1))
pred <- pred %>% mutate(answer_0.8 = ifelse(prob <= threshold[8], 0, 1))
pred <- pred %>% mutate(answer_0.9 = ifelse(prob <= threshold[9], 0, 1))

# Calculate F1 Score
F1_Score(y_pred = pred$answer_0.1, y_true = test$TARGET, positive = '1')
F1_Score(y_pred = pred$answer_0.2, y_true = test$TARGET, positive = '1')
F1_Score(y_pred = pred$answer_0.3, y_true = test$TARGET, positive = '1')
F1_Score(y_pred = pred$answer_0.4, y_true = test$TARGET, positive = '1')
F1_Score(y_pred = pred$answer_0.5, y_true = test$TARGET, positive = '1')
F1_Score(y_pred = pred$answer_0.6, y_true = test$TARGET, positive = '1')
F1_Score(y_pred = pred$answer_0.7, y_true = test$TARGET, positive = '1')
F1_Score(y_pred = pred$answer_0.8, y_true = test$TARGET, positive = '1') # 얘가 최선으로 보임
F1_Score(y_pred = pred$answer_0.9, y_true = test$TARGET, positive = '1')

이걸 10번 반복해야 하는데????
