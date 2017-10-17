
set.seed(123)
library(DMwR)
library(caret)
library(e1071)

mydata <- read_csv('Data_set.csv')
mydata_imp <- read_csv('Data_imputed.csv')

feature_names <- c("BNK_LNIF_CNT", "CPT_LNIF_CNT", "SPART_LNIF_CNT",
                   "ECT_LNIF_CNT", "TOT_LNIF_AMT", "TOT_CLIF_AMT",     
                   "BNK_LNIF_AMT", "CPT_LNIF_AMT", "CRDT_OCCR_MDIF",
                   "SPTCT_OCCR_MDIF", "CRDT_CARD_CNT", "CTCD_OCCR_MDIF",
                   "CB_GUIF_CNT", "CB_GUIF_AMT")

new_features <- c("SUSP_TOT_CATE")
# "FIRST_SECOND_RATIO", "ONLY_FIRST", "TOT_LNIF_CNT",
# "TOT_SECOND_CNT", "TOT_SECOND_AMT", "CB_GUIF_AMT_PER_CNT",

data_new <- mydata %>% select(TARGET, one_of(feature_names),
                               one_of(new_features))

data_new <- mydata %>% select(TARGET, one_of(feature_names))

# Split train and test data
train_ind <- createDataPartition(1:nrow(data_new), p = 0.7, list = F)
train <- data_new[train_ind, ]
test <- data_new[-train_ind, ]

# SMOTE target of train set
# train$TARGET <- as.factor(train$TARGET)
# train <- SMOTE(TARGET ~ ., train, perc.over = 100, perc.under=200)
# train$TARGET <- as.numeric(train$TARGET)

# Model fitting (xgb)
xgb_control <- trainControl(method = 'cv', 
                           number = 10, 
                           sampling = 'up',
                           verboseIter = TRUE,
                           returnData=FALSE,
                           returnResamp = 'all',
                           allowParallel = TRUE
                           )

xgb_grid <- expand.grid(nrounds = 100,
                        max_depth = 3,
                        eta = 0.1,
                        gamma = 1,
                        colsample_bytree = 1,
                        min_child_weight = 1,
                        subsample = 1)

train$TARGET <- as.factor(train$TARGET)

xgb_fit <- train(TARGET ~ ., 
                data = train,
                method = 'xgbTree',
                trControl = xgb_control,
                tuneGrid = xgb_grid
                )

xgb_pred <- predict(xgb_fit, newdata = test)

xgb_pred %>% confusionMatrix(test$TARGET)

# F1-score
precision <- 1075/(217+1075)
recall <- 1075/(5922+1075)
(F1 <- (2 * precision * recall) / (precision + recall))

# Feature Importance
plot(varImp(object = xgb_fit), main = 'XGB - Variable Importance')

