install.packages('caret')
install.packages('tidyverse')
install.packages('MLmetrics')
install.packages('mlbench')

library(caret)
library(tidyverse)
library(MLmetrics)
library(mlbench)

mydata <- read.csv('C:\\Users\\Kim Sang-Hyun\\Desktop\\train.csv')
mydata <- mydata %>% select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare)

# Check Missing Values
lapply(mydata, function(x) sum(is.na(x)))

# Impute NA in Age feature
mydata <- mydata %>%
  mutate(Age = ifelse(is.na(Age), median(Age, na.rm = T), Age))

# Factorize features
str(mydata)
mydata$Survived <- as.factor(mydata$Survived)

# Split Train and Test set
train_ind <- createDataPartition(1:nrow(mydata), p = 0.7, list = F)
train <- mydata[train_ind, ]
test <- mydata[-train_ind, ]

# Logistic Model fitting
logit_model <- glm(Survived~., data = train, family = 'binomial')
summary(logit_model)

# Predict Test set
pred <- predict(logit_model, newdata = test, type = 'response')
pred <- as.data.frame(pred)
names(pred) <- 'prob'

# Define Target Result by thresholds
threshold <- seq(0.1, 0.9, 0.1)

for(i in 1:length(threshold)){
  col_name <- paste('answer', threshold[i], sep = '_')
  pred[[col_name]] <- ifelse(pred$prob <= threshold[i], 0, 1)
}

# Calculate F1 Score
score <- list()

for(i in 1:length(threshold)){
  score[i] <- F1_Score(y_pred = pred[[i+1]], y_true = test$Survived, positive = '1')
}

score <- unlist(score)
score_table <- data.frame(threshold, score)

ggplot(score_table, aes(threshold, score)) +
  geom_point() +
  geom_line() +
  labs(y = 'F1_Score')
