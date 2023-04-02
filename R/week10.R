# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven)
library(caret)

# Data Import and Cleaning
gss_tbl <- read_sav(file = "../data/GSS2016.sav") %>% 
  # Remove labels 
  zap_labels() %>% 
  # Remove anyone who has a missing value for the workhours (HRS1) variable
  filter(!is.na(HRS1)) %>% 
  # Rename hrs1 to workhours %>% 
  rename(workhours = HRS1) %>% 
  # Use colMeans to calculate the % of missing values in each column and retain only variables with less than 75% missingness
  select_if(colMeans(is.na(.)) < .75)

# Visualization
# Visualization of the univariate distribution of workhours
ggplot(gss_tbl, aes(x = workhours)) +
  geom_histogram(binwidth = 5) +
  labs(x = "Hours worked per week", y = "Count", title = "Distribution of Workhours")

# Analysis
# Create a training and testing set using a 75/25 split
set.seed(107)
# Create partition index with caret
index <- createDataPartition(gss_tbl$workhours, p = .75, list = FALSE)
# Create train and test set
train <- gss_tbl[index,]
test <- gss_tbl[-index,]

# OLS
# Train an OLS model with grid search predicting workhours from all remaining variables
model_ols <- train(
  workhours ~ ., 
  data = train, 
  na.action = na.pass,
  method = "lm",
  # Median imputation to impute any remaining missing values
  preProcess= c("center", "scale", "zv", "medianImpute"),
  # 10-fold cross-validation
  trControl = trainControl(method = "cv", number = 10))

# R^2 for 10-fold CV 
cv_rsq_ols <- model_ols$results$Rsquared[1]
cv_rsq_ols

# Impute test set with median
test <- predict(preProcess(test, method = "medianImpute"), newdata = test)

# R^2 for CV
hocv_cor_ols <- cor(
  predict(model_ols, test),
  test$workhours
)^2
hocv_cor_ols

# Elastic net
# Train an elastic net model with grid search predicting workhours from all remaining variables
model_glmnet <- train(
  workhours ~ ., 
  data = train, 
  na.action = na.pass,
  method = "glmnet",
  # Median imputation to impute any remaining missing values
  preProcess= c("center", "scale", "zv", "medianImpute"),
  # 10-fold cross-validation
  trControl = trainControl(method = "cv", number = 10),
  tuneGrid = expand.grid(alpha = seq(0, 1, 0.1), lambda = seq(0, 1, 0.1))
)

# R^2 for 10-fold CV 
cv_rsq_glmnet <- max(model_glmnet$results$Rsquared)
cv_rsq_glmnet

# R^2 for CV
hocv_cor_glmnet <- cor(
  predict(model_glmnet, test),
  test$workhours
)^2
hocv_cor_glmnet

# Random forest
# Train an random forest model with grid search predicting workhours from all remaining variables
model_rf <- train(
  workhours ~ ., 
  data = train, 
  na.action = na.pass,
  method = "rf",
  # Median imputation to impute any remaining missing values
  preProcess= c("center", "scale", "zv", "medianImpute"),
  # 10-fold cross-validation
  trControl = trainControl(method = "cv", number = 10),
  tuneGrid = expand.grid(mtry=c(1, 3, 5))
)

# R^2 for 10-fold CV 
cv_rsq_rf <- max(model_rf$results$Rsquared)
cv_rsq_rf

# R^2 for CV
hocv_cor_rf <- cor(
  predict(model_rf, test),
  test$workhours
)^2
hocv_cor_rf

# eXtreme Gradient Boosting
# Train an XGB model with grid search predicting workhours from all remaining variables
model_xgb <- train(
  workhours ~ ., 
  data = train, 
  na.action = na.pass,
  method = "xgbTree",
  # Median imputation to impute any remaining missing values
  preProcess= c("center", "scale", "zv", "medianImpute"),
  # 10-fold cross-validation
  trControl = trainControl(method = "cv", number = 10),
  tuneGrid = expand.grid(nrounds = c(10, 100, 500), 
                         max_depth = c(2,6,8),
                         eta = c(0.01, 0.1, 0.3),
                         gamma = 0,
                         colsample_bytree = 1,
                         min_child_weight = 1,
                         subsample = 1
  )
)

# R^2 for 10-fold CV 
cv_rsq_xgb <- max(model_xgb$results$Rsquared)
cv_rsq_xgb

# R^2 for CV
hocv_cor_xgb <- cor(
  predict(model_xgb, test),
  test$workhours
)^2
hocv_cor_xgb







  