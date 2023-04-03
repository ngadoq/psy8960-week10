# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven)
library(caret)

# Data Import and Cleaning
gss_tbl <- read_sav(file = "../data/GSS2016.sav") %>% 
  # Remove labels 
  zap_labels() %>% 
  # Recode IAP, Don't know and No answer to NA
  mutate(across(where(is.character), ~na_if(., c("DK", "IAP")))) %>% 
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
# Create train and test set
train_cases <- sample(1:nrow(gss_tbl), 1235)

train <- gss_tbl[train_cases,]
test <- gss_tbl[-train_cases,]

# Create train folds
training_folds <- createFolds(train$workhours, 10)

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
  trControl = trainControl(method = "cv",indexOut=training_folds, number = 10))

# R^2 for 10-fold CV 
cv_rsq_ols <- model_ols$results$Rsquared[1]
cv_rsq_ols

# Predict test data using trained model
# R^2 for CV
hocv_cor_ols <- cor(
  predict(model_ols, test, na.action = na.pass),
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
  trControl = trainControl(method = "cv", indexOut=training_folds, number = 10),
  tuneGrid = expand.grid(alpha = seq(0, 1, 0.1), lambda = seq(0, 1, 0.1))
)

# R^2 for 10-fold CV 
cv_rsq_glmnet <- max(model_glmnet$results$Rsquared)
cv_rsq_glmnet

# R^2 for CV
hocv_cor_glmnet <- cor(
  predict(model_glmnet, test, na.action = na.pass),
  test$workhours
)^2
hocv_cor_glmnet

# Random forest
# Train an random forest model with grid search predicting workhours from all remaining variables
model_rf <- train(
  workhours ~ ., 
  data = train, 
  na.action = na.pass,
  method = "ranger",
  # Median imputation to impute any remaining missing values
  preProcess= c("center", "scale", "zv", "medianImpute"),
  # 10-fold cross-validation
  trControl = trainControl(method = "cv", indexOut=training_folds, number = 10),
  tuneLength = 10
)

# R^2 for 10-fold CV 
cv_rsq_rf <- max(model_rf$results$Rsquared)
cv_rsq_rf

# R^2 for CV
hocv_cor_rf <- cor(
  predict(model_rf, test, na.action = na.pass),
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
  trControl = trainControl(method = "cv", indexOut=training_folds, number = 10),
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
  predict(model_xgb, test, na.action = na.pass),
  test$workhours
)^2
hocv_cor_xgb

# Publication
# Construct table1_tbl
table1_tbl <- data.frame(
  algo = c("OLS Regression", "Elastic net", "Random forest", "eXtreme Gradient Boosting"),
  cv_rsq = format(c(cv_rsq_ols, cv_rsq_glmnet, cv_rsq_rf, cv_rsq_xgb), nsmall = 2),
  ho_rsq = format(c(hocv_cor_ols, hocv_cor_glmnet, hocv_cor_rf, hocv_cor_xgb), nsmall = 2)
)
table1_tbl

# XGBoost model has the highest k-fold CV R squared and random forest model has the highest holdout CV R squared. This might be due the complicated nature of these models (able to model non-linear relationship), and therefore they are able to explain more variance in the data compared to more simple model like OLS regression and elastic net. However, models such as XGB and random forest are more complicated, and may require more tuning for hyperparmeters. For OLS regression and Elastic net, there is large difference between k-fold CV R squared and holdout CV R squared. 

# All models seem to suffer from a certain degress of overfitting because R squared values for k-fold CV are higher than holdout CV. However, OLS regression has the largest difference in R squared between k-fold CV and holdout CV R squared which suggested severe overfitting. For other models, the difference is less serious. This can be explained when we look at the k/N ratio. We can see that there are not enough data to train and test the model compared to the number of the parameters. For elastic net and XGB, the R squared for k-fold CV and holdout CV are more similar.

# Given the complex nature of the data (more than 600 predictors in comparison to the number of observations), intepretability is probably not the number one concern. Therefore, I'd like to choose a model that can maximize predictability. Among the four models, I would choose random forest because its holdout CV index is the highest among all the models and its k-fold CV R squared is second to highest. In addition, compared to XGBoost, random forest is less computationally intensive and might more applicable in applied situations. Also, compared to the other three models, OLS regression model suffers much more from rank-deficiency (predictors are perfectly correlated) which also contributes to overfitting. 



  