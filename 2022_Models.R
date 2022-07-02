#### 2022 Model Training ####
library(tidyverse)
library(caTools)
library(readxl)
library(tidymodels)
library(broom)
library(ggfortify)
library(caret)
library(glmnet)
library(InformationValue)
library(class)
library(e1071)
devtools::install_github("bips-hb/neuralnet", force = T)
library(neuralnet)
library(xgboost)

rm(list=ls())

nba <- read_xlsx("/Users/Jesse/Documents/MyStuff/NBA Betting/NBAdb/NBAdb1722_oneadj.xlsx") # use TS
nba <- read_xlsx("/Users/Jesse/Documents/MyStuff/NBA Betting/NBAdb/NBAdb1722_oneadj_ftm_fta.xlsx") # use eFG

nba <- nba %>%
    filter(Date > '2018-08-01')
nba <- nba %>%
    filter(Date > '2019-08-01')


########### Spread ------------------------------------------------


# regression nba stats

## no correlation - eFG
nba_reg <- nba %>%
    select(7,
           14,15,16,18,19,21,23,30,34,35,37,39,43,
           49,50,51,53,54,56,58,65,69,70,72,74,78)

## no correlation - ts
nba_reg <- nba %>%
    select(7,
           14,15,16,18,19,21,24,30,34,35,37,40,43,
           49,50,51,53,54,56,59,65,69,70,72,75,78)



set.seed(214)
sample <- sample.split(nba_reg$Margin, SplitRatio = .70)
train <- nba_reg %>% filter(sample == TRUE)
test <- nba_reg %>% filter(sample == FALSE)

pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))

train[,-1] = predict(pre_proc_val, train[,-1])
test[,-1] = predict(pre_proc_val, test[,-1])

summary(train)




################
#### Linear #### - regression
################

lin_mod <- lm(Margin ~., data = train)
summary(lin_mod)


#Step 1 - create the evaluation metrics function
eval_metrics <- function(model, df, predictions, target) {
    resids = df[,target] - predictions
    resids2 = resids**2
    N = length(predictions)
    r2 = as.character(round(summary(model)$r.squared, 4))
    adj_r2 = as.character(round(summary(model)$adj.r.squared, 4))
    print(adj_r2) #Adjusted R-squared
    print(as.character(round(sqrt(sum(resids2)/N), 4))) #RMSE
}
# Step 2 - predicting and evaluating the model on train data
predictions = predict(lin_mod, newdata = train)
eval_metrics(lin_mod, train, predictions, target = 'Margin')

# Step 3 - predicting and evaluating the model on test data
predictions = predict(lin_mod, newdata = test)
eval_metrics(lin_mod, test, predictions, target = 'Margin')



# Visualizations
# http://www.sthda.com/english/articles/39-regression-model-diagnostics/161-linear-regression-assumptions-and-diagnostics-in-r-essentials/  
library(broom)
ls_marg_diag <- augment(lin_mod)
ls_marg_diag %>% 
    mutate(index = 1:nrow(lin_mod)) %>%
    select(34,1,28:30,32:33) %>%
    head()

ls_marg_diag %>%
    top_n(3, wt = .cooksd)

plot(lin_mod)
autoplot(lin_mod)




### Ridge & Lasso models
dummies <- dummyVars(Margin ~ ., data = nba_reg)
train_dummies = predict(dummies, newdata = train)
test_dummies = predict(dummies, newdata = test)
print(dim(train_dummies)); print(dim(test_dummies))

x = as.matrix(train_dummies)
y_train = train$Margin

x_test = as.matrix(test_dummies)
y_test = test$Margin

# Compute R^2 from true and predicted values
eval_results <- function(true, predicted, df) {
    SSE <- sum((predicted - true)^2)
    SST <- sum((true - mean(true))^2)
    R_square <- 1 - SSE / SST
    RMSE = sqrt(SSE/nrow(df))
    
    
    # Model performance metrics
    data.frame(
        Rsquare = R_square,
        RMSE = RMSE
    )
    
}



### Ridge model
lambdas <- 10^seq(2, -3, by = -.1)
ridge_reg = glmnet(x, y_train, nlambda = 25, alpha = 0, family = 'gaussian', lambda = lambdas)

par(mfrow = c(1, 2))
plot(ridge_reg)
plot(ridge_reg, xvar = "lambda", label = TRUE)

summary(ridge_reg)


cv_ridge <- cv.glmnet(x, y_train, alpha = 0, lambda = lambdas)
plot(cv_ridge)

optimal_lambda <- cv_ridge$lambda.min
optimal_lambda


# Prediction and evaluation on train data
predictions_train <- predict(ridge_reg, s = optimal_lambda, newx = x)
eval_results(y_train, predictions_train, train)

# Prediction and evaluation on test data
predictions_test <- predict(ridge_reg, s = optimal_lambda, newx = x_test)
eval_results(y_test, predictions_test, test)




# Lasso model
lambdas <- 10^seq(2, -3, by = -.1)

# Setting alpha = 1 implements lasso regression
lasso_reg <- cv.glmnet(x, y_train, alpha = 1, lambda = lambdas, standardize = TRUE, nfolds = 5,
                       family='gaussian', type.measure='mse')
plot(lasso_reg)
plot(lasso_reg$glmnet.fit, xvar="lambda", label=TRUE)

cat('Min Lambda: ', lasso_reg$lambda.min, '\n 1Sd Lambda: ', lasso_reg$lambda.1se)
df_coef <- round(as.matrix(coef(lasso_reg, s=lasso_reg$lambda.min)), 2)

# See all contributing variables
df_coef[df_coef[, 1] != 0, ]

# Best 
lambda_best <- lasso_reg$lambda.min 
lambda_best


lasso_model <- glmnet(x, y_train, alpha = 1, lambda = lambda_best, standardize = TRUE, family='gaussian')
lasso_model$beta

predictions_train <- predict(lasso_model, s = lambda_best, newx = x)
eval_results(y_train, predictions_train, train)

predictions_test <- predict(lasso_model, s = lambda_best, newx = x_test)
eval_results(y_test, predictions_test, test)



# Elastic Net
# Set training control
train_cont <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 5,
                           search = "random",
                           verboseIter = TRUE)

# Train the model
elastic_reg <- train(Margin ~ .,
                     data = train,
                     method = "glmnet",
                     preProcess = c("center", "scale"),
                     tuneLength = 10,
                     trControl = train_cont)



# Best tuning parameter
elastic_reg$bestTune

# Make predictions on training set
predictions_train <- predict(elastic_reg, x)
eval_results(y_train, predictions_train, train) 

# Make predictions on test set
predictions_test <- predict(elastic_reg, x_test)
eval_results(y_test, predictions_test, test)




#############
#### KNN #### - regression
#############


# rmse function
rmse = function(actual, predicted) {
    sqrt(mean((actual - predicted) ^ 2))
}

# define helper function for getting knn.reg predictions
# note: this function is highly specific to this situation and dataset
make_knn_pred = function(k = 1, training, predicting) {
    pred = FNN::knn.reg(as.data.frame(training[,-1]), 
                        as.data.frame(predicting[,-1]), 
                        y = as.numeric(train$Margin), k = k)$pred
    act  = predicting$Margin
    rmse(predicted = pred, actual = act)
}


# define values of k to evaluate
# k = seq(21,91,5)
k = 1:101

# get requested train RMSEs
knn_trn_rmse = sapply(k, make_knn_pred, 
                      training = train, 
                      predicting = train)
# get requested test RMSEs
knn_tst_rmse = sapply(k, make_knn_pred, 
                      training = train, 
                      predicting = test)

# determine "best" k
best_k = k[which.min(knn_tst_rmse)]

# find overfitting, underfitting, and "best"" k
fit_status = ifelse(k < best_k, "Over", ifelse(k == best_k, "Best", "Under"))


# summarize results
knn_results = data.frame(
    k,
    round(knn_trn_rmse, 2),
    round(knn_tst_rmse, 2),
    fit_status
)
colnames(knn_results) = c("k", "Train RMSE", "Test RMSE", "Fit?")

# display results
knitr::kable(knn_results, escape = FALSE, booktabs = TRUE)





knn_model <- FNN::knn.reg(train = as.data.frame(train[,-1]), 
                          test = as.data.frame(test[,-1]), 
                          y = as.numeric(train$Margin), 
                          k = best_k)
pred_y  = knn_model$pred


mse = mean((as.numeric(unlist(test[,1])) - pred_y)^2)
mae = caret::MAE(as.numeric(unlist(test[,1])), pred_y)
rmse = caret::RMSE(as.numeric(unlist(test[,1])), pred_y)

cat("MSE: ", mse, "MAE: ", mae, " RMSE: ", rmse)






#################
##### Tuning ####
#################

calc_rmse = function(actual, predicted) {
    sqrt(mean((actual - predicted) ^ 2))
}

# regression
# random forest
oob = trainControl(method = "oob")
cv_5 = trainControl(method = "cv", number = 5)

dim(train)

rf_grid =  expand.grid(mtry = 1:26)

set.seed(214)
nba_rf_tune = train(Margin ~ ., data = train,
                    method = "rf", #ranger
                    trControl = oob,
                    verbose = T,
                    tuneGrid = rf_grid)
nba_rf_tune

calc_rmse(predict(nba_rf_tune, test), test$Margin)

nba_rf_tune$bestTune

# boosting
gbm_grid =  expand.grid(interaction.depth = 1:5,
                        n.trees = (1:6) * 500,
                        shrinkage = c(0.001, 0.01, 0.1),
                        n.minobsinnode = 10)

nba_gbm_tune = train(Margin ~ ., data = train,
                     method = "gbm",
                     trControl = cv_5,
                     verbose = FALSE,
                     tuneGrid = gbm_grid)

plot(nba_gbm_tune)

calc_rmse(predict(nba_gbm_tune, test), test$Margin)

nba_gbm_tune$bestTune



##################
#### Ensemble ####
##################


# ensemble regression style

library(rpart)
library(rpart.plot)
library(randomForest)
library(gbm)
library(caret)
library(MASS)



# tree
nba_tree = rpart(Margin ~ ., data = train)

nba_tree_tst_pred = predict(nba_tree, newdata = test)
plot(nba_tree_tst_pred, test$Margin, 
     xlab = "Predicted", ylab = "Actual", 
     main = "Predicted vs Actual: Single Tree, Test Data",
     col = "dodgerblue", pch = 20)
grid()
abline(0, 1, col = "darkorange", lwd = 2)

(tree_tst_rmse = calc_rmse(nba_tree_tst_pred, test$Margin))

# linear
nba_lm = lm(Margin ~ ., data = train)

nba_lm_tst_pred = predict(nba_lm, newdata = test)

plot(nba_lm_tst_pred, test$Margin,
     xlab = "Predicted", ylab = "Actual",
     main = "Predicted vs Actual: Linear Model, Test Data",
     col = "dodgerblue", pch = 20)
grid()
abline(0, 1, col = "darkorange", lwd = 2)

(lm_tst_rmse = calc_rmse(nba_lm_tst_pred, test$Margin))

# bagging
nba_bag = randomForest(Margin ~ ., data = train, mtry = 26, 
                       importance = TRUE, ntrees = 500)
nba_bag

nba_bag_tst_pred = predict(nba_bag, newdata = test)
plot(nba_bag_tst_pred,test$Margin,
     xlab = "Predicted", ylab = "Actual",
     main = "Predicted vs Actual: Bagged Model, Test Data",
     col = "dodgerblue", pch = 20)
grid()
abline(0, 1, col = "darkorange", lwd = 2)

(bag_tst_rmse = calc_rmse(nba_bag_tst_pred, test$Margin))

plot(nba_bag, col = "dodgerblue", lwd = 2, main = "Bagged Trees: Error vs Number of Trees")
grid()

# random forest
nba_forest = randomForest(Margin ~ ., data = train, mtry = 9, 
                          importance = TRUE, ntrees = 500)
nba_forest

importance(nba_forest, type = 1)

varImpPlot(nba_forest, type = 1)

nba_forest_tst_pred = predict(nba_forest, newdata = test)
plot(nba_forest_tst_pred, test$Margin,
     xlab = "Predicted", ylab = "Actual",
     main = "Predicted vs Actual: Random Forest, Test Data",
     col = "dodgerblue", pch = 20)
grid()
abline(0, 1, col = "darkorange", lwd = 2)

(forest_tst_rmse = calc_rmse(nba_forest_tst_pred, test$Margin))

nba_forest_trn_pred = predict(nba_forest, newdata = train)
forest_trn_rmse = calc_rmse(nba_forest_trn_pred, train$Margin)
forest_oob_rmse = calc_rmse(nba_forest$predicted, train$Margin)

# boosting
nba_boost = gbm(Margin ~ ., data = train, distribution = "gaussian", 
          n.trees = 1500, interaction.depth = 2, shrinkage = 0.01)
nba_boost

tibble::as_tibble(summary(nba_boost))

par(mfrow = c(1, 3))
plot(nba_boost, i = "oeFG_home", col = "dodgerblue", lwd = 2)

plot(nba_boost, i = "oeFG_away", col = "dodgerblue", lwd = 2)

plot(nba_boost, i = "eFG_home", col = "dodgerblue", lwd = 2)

plot(nba_boost, i = "eFG_away", col = "dodgerblue", lwd = 2)

nba_boost_tst_pred = predict(nba_boost, newdata = test, n.trees = 1500)
(boost_tst_rmse = calc_rmse(nba_boost_tst_pred, test$Margin))

plot(nba_boost_tst_pred, test$Margin,
     xlab = "Predicted", ylab = "Actual", 
     main = "Predicted vs Actual: Boosted Model, Test Data",
     col = "dodgerblue", pch = 20)
grid()
abline(0, 1, col = "darkorange", lwd = 2)

# results
(nba_rmse = data.frame(
    Model = c("Single Tree", "Linear Model", "Bagging",  "Random Forest",  "Boosting"),
    TestError = c(tree_tst_rmse, lm_tst_rmse, bag_tst_rmse, forest_tst_rmse, boost_tst_rmse)
)
)


#############
#### SVM #### - regression
#############

# Setup for cross validation
ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 3)         

# grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
grid <- expand.grid(C = c(10, 5))
svm.tune <- train(Margin ~ .,
                  data = train,
                  method = "svmLinear",
                  tuneLength = 10,      
                  tuneGrid = grid,
                  trControl=ctrl)

svm.tune
plot(svm.tune)



modelsvm = svm(Margin ~., train, cost=10, kernel = "linear")
predsvm = predict(modelsvm, test)
RMSEsvm = RMSE(predsvm, test$Margin)




############
#### NN #### - regression
############

nn_model <- neuralnet(Margin ~., data = train, linear.output = T, rep = 1, threshold = .8, stepmax = 1e7)
plot(nn_model)

nn_margin <- compute(nn_model, test)

margin_rmse <- RMSE(nn_margin$net.result, test$Margin)
margin_rmse


#############
#### XGB #### - regression
#############

# xgb_params <- list(
#     objective = "reg:squarederror",
#     eval_metric = "rmse",
#     eta = 0.0168,
#     max_depth = 7,
#     subsample = 0.6758,
#     colsample_bytree = 0.5995,
#     min_child_weight = 39,
#     max_delta_step = 5
# )





trainx <- as.matrix(train[-1])
trainy <- as.matrix(train[1])
testx <- as.matrix(test[-1])
testy <- as.matrix(test[1])

dtrain <- xgb.DMatrix(trainx, label = trainy)
dtest <- xgb.DMatrix(testx, label = testy)



best_param <- list()
best_seednumber <- 1234
best_rmse <- Inf
best_rmse_index <- 0

set.seed(123)
for (iter in 1:100) {
    param <- list(objective = "reg:squarederror",
                  eval_metric = "rmse",
                  max_depth = sample(6:10, 1),
                  eta = runif(1, .01, .3), # Learning rate, default: 0.3
                  subsample = runif(1, .6, .9),
                  colsample_bytree = runif(1, .5, .8), 
                  min_child_weight = sample(1:40, 1),
                  max_delta_step = sample(1:10, 1)
    )
    cv.nround <-  1000
    cv.nfold <-  5 # 5-fold cross-validation
    seed.number  <-  sample.int(10000, 1) # set seed for the cv
    set.seed(seed.number)
    mdcv <- xgb.cv(data = dtrain, params = param,  
                   nfold = cv.nfold, nrounds = cv.nround,
                   verbose = F, early_stopping_rounds = 8, maximize = FALSE)
    
    min_rmse_index  <-  mdcv$best_iteration
    min_rmse <-  mdcv$evaluation_log[min_rmse_index]$test_rmse_mean
    
    if (min_rmse < best_rmse) {
        best_rmse <- min_rmse
        best_rmse_index <- min_rmse_index
        best_seednumber <- seed.number
        best_param <- param
    }
}

# The best index (min_rmse_index) is the best "nround" in the model
nround = best_rmse_index
set.seed(best_seednumber)
xg_mod <- xgboost(data = dtest, params = best_param, nround = nround, verbose = F)

# Check error in testing data
yhat_xg <- predict(xg_mod, dtest)
(RMSE(yhat_xg, testy))

# -----------------------------------------------------------

# https://www.youtube.com/watch?v=xiqHCLSXbcA

# xgb_model_linear <- train(Margin ~.,
#                           data = train,
#                           method = "xgbLinear",
#                           metric = "rmse",
#                           trControl = trainControl(
#                               method = "repeatedcv",
#                               number = 2,
#                               repeats = 3,
#                               verboseIter = T),
#                           tuneGrid = expand.grid(
#                               nrounds = c(100, 500, 1000, 1500),
#                               eta = c(0.01, 0.05),
#                               alpha = c(0, 1, 100),
#                               lambda = c(0, 1, 100)),
#                           objective = "reg:squarederror")

xgb_model_linear <- train(Margin ~.,
                          data = train,
                          method = "xgbLinear",
                          metric = "rmse",
                          trControl = trainControl(
                              method = "repeatedcv",
                              number = 3,
                              repeats = 3,
                              verboseIter = T,
                              search = "grid"),
                          objective = "reg:squarederror",
                          tuneLength = 3)

plot(xgb_model_linear)

test$pred <- predict(xgb_model_linear, test)
(RMSE(test$pred, test$Margin))


expand.grid




#### Visualizations
# Get the feature real names
names <- dimnames(data.matrix(X[,-1]))[[2]]
# Compute feature importance matrix
importance_matrix <- xgb.importance(names, model = xgb)
# Nice graph
xgb.plot.importance(importance_matrix[1:10,])





########### ML ------------------------------------------------


## no correlation - eFG
nba_cla <- nba %>%
    select(8,
           14,15,16,18,19,21,23,30,34,35,37,39,43,
           49,50,51,53,54,56,58,65,69,70,72,74,78)

## no correlation - ts
nba_cla <- nba %>%
    select(8,
           14,15,16,18,19,21,24,30,34,35,37,40,43,
           49,50,51,53,54,56,59,65,69,70,72,75,78)



set.seed(214)
sample <- sample.split(nba_cla$Win, SplitRatio = .70)
train <- nba_cla %>% filter(sample == TRUE)
test <- nba_cla %>% filter(sample == FALSE)

pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))

train[,-1] = predict(pre_proc_val, train[,-1])
test[,-1] = predict(pre_proc_val, test[,-1])



### logistic model

log_mod <- glm(Win ~., data = train, family = "binomial")
summary(log_mod)



# predicting and evaluating the model on test data
predictions = predict(log_mod, newdata = test, type = 'response')

plotROC(actuals=test$Win, predictedScores=predictions, returnSensitivityMat = F) # .7-.8 = good; .8+ = great
confusionMatrix(test$Win, predictions)
misClassError(test$Win, predictions)

# https://stats.stackexchange.com/questions/234998/logistic-regression-diagnostic-plots-in-r
# http://www.sthda.com/english/articles/36-classification-methods-essentials/148-logistic-regression-assumptions-and-diagnostics-in-r/


# Ridge & Lasso Models
dummies <- dummyVars(Win ~ ., data = nba_cla)
train_dummies = predict(dummies, newdata = train)
test_dummies = predict(dummies, newdata = test)
print(dim(train_dummies)); print(dim(test_dummies))

x = as.matrix(train_dummies)
y_train = train$Win

x_test = as.matrix(test_dummies)
y_test = test$Win


### Ridge model
lambdas <- 10^seq(2, -3, by = -.1)
ridge_reg = glmnet(x, y_train, nlambda = 25, alpha = 0, family = "binomial", lambda = lambdas)

par(mfrow = c(1, 2))
plot(ridge_reg)
plot(ridge_reg, xvar = "lambda", label = TRUE)

summary(ridge_reg)


cv_ridge <- cv.glmnet(x, y_train, alpha = 0, lambda = lambdas, family = 'binomial')
plot(cv_ridge)

optimal_lambda <- cv_ridge$lambda.min
optimal_lambda


# Prediction and evaluation on train data
predictions_train <- predict(ridge_reg, s = optimal_lambda, newx = x, type = "response")

# Prediction and evaluation on test data
predictions_test <- predict(ridge_reg, s = optimal_lambda, newx = x_test, type = "response")


plotROC(actuals=y_train, predictedScores=predictions_train, returnSensitivityMat = F)
confusionMatrix(train$Win, predictions_train)
misClassError(train$Win, predictions_train)


plotROC(actuals=y_test, predictedScores=predictions_test, returnSensitivityMat = F)
confusionMatrix(test$Win, predictions_test)
misClassError(test$Win, predictions_test)




### lasso model
lambdas <- 10^seq(2, -3, by = -.1)

# Setting alpha = 1 implements lasso regression
lasso_reg <- cv.glmnet(x, y_train, alpha = 1, family = 'binomial', lambda = lambdas, standardize = T, nfolds = 5)

# Best 
lambda_best <- lasso_reg$lambda.min 
lambda_best



lasso_model <- glmnet(x, y_train, alpha = 1, family = 'binomial', lambda = lambda_best, standardize = TRUE)
assess.glmnet(lasso_model, x, y_train)

# Prediction and evaluation on train data
predictions_train <- as.numeric(predict(lasso_model, s = lambda_best, newx = x, type = 'response'))

# Prediction and evaluation on test data
predictions_test <- as.numeric(predict(lasso_model, s = lambda_best, newx = x_test, type = 'response'))


plotROC(actuals=y_train, predictedScores=predictions_train, returnSensitivityMat = F)
confusionMatrix(train$Win, predictions_train)
misClassError(train$Win, predictions_train)


plotROC(actuals=y_test, predictedScores=predictions_test, returnSensitivityMat = F)
confusionMatrix(test$Win, predictions_test)
misClassError(test$Win, predictions_test)



# Elastic Net
# Set training control
train_cont <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 5,
                           search = "random",
                           verboseIter = TRUE)

# Train the model
elastic_reg <- train(Win ~ .,
                     data = train,
                     method = "glmnet",
                     preProcess = c("center", "scale"),
                     tuneLength = 10,
                     trControl = train_cont)



# Best tuning parameter
elastic_reg$bestTune

# Make predictions on training set
predictions_train <- predict(elastic_reg, x)

# Make predictions on test set
predictions_test <- predict(elastic_reg, x_test)


plotROC(actuals=y_train, predictedScores=predictions_train, returnSensitivityMat = F)
confusionMatrix(train$Win, predictions_train)
misClassError(train$Win, predictions_train)


plotROC(actuals=y_test, predictedScores=predictions_test, returnSensitivityMat = F)
confusionMatrix(test$Win, predictions_test)
misClassError(test$Win, predictions_test)










# logistic - in depth
# plotROC(actuals=test$Win, predictedScores=predictions, returnSensitivityMat = TRUE)
# 
# InformationValue::sensitivity(actuals=test$Win, predictedScores=predictions)
# InformationValue::specificity(actuals=test$Win, predictedScores=predictions)
# 
# InformationValue::precision(actuals=test$Win, predictedScores=predictions)
# InformationValue::npv(actuals=test$Win, predictedScores=predictions)
# 
# InformationValue::youdensIndex(actuals=test$Win, predictedScores=predictions)
# InformationValue::misClassError(actuals=test$Win, predictedScores=predictions)
# 
# InformationValue::Concordance(actuals=test$Win, predictedScores=predictions)
# InformationValue::somersD(actuals=test$Win, predictedScores=predictions)    
# 
# InformationValue::ks_stat(actuals=test$Win, predictedScores=predictions)    
# InformationValue::ks_plot(actuals=test$Win, predictedScores=predictions)    
# 
# InformationValue::optimalCutoff(actuals=test$Win, predictedScores=predictions) # returns cutoff that gives minimum misclassification error
# InformationValue::optimalCutoff(actuals=test$Win, predictedScores=predictions, optimiseFor = "Both")  # returns cutoff that gives maximum of Youden's J Index)






#############
#### KNN #### - class
#############


# error
calc_class_err = function(actual, predicted) {
    mean(actual != predicted)
}


set.seed(214)
k_to_try = 1:250
err_k = rep(x = 0, times = length(k_to_try))

for (i in seq_along(k_to_try)) {
    pred = knn(train = train[,-1],
               test  = test[,-1],
               cl    = train$Win,
               k     = k_to_try[i])
    err_k[i] = calc_class_err(test$Win, pred)
}


# plot error vs choice of k
plot(err_k, type = "b", col = "dodgerblue", cex = 1, pch = 20, 
     xlab = "k, number of neighbors", ylab = "classification error",
     main = "(Test) Error Rate vs Neighbors")
# add line for min error seen
abline(h = min(err_k), col = "darkorange", lty = 3)
# add line for minority prevalence in test set
abline(h = mean(test$Win == "Yes"), col = "grey", lty = 2)

min(err_k)

which(err_k == min(err_k))

max(which(err_k == min(err_k)))

table(test$Win)

mean(test$Win == 1)

calc_class_err(actual = test$Win,
               predicted = knn(train = train[,-1],
                               test  = test[,-1],
                               cl    = train$Win,
                               k     = 179))



#################
##### Tuning ####
#################

train$Win <- as.factor(train$Win)
test$Win <- as.factor(test$Win)

# ensemble classification style 
calc_acc = function(actual, predicted) {
    mean(actual == predicted)
}

# classification
# random forest
oob = trainControl(method = "oob")
cv_5 = trainControl(method = "cv", number = 5)

dim(train)

rf_grid =  expand.grid(mtry = 1:26)

set.seed(214)
nba_rf_tune = train(Win ~ ., data = train,
                    method = "rf", #ranger
                    trControl = oob,
                    verbose = T,
                    tuneGrid = rf_grid)
nba_rf_tune

calc_acc(predict(nba_rf_tune, test), test$Win)

nba_rf_tune$bestTune

# boosting
gbm_grid =  expand.grid(interaction.depth = 1:5,
                        n.trees = (1:6) * 500,
                        shrinkage = c(0.001, 0.01, 0.1),
                        n.minobsinnode = 10)

nba_gbm_tune = train(Win ~ ., data = train,
                     method = "gbm",
                     trControl = cv_5,
                     verbose = FALSE,
                     tuneGrid = gbm_grid)

plot(nba_gbm_tune)

calc_acc(predict(nba_gbm_tune, test), test$Win)

nba_gbm_tune$bestTune






##################
#### Ensemble ####
##################

library(rpart)
library(rpart.plot)
library(randomForest)
library(gbm)
library(caret)
library(MASS)


# ensemble classification style 
calc_acc = function(actual, predicted) {
    mean(actual != predicted)
}


# tree
nba_tree = rpart(Win ~ ., data = train)

rpart.plot(nba_tree)

nba_tree_tst_pred = predict(nba_tree, test, type = "class")
table(predicted = nba_tree_tst_pred, actual = test$Win)

(tree_tst_acc = calc_acc(predicted = nba_tree_tst_pred, actual = test$Win))

# logistic
nba_glm = glm(Win ~ ., data = train, family = "binomial")

nba_glm_tst_pred = ifelse(predict(nba_glm, test, "response") > 0.5, 
                          1, 0)
table(predicted = nba_glm_tst_pred, actual = test$Win)

(glm_tst_acc = calc_acc(predicted = nba_glm_tst_pred, actual = test$Win))

# bagging
nba_bag = randomForest(Win ~ ., data = train, mtry = 26, 
                       importance = TRUE, ntrees = 500)
nba_bag

nba_bag_tst_pred = predict(nba_bag, newdata = test)
table(predicted = nba_bag_tst_pred, actual = test$Win)

(bag_tst_acc = calc_acc(predicted = nba_bag_tst_pred, actual = test$Win))

# random forest
nba_forest = randomForest(Win ~ ., data = train, mtry = 4, importance = TRUE, ntrees = 500)
nba_forest

nba_forest_tst_perd = predict(nba_forest, newdata = test)
table(predicted = nba_forest_tst_perd, actual = test$Win)

(forest_tst_acc = calc_acc(predicted = nba_forest_tst_perd, actual = test$Win))

# boosting
nba_trn_mod = train
nba_trn_mod$Win = as.numeric(ifelse(nba_trn_mod$Win == 1, 1, 0))

nba_boost = gbm(Win ~ ., data = nba_trn_mod, distribution = "bernoulli", 
                n.trees = 2500, interaction.depth = 1, shrinkage = 0.01)
nba_boost

nba_boost_tst_pred = ifelse(predict(nba_boost, test, n.trees = 2500, "response") > 0.5, 
                            1, 0)
table(predicted = nba_boost_tst_pred, actual = test$Win)

(boost_tst_acc = calc_acc(predicted = nba_boost_tst_pred, actual = test$Win))

# results
(nba_acc = data.frame(
    Model = c("Single Tree", "Logistic Regression", "Bagging",  "Random Forest",  "Boosting"),
    TestAccuracy = c(tree_tst_acc, glm_tst_acc, bag_tst_acc, forest_tst_acc, boost_tst_acc)
)
)


#############
#### SVM #### - classification
#############

nba_cla$Win <- as.factor(nba_cla$Win)
levels(nba_cla$Win)=c("No","Yes")

# Setup for cross validation
ctrl <- trainControl(method="repeatedcv",   # 10fold cross validation
                     repeats=3,         # do 5 repetitions of cv
                     summaryFunction=twoClassSummary,   # Use AUC to pick the best model
                     classProbs=TRUE)


#Train and Tune the SVM
svm.tune <- train(Win ~ .,
                  data = train,
                  method = "svmLinear",   # Radial kernel
                  tuneLength = 10,       # 5 values of the cost function
                  metric="ROC",
                  trControl=ctrl)

svm.tune


modelsvm = svm(Win ~., train, cost=0.005, kernel = "linear")
predYsvm = predict(modelsvm, test)
misClassError(test$Win, predYsvm)



############
#### NN #### - classification
############

nn_model <- neuralnet(Win ~., data = train, linear.output = F, rep = 1, threshold = .01, stepmax = 1e7)
plot(nn_model)

nn_win <- compute(nn_model, test)

win_error <- misClassError(test$Win, nn_win$net.result)
win_error


#############
#### XGB #### - classification
#############

# Randomly select 80% of the observations without replacement 
set.seed(214)
train_id <- sample(1:nrow(train), size = floor(0.8 * nrow(train)), replace=FALSE) 

# Split in training and validation (80/20)
training <- train[train_id,]
validation <- train[-train_id,]



# Returns the NA object unchanged, if not changed, NA would be dropped
options(na.action='na.pass')

# Prepare matrix for XGBoost algorithm
training_matrix <- model.matrix(Win ~.-1, data = training)
validation_matrix <- model.matrix(Win ~.-1, data = validation)
test_matrix <-model.matrix(~.-1, data = test)
dtrain <- xgb.DMatrix(data = training_matrix, label = training$Win) 
dvalid <- xgb.DMatrix(data = validation_matrix, label = validation$Win)
dtest <- xgb.DMatrix(data = test_matrix)


# Base XGBoost model
set.seed(214)
params <- list(booster = "gbtree", 
               objective = "binary:logistic")
xgb_base <- xgb.train (params = params,
                       data = dtrain,
                       nrounds = 1000,
                       print_every_n = 10,
                       eval_metric = "auc",
                       eval_metric = "error",
                       early_stopping_rounds = 50,
                       watchlist = list(train= dtrain, val= dvalid))



# Make prediction on dvalid
validation$pred_win_base <- predict(xgb_base, dvalid)
validation$pred_win_factor_base <- factor(ifelse(validation$pred_win_base > 0.5, 1, 0), 
                                          labels=c("Loss","Win"))

# Check accuracy with the confusion matrix
confusionMatrix(validation$pred_win_factor_base, 
                factor(validation$Win ,
                       labels=c("Loss", "Win")),
                positive = "Win", 
                dnn = c("Prediction", "Actual Data"))

# Take start time to measure time of random search algorithm
start.time <- Sys.time()

# Create empty lists
lowest_error_list = list()
parameters_list = list()

# Create 10,000 rows with random hyperparameters
set.seed(214)
for (iter in 1:5000){
    param <- list(booster = "gbtree",
                  objective = "binary:logistic",
                  max_depth = sample(3:10, 1),
                  eta = runif(1, .01, .3),
                  subsample = runif(1, .7, 1),
                  colsample_bytree = runif(1, .6, 1),
                  min_child_weight = sample(0:10, 1)
    )
    parameters <- as.data.frame(param)
    parameters_list[[iter]] <- parameters
}

# Create object that contains all randomly created hyperparameters
parameters_df = do.call(rbind, parameters_list)

# Use randomly created parameters to create 10,000 XGBoost-models
for (row in 1:nrow(parameters_df)){
    set.seed(214)
    mdcv <- xgb.train(data=dtrain,
                      booster = "gbtree",
                      objective = "binary:logistic",
                      max_depth = parameters_df$max_depth[row],
                      eta = parameters_df$eta[row],
                      subsample = parameters_df$subsample[row],
                      colsample_bytree = parameters_df$colsample_bytree[row],
                      min_child_weight = parameters_df$min_child_weight[row],
                      nrounds= 300,
                      eval_metric = "error",
                      early_stopping_rounds= 30,
                      print_every_n = 100,
                      watchlist = list(train= dtrain, val= dvalid)
    )
    lowest_error <- as.data.frame(1 - min(mdcv$evaluation_log$val_error))
    lowest_error_list[[row]] <- lowest_error
}

# Create object that contains all accuracy's
lowest_error_df = do.call(rbind, lowest_error_list)

# Bind columns of accuracy values and random hyperparameter values
randomsearch = cbind(lowest_error_df, parameters_df)

# Quickly display highest accuracy
max(randomsearch$`1 - min(mdcv$evaluation_log$val_error)`)

# Stop time and calculate difference
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken




# Prepare table
randomsearch <- as.data.frame(randomsearch) %>%
    rename(val_acc = `1 - min(mdcv$evaluation_log$val_error)`) %>%
    arrange(-val_acc)

# eFG
# params <- list(booster = "gbtree", 
#                objective = "binary:logistic",
#                max_depth = 4,
#                eta = 0.1480,
#                subsample = 0.7353,
#                colsample_bytree = 0.9768,
#                min_child_weight = 4)

# TS
# params <- list(booster = "gbtree", 
#                objective = "binary:logistic",
#                max_depth = 10,
#                eta = 0.2233,
#                subsample = 0.8744,
#                colsample_bytree = 0.8035,
#                min_child_weight = 6)


# Tuned-XGBoost model
set.seed(214)
params <- list(booster = "gbtree", 
               objective = "binary:logistic",
               max_depth = randomsearch[1,]$max_depth,
               eta = randomsearch[1,]$eta,
               subsample = randomsearch[1,]$subsample,
               colsample_bytree = randomsearch[1,]$colsample_bytree,
               min_child_weight = randomsearch[1,]$min_child_weight)
xgb_tuned <- xgb.train(params = params,
                       data = dtrain,
                       nrounds = 1000,
                       print_every_n = 10,
                       eval_metric = "auc",
                       eval_metric = "error",
                       early_stopping_rounds = 30,
                       watchlist = list(train= dtrain, val= dvalid))

# Make prediction on dvalid
validation$pred_win_tuned <- predict(xgb_tuned, dvalid)
validation$pred_win_factor_tuned <- factor(ifelse(validation$pred_win_tuned > 0.5, 1, 0), 
                                           labels=c("Loss","Win"))

# Check accuracy with the confusion matrix
confusionMatrix(validation$pred_win_factor_tuned, 
                factor(validation$Win ,
                       labels=c("Loss", "Win")),
                positive = "Win", 
                dnn = c("Prediction", "Actual Data"))





########### Totals ------------------------------------------------
# regression nba stats

## no correlation - eFG
nba_reg <- nba %>%
    select(6,
           14,15,16,18,19,21,23,30,34,35,37,39,43,
           49,50,51,53,54,56,58,65,69,70,72,74,78)

## no correlation - ts
nba_reg <- nba %>%
    select(6,
           14,15,16,18,19,21,24,30,34,35,37,40,43,
           49,50,51,53,54,56,59,65,69,70,72,75,78)



set.seed(214)
sample <- sample.split(nba_reg$HS, SplitRatio = .70)
train <- nba_reg %>% filter(sample == TRUE)
test <- nba_reg %>% filter(sample == FALSE)

pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))

train[,-1] = predict(pre_proc_val, train[,-1])
test[,-1] = predict(pre_proc_val, test[,-1])

summary(train)




################
#### Linear #### - regression
################

lin_mod <- lm(HS ~., data = train)
summary(lin_mod)


#Step 1 - create the evaluation metrics function
eval_metrics <- function(model, df, predictions, target) {
    resids = df[,target] - predictions
    resids2 = resids**2
    N = length(predictions)
    r2 = as.character(round(summary(model)$r.squared, 4))
    adj_r2 = as.character(round(summary(model)$adj.r.squared, 4))
    print(adj_r2) #Adjusted R-squared
    print(as.character(round(sqrt(sum(resids2)/N), 4))) #RMSE
}
# Step 2 - predicting and evaluating the model on train data
predictions = predict(lin_mod, newdata = train)
eval_metrics(lin_mod, train, predictions, target = 'HS')

# Step 3 - predicting and evaluating the model on test data
predictions = predict(lin_mod, newdata = test)
eval_metrics(lin_mod, test, predictions, target = 'HS')



### Ridge & Lasso models
dummies <- dummyVars(HS ~ ., data = nba_reg)
train_dummies = predict(dummies, newdata = train)
test_dummies = predict(dummies, newdata = test)
print(dim(train_dummies)); print(dim(test_dummies))

x = as.matrix(train_dummies)
y_train = train$HS

x_test = as.matrix(test_dummies)
y_test = test$HS

# Compute R^2 from true and predicted values
eval_results <- function(true, predicted, df) {
    SSE <- sum((predicted - true)^2)
    SST <- sum((true - mean(true))^2)
    R_square <- 1 - SSE / SST
    RMSE = sqrt(SSE/nrow(df))
    
    
    # Model performance metrics
    data.frame(
        Rsquare = R_square,
        RMSE = RMSE
    )
    
}


### Ridge model
lambdas <- 10^seq(2, -3, by = -.1)
ridge_reg = glmnet(x, y_train, nlambda = 25, alpha = 0, family = 'gaussian', lambda = lambdas)

par(mfrow = c(1, 2))
plot(ridge_reg)
plot(ridge_reg, xvar = "lambda", label = TRUE)

summary(ridge_reg)


cv_ridge <- cv.glmnet(x, y_train, alpha = 0, lambda = lambdas)
plot(cv_ridge)

optimal_lambda <- cv_ridge$lambda.min
optimal_lambda


# Prediction and evaluation on train data
predictions_train <- predict(ridge_reg, s = optimal_lambda, newx = x)
eval_results(y_train, predictions_train, train)

# Prediction and evaluation on test data
predictions_test <- predict(ridge_reg, s = optimal_lambda, newx = x_test)
eval_results(y_test, predictions_test, test)




# Lasso model
lambdas <- 10^seq(2, -3, by = -.1)

# Setting alpha = 1 implements lasso regression
lasso_reg <- cv.glmnet(x, y_train, alpha = 1, lambda = lambdas, standardize = TRUE, nfolds = 5,
                       family='gaussian', type.measure='mse')
plot(lasso_reg)
plot(lasso_reg$glmnet.fit, xvar="lambda", label=TRUE)

cat('Min Lambda: ', lasso_reg$lambda.min, '\n 1Sd Lambda: ', lasso_reg$lambda.1se)
df_coef <- round(as.matrix(coef(lasso_reg, s=lasso_reg$lambda.min)), 2)

# See all contributing variables
df_coef[df_coef[, 1] != 0, ]

# Best 
lambda_best <- lasso_reg$lambda.min 
lambda_best


lasso_model <- glmnet(x, y_train, alpha = 1, lambda = lambda_best, standardize = TRUE, family='gaussian')

predictions_train <- predict(lasso_model, s = lambda_best, newx = x)
eval_results(y_train, predictions_train, train)

predictions_test <- predict(lasso_model, s = lambda_best, newx = x_test)
eval_results(y_test, predictions_test, test)



# Elastic Net
# Set training control
train_cont <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 5,
                           search = "random",
                           verboseIter = TRUE)

# Train the model
elastic_reg <- train(HS ~ .,
                     data = train,
                     method = "glmnet",
                     preProcess = c("center", "scale"),
                     tuneLength = 10,
                     trControl = train_cont)



# Best tuning parameter
elastic_reg$bestTune

# Make predictions on training set
predictions_train <- predict(elastic_reg, x)
eval_results(y_train, predictions_train, train) 

# Make predictions on test set
predictions_test <- predict(elastic_reg, x_test)
eval_results(y_test, predictions_test, test)




#############
#### KNN #### - regression
#############


# rmse function
rmse = function(actual, predicted) {
    sqrt(mean((actual - predicted) ^ 2))
}

# define helper function for getting knn.reg predictions
# note: this function is highly specific to this situation and dataset
make_knn_pred = function(k = 1, training, predicting) {
    pred = FNN::knn.reg(as.data.frame(training[,-1]), 
                        as.data.frame(predicting[,-1]), 
                        y = as.numeric(train$HS), k = k)$pred
    act  = predicting$HS
    rmse(predicted = pred, actual = act)
}


# define values of k to evaluate
# k = seq(21,91,5)
k = 1:101

# get requested train RMSEs
knn_trn_rmse = sapply(k, make_knn_pred, 
                      training = train, 
                      predicting = train)
# get requested test RMSEs
knn_tst_rmse = sapply(k, make_knn_pred, 
                      training = train, 
                      predicting = test)

# determine "best" k
best_k = k[which.min(knn_tst_rmse)]

# find overfitting, underfitting, and "best"" k
fit_status = ifelse(k < best_k, "Over", ifelse(k == best_k, "Best", "Under"))


# summarize results
knn_results = data.frame(
    k,
    round(knn_trn_rmse, 2),
    round(knn_tst_rmse, 2),
    fit_status
)
colnames(knn_results) = c("k", "Train RMSE", "Test RMSE", "Fit?")

# display results
knitr::kable(knn_results, escape = FALSE, booktabs = TRUE)





knn_model <- FNN::knn.reg(train = as.data.frame(train[,-1]), 
                          test = as.data.frame(test[,-1]), 
                          y = as.numeric(train$HS), 
                          k = best_k)
pred_y  = knn_model$pred


mse = mean((as.numeric(unlist(test[,1])) - pred_y)^2)
mae = caret::MAE(as.numeric(unlist(test[,1])), pred_y)
rmse = caret::RMSE(as.numeric(unlist(test[,1])), pred_y)

cat("MSE: ", mse, "MAE: ", mae, " RMSE: ", rmse)






#################
##### Tuning ####
#################

calc_rmse = function(actual, predicted) {
    sqrt(mean((actual - predicted) ^ 2))
}

# regression
# random forest
oob = trainControl(method = "oob")
cv_5 = trainControl(method = "cv", number = 5)

dim(train)

rf_grid =  expand.grid(mtry = 1:26)

set.seed(214)
nba_rf_tune = train(HS ~ ., data = train,
                    method = "rf", #ranger
                    trControl = oob,
                    verbose = T,
                    tuneGrid = rf_grid)
nba_rf_tune

calc_rmse(predict(nba_rf_tune, test), test$HS)

nba_rf_tune$bestTune

# boosting
gbm_grid =  expand.grid(interaction.depth = 1:5,
                        n.trees = (1:6) * 500,
                        shrinkage = c(0.001, 0.01, 0.1),
                        n.minobsinnode = 10)

nba_gbm_tune = train(HS ~ ., data = train,
                     method = "gbm",
                     trControl = cv_5,
                     verbose = FALSE,
                     tuneGrid = gbm_grid)

plot(nba_gbm_tune)

calc_rmse(predict(nba_gbm_tune, test), test$HS)

nba_gbm_tune$bestTune



##################
#### Ensemble ####
##################


# ensemble regression style

library(rpart)
library(rpart.plot)
library(randomForest)
library(gbm)
library(caret)
library(MASS)



# tree
nba_tree = rpart(HS ~ ., data = train)

nba_tree_tst_pred = predict(nba_tree, newdata = test)
plot(nba_tree_tst_pred, test$HS, 
     xlab = "Predicted", ylab = "Actual", 
     main = "Predicted vs Actual: Single Tree, Test Data",
     col = "dodgerblue", pch = 20)
grid()
abline(0, 1, col = "darkorange", lwd = 2)

(tree_tst_rmse = calc_rmse(nba_tree_tst_pred, test$HS))

# linear
nba_lm = lm(HS ~ ., data = train)

nba_lm_tst_pred = predict(nba_lm, newdata = test)

plot(nba_lm_tst_pred, test$HS,
     xlab = "Predicted", ylab = "Actual",
     main = "Predicted vs Actual: Linear Model, Test Data",
     col = "dodgerblue", pch = 20)
grid()
abline(0, 1, col = "darkorange", lwd = 2)

(lm_tst_rmse = calc_rmse(nba_lm_tst_pred, test$HS))

# bagging
nba_bag = randomForest(HS ~ ., data = train, mtry = 26, 
                       importance = TRUE, ntrees = 500)
nba_bag

nba_bag_tst_pred = predict(nba_bag, newdata = test)
plot(nba_bag_tst_pred,test$HS,
     xlab = "Predicted", ylab = "Actual",
     main = "Predicted vs Actual: Bagged Model, Test Data",
     col = "dodgerblue", pch = 20)
grid()
abline(0, 1, col = "darkorange", lwd = 2)

(bag_tst_rmse = calc_rmse(nba_bag_tst_pred, test$HS))

plot(nba_bag, col = "dodgerblue", lwd = 2, main = "Bagged Trees: Error vs Number of Trees")
grid()

# random forest
nba_forest = randomForest(HS ~ ., data = train, mtry = 6, 
                          importance = TRUE, ntrees = 500)
nba_forest

importance(nba_forest, type = 1)

varImpPlot(nba_forest, type = 1)

nba_forest_tst_pred = predict(nba_forest, newdata = test)
plot(nba_forest_tst_pred, test$HS,
     xlab = "Predicted", ylab = "Actual",
     main = "Predicted vs Actual: Random Forest, Test Data",
     col = "dodgerblue", pch = 20)
grid()
abline(0, 1, col = "darkorange", lwd = 2)

(forest_tst_rmse = calc_rmse(nba_forest_tst_pred, test$HS))

nba_forest_trn_pred = predict(nba_forest, newdata = train)
forest_trn_rmse = calc_rmse(nba_forest_trn_pred, train$HS)
forest_oob_rmse = calc_rmse(nba_forest$predicted, train$HS)

# boosting
nba_boost = gbm(HS ~ ., data = train, distribution = "gaussian", 
                n.trees = 1500, interaction.depth = 3, shrinkage = 0.01)
nba_boost

tibble::as_tibble(summary(nba_boost))

par(mfrow = c(1, 3))
plot(nba_boost, i = "oeFG_home", col = "dodgerblue", lwd = 2)

plot(nba_boost, i = "oeFG_away", col = "dodgerblue", lwd = 2)

plot(nba_boost, i = "eFG_home", col = "dodgerblue", lwd = 2)

plot(nba_boost, i = "eFG_away", col = "dodgerblue", lwd = 2)

nba_boost_tst_pred = predict(nba_boost, newdata = test, n.trees = 1500)
(boost_tst_rmse = calc_rmse(nba_boost_tst_pred, test$HS))

plot(nba_boost_tst_pred, test$HS,
     xlab = "Predicted", ylab = "Actual", 
     main = "Predicted vs Actual: Boosted Model, Test Data",
     col = "dodgerblue", pch = 20)
grid()
abline(0, 1, col = "darkorange", lwd = 2)

# results
(nba_rmse = data.frame(
    Model = c("Single Tree", "Linear Model", "Bagging",  "Random Forest",  "Boosting"),
    TestError = c(tree_tst_rmse, lm_tst_rmse, bag_tst_rmse, forest_tst_rmse, boost_tst_rmse)
)
)


#############
#### SVM #### - regression
#############

# Setup for cross validation
ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 3)         

# grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
grid <- expand.grid(C = c(10, 5))
svm.tune <- train(HS ~ .,
                  data = train,
                  method = "svmLinear",
                  tuneLength = 10,      
                  tuneGrid = grid,
                  trControl=ctrl)

svm.tune
plot(svm.tune)



modelsvm = svm(HS ~., train, cost=10, kernel = "linear")
predsvm = predict(modelsvm, test)
RMSEsvm = RMSE(predsvm, test$HS)


############
#### NN #### - regression
############

nn_model <- neuralnet(HS ~., data = train, linear.output = T, rep = 1, threshold = .8, stepmax = 1e7)
plot(nn_model)

nn_margin <- compute(nn_model, test)

margin_rmse <- RMSE(nn_margin$net.result, test$Margin)
margin_rmse



































### calibration -----------------------------

log_mod <- rms::lrm(Win ~., data = train, x=T, y=T)
cal <- rms::calibrate(log_mod)
plot(cal, legend=F)
legend(x=.6, y=.6, legend=c("Apparent", "Bias-corrected", "Ideal"), 
       lty=c(3, 1, 2), bty="n")

lin_mod <- rms::ols(Margin ~., data = train, x=T, y=T)
cal <- rms::calibrate(lin_mod)
plot(cal, legend=T)





