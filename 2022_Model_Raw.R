if (!require("pacman")) install.packages("pacman"); library(pacman)
pacman::p_load(tidyverse, readxl, na.tools, caTools, Amelia, lubridate, hms,
               ggthemes, ggrepel, ggimage, XML, RCurl, openxlsx,
               rvest, nflfastR, nbastatR, nbaTools, data.table,
               here, skimr, janitor, SimDesign, zoo, future,
               corrgram, corrplot, tidymodels, broom, ggfortify)
library(glmnet)
library(caret)
library(InformationValue)
library(class)
# nortest, multcomp, agricolae, coin, DTK, mutoss)

# install.packages("devtools")
# library(devtools)
# devtools::install_github("abresler/nbastatR")
# devtools::install_github("ccagrawal/nbaTools")

rm(list=ls())
# nba <- read_xlsx("../Jesse/Documents/MyStuff/NBA Betting/NBAdb/NBAdb1722.xlsx")
# setwd("/Users/Jesse/Documents/MyStuff/NBA Analysis")

nba <- read_xlsx("/Users/Jesse/Documents/MyStuff/NBA Betting/NBAdb/NBAdb1722.xlsx")

    
##### Kendall - Linear Model #####

# nba_lin <- nba %>%
#     select(7,13:16,18:22,42:43,48:51,53:57,77:78,23,58)
# nba_lin <- nba %>%
#     select(7,13:16,18:22,42:43,48:51,53:57,77:78,24,59)
nba_lin <- nba %>%
    select(7,14,15,16,18,19,21,23,30,34,35,37,39,43,
             49,50,51,53,54,56,58,65,69,70,72,74,78)

#################################################################################
#### https://www.machinelearningplus.com/machine-learning/feature-selection/ ####
#################################################################################

## 1. Boruta ***
library(Boruta)

# Perform Boruta search
boruta_output <- Boruta(Margin ~ ., data=nba_lin, doTrace=2, maxRuns = 15) 

# Get significant variables including tentatives
boruta_signif <- getSelectedAttributes(boruta_output, withTentative = TRUE)
print(boruta_signif)  

# Do a tentative rough fix
roughFixMod <- TentativeRoughFix(boruta_output)
boruta_signif <- getSelectedAttributes(roughFixMod)
print(boruta_signif)

# Variable Importance Scores
imps <- attStats(roughFixMod)
imps2 = imps[imps$decision != 'Rejected', c('meanImp', 'decision')]
head(imps2[order(-imps2$meanImp), ])  # descending sort

# Plot variable importance
plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance") 


## 2. Variable Importance from Machine Learning Algorithms - use short list
library(caret)

# Train an rpart model and compute variable importance
set.seed(100)
rPartMod <- train(Margin ~ ., data=nba_lin, method="rpart")
rpartImp <- varImp(rPartMod)
print(rpartImp)

# Train an RRF model and compute variable importance.
set.seed(100)
rrfMod <- train(Margin ~ ., data=nba_lin, method="RRF")
rrfImp <- varImp(rrfMod, scale=F)
rrfImp

plot(rrfImp, top = 20, main='Variable Importance')



## 3. Lasso Regression ***
library(glmnet)

x <- as.matrix(nba_lin[,-1]) # all X vars
y <- as.double(as.matrix(nba_lin[, 1])) # Only Class

# Fit the LASSO model (Lasso: Alpha = 1)
set.seed(100)
cv.lasso <- cv.glmnet(x, y, family='gaussian', alpha=1, type.measure='mse')

# Results
plot(cv.lasso)

# plot(cv.lasso$glmnet.fit, xvar="lambda", label=TRUE)
cat('Min Lambda: ', cv.lasso$lambda.min, '\n 1Sd Lambda: ', cv.lasso$lambda.1se)
df_coef <- round(as.matrix(coef(cv.lasso, s=cv.lasso$lambda.min)), 2)

# See all contributing variables
df_coef[df_coef[, 1] != 0, ]



## 4. Step wise Forward and Backward Selection ***
# Step 1: Define base intercept only model
base.mod <- lm(Margin ~ 1 , data=nba_lin)  

# Step 2: Full model with all predictors
all.mod <- lm(Margin ~ . , data= nba_lin) 

# Step 3: Perform step-wise algorithm. direction='both' implies both forward and backward stepwise
stepMod <- stats::step(base.mod, scope = list(lower = base.mod, upper = all.mod), direction = "both", trace = 0, steps = 1000)  

# Step 4: Get the shortlisted variable.
shortlistedVars <- names(unlist(stepMod[[1]])) 
shortlistedVars <- shortlistedVars[!shortlistedVars %in% "(Intercept)"] # remove intercept

# Show
print(shortlistedVars)


## 5. Relative Importance from Linear Regression - bootsub is long computing
library(relaimpo)

# Build linear regression model
lmMod <- lm(Margin~., data=nba_lin)

# calculate relative importance
relImportance <- calc.relimp(lmMod, type = "lmg", rela = F)  

# Sort
cat('Relative Importances: \n')
sort(round(relImportance$lmg, 3), decreasing=TRUE)


bootsub <- boot.relimp(Margin~., data=nba_lin,
                       b = 1000, type = 'lmg', rank = TRUE, diff = TRUE)

plot(booteval.relimp(bootsub, level=.95))


## 6. Recursive Feature Elimination (RFE) - long computing
library(caret)

set.seed(100)
options(warn=-1)

subsets <- c(1:5, 10, 15, 18)

ctrl <- rfeControl(functions = rfFuncs,
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)

lmProfile <- rfe(x=nba_lin[,-1], y=nba_lin$Margin,
                 sizes = subsets,
                 rfeControl = ctrl)

lmProfile



## 7. Genetic Algorithm - long computing
# Define control function
ga_ctrl <- gafsControl(functions = rfGA,  # another option is `caretGA`.
                       method = "cv",
                       repeats = 3)

# Genetic Algorithm feature selection
set.seed(100)
ga_obj <- gafs(x=nba_lin[, -1], 
               y=nba_lin[[1]], 
               iters = 3,   # normally much higher (100+)
               gafsControl = ga_ctrl)

ga_obj

# Optimal variables
ga_obj$optVariables



## 8. Simulated Annealing - long computing
# Define control function
sa_ctrl <- safsControl(functions = rfSA,
                       method = "repeatedcv",
                       repeats = 3,
                       improve = 5) # n iterations without improvement before a reset

# Genetic Algorithm feature selection
set.seed(100)
sa_obj <- safs(x=nba_lin[, -1], 
               y=nba_lin[[1]],
               safsControl = sa_ctrl)

sa_obj

# Optimal variables
print(sa_obj$optVariables)



## 9. Information Value and Weights of Evidence - doesn't work
library(InformationValue)
inputData <- read.csv("http://rstatistics.net/wp-content/uploads/2015/09/adult.csv")
print(head(inputData))

# Choose Categorical Variables to compute Info Value.
cat_vars <- c ("WORKCLASS", "EDUCATION", "MARITALSTATUS", "OCCUPATION", "RELATIONSHIP", "RACE", "SEX", "NATIVECOUNTRY")  # get all categorical variables

# Init Output
df_iv <- data.frame(VARS=cat_vars, IV=numeric(length(cat_vars)), STRENGTH=character(length(cat_vars)), stringsAsFactors = F)  # init output dataframe

# Get Information Value for each variable
for (factor_var in factor_vars){
    df_iv[df_iv$VARS == factor_var, "IV"] <- InformationValue::IV(X=inputData[, factor_var], Y=inputData$ABOVE50K)
    df_iv[df_iv$VARS == factor_var, "STRENGTH"] <- attr(InformationValue::IV(X=inputData[, factor_var], Y=inputData$ABOVE50K), "howgood")
}

# Sort
df_iv <- df_iv[order(-df_iv$IV), ]

df_iv

WOETable(X=inputData[, 'WORKCLASS'], Y=inputData$ABOVE50K)



## 10. DALEX Package ***
library(randomForest)
library(DALEX)

# Load data
inputData <- nba_lin

# Train random forest model
rf_mod <- randomForest(Margin ~ ., data=inputData, ntree=100)
rf_mod

# Variable importance with DALEX
explained_rf <- explain(rf_mod, data=inputData, y=inputData$Margin)

# Get the variable importances
varimps = variable_importance(explained_rf, type='raw')

# variable_dropout() -> variable_importance()
# single_variable() -> variable_response()
# single_prediction() -> prediction_breakdown()

print(varimps)

plot(varimps)




##############################################################################
#### http://r-statistics.co/Variable-Selection-and-Importance-With-R.html ####
##############################################################################


## 1. Random Forest Method
library(party)

cf1 <- cforest(Margin ~ . , 
               data = nba_lin, control=cforest_unbiased(mtry=2,ntree=500)) # fit the random forest

varimp(cf1) # get variable importance, based on mean decrease in accuracy

varimp(cf1, conditional=TRUE)  # conditional=True, adjusts for correlations between predictors

varimpAUC(cf1)  # more robust towards class imbalance.



## 4. MARS
library(earth)

marsModel <- earth(Margin ~ ., data=nba_lin) # build model
ev <- evimp(marsModel) # estimate variable importance
ev

plot(ev)



##############################################################################################################
#### https://towardsdatascience.com/selecting-the-best-predictors-for-linear-regression-in-r-f385bf3d93e9 ####
##############################################################################################################

## leaps
library(leaps)

best_lin <- regsubsets(Margin~.,
                       data = nba_lin,
                       nbest = 1,      # 1 best model for each number of predictors
                       nvmax = NULL,    # NULL for no limit on number of variables
                       force.in = NULL, force.out = NULL,
                       really.big = T,
                       method = "exhaustive")

summary_best_subset <- summary(best_lin)
as.data.frame(summary_best_subset$outmat)

which.max(summary_best_subset$adjr2)
summary_best_subset$which[10,]



##### Kendall - Logistic Model #####

nba_log <- nba %>%
    select(8,14,15,16,18,19,21,23,30,34,35,37,39,43,
           49,50,51,53,54,56,58,65,69,70,72,74,78)


#################################################################################
#### https://www.machinelearningplus.com/machine-learning/feature-selection/ ####
#################################################################################


## 3. Lasso Regression
library(glmnet)

x <- as.matrix(nba_log[,-1]) # all X vars
y <- as.double(as.matrix(nba_log[, 1])) # Only Class

# Fit the LASSO model (Lasso: Alpha = 1)
set.seed(100)
cv.lasso <- cv.glmnet(x, y, family='binomial', alpha=1, parallel=TRUE, standardize=TRUE, type.measure='auc')

# Results
plot(cv.lasso)

# plot(cv.lasso$glmnet.fit, xvar="lambda", label=TRUE)
cat('Min Lambda: ', cv.lasso$lambda.min, '\n 1Sd Lambda: ', cv.lasso$lambda.1se)
df_coef <- round(as.matrix(coef(cv.lasso, s=cv.lasso$lambda.min)), 2)

# See all contributing variables
df_coef[df_coef[, 1] != 0, ]



## Information
library(Information)
library(gridExtra)

set.seed(100)

sample <- sample.split(nba_log$Win, SplitRatio = .70)
train <- nba_log %>% filter(sample == TRUE)
test <- nba_log %>% filter(sample == FALSE)

### Ranking variables using penalized IV  
IV <- create_infotables(data=train,
                   y="Win")

print(IV$Summary, row.names=FALSE)

print(IV$Tables$N_OPEN_REV_ACTS, row.names=FALSE)




##############################################################
#### http://r-statistics.co/Information-Value-With-R.html ####
##############################################################
library(InformationValue)

set.seed(100)
sample <- sample.split(nba_log$Win, SplitRatio = .70)
train <- nba_log %>% filter(sample == TRUE)
test <- nba_log %>% filter(sample == FALSE)

win_fit <- glm(Win ~ ., data = train, family = "binomial")
summary(win_fit)

win_pred <- predict(win_fit, newdata = test, type = "response")



plotROC(actuals=test$Win, predictedScores=win_pred, returnSensitivityMat = TRUE)

InformationValue::sensitivity(actuals=test$Win, predictedScores=win_pred)
InformationValue::specificity(actuals=test$Win, predictedScores=win_pred)

InformationValue::precision(actuals=test$Win, predictedScores=win_pred)
InformationValue::npv(actuals=test$Win, predictedScores=win_pred)

InformationValue::youdensIndex(actuals=test$Win, predictedScores=win_pred)
InformationValue::misClassError(actuals=test$Win, predictedScores=win_pred)

InformationValue::Concordance(actuals=test$Win, predictedScores=win_pred)
InformationValue::somersD(actuals=test$Win, predictedScores=win_pred)    

InformationValue::ks_stat(actuals=test$Win, predictedScores=win_pred)    
InformationValue::ks_plot(actuals=test$Win, predictedScores=win_pred)    

InformationValue::optimalCutoff(actuals=test$Win, predictedScores=win_pred) # returns cutoff that gives minimum misclassification error
InformationValue::optimalCutoff(actuals=test$Win, predictedScores=win_pred, optimiseFor = "Both")  # returns cutoff that gives maximum of Youden's J Index)



## MASS
library(MASS)
step_model <- win_fit %>% MASS::stepAIC(trace = FALSE)
coef(step_model)


# plots
res_wf <- as.data.frame(residuals(win_fit))
ggplot(res_wf,aes(residuals(win_fit))) +  geom_histogram(fill='blue',alpha=0.5, bins = 30)
plot(win_fit)

anova(win_fit)
coef(win_fit)








###### Correlation #####
library(mctest)

model <- lm(Margin ~., data = nba_lin)

## all individual diagnostic measures with correlation matrix
mctest(model, type="i", corr=TRUE)

## VIF and correlation matrix with collinearity detection indication
mctest(model, type="i", method="VIF", corr=TRUE)

## both overall and individual collinearity diagnostics
mctest(model, type="b")
mctest(model, type="b", method="VIF", cor=TRUE)

## all overall and vif with correlation matrix
## VIF and CN desired threshold
## eigenvalues without intercept term
mctest(model, type="b", method="VIF", Inter=FALSE, vif=15, cn=35)

## Individual collinearity diagnostic measures in matrix of 0 or 1
mctest(model, all = TRUE)
mctest(model, method = "VIF", all = TRUE)
mctest(model, type="b", all = TRUE)




cor_mx <- cor(nba[9:78])
corrplot(cor_mx, method = "color", title = "Correlation Matrix", 
         mar=c(0,0,1,0))

cor_marg <- cor(nba_lin, nba_lin$Margin)
cor_marg <- as.matrix(cor_marg[order(cor_marg[,1], decreasing = T),]) # ordered by highest correlation
cor_marg


cor_mx <- cor(nba_log)
corrplot(cor_mx, method = "color", title = "Correlation Matrix", 
         mar=c(0,0,1,0))




setwd("/Users/Jesse/Desktop/")
wb <- createWorkbook()
addWorksheet(wb, sheetName = "cor")
writeData(wb, sheet = "cor", x = cor_mx)
saveWorkbook(wb, file = "/Users/Jesse/Desktop/cor_full2.xlsx")



###################
##### Testing #####
###################


set.seed(100)
sample <- sample.split(nba_lin$Margin, SplitRatio = .70)
train <- nba_lin %>% filter(sample == TRUE)
test <- nba_lin %>% filter(sample == FALSE)

lin_mod <- lm(Margin ~., data = train)
lin_pred <- predict(lin_mod, newdata = test)
sum((test$Margin - lin_pred)^2)/nrow(test)


# Regression diagnostics
model.diag.metrics <- augment(lin_mod)
head(model.diag.metrics)

par(mfrow = c(2, 2))
plot(lin_mod)
autoplot(lin_mod)

# Add observations indices and
# drop some columns (.se.fit, .sigma) for simplification
model.diag.metrics <- model.diag.metrics %>%
    mutate(index = 1:nrow(model.diag.metrics)) %>%
    select(index, everything(), -.sigma)
# Inspect the data
head(model.diag.metrics, 4)

# Cook's distance
plot(lin_mod, 4)
# Residuals vs Leverage
plot(lin_mod, 5)

model.diag.metrics %>%
    top_n(3, wt = .cooksd)






set.seed(100)
sample <- sample.split(nba_log$Win, SplitRatio = .70)
train <- nba_log %>% filter(sample == TRUE)
test <- nba_log %>% filter(sample == FALSE)

log_mod <- glm(Win ~., data = nba_log, family = "binomial")
log_pred <- predict(log_mod, newdata = test, type = "response")
mean(test$Win != round(log_pred,0))















## all
# nba_lin <- nba %>%
#     select(7,9:78)

## no correlation - eFG
nba_lin <- nba %>%
    select(7,
           14,15,16,18,19,21,23,30,34,35,37,39,43,
           49,50,51,53,54,56,58,65,69,70,72,74,78)

## no correlation - ts
nba_lin <- nba %>%
    select(7,
           14,15,16,18,19,21,24,30,34,35,37,40,43,
           49,50,51,53,54,56,59,65,69,70,72,75,78)

## no correlation - rtg
nba_lin <- nba %>%
    select(7,
           14,15,16,18,19,21,30,34,35,37,43,41,42,
           49,50,51,53,54,56,65,69,70,72,78,76,77)











set.seed(214)
sample <- sample.split(nba_lin$Margin, SplitRatio = .70)
train <- nba_lin %>% filter(sample == TRUE)
test <- nba_lin %>% filter(sample == FALSE)


pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))

train[,-1] = predict(pre_proc_val, train[,-1])
test[,-1] = predict(pre_proc_val, test[,-1])

summary(train)


### linear model

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



### ridge model

dummies <- dummyVars(Margin ~ ., data = nba_lin)
train_dummies = predict(dummies, newdata = train)
test_dummies = predict(dummies, newdata = test)
print(dim(train_dummies)); print(dim(test_dummies))



x = as.matrix(train_dummies)
y_train = train$Margin

x_test = as.matrix(test_dummies)
y_test = test$Margin

lambdas <- 10^seq(2, -3, by = -.1)
ridge_reg = glmnet(x, y_train, nlambda = 25, alpha = 0, family = 'gaussian', lambda = lambdas)

summary(ridge_reg)


cv_ridge <- cv.glmnet(x, y_train, alpha = 0, lambda = lambdas)
optimal_lambda <- cv_ridge$lambda.min
optimal_lambda




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

# Prediction and evaluation on train data
predictions_train <- predict(ridge_reg, s = optimal_lambda, newx = x)
eval_results(y_train, predictions_train, train)

# Prediction and evaluation on test data
predictions_test <- predict(ridge_reg, s = optimal_lambda, newx = x_test)
eval_results(y_test, predictions_test, test)




### lasso model

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




### elastic net

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

















## no correlation - eFG
nba_log <- nba %>%
    select(8,
           14,15,16,18,19,21,23,30,34,35,37,39,43,
           49,50,51,53,54,56,58,65,69,70,72,74,78)

## no correlation - ts
nba_log <- nba %>%
    select(8,
           14,15,16,18,19,21,24,30,34,35,37,40,43,
           49,50,51,53,54,56,59,65,69,70,72,75,78)

## no correlation - rtg
nba_log <- nba %>%
    select(8,
           14,15,16,18,19,21,30,34,35,37,43,41,42,
           49,50,51,53,54,56,65,69,70,72,78,76,77)





set.seed(214)
sample <- sample.split(nba_log$Win, SplitRatio = .70)
train <- nba_log %>% filter(sample == TRUE)
test <- nba_log %>% filter(sample == FALSE)


pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))

train[,-1] = predict(pre_proc_val, train[,-1])
test[,-1] = predict(pre_proc_val, test[,-1])



### logistic model

log_mod <- lm(Win ~., data = train)
summary(log_mod)


#Step 1 - create the evaluation metrics function
eval_metrics <- function(model, df, predictions, target) {
    # resids = df[,target] - predictions
    # resids2 = resids**2
    # N = length(predictions)
    r2 = as.character(round(summary(model)$r.squared, 4))
    adj_r2 = as.character(round(summary(model)$adj.r.squared, 4))
    print(adj_r2) #Adjusted R-squared
    print(as.character(mean(df != round(predictions,0)))) #RMSE
}
# Step 2 - predicting and evaluating the model on train data
predictions = predict(log_mod, newdata = train, type = 'response')
eval_metrics(log_mod, train$Win, predictions, target = 'Win')

# Step 3 - predicting and evaluating the model on test data
predictions = predict(log_mod, newdata = test, type = 'response')
eval_metrics(log_mod, test$Win, predictions, target = 'Win')


plotROC(actuals=test$Win, predictedScores=predictions, returnSensitivityMat = F) # .7-.8 = good; .8+ = great
confusionMatrix(test$Win, predictions)
misClassError(test$Win, predictions)





### ridge model

dummies <- dummyVars(Win ~ ., data = nba_log)
train_dummies = predict(dummies, newdata = train)
test_dummies = predict(dummies, newdata = test)
print(dim(train_dummies)); print(dim(test_dummies))



x = as.matrix(train_dummies)
y_train = train$Win

x_test = as.matrix(test_dummies)
y_test = test$Win

lambdas <- 10^seq(2, -3, by = -.1)
ridge_reg = glmnet(x, y_train, nlambda = 25, alpha = 0, family = 'binomial', lambda = lambdas)


cv_ridge <- cv.glmnet(x, y_train, alpha = 0, family = 'binomial', lambda = lambdas)
optimal_lambda <- cv_ridge$lambda.min
optimal_lambda


# Prediction and evaluation on train data
predictions_train <- as.numeric(predict(ridge_reg, s = optimal_lambda, newx = x, type = 'response'))

# Prediction and evaluation on test data
predictions_test <- as.numeric(predict(ridge_reg, s = optimal_lambda, newx = x_test, type = 'response'))


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




# logistic - in depth
plotROC(actuals=test$Win, predictedScores=predictions, returnSensitivityMat = TRUE)

InformationValue::sensitivity(actuals=test$Win, predictedScores=predictions)
InformationValue::specificity(actuals=test$Win, predictedScores=predictions)

InformationValue::precision(actuals=test$Win, predictedScores=predictions)
InformationValue::npv(actuals=test$Win, predictedScores=predictions)

InformationValue::youdensIndex(actuals=test$Win, predictedScores=predictions)
InformationValue::misClassError(actuals=test$Win, predictedScores=predictions)

InformationValue::Concordance(actuals=test$Win, predictedScores=predictions)
InformationValue::somersD(actuals=test$Win, predictedScores=predictions)    

InformationValue::ks_stat(actuals=test$Win, predictedScores=predictions)    
InformationValue::ks_plot(actuals=test$Win, predictedScores=predictions)    

InformationValue::optimalCutoff(actuals=test$Win, predictedScores=predictions) # returns cutoff that gives minimum misclassification error
InformationValue::optimalCutoff(actuals=test$Win, predictedScores=predictions, optimiseFor = "Both")  # returns cutoff that gives maximum of Youden's J Index)




#############
#### KNN #### - regression
#############
## no correlation - eFG
nba_knnr <- nba %>%
    select(7,
           14,15,16,18,19,21,23,30,34,35,37,39,43,
           49,50,51,53,54,56,58,65,69,70,72,74,78)

## no correlation - ts
nba_knnr <- nba %>%
    select(7,
           14,15,16,18,19,21,24,30,34,35,37,40,43,
           49,50,51,53,54,56,59,65,69,70,72,75,78)

## no correlation - rtg
nba_knnr <- nba %>%
    select(7,
           14,15,16,18,19,21,30,34,35,37,43,41,42,
           49,50,51,53,54,56,65,69,70,72,78,76,77)


set.seed(214)
sample <- sample.split(nba_knnr$Margin, SplitRatio = .70)
train <- nba_knnr %>% filter(sample == TRUE)
test <- nba_knnr %>% filter(sample == FALSE)

pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))

train[,-1] <- predict(pre_proc_val, train[,-1])
test[,-1] <- predict(pre_proc_val, test[,-1])


# predicted_margin = NULL
# error_rate = NULL

# for(i in 1:100){
#     set.seed(214)
#     predicted_margin = FNN::knn.reg(train[-1], test[-1], as.numeric(train$Margin), k=i)
#     knnr_margin <- predicted_margin[["pred"]] * (max(nba_knnr$Margin) - min(nba_knnr$Margin)) + min(nba_knnr$Margin)
#     error_rate[i] = mean((knnr_margin - test$Margin ) ^ 2)
# }
# 
# print(error_rate)
# 
# k_values <- 1:100
# error_df <- data.frame(error_rate, k_values)
# order(error_df$error_rate)
# 
# ggplot(error_df, aes(k_values, error_rate)) + geom_point()+ geom_line(lty="dotted", color='red')
# ggplot(test,aes(x=Margin, y=pred)) + geom_point() + stat_smooth(method = "lm") + coord_obs_pred()










# train_x = train[, -1]
# train_y = as.numeric(unlist(train[,1]))
# 
# test_x = test[, -1]
# test_y = as.numeric(unlist(test[,1]))


# knnmodel = knnreg(train_x, train_y, k = 32)
# str(knnmodel)
# 
# pred_y = predict(knnmodel, data.frame(test_x))
# 
# mse = mean((test_y - pred_y)^2)
# mae = caret::MAE(test_y, pred_y)
# rmse = caret::RMSE(test_y, pred_y)
# 
# cat("MSE: ", mse, "MAE: ", mae, " RMSE: ", rmse)


# x = 1:length(test_y)
# x = 1:100
# plot(x, test_y[1:100], col = "red", type = "l", lwd=2,
#      main = "KNN Regression Analysis")
# lines(x, pred_y[1:100], col = "blue", lwd=2)
# legend("topright",  legend = c("actual", "predicted"), 
#        fill = c("red", "blue"), col = 2:3,  adj = c(0, 0.6))
# grid() 






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
k = seq(21,91,5)

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



#############
#### KNN #### - class
#############
## no correlation - eFG
nba_knn <- nba %>%
    select(8,
           14,15,16,18,19,21,23,30,34,35,37,39,43,
           49,50,51,53,54,56,58,65,69,70,72,74,78)

## no correlation - ts
nba_knn <- nba %>%
    select(8,
           14,15,16,18,19,21,24,30,34,35,37,40,43,
           49,50,51,53,54,56,59,65,69,70,72,75,78)

## no correlation - rtg
nba_knn <- nba %>%
    select(8,
           14,15,16,18,19,21,30,34,35,37,43,41,42,
           49,50,51,53,54,56,65,69,70,72,78,76,77)





set.seed(214)
sample <- sample.split(nba_knn$Win, SplitRatio = .70)
train <- nba_knn %>% filter(sample == TRUE)
test <- nba_knn %>% filter(sample == FALSE)


pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))

train[,-1] = predict(pre_proc_val, train[,-1])
test[,-1] = predict(pre_proc_val, test[,-1])


# error
calc_class_err = function(actual, predicted) {
    mean(actual != predicted)
}

calc_class_err(actual = test$Win,
               predicted = knn(train = train[,-1],
                               test  = test[,-1],
                               cl    = train$Win,
                               k     = 125))



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

mean(test$Win == "Yes")










# ridge part 2
X = model.matrix(Margin ~ ., nba_lin)[, -1]
y = nba_lin$Margin

fit = lm(Margin ~ ., nba_lin)
coef(fit)

sum(abs(coef(fit)[-1]))

sum(coef(fit)[-1] ^ 2)



par(mfrow = c(1, 2))
fit_ridge = glmnet(X, y, alpha = 0)
plot(fit_ridge)
plot(fit_ridge, xvar = "lambda", label = TRUE)

fit_ridge_cv = cv.glmnet(X, y, alpha = 0)
plot(fit_ridge_cv)

# fitted coefficients, using 1-SE rule lambda, default behavior
coef(fit_ridge_cv)

# fitted coefficients, using minimum lambda
coef(fit_ridge_cv, s = "lambda.min")

# penalty term using minimum lambda
sum(coef(fit_ridge_cv, s = "lambda.min")[-1] ^ 2)

# fitted coefficients, using 1-SE rule lambda
coef(fit_ridge_cv, s = "lambda.1se")

# penalty term using 1-SE rule lambda
sum(coef(fit_ridge_cv, s = "lambda.1se")[-1] ^ 2)

# predict using minimum lambda
predict(fit_ridge_cv, X, s = "lambda.min")

# predict using 1-SE rule lambda, default behavior
predict(fit_ridge_cv, X)

# calcualte "train error"
mean((y - predict(fit_ridge_cv, X)) ^ 2)

# CV-RMSEs
sqrt(fit_ridge_cv$cvm)

# CV-RMSE using minimum lambda
sqrt(fit_ridge_cv$cvm[fit_ridge_cv$lambda == fit_ridge_cv$lambda.min])

# CV-RMSE using 1-SE rule lambda
sqrt(fit_ridge_cv$cvm[fit_ridge_cv$lambda == fit_ridge_cv$lambda.1se]) 




# lasso part 2
par(mfrow = c(1, 2))
fit_lasso = glmnet(X, y, alpha = 1)
plot(fit_lasso)
plot(fit_lasso, xvar = "lambda", label = TRUE)

fit_lasso_cv = cv.glmnet(X, y, alpha = 1)
plot(fit_lasso_cv)

# fitted coefficients, using 1-SE rule lambda, default behavior
coef(fit_lasso_cv)

# fitted coefficients, using minimum lambda
coef(fit_lasso_cv, s = "lambda.min")

# penalty term using minimum lambda
sum(coef(fit_lasso_cv, s = "lambda.min")[-1] ^ 2)

# fitted coefficients, using 1-SE rule lambda
coef(fit_lasso_cv, s = "lambda.1se")

# penalty term using 1-SE rule lambda
sum(coef(fit_lasso_cv, s = "lambda.1se")[-1] ^ 2)

# predict using minimum lambda
predict(fit_lasso_cv, X, s = "lambda.min")

# predict using 1-SE rule lambda, default behavior
predict(fit_lasso_cv, X)

# calcualte "train error"
mean((y - predict(fit_lasso_cv, X)) ^ 2)

# CV-RMSEs
sqrt(fit_lasso_cv$cvm)

# CV-RMSE using minimum lambda
sqrt(fit_lasso_cv$cvm[fit_lasso_cv$lambda == fit_lasso_cv$lambda.min])

# CV-RMSE using 1-SE rule lambda
sqrt(fit_lasso_cv$cvm[fit_lasso_cv$lambda == fit_lasso_cv$lambda.1se]) 



library(broom)
# the output from the commented line would be immense
# fit_lasso_cv
tidy(fit_lasso_cv)

# the two lambda values of interest
glance(fit_lasso_cv) 





# lasso ML
X = model.matrix(Win ~ ., nba_log)[, -1]
y = nba_log$Win

fit = lm(Win ~ ., nba_log)
coef(fit)

sum(abs(coef(fit)[-1]))

sum(coef(fit)[-1] ^ 2)


library(glmnet)
fit_cv = cv.glmnet(X, y, family = "binomial", alpha = 1)
plot(fit_cv)

head(coef(fit_cv), n = 10)

fit_1se = glmnet(X, y, family = "binomial", lambda = fit_cv$lambda.1se)
which(as.vector(as.matrix(fit_1se$beta)) != 0)

par(mfrow = c(1, 2))
plot(glmnet(X, y, family = "binomial"))
plot(glmnet(X, y, family = "binomial"), xvar = "lambda")

fit_cv$lambda.min

fit_cv$lambda.1se

library(caret)
cv_5 = trainControl(method = "cv", number = 5)
lasso_grid = expand.grid(alpha = 1, 
                         lambda = c(fit_cv$lambda.min, fit_cv$lambda.1se))
lasso_grid

sim_data = data.frame(y, X)
fit_lasso = train(
    factor(y) ~ ., data = sim_data,
    method = "glmnet",
    trControl = cv_5,
    tuneGrid = lasso_grid
)
fit_lasso$results






##################
#### Ensemble ####
##################


# ensemble regression style
calc_rmse = function(actual, predicted) {
    sqrt(mean((actual - predicted) ^ 2))
}

library(rpart)
library(rpart.plot)
library(randomForest)
library(gbm)
library(caret)
library(MASS)
library(ISLR)

set.seed(214)
sample <- sample.split(nba_reg$Margin, SplitRatio = .70)
train <- nba_reg %>% filter(sample == TRUE)
test <- nba_reg %>% filter(sample == FALSE)

pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))

train[,-1] <- predict(pre_proc_val, train[,-1])
test[,-1] <- predict(pre_proc_val, test[,-1])



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
nba_forest = randomForest(Margin ~ ., data = train, mtry = 7, 
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
nba = gbm(Margin ~ ., data = train, distribution = "gaussian", 
                    n.trees = 1500, interaction.depth = 3, shrinkage = 0.01)
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




# ensemble classification style 
calc_acc = function(actual, predicted) {
    mean(actual == predicted)
}

set.seed(214)
sample <- sample.split(nba_cla$Win, SplitRatio = .70)
train <- nba_cla %>% filter(sample == TRUE)
test <- nba_cla %>% filter(sample == FALSE)

pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))

train[,-1] <- predict(pre_proc_val, train[,-1])
test[,-1] <- predict(pre_proc_val, test[,-1])

train$Win <- factor(train$Win)
test$Win <- factor(test$Win)



# tree
nba_tree = rpart(Win ~ ., data = train)

rpart.plot(nba_tree)

nba_tree_tst_pred = predict(nba_tree, test, type = "class")
table(predicted = nba_tree_tst_pred, actual = test$Win)

(tree_tst_acc = calc_acc(predicted = nba_tree_tst_pred, actual = test$Win))

# logistic
nba_glm = glm(Win ~ ., data = train, family = "binomial")

nba_glm_tst_pred = ifelse(predict(nba_glm, test, "response") > 0.5, 
                           "1", "0")
table(predicted = nba_glm_tst_pred, actual = test$Win)

(glm_tst_acc = calc_acc(predicted = nba_glm_tst_pred, actual = test$Win))

# bagging
nba_bag = randomForest(Win ~ ., data = train, mtry = 20, 
                        importance = TRUE, ntrees = 500)
nba_bag

nba_bag_tst_pred = predict(nba_bag, newdata = test)
table(predicted = nba_bag_tst_pred, actual = test$Win)

(bag_tst_acc = calc_acc(predicted = nba_bag_tst_pred, actual = test$Win))

# random forest
nba_forest = randomForest(Win ~ ., data = train, mtry = 20, importance = TRUE, ntrees = 500)
nba_forest

nba_forest_tst_perd = predict(nba_forest, newdata = test)
table(predicted = nba_forest_tst_perd, actual = test$Win)

(forest_tst_acc = calc_acc(predicted = nba_forest_tst_perd, actual = test$Win))

# boosting
nba_trn_mod = train
nba_trn_mod$Win = as.numeric(ifelse(nba_trn_mod$Win == 1, "1", "0"))

nba_boost = gbm(Win ~ ., data = nba_trn_mod, distribution = "bernoulli", 
                 n.trees = 1500, interaction.depth = 2, shrinkage = 0.01)
nba_boost

nba_boost_tst_pred = ifelse(predict(nba_boost, test, n.trees = 1500, "response") > 0.5, 
                             "1", "0")
table(predicted = nba_boost_tst_pred, actual = test$Win)

(boost_tst_acc = calc_acc(predicted = nba_boost_tst_pred, actual = test$Win))

# results
(nba_acc = data.frame(
    Model = c("Single Tree", "Logistic Regression", "Bagging",  "Random Forest",  "Boosting"),
    TestAccuracy = c(tree_tst_acc, glm_tst_acc, bag_tst_acc, forest_tst_acc, boost_tst_acc)
)
)



#################
##### Tuning ####
#################

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








