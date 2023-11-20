### model tuning ----

library(tidyverse)
library(RSQLite) # db
library(DBI) # db
library(caret) # model training
library(tidymodels) # model eval
library(ggfortify) # autoplot

# library(MLeval) # model eval
# library(pROC) # model eval

# library(glmnet) # regularization
# library(ranger) # rf
# library(xgboost) # xgb
# library(e1071) # svm
# library(nnet) # nn

# library(nbastatR)
# library(caTools)

# library(mctest) # correlations
# library(corrplot) # correlations
# library(corrgram) # correlations

# Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)

# pull all historical data
nba_final <- tbl(dbConnect(SQLite(),
                           "../nba_sql_db/nba_db"), "game_logs_adj") %>% 
    collect() %>%
    mutate(game_date = as_date(game_date, origin ="1970-01-01"),
           team_winner = factor(team_winner, levels = c("win", "loss")))

set.seed(214)

# correlations ----

# feature correlations
cor_df <- nba_final %>%
    select(away_implied_prob,
           b2b_first:opp_b2b_second,
           away_fg2m:away_fg3_pct,
           away_fgm:away_opp_fg3_pct,
           away_opp_fgm:away_opp_ftr,
           away_opp_ast:away_pace,
           home_fg2m:home_fg3_pct,
           home_fgm:home_opp_fg3_pct,
           home_opp_fgm:home_opp_ftr,
           home_opp_ast:home_pace)

# check for extreme correlation
cor_mx <- cor(cor_df)
extreme_cor <- sum(abs(cor_mx[upper.tri(cor_mx)]) > .999)
extreme_cor
summary(cor_mx[upper.tri(cor_mx)])

# find highly correlated features
cor_cols <- findCorrelation(cor_mx, cutoff = .5, exact = F, names = T)
cor_cols

# filter highly correlated features
cor_df_new <- cor_df %>% select(-all_of(cor_cols))

# check new set of features for correlation
cor_mx_new <- cor(cor_df_new)
findCorrelation(cor_mx_new, cutoff = .5)
summary(cor_mx_new[upper.tri(cor_mx_new)])

# correlations - win
model_win_all <- nba_final %>%
    select(team_winner, away_implied_prob,
           b2b_first:opp_b2b_second,
           away_fg2m:away_fg3_pct,
           away_fgm:away_opp_fg3_pct,
           away_opp_fgm:away_opp_ftr,
           away_opp_ast:away_pace,
           home_fg2m:home_fg3_pct,
           home_fgm:home_opp_fg3_pct,
           home_opp_fgm:home_opp_ftr,
           home_opp_ast:home_pace) %>%
    mutate(team_winner = if_else(team_winner == "win", 1, 0))

# near zero variables
nearZeroVar(model_win_all, saveMetrics = T)

# filter highly correlated features
model_win <- model_win_all %>% select(-all_of(cor_cols))

# correlations - all variables
cor_mx <- cor(model_win_all, model_win_all$team_winner)
cor_mx <- as.matrix(cor_mx[order(abs(cor_mx[,1]), decreasing = T),])
cor_mx

# correlations - filtered variables
cor_mx <- cor(model_win, model_win$team_winner)
cor_mx <- as.matrix(cor_mx[order(abs(cor_mx[,1]), decreasing = T),])
cor_mx

# correlations - team score
model_ts_all <- nba_final %>%
    select(team_score, away_implied_prob,
           b2b_first:opp_b2b_second,
           away_fg2m:away_fg3_pct,
           away_fgm:away_opp_fg3_pct,
           away_opp_fgm:away_opp_ftr,
           away_opp_ast:away_pace,
           home_fg2m:home_fg3_pct,
           home_fgm:home_opp_fg3_pct,
           home_opp_fgm:home_opp_ftr,
           home_opp_ast:home_pace)

# near zero variables
nearZeroVar(model_ts_all, saveMetrics = T)

# filter highly correlated features
model_ts <- model_ts_all %>% select(-all_of(cor_cols))

# correlations - all variables
cor_mx <- cor(model_ts_all, model_ts_all$team_score)
cor_mx <- as.matrix(cor_mx[order(abs(cor_mx[,1]), decreasing = T),])
cor_mx

# correlations - filtered variables
cor_mx <- cor(model_ts, model_ts$team_score)
cor_mx <- as.matrix(cor_mx[order(abs(cor_mx[,1]), decreasing = T),])
cor_mx

# correlations - opp score
model_os_all <- nba_final %>%
    select(opp_score, away_implied_prob,
           b2b_first:opp_b2b_second,
           away_fg2m:away_fg3_pct,
           away_fgm:away_opp_fg3_pct,
           away_opp_fgm:away_opp_ftr,
           away_opp_ast:away_pace,
           home_fg2m:home_fg3_pct,
           home_fgm:home_opp_fg3_pct,
           home_opp_fgm:home_opp_ftr,
           home_opp_ast:home_pace)

# near zero variables
nearZeroVar(model_os_all, saveMetrics = T)

# filter highly correlated features
model_os <- model_os_all %>% select(-all_of(cor_cols))

# correlations - all variables
cor_mx <- cor(model_os_all, model_os_all$opp_score)
cor_mx <- as.matrix(cor_mx[order(abs(cor_mx[,1]), decreasing = T),])
cor_mx

# correlations - filtered variables
cor_mx <- cor(model_os, model_os$opp_score)
cor_mx <- as.matrix(cor_mx[order(abs(cor_mx[,1]), decreasing = T),])
cor_mx

# clear environment ----
rm(list=ls()[! ls() %in% c("nba_final", "cor_cols")])

# team winner logistic regression model ----

# all features
train <- nba_final %>%
    filter(season <= 2021) %>%
    select(team_winner, away_implied_prob,
           b2b_first:opp_b2b_second,
           away_fg2m:away_fg3_pct,
           away_fgm:away_opp_fg3_pct,
           away_opp_fgm:away_opp_ftr,
           away_opp_ast:away_pace,
           home_fg2m:home_fg3_pct,
           home_fgm:home_opp_fg3_pct,
           home_opp_fgm:home_opp_ftr,
           home_opp_ast:home_pace)

test <- nba_final %>%
    filter(season > 2021) %>%
    select(team_winner, away_implied_prob,
           b2b_first:opp_b2b_second,
           away_fg2m:away_fg3_pct,
           away_fgm:away_opp_fg3_pct,
           away_opp_fgm:away_opp_ftr,
           away_opp_ast:away_pace,
           home_fg2m:home_fg3_pct,
           home_fgm:home_opp_fg3_pct,
           home_opp_fgm:home_opp_ftr,
           home_opp_ast:home_pace)

# highly correlated features removed
train <- train %>% select(-all_of(cor_cols))
test <- test %>% select(-all_of(cor_cols))

# normalize features
pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))

train[,-1] = predict(pre_proc_val, train[,-1])
test[,-1] = predict(pre_proc_val, test[,-1])

# model
ctrl <- trainControl(method = "cv", number = 5, verboseIter = T, 
                     classProbs = T, summaryFunction = twoClassSummary)
log_win <- train(team_winner ~., data = train,
                 trControl = ctrl,
                 method = "glm",
                 metric = "ROC",
                 family = "binomial")

log_win
log_win$resample
log_win$results
summary(log_win) # model components
confusionMatrix(log_win) # confusion matrix
glance(log_win$finalModel) # entire model - tidymodels
tidy(log_win$finalModel) # model components - tidymodels
augment(log_win$finalModel) # observations - tidymodels

# predictions
win_pred <- predict(log_win, test, type = "prob")
confusionMatrix(test$team_winner,
                factor(ifelse(win_pred[,1] > 0.5, "win", "loss"), 
                       levels = c("win","loss")),
                positive = "win")

away_pred <- as.numeric(win_pred[,1])
home_pred <- as.numeric(win_pred[,2])
obs <- test$team_winner
pred <- factor(ifelse(away_pred > 0.5, "win", "loss"), levels = c("win","loss"))
obs_pred <- data.frame(obs = obs,
                       pred = pred,
                       win = away_pred, 
                       loss = home_pred)

# model evaluation
twoClassSummary(obs_pred, lev = levels(obs)) # roc
prSummary(obs_pred, lev = levels(obs)) # auc
mnLogLoss(obs_pred, lev = levels(obs)) # log loss

roc_score <- pROC::roc(test$team_winner, away_pred, plot = T, legacy.axes = T,
                       percent = T,
                       xlab="False Positive Percentage",
                       ylab="True Postive Percentage",
                       col="#377eb8", lwd = 4, print.auc = T) # roc chart

postResample(pred = pred, obs = obs) # caret eval
log_win_metrics <- postResample(pred = pred, obs = obs)[1]

# feature importance
importance <- varImp(log_win, scale = F)
plot(importance)

log_win_imp <- rownames_to_column(importance[["importance"]], "Var") %>%
    arrange(desc(Overall)) %>%
    head(20)
log_win_imp


# team score linear regression model ----

# all features
train <- nba_final %>%
    filter(season <= 2021) %>%
    select(team_score, away_implied_prob,
           b2b_first:opp_b2b_second,
           away_fg2m:away_fg3_pct,
           away_fgm:away_opp_fg3_pct,
           away_opp_fgm:away_opp_ftr,
           away_opp_ast:away_pace,
           home_fg2m:home_fg3_pct,
           home_fgm:home_opp_fg3_pct,
           home_opp_fgm:home_opp_ftr,
           home_opp_ast:home_pace)

test <- nba_final %>%
    filter(season > 2021) %>%
    select(team_score, away_implied_prob,
           b2b_first:opp_b2b_second,
           away_fg2m:away_fg3_pct,
           away_fgm:away_opp_fg3_pct,
           away_opp_fgm:away_opp_ftr,
           away_opp_ast:away_pace,
           home_fg2m:home_fg3_pct,
           home_fgm:home_opp_fg3_pct,
           home_opp_fgm:home_opp_ftr,
           home_opp_ast:home_pace)

# highly correlated features removed
train <- train %>% select(-all_of(cor_cols))
test <- test %>% select(-all_of(cor_cols))

# normalize features
pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))

train[,-1] = predict(pre_proc_val, train[,-1])
test[,-1] = predict(pre_proc_val, test[,-1])

# model
ctrl <- trainControl(method = "cv", number = 5, verboseIter = T)
lin_team <- train(team_score ~., data = train,
                  trControl = ctrl,
                  method = "lm")

lin_team
lin_team$resample
lin_team$results
summary(lin_team) # model components
autoplot(lin_team$finalModel) # viz - ggfortify
glance(lin_team$finalModel) # entire model - tidymodels
tidy(lin_team$finalModel) # model components - tidymodels
augment(lin_team$finalModel) # observations - tidymodels

# predictions
team_pred <- predict(lin_team, test)

# model evaluation
postResample(pred = team_pred, obs = test$team_score) # caret eval
lin_team_metrics <- postResample(pred = team_pred, obs = test$team_score)[1]

# variable importance
importance <- varImp(lin_team, scale = F)
plot(importance)

lin_team_imp <- rownames_to_column(importance[["importance"]], "Var") %>%
    arrange(desc(Overall)) %>%
    head(20)
lin_team_imp


# opp score linear regression model ----

# all features
train <- nba_final %>%
    filter(season <= 2021) %>%
    select(opp_score, away_implied_prob,
           b2b_first:opp_b2b_second,
           away_fg2m:away_fg3_pct,
           away_fgm:away_opp_fg3_pct,
           away_opp_fgm:away_opp_ftr,
           away_opp_ast:away_pace,
           home_fg2m:home_fg3_pct,
           home_fgm:home_opp_fg3_pct,
           home_opp_fgm:home_opp_ftr,
           home_opp_ast:home_pace)

test <- nba_final %>%
    filter(season > 2021) %>%
    select(opp_score, away_implied_prob,
           b2b_first:opp_b2b_second,
           away_fg2m:away_fg3_pct,
           away_fgm:away_opp_fg3_pct,
           away_opp_fgm:away_opp_ftr,
           away_opp_ast:away_pace,
           home_fg2m:home_fg3_pct,
           home_fgm:home_opp_fg3_pct,
           home_opp_fgm:home_opp_ftr,
           home_opp_ast:home_pace)

# highly correlated features removed
train <- train %>% select(-all_of(cor_cols))
test <- test %>% select(-all_of(cor_cols))

# normalize features
pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))

train[,-1] = predict(pre_proc_val, train[,-1])
test[,-1] = predict(pre_proc_val, test[,-1])

# model
ctrl <- trainControl(method = "cv", number = 5, verboseIter = T)
lin_opp <- train(opp_score ~., data = train,
                 trControl = ctrl,
                 method = "lm")

lin_opp
lin_opp$resample
lin_opp$results
summary(lin_opp) # model components
autoplot(lin_opp$finalModel) # viz - ggfortify
glance(lin_opp$finalModel) # entire model - tidymodels
tidy(lin_opp$finalModel) # model components - tidymodels
augment(lin_opp$finalModel) # observations - tidymodels

# predictions
opp_pred <- predict(lin_opp, test)

# model evaluation
postResample(pred = opp_pred, obs = test$opp_score) # caret eval
lin_opp_metrics <- postResample(pred = team_pred, obs = test$opp_score)[1]

# variable importance
importance <- varImp(lin_opp, scale = F)
plot(importance)

lin_opp_imp <- rownames_to_column(importance[["importance"]], "Var") %>%
    arrange(desc(Overall)) %>%
    head(20)
lin_opp_imp


# team winner ridge regression model ----

# all features
train <- nba_final %>%
    filter(season <= 2021) %>%
    select(team_winner, away_implied_prob,
           b2b_first:opp_b2b_second,
           away_fg2m:away_fg3_pct,
           away_fgm:away_opp_fg3_pct,
           away_opp_fgm:away_opp_ftr,
           away_opp_ast:away_pace,
           home_fg2m:home_fg3_pct,
           home_fgm:home_opp_fg3_pct,
           home_opp_fgm:home_opp_ftr,
           home_opp_ast:home_pace)

test <- nba_final %>%
    filter(season > 2021) %>%
    select(team_winner, away_implied_prob,
           b2b_first:opp_b2b_second,
           away_fg2m:away_fg3_pct,
           away_fgm:away_opp_fg3_pct,
           away_opp_fgm:away_opp_ftr,
           away_opp_ast:away_pace,
           home_fg2m:home_fg3_pct,
           home_fgm:home_opp_fg3_pct,
           home_opp_fgm:home_opp_ftr,
           home_opp_ast:home_pace)

# highly correlated features removed
train <- train %>% select(-all_of(cor_cols))
test <- test %>% select(-all_of(cor_cols))

# normalize features
pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))

train[,-1] = predict(pre_proc_val, train[,-1])
test[,-1] = predict(pre_proc_val, test[,-1])

# model
ctrl <- trainControl(method = "cv", number = 5, verboseIter = T, 
                     classProbs = T, summaryFunction = twoClassSummary)
grid <- expand.grid(
    alpha = 0, # ridge = 0 / lasso = 1
    lambda = 10^seq(2, -3, by = -.1)
)
reg_win <- train(team_winner ~., data = train,
                 method = "glmnet",
                 metric = "ROC",
                 trControl = ctrl,
                 tuneGrid = grid)

reg_win
reg_win$resample
reg_win$results
summary(reg_win) # model components
confusionMatrix(reg_win) # confusion matrix
glance(reg_win$finalModel) # entire model - tidymodels
tidy(reg_win$finalModel) # model components - tidymodels
plot(reg_win) # viz

# predictions
win_pred <- predict(reg_win, test, type = "prob")
confusionMatrix(test$team_winner,
                factor(ifelse(win_pred[,1] > 0.5, "win", "loss"), 
                       levels = c("win","loss")),
                positive = "win")

away_pred <- as.numeric(win_pred[,1])
home_pred <- as.numeric(win_pred[,2])
obs <- test$team_winner
pred <- factor(ifelse(away_pred > 0.5, "win", "loss"), levels = c("win","loss"))
obs_pred <- data.frame(obs = obs,
                       pred = pred,
                       win = away_pred, 
                       loss = home_pred)

# model evaluation
twoClassSummary(obs_pred, lev = levels(obs)) # roc
prSummary(obs_pred, lev = levels(obs)) # auc
mnLogLoss(obs_pred, lev = levels(obs)) # log loss

roc_score <- pROC::roc(test$team_winner, away_pred, plot = T, legacy.axes = T,
                       percent = T,
                       xlab="False Positive Percentage",
                       ylab="True Postive Percentage",
                       col="#377eb8", lwd = 4, print.auc = T) # roc chart

postResample(pred = pred, obs = obs) # caret eval
reg_win_metrics <- postResample(pred = pred, obs = obs)[1]

# feature importance
importance <- varImp(reg_win, scale = F)
plot(importance)

reg_win_imp <- rownames_to_column(importance[["importance"]], "Var") %>%
    arrange(desc(Overall)) %>%
    head(20)
reg_win_imp


# team score ridge regression model ----

# all features
train <- nba_final %>%
    filter(season <= 2021) %>%
    select(team_score, away_implied_prob,
           b2b_first:opp_b2b_second,
           away_fg2m:away_fg3_pct,
           away_fgm:away_opp_fg3_pct,
           away_opp_fgm:away_opp_ftr,
           away_opp_ast:away_pace,
           home_fg2m:home_fg3_pct,
           home_fgm:home_opp_fg3_pct,
           home_opp_fgm:home_opp_ftr,
           home_opp_ast:home_pace)

test <- nba_final %>%
    filter(season > 2021) %>%
    select(team_score, away_implied_prob,
           b2b_first:opp_b2b_second,
           away_fg2m:away_fg3_pct,
           away_fgm:away_opp_fg3_pct,
           away_opp_fgm:away_opp_ftr,
           away_opp_ast:away_pace,
           home_fg2m:home_fg3_pct,
           home_fgm:home_opp_fg3_pct,
           home_opp_fgm:home_opp_ftr,
           home_opp_ast:home_pace)

# highly correlated features removed
train <- train %>% select(-all_of(cor_cols))
test <- test %>% select(-all_of(cor_cols))

# normalize features
pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))

train[,-1] = predict(pre_proc_val, train[,-1])
test[,-1] = predict(pre_proc_val, test[,-1])

# model
ctrl <- trainControl(method = "cv", number = 5, verboseIter = T)
grid <- expand.grid(
    alpha = 0, 
    lambda = 10^seq(2, -3, by = -.1)
)
reg_team <- train(team_score ~., data = train,
                  method = "glmnet",
                  trControl = ctrl,
                  tuneGrid = grid)

reg_team
reg_team$resample
reg_team$results
summary(reg_team) # model components
autoplot(reg_team$finalModel) # viz - ggfortify
glance(reg_team$finalModel) # entire model - tidymodels
tidy(reg_team$finalModel) # model components - tidymodels
plot(reg_team) # viz

# predictions
team_pred <- predict(reg_team, test)

# model evaluation
postResample(pred = team_pred, obs = test$team_score) # caret eval
reg_team_metrics <- postResample(pred = team_pred, obs = test$team_score)[1]

# variable importance
importance <- varImp(reg_team, scale = F)
plot(importance)

reg_team_imp <- rownames_to_column(importance[["importance"]], "Var") %>%
    arrange(desc(Overall)) %>%
    head(20)
reg_team_imp


# opp score ridge regression model ----

# all features
train <- nba_final %>%
    filter(season <= 2021) %>%
    select(opp_score, away_implied_prob,
           b2b_first:opp_b2b_second,
           away_fg2m:away_fg3_pct,
           away_fgm:away_opp_fg3_pct,
           away_opp_fgm:away_opp_ftr,
           away_opp_ast:away_pace,
           home_fg2m:home_fg3_pct,
           home_fgm:home_opp_fg3_pct,
           home_opp_fgm:home_opp_ftr,
           home_opp_ast:home_pace)

test <- nba_final %>%
    filter(season > 2021) %>%
    select(opp_score, away_implied_prob,
           b2b_first:opp_b2b_second,
           away_fg2m:away_fg3_pct,
           away_fgm:away_opp_fg3_pct,
           away_opp_fgm:away_opp_ftr,
           away_opp_ast:away_pace,
           home_fg2m:home_fg3_pct,
           home_fgm:home_opp_fg3_pct,
           home_opp_fgm:home_opp_ftr,
           home_opp_ast:home_pace)

# highly correlated features removed
train <- train %>% select(-all_of(cor_cols))
test <- test %>% select(-all_of(cor_cols))

# normalize features
pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))

train[,-1] = predict(pre_proc_val, train[,-1])
test[,-1] = predict(pre_proc_val, test[,-1])

# model
ctrl <- trainControl(method = "cv", number = 5, verboseIter = T)
grid <- expand.grid(
    alpha = 0, 
    lambda = 10^seq(2, -3, by = -.1)
)
reg_opp <- train(opp_score ~., data = train,
                 method = "glmnet",
                 trControl = ctrl,
                 tuneGrid = grid)

reg_opp
reg_opp$resample
reg_opp$results
summary(reg_opp) # model components
autoplot(reg_opp$finalModel) # viz - ggfortify
glance(reg_opp$finalModel) # entire model - tidymodels
tidy(reg_opp$finalModel) # model components - tidymodels
plot(reg_opp)

# predictions
opp_pred <- predict(reg_opp, test)

# model evaluation
postResample(pred = opp_pred, obs = test$opp_score) # caret eval
reg_opp_metrics <- postResample(pred = team_pred, obs = test$opp_score)[1]

# variable importance
importance <- varImp(reg_opp, scale = F)
plot(importance)

reg_opp_imp <- rownames_to_column(importance[["importance"]], "Var") %>%
    arrange(desc(Overall)) %>%
    head(20)
reg_opp_imp


# team winner knn model ----

# all features
train <- nba_final %>%
    filter(season <= 2021) %>%
    select(team_winner, away_implied_prob,
           b2b_first:opp_b2b_second,
           away_fg2m:away_fg3_pct,
           away_fgm:away_opp_fg3_pct,
           away_opp_fgm:away_opp_ftr,
           away_opp_ast:away_pace,
           home_fg2m:home_fg3_pct,
           home_fgm:home_opp_fg3_pct,
           home_opp_fgm:home_opp_ftr,
           home_opp_ast:home_pace)

test <- nba_final %>%
    filter(season > 2021) %>%
    select(team_winner, away_implied_prob,
           b2b_first:opp_b2b_second,
           away_fg2m:away_fg3_pct,
           away_fgm:away_opp_fg3_pct,
           away_opp_fgm:away_opp_ftr,
           away_opp_ast:away_pace,
           home_fg2m:home_fg3_pct,
           home_fgm:home_opp_fg3_pct,
           home_opp_fgm:home_opp_ftr,
           home_opp_ast:home_pace)

# highly correlated features removed
train <- train %>% select(-all_of(cor_cols))
test <- test %>% select(-all_of(cor_cols))

# normalize features
pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))

train[,-1] = predict(pre_proc_val, train[,-1])
test[,-1] = predict(pre_proc_val, test[,-1])

# model
ctrl <- trainControl(method = "cv", number = 5, verboseIter = T, 
                     classProbs = T, summaryFunction = twoClassSummary)
grid <- expand.grid(
    k = seq(5, 125, 5)
)
knn_win <- train(team_winner ~., data = train, 
                 method = "knn",
                 metric = "ROC",
                 trControl = ctrl,
                 tuneGrid = grid)
knn_win
knn_win$resample
knn_win$results
summary(knn_win) # model components
confusionMatrix(knn_win) # confusion matrix
plot(knn_win)

# predictions
win_pred <- predict(knn_win, test, type = "prob")
confusionMatrix(test$team_winner,
                factor(ifelse(win_pred[,1] > 0.5, "win", "loss"), 
                       levels = c("win","loss")),
                positive = "win")

away_pred <- as.numeric(win_pred[,1])
home_pred <- as.numeric(win_pred[,2])
obs <- test$team_winner
pred <- factor(ifelse(away_pred > 0.5, "win", "loss"), levels = c("win","loss"))
obs_pred <- data.frame(obs = obs,
                       pred = pred,
                       win = away_pred, 
                       loss = home_pred)

# model evaluation
twoClassSummary(obs_pred, lev = levels(obs)) # roc
prSummary(obs_pred, lev = levels(obs)) # auc
mnLogLoss(obs_pred, lev = levels(obs)) # log loss

eval <- MLeval::evalm(data.frame(win_pred, test$team_winner)) # roc chart
roc_score <- pROC::roc(test$team_winner, away_pred, plot = T, legacy.axes = T,
                       percent = T,
                       xlab="False Positive Percentage",
                       ylab="True Postive Percentage",
                       col="#377eb8", lwd = 4, print.auc = T) # roc chart

postResample(pred = pred, obs = obs) # caret eval
knn_win_metrics <- postResample(pred = pred, obs = obs)[1]


# team score knn model ----

# all features
train <- nba_final %>%
    filter(season <= 2021) %>%
    select(team_score, away_implied_prob,
           b2b_first:opp_b2b_second,
           away_fg2m:away_fg3_pct,
           away_fgm:away_opp_fg3_pct,
           away_opp_fgm:away_opp_ftr,
           away_opp_ast:away_pace,
           home_fg2m:home_fg3_pct,
           home_fgm:home_opp_fg3_pct,
           home_opp_fgm:home_opp_ftr,
           home_opp_ast:home_pace)

test <- nba_final %>%
    filter(season > 2021) %>%
    select(team_score, away_implied_prob,
           b2b_first:opp_b2b_second,
           away_fg2m:away_fg3_pct,
           away_fgm:away_opp_fg3_pct,
           away_opp_fgm:away_opp_ftr,
           away_opp_ast:away_pace,
           home_fg2m:home_fg3_pct,
           home_fgm:home_opp_fg3_pct,
           home_opp_fgm:home_opp_ftr,
           home_opp_ast:home_pace)

# highly correlated features removed
train <- train %>% select(-all_of(cor_cols))
test <- test %>% select(-all_of(cor_cols))

# normalize features
pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))

train[,-1] = predict(pre_proc_val, train[,-1])
test[,-1] = predict(pre_proc_val, test[,-1])

# model
ctrl <- trainControl(method = "cv", number = 5, verboseIter = T)
grid <- expand.grid(
    k = seq(2, 50, 2)
)
knn_team <- train(team_score ~., data = train, 
                  method = "knn",
                  trControl = ctrl,
                  tuneGrid = grid)
knn_team
knn_team$results
knn_team$resample
summary(knn_team) # model components
plot(knn_team) # viz

# predictions
team_pred <- predict(knn_team, test)

# model evaluation
postResample(pred = team_pred, obs = test$team_score) # caret eval
knn_team_metrics <- postResample(pred = team_pred, obs = test$team_score)[1]


# opp score knn model ----

# all features
train <- nba_final %>%
    filter(season <= 2021) %>%
    select(opp_score, away_implied_prob,
           b2b_first:opp_b2b_second,
           away_fg2m:away_fg3_pct,
           away_fgm:away_opp_fg3_pct,
           away_opp_fgm:away_opp_ftr,
           away_opp_ast:away_pace,
           home_fg2m:home_fg3_pct,
           home_fgm:home_opp_fg3_pct,
           home_opp_fgm:home_opp_ftr,
           home_opp_ast:home_pace)

test <- nba_final %>%
    filter(season > 2021) %>%
    select(opp_score, away_implied_prob,
           b2b_first:opp_b2b_second,
           away_fg2m:away_fg3_pct,
           away_fgm:away_opp_fg3_pct,
           away_opp_fgm:away_opp_ftr,
           away_opp_ast:away_pace,
           home_fg2m:home_fg3_pct,
           home_fgm:home_opp_fg3_pct,
           home_opp_fgm:home_opp_ftr,
           home_opp_ast:home_pace)

# highly correlated features removed
train <- train %>% select(-all_of(cor_cols))
test <- test %>% select(-all_of(cor_cols))

# normalize features
pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))

train[,-1] = predict(pre_proc_val, train[,-1])
test[,-1] = predict(pre_proc_val, test[,-1])

# model
ctrl <- trainControl(method = "cv", number = 5, verboseIter = T)
grid <- expand.grid(
    k = seq(2, 50, 2)
)
knn_opp <- train(opp_score ~., data = train, 
                 method = "knn",
                 trControl = ctrl,
                 tuneGrid = grid)
knn_opp
knn_opp$results
knn_opp$resample
summary(knn_opp) # model components
plot(knn_opp) # viz

# predictions
opp_pred <- predict(knn_opp, test)

# model evaluation
postResample(pred = opp_pred, obs = test$opp_score) # caret eval
knn_opp_metrics <- postResample(pred = team_pred, obs = test$opp_score)[1]


# team winner random forest model ----

# all features
train <- nba_final %>%
    filter(season <= 2021) %>%
    select(team_winner, away_implied_prob,
           b2b_first:opp_b2b_second,
           away_fg2m:away_fg3_pct,
           away_fgm:away_opp_fg3_pct,
           away_opp_fgm:away_opp_ftr,
           away_opp_ast:away_pace,
           home_fg2m:home_fg3_pct,
           home_fgm:home_opp_fg3_pct,
           home_opp_fgm:home_opp_ftr,
           home_opp_ast:home_pace)

test <- nba_final %>%
    filter(season > 2021) %>%
    select(team_winner, away_implied_prob,
           b2b_first:opp_b2b_second,
           away_fg2m:away_fg3_pct,
           away_fgm:away_opp_fg3_pct,
           away_opp_fgm:away_opp_ftr,
           away_opp_ast:away_pace,
           home_fg2m:home_fg3_pct,
           home_fgm:home_opp_fg3_pct,
           home_opp_fgm:home_opp_ftr,
           home_opp_ast:home_pace)

# highly correlated features removed
train <- train %>% select(-all_of(cor_cols))
test <- test %>% select(-all_of(cor_cols))

# normalize features
pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))

train[,-1] = predict(pre_proc_val, train[,-1])
test[,-1] = predict(pre_proc_val, test[,-1])

# model
ctrl <- trainControl(method = "cv", number = 5, verboseIter = T, 
                     classProbs = T, summaryFunction = twoClassSummary)
grid <- expand.grid(
    .mtry = 1:6,
    .splitrule = "gini",
    .min.node.size = 1
)
rf_win <- train(team_winner ~., data = train,
                method = "ranger",
                metric = "ROC",
                trControl = ctrl,
                tuneGrid = grid)
rf_win
rf_win$resample
rf_win$results
summary(rf_win) # model components
confusionMatrix(rf_win) # confusion matrix
plot(rf_win) # viz

# predictions
win_pred <- predict(rf_win, test, type = "prob")
confusionMatrix(test$team_winner,
                factor(ifelse(win_pred[,1] > 0.5, "win", "loss"), 
                       levels = c("win","loss")),
                positive = "win")

away_pred <- as.numeric(win_pred[,1])
home_pred <- as.numeric(win_pred[,2])
obs <- test$team_winner
pred <- factor(ifelse(away_pred > 0.5, "win", "loss"), levels = c("win","loss"))
obs_pred <- data.frame(obs = obs,
                       pred = pred,
                       win = away_pred, 
                       loss = home_pred)

# model evaluation
twoClassSummary(obs_pred, lev = levels(obs)) # roc
prSummary(obs_pred, lev = levels(obs)) # auc
mnLogLoss(obs_pred, lev = levels(obs)) # log loss

roc_score <- pROC::roc(test$team_winner, away_pred, plot = T, legacy.axes = T,
                       percent = T,
                       xlab="False Positive Percentage",
                       ylab="True Postive Percentage",
                       col="#377eb8", lwd = 4, print.auc = T) # roc chart

postResample(pred = pred, obs = obs) # caret eval
rf_win_metrics <- postResample(pred = pred, obs = obs)[1]


# team score random forest model ----

# all features
train <- nba_final %>%
    filter(season <= 2021) %>%
    select(team_score, away_implied_prob,
           b2b_first:opp_b2b_second,
           away_fg2m:away_fg3_pct,
           away_fgm:away_opp_fg3_pct,
           away_opp_fgm:away_opp_ftr,
           away_opp_ast:away_pace,
           home_fg2m:home_fg3_pct,
           home_fgm:home_opp_fg3_pct,
           home_opp_fgm:home_opp_ftr,
           home_opp_ast:home_pace)

test <- nba_final %>%
    filter(season > 2021) %>%
    select(team_score, away_implied_prob,
           b2b_first:opp_b2b_second,
           away_fg2m:away_fg3_pct,
           away_fgm:away_opp_fg3_pct,
           away_opp_fgm:away_opp_ftr,
           away_opp_ast:away_pace,
           home_fg2m:home_fg3_pct,
           home_fgm:home_opp_fg3_pct,
           home_opp_fgm:home_opp_ftr,
           home_opp_ast:home_pace)

# highly correlated features removed
train <- train %>% select(-all_of(cor_cols))
test <- test %>% select(-all_of(cor_cols))

# normalize features
pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))

train[,-1] = predict(pre_proc_val, train[,-1])
test[,-1] = predict(pre_proc_val, test[,-1])

# model
ctrl <- trainControl(method = "cv", number = 5, verboseIter = T)
grid <- expand.grid(
    .mtry = seq(2, 18, 2),
    .splitrule = "variance",
    .min.node.size = 1
)
rf_team <- train(team_score ~., data = train,
                 method = "ranger",
                 trControl = ctrl,
                 tuneGrid = grid)
rf_team
rf_team$resample
rf_team$results
plot(rf_team) # viz

# predictions
team_pred <- predict(rf_team, test)

# model evaluation
postResample(pred = team_pred, obs = test$team_score) # caret eval
rf_team_metrics <- postResample(pred = team_pred, obs = test$team_score)[1]


# opp score random forest model ----

# all features
train <- nba_final %>%
    filter(season <= 2021) %>%
    select(opp_score, away_implied_prob,
           b2b_first:opp_b2b_second,
           away_fg2m:away_fg3_pct,
           away_fgm:away_opp_fg3_pct,
           away_opp_fgm:away_opp_ftr,
           away_opp_ast:away_pace,
           home_fg2m:home_fg3_pct,
           home_fgm:home_opp_fg3_pct,
           home_opp_fgm:home_opp_ftr,
           home_opp_ast:home_pace)

test <- nba_final %>%
    filter(season > 2021) %>%
    select(opp_score, away_implied_prob,
           b2b_first:opp_b2b_second,
           away_fg2m:away_fg3_pct,
           away_fgm:away_opp_fg3_pct,
           away_opp_fgm:away_opp_ftr,
           away_opp_ast:away_pace,
           home_fg2m:home_fg3_pct,
           home_fgm:home_opp_fg3_pct,
           home_opp_fgm:home_opp_ftr,
           home_opp_ast:home_pace)

# highly correlated features removed
train <- train %>% select(-all_of(cor_cols))
test <- test %>% select(-all_of(cor_cols))

# normalize features
pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))

train[,-1] = predict(pre_proc_val, train[,-1])
test[,-1] = predict(pre_proc_val, test[,-1])

# model
ctrl <- trainControl(method = "cv", number = 5, verboseIter = T)
grid <- expand.grid(
    .mtry = seq(2, 18, 2),
    .splitrule = "variance",
    .min.node.size = 1
)
rf_opp <- train(opp_score ~., data = train,
                 method = "ranger",
                 trControl = ctrl,
                 tuneGrid = grid)
rf_opp
rf_opp$resample
rf_opp$results
plot(rf_opp) # viz

# predictions
opp_pred <- predict(rf_opp, test)

# model evaluation
postResample(pred = opp_pred, obs = test$opp_score) # caret eval
rf_opp_metrics <- postResample(pred = team_pred, obs = test$opp_score)[1]


# team winner support vector machines model ----

# all features
train <- nba_final %>%
    filter(season <= 2021) %>%
    select(team_winner, away_implied_prob,
           b2b_first:opp_b2b_second,
           away_fg2m:away_fg3_pct,
           away_fgm:away_opp_fg3_pct,
           away_opp_fgm:away_opp_ftr,
           away_opp_ast:away_pace,
           home_fg2m:home_fg3_pct,
           home_fgm:home_opp_fg3_pct,
           home_opp_fgm:home_opp_ftr,
           home_opp_ast:home_pace)

test <- nba_final %>%
    filter(season > 2021) %>%
    select(team_winner, away_implied_prob,
           b2b_first:opp_b2b_second,
           away_fg2m:away_fg3_pct,
           away_fgm:away_opp_fg3_pct,
           away_opp_fgm:away_opp_ftr,
           away_opp_ast:away_pace,
           home_fg2m:home_fg3_pct,
           home_fgm:home_opp_fg3_pct,
           home_opp_fgm:home_opp_ftr,
           home_opp_ast:home_pace)

# highly correlated features removed
train <- train %>% select(-all_of(cor_cols))
test <- test %>% select(-all_of(cor_cols))

# normalize features
pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))

train[,-1] = predict(pre_proc_val, train[,-1])
test[,-1] = predict(pre_proc_val, test[,-1])

# model
ctrl <- trainControl(method = "cv", number = 5, verboseIter = T, 
                     classProbs = T, summaryFunction = twoClassSummary)
grid <- expand.grid(
    sigma = c(0.001, 0.005, 0.01, 0.05),
    C = c(0.1, 0.25, 0.5, 0.75)
)
svm_win <- train(team_winner ~., data = train,
                 method = "svmRadial",
                 metric = "ROC",
                 trControl = ctrl,
                 tuneGrid = grid)
svm_win
svm_win$resample
svm_win$results
summary(svm_win) # model components
confusionMatrix(svm_win) # confusion matrix
plot(svm_win) # viz

# predictions
win_pred <- predict(svm_win, test, type = "prob")
confusionMatrix(test$team_winner,
                factor(ifelse(win_pred[,1] > 0.5, "win", "loss"), 
                       levels = c("win","loss")),
                positive = "win")

away_pred <- as.numeric(win_pred[,1])
home_pred <- as.numeric(win_pred[,2])
obs <- test$team_winner
pred <- factor(ifelse(away_pred > 0.5, "win", "loss"), levels = c("win","loss"))
obs_pred <- data.frame(obs = obs,
                       pred = pred,
                       win = away_pred, 
                       loss = home_pred)

# model evaluation
twoClassSummary(obs_pred, lev = levels(obs)) # roc
prSummary(obs_pred, lev = levels(obs)) # auc
mnLogLoss(obs_pred, lev = levels(obs)) # log loss

roc_score <- pROC::roc(test$team_winner, away_pred, plot = T, legacy.axes = T,
                       percent = T,
                       xlab="False Positive Percentage",
                       ylab="True Postive Percentage",
                       col="#377eb8", lwd = 4, print.auc = T) # roc chart

postResample(pred = pred, obs = obs) # caret eval
svm_win_metrics <- postResample(pred = pred, obs = obs)[1]


# team score support vector machines model ----

# all features
train <- nba_final %>%
    filter(season <= 2021) %>%
    select(team_score, away_implied_prob,
           b2b_first:opp_b2b_second,
           away_fg2m:away_fg3_pct,
           away_fgm:away_opp_fg3_pct,
           away_opp_fgm:away_opp_ftr,
           away_opp_ast:away_pace,
           home_fg2m:home_fg3_pct,
           home_fgm:home_opp_fg3_pct,
           home_opp_fgm:home_opp_ftr,
           home_opp_ast:home_pace)

test <- nba_final %>%
    filter(season > 2021) %>%
    select(team_score, away_implied_prob,
           b2b_first:opp_b2b_second,
           away_fg2m:away_fg3_pct,
           away_fgm:away_opp_fg3_pct,
           away_opp_fgm:away_opp_ftr,
           away_opp_ast:away_pace,
           home_fg2m:home_fg3_pct,
           home_fgm:home_opp_fg3_pct,
           home_opp_fgm:home_opp_ftr,
           home_opp_ast:home_pace)

# highly correlated features removed
train <- train %>% select(-all_of(cor_cols))
test <- test %>% select(-all_of(cor_cols))

# normalize features
pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))

train[,-1] = predict(pre_proc_val, train[,-1])
test[,-1] = predict(pre_proc_val, test[,-1])

# model
ctrl <- trainControl(method = "cv", number = 5, verboseIter = T)
grid <- expand.grid(
    sigma = c(0.005, 0.01, 0.05),
    C = c(0.25, 0.5)
)
svm_team <- train(team_score ~., data = train,
                  method = "svmRadial",
                  trControl = ctrl,
                  tuneGrid = grid)
svm_team
svm_team$resample
svm_team$results
summary(svm_team) # model components
plot(svm_team) # viz

# predictions
team_pred <- predict(svm_team, test)

# model evaluation
postResample(pred = team_pred, obs = test$team_score) # caret eval
svm_team_metrics <- postResample(pred = team_pred, obs = test$team_score)[1]


# opp score support vector machines model ----

# all features
train <- nba_final %>%
    filter(season <= 2021) %>%
    select(opp_score, away_implied_prob,
           b2b_first:opp_b2b_second,
           away_fg2m:away_fg3_pct,
           away_fgm:away_opp_fg3_pct,
           away_opp_fgm:away_opp_ftr,
           away_opp_ast:away_pace,
           home_fg2m:home_fg3_pct,
           home_fgm:home_opp_fg3_pct,
           home_opp_fgm:home_opp_ftr,
           home_opp_ast:home_pace)

test <- nba_final %>%
    filter(season > 2021) %>%
    select(opp_score, away_implied_prob,
           b2b_first:opp_b2b_second,
           away_fg2m:away_fg3_pct,
           away_fgm:away_opp_fg3_pct,
           away_opp_fgm:away_opp_ftr,
           away_opp_ast:away_pace,
           home_fg2m:home_fg3_pct,
           home_fgm:home_opp_fg3_pct,
           home_opp_fgm:home_opp_ftr,
           home_opp_ast:home_pace)

# highly correlated features removed
train <- train %>% select(-all_of(cor_cols))
test <- test %>% select(-all_of(cor_cols))

# normalize features
pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))

train[,-1] = predict(pre_proc_val, train[,-1])
test[,-1] = predict(pre_proc_val, test[,-1])

# model
ctrl <- trainControl(method = "cv", number = 5, verboseIter = T)
grid <- expand.grid(
    sigma = c(0.005, 0.01, 0.05),
    C = c(0.25, 0.5)
)
svm_opp <- train(opp_score ~., data = train,
                 method = "svmRadial",
                 trControl = ctrl,
                 tuneGrid = grid)
svm_opp
svm_opp$resample
svm_opp$results
summary(svm_opp) # model components
plot(svm_opp) # viz

# predictions
team_pred <- predict(svm_opp, test)

# model evaluation
postResample(pred = team_pred, obs = test$opp_score) # caret eval
svm_opp_metrics <- postResample(pred = team_pred, obs = test$opp_score)[1]


# team winner neural net model ----

# all features
train <- nba_final %>%
    filter(season <= 2021) %>%
    select(team_winner, away_implied_prob,
           b2b_first:opp_b2b_second,
           away_fg2m:away_fg3_pct,
           away_fgm:away_opp_fg3_pct,
           away_opp_fgm:away_opp_ftr,
           away_opp_ast:away_pace,
           home_fg2m:home_fg3_pct,
           home_fgm:home_opp_fg3_pct,
           home_opp_fgm:home_opp_ftr,
           home_opp_ast:home_pace)

test <- nba_final %>%
    filter(season > 2021) %>%
    select(team_winner, away_implied_prob,
           b2b_first:opp_b2b_second,
           away_fg2m:away_fg3_pct,
           away_fgm:away_opp_fg3_pct,
           away_opp_fgm:away_opp_ftr,
           away_opp_ast:away_pace,
           home_fg2m:home_fg3_pct,
           home_fgm:home_opp_fg3_pct,
           home_opp_fgm:home_opp_ftr,
           home_opp_ast:home_pace)

# highly correlated features removed
train <- train %>% select(-all_of(cor_cols))
test <- test %>% select(-all_of(cor_cols))

# normalize features
pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))

train[,-1] = predict(pre_proc_val, train[,-1])
test[,-1] = predict(pre_proc_val, test[,-1])

# model
ctrl <- trainControl(method = "cv", number = 5, verboseIter = T, 
                     classProbs = T, summaryFunction = twoClassSummary)
grid <- expand.grid(
    decay = c(0.75, 0.5, 0.1, 1e-2, 1e-3, 1e-4, 1e-5),
    size = c(1, 3, 5, 10)
)
nn_win <- train(team_winner ~., data = train,
                method = "nnet",
                metric = "ROC",
                trControl = ctrl,
                tuneGrid = grid)
nn_win
nn_win$resample
nn_win$results
summary(nn_win) # model components
confusionMatrix(nn_win) # confusion matrix
plot(nn_win) # viz

# predictions
win_pred <- predict(nn_win, test, type = "prob")
confusionMatrix(test$team_winner,
                factor(ifelse(win_pred[,1] > 0.5, "win", "loss"), 
                       levels = c("win","loss")),
                positive = "win")

away_pred <- as.numeric(win_pred[,1])
home_pred <- as.numeric(win_pred[,2])
obs <- test$team_winner
pred <- factor(ifelse(away_pred > 0.5, "win", "loss"), levels = c("win","loss"))
obs_pred <- data.frame(obs = obs,
                       pred = pred,
                       win = away_pred, 
                       loss = home_pred)

# model evaluation
twoClassSummary(obs_pred, lev = levels(obs)) # roc
prSummary(obs_pred, lev = levels(obs)) # auc
mnLogLoss(obs_pred, lev = levels(obs)) # log loss

roc_score <- pROC::roc(test$team_winner, away_pred, plot = T, legacy.axes = T,
                       percent = T,
                       xlab="False Positive Percentage",
                       ylab="True Postive Percentage",
                       col="#377eb8", lwd = 4, print.auc = T) # roc chart

postResample(pred = pred, obs = obs) # caret eval

# feature importance
importance <- varImp(nn_win, scale = F)
plot(importance)

nn_win_imp <- rownames_to_column(importance[["importance"]], "Var") %>%
    arrange(desc(Overall)) %>%
    head(20)
nn_win_imp


# team score neural net model ----

# all features
train <- nba_final %>%
    filter(season <= 2021) %>%
    select(team_score, away_implied_prob,
           b2b_first:opp_b2b_second,
           away_fg2m:away_fg3_pct,
           away_fgm:away_opp_fg3_pct,
           away_opp_fgm:away_opp_ftr,
           away_opp_ast:away_pace,
           home_fg2m:home_fg3_pct,
           home_fgm:home_opp_fg3_pct,
           home_opp_fgm:home_opp_ftr,
           home_opp_ast:home_pace)

test <- nba_final %>%
    filter(season > 2021) %>%
    select(team_score, away_implied_prob,
           b2b_first:opp_b2b_second,
           away_fg2m:away_fg3_pct,
           away_fgm:away_opp_fg3_pct,
           away_opp_fgm:away_opp_ftr,
           away_opp_ast:away_pace,
           home_fg2m:home_fg3_pct,
           home_fgm:home_opp_fg3_pct,
           home_opp_fgm:home_opp_ftr,
           home_opp_ast:home_pace)

# highly correlated features removed
train <- train %>% select(-all_of(cor_cols))
test <- test %>% select(-all_of(cor_cols))

# normalize features
pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))

train[,-1] = predict(pre_proc_val, train[,-1])
test[,-1] = predict(pre_proc_val, test[,-1])

# model
ctrl <- trainControl(method = "cv", number = 5, verboseIter = T)
grid <- expand.grid(
    decay = c(0.75, 0.5, 0.1, 1e-2, 1e-3, 1e-4, 1e-5, 1e-6, 1e-7),
    size = c(1, 3, 5, 10, 20)
)
nn_team <- train(team_score ~., data = train,
                 method = "nnet",
                 trControl = ctrl,
                 tuneGrid = grid,
                 linout = 1)
nn_team
nn_team$resample
nn_team$results
summary(nn_team) # model components
plot(nn_team) # viz

# predictions
team_pred <- predict(nn_team, test)

# model evaluation
postResample(pred = team_pred, obs = test$team_score) # caret eval

# variable importance
importance <- varImp(nn_team, scale = F)
plot(importance)

nn_team_imp <- rownames_to_column(importance[["importance"]], "Var") %>%
    arrange(desc(Overall)) %>%
    head(20)
nn_team_imp


# opp score neural net model ----

# all features
train <- nba_final %>%
    filter(season <= 2021) %>%
    select(opp_score, away_implied_prob,
           b2b_first:opp_b2b_second,
           away_fg2m:away_fg3_pct,
           away_fgm:away_opp_fg3_pct,
           away_opp_fgm:away_opp_ftr,
           away_opp_ast:away_pace,
           home_fg2m:home_fg3_pct,
           home_fgm:home_opp_fg3_pct,
           home_opp_fgm:home_opp_ftr,
           home_opp_ast:home_pace)

test <- nba_final %>%
    filter(season > 2021) %>%
    select(opp_score, away_implied_prob,
           b2b_first:opp_b2b_second,
           away_fg2m:away_fg3_pct,
           away_fgm:away_opp_fg3_pct,
           away_opp_fgm:away_opp_ftr,
           away_opp_ast:away_pace,
           home_fg2m:home_fg3_pct,
           home_fgm:home_opp_fg3_pct,
           home_opp_fgm:home_opp_ftr,
           home_opp_ast:home_pace)

# highly correlated features removed
train <- train %>% select(-all_of(cor_cols))
test <- test %>% select(-all_of(cor_cols))

# normalize features
pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))

train[,-1] = predict(pre_proc_val, train[,-1])
test[,-1] = predict(pre_proc_val, test[,-1])

# model
ctrl <- trainControl(method = "cv", number = 5, verboseIter = T)
grid <- expand.grid(
    decay = c(0.75, 0.5, 0.1, 1e-2, 1e-3, 1e-4, 1e-5, 1e-6, 1e-7),
    size = c(1, 3, 5, 10, 20)
)
nn_opp <- train(opp_score ~., data = train,
                method = "nnet",
                trControl = ctrl,
                tuneGrid = grid,
                linout = 1)
nn_opp
nn_opp$resample
nn_opp$results
summary(nn_opp) # model components
plot(nn_opp) # viz

# predictions
team_pred <- predict(nn_opp, test)

# model evaluation
postResample(pred = team_pred, obs = test$opp_score) # caret eval

# variable importance
importance <- varImp(nn_opp, scale = F)
plot(importance)

nn_opp_imp <- rownames_to_column(importance[["importance"]], "Var") %>%
    arrange(desc(Overall)) %>%
    head(20)
nn_opp_imp


# team winner extreme gradient boosting model ----
# all features
train <- nba_final %>%
    filter(season <= 2021) %>%
    select(team_winner, away_implied_prob,
           b2b_first:opp_b2b_second,
           away_fg2m:away_fg3_pct,
           away_fgm:away_opp_fg3_pct,
           away_opp_fgm:away_opp_ftr,
           away_opp_ast:away_pace,
           home_fg2m:home_fg3_pct,
           home_fgm:home_opp_fg3_pct,
           home_opp_fgm:home_opp_ftr,
           home_opp_ast:home_pace)

test <- nba_final %>%
    filter(season > 2021) %>%
    select(team_winner, away_implied_prob,
           b2b_first:opp_b2b_second,
           away_fg2m:away_fg3_pct,
           away_fgm:away_opp_fg3_pct,
           away_opp_fgm:away_opp_ftr,
           away_opp_ast:away_pace,
           home_fg2m:home_fg3_pct,
           home_fgm:home_opp_fg3_pct,
           home_opp_fgm:home_opp_ftr,
           home_opp_ast:home_pace)

# highly correlated features removed
train <- train %>% select(-all_of(cor_cols))
test <- test %>% select(-all_of(cor_cols))

# normalize features
pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))

train[,-1] = predict(pre_proc_val, train[,-1])
test[,-1] = predict(pre_proc_val, test[,-1])

# model
ctrl <- trainControl(method = "cv", number = 5, verboseIter = T, 
                     classProbs = T, summaryFunction = twoClassSummary)
grid <- expand.grid(
    nrounds = xgb_tune$bestTune$nrounds,
    eta = xgb_tune$bestTune$eta,
    max_depth = xgb_tune$bestTune$max_depth,
    gamma = xgb_tune$bestTune$gamma,
    colsample_bytree = xgb_tune$bestTune$colsample_bytree,
    min_child_weight = xgb_tune$bestTune$min_child_weight,
    subsample = xgb_tune$bestTune$subsample
)
xgb_win <- train(team_winner ~., data = train,
                 method = "xgbTree",
                 metric = "ROC",
                 trControl = ctrl,
                 tuneGrid = grid)
xgb_tune <- xgb_win

# helper function for the plots
tuneplot <- function(x, probs = .90) {
    ggplot(x) +
        coord_cartesian(ylim = c(quantile(x$results$ROC, probs = probs), min(x$results$ROC))) +
        theme_bw()
}

tuneplot(xgb_tune)

# predictions
win_pred <- predict(xgb_win, test, type = "prob")

model_outputs <- model_outputs %>%
    mutate(xgb_win_away = as.numeric(win_pred[,1]),
           xgb_win_home = as.numeric(win_pred[,2]))

# team score extreme gradient boosting model ----
# all features
train <- nba_final %>%
    filter(season <= 2021) %>%
    select(team_score, away_implied_prob,
           b2b_first:opp_b2b_second,
           away_fg2m:away_fg3_pct,
           away_fgm:away_opp_fg3_pct,
           away_opp_fgm:away_opp_ftr,
           away_opp_ast:away_pace,
           home_fg2m:home_fg3_pct,
           home_fgm:home_opp_fg3_pct,
           home_opp_fgm:home_opp_ftr,
           home_opp_ast:home_pace)

test <- nba_final %>%
    filter(season > 2021) %>%
    select(team_score, away_implied_prob,
           b2b_first:opp_b2b_second,
           away_fg2m:away_fg3_pct,
           away_fgm:away_opp_fg3_pct,
           away_opp_fgm:away_opp_ftr,
           away_opp_ast:away_pace,
           home_fg2m:home_fg3_pct,
           home_fgm:home_opp_fg3_pct,
           home_opp_fgm:home_opp_ftr,
           home_opp_ast:home_pace)

# highly correlated features removed
train <- train %>% select(-all_of(cor_cols))
test <- test %>% select(-all_of(cor_cols))

# normalize features
pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))

train[,-1] = predict(pre_proc_val, train[,-1])
test[,-1] = predict(pre_proc_val, test[,-1])

# model
ctrl <- trainControl(method = "cv", number = 5, verboseIter = T)
grid <- expand.grid(
    nrounds = xgb_tune$bestTune$nrounds,
    eta = xgb_tune$bestTune$eta,
    max_depth = xgb_tune$bestTune$max_depth,
    gamma = xgb_tune$bestTune$gamma,
    colsample_bytree = xgb_tune$bestTune$colsample_bytree,
    min_child_weight = xgb_tune$bestTune$min_child_weight,
    subsample = xgb_tune$bestTune$subsample
)
xgb_team <- train(team_score ~., data = train,
                  method = "xgbTree",
                  trControl = ctrl,
                  tuneGrid = grid)
xgb_tune <- xgb_team

# helper function for the plots
tuneplot <- function(x, probs = .90) {
    ggplot(x) +
        coord_cartesian(ylim = c(quantile(x$results$RMSE, probs = probs), min(x$results$RMSE))) +
        theme_bw()
}

tuneplot(xgb_tune)

# predictions
team_pred <- predict(xgb_team, test)
model_outputs <- model_outputs %>%
    mutate(xgb_team_score = as.numeric(team_pred))


# opp score extreme gradient boosting model ----
# all features
train <- nba_final %>%
    filter(season <= 2021) %>%
    select(opp_score, away_implied_prob,
           b2b_first:opp_b2b_second,
           away_fg2m:away_fg3_pct,
           away_fgm:away_opp_fg3_pct,
           away_opp_fgm:away_opp_ftr,
           away_opp_ast:away_pace,
           home_fg2m:home_fg3_pct,
           home_fgm:home_opp_fg3_pct,
           home_opp_fgm:home_opp_ftr,
           home_opp_ast:home_pace)

test <- nba_final %>%
    filter(season > 2021) %>%
    select(opp_score, away_implied_prob,
           b2b_first:opp_b2b_second,
           away_fg2m:away_fg3_pct,
           away_fgm:away_opp_fg3_pct,
           away_opp_fgm:away_opp_ftr,
           away_opp_ast:away_pace,
           home_fg2m:home_fg3_pct,
           home_fgm:home_opp_fg3_pct,
           home_opp_fgm:home_opp_ftr,
           home_opp_ast:home_pace)

# highly correlated features removed
train <- train %>% select(-all_of(cor_cols))
test <- test %>% select(-all_of(cor_cols))

# normalize features
pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))

train[,-1] = predict(pre_proc_val, train[,-1])
test[,-1] = predict(pre_proc_val, test[,-1])

# model
ctrl <- trainControl(method = "cv", number = 5, verboseIter = T)
grid <- expand.grid(
    nrounds = xgb_tune$bestTune$nrounds,
    eta = xgb_tune$bestTune$eta,
    max_depth = xgb_tune$bestTune$max_depth,
    gamma = xgb_tune$bestTune$gamma,
    colsample_bytree = xgb_tune$bestTune$colsample_bytree,
    min_child_weight = xgb_tune$bestTune$min_child_weight,
    subsample = xgb_tune$bestTune$subsample
)
xgb_opp <- train(opp_score ~., data = train,
                 method = "xgbTree",
                 trControl = ctrl,
                 tuneGrid = grid)
xgb_tune <- xgb_opp

# helper function for the plots
tuneplot <- function(x, probs = .90) {
    ggplot(x) +
        coord_cartesian(ylim = c(quantile(x$results$RMSE, probs = probs), min(x$results$RMSE))) +
        theme_bw()
}

tuneplot(xgb_tune)

# predictions
opp_pred <- predict(xgb_opp, test)
model_outputs <- model_outputs %>%
    mutate(xgb_opp_score = as.numeric(opp_pred))








