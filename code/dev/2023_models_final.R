### model tuning ----

library(tidyverse)
library(RSQLite) # db
library(DBI) # db
library(caret) # model training
library(tidymodels) # model eval
# library(MLeval) # model eval
# library(pROC) # model eval
library(ggfortify) # autoplot
library(glmnet) # regularization
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
summary(win_mx[upper.tri(cor_mx_new)])


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
cor_mx <- cor(model_win_cor, model_win_cor$team_winner)
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
summary(lin_team) # model components
autoplot(lin_team$finalModel) # viz - ggfortify
glance(lin_team$finalModel) # entire model - tidymodels
tidy(lin_team$finalModel) # model components - tidymodels
augment(lin_team$finalModel) # observations - tidymodels

# predictions
team_pred <- predict(lin_team, test)

# model evaluation
postResample(pred = team_pred, obs = test$team_score) # caret eval

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
summary(lin_opp) # model components
autoplot(lin_opp$finalModel) # viz - ggfortify
glance(lin_opp$finalModel) # entire model - tidymodels
tidy(lin_opp$finalModel) # model components - tidymodels
augment(lin_opp$finalModel) # observations - tidymodels

# predictions
opp_pred <- predict(lin_opp, test)

# model evaluation
postResample(pred = opp_pred, obs = test$opp_score) # caret eval

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

# feature importance
importance <- varImp(reg_win, scale = F)
plot(importance)

reg_win_imp <- rownames_to_column(importance[["importance"]], "Var") %>%
    arrange(desc(Overall)) %>%
    head(20)
reg_win_imp


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
grid <- expand.grid(
    alpha = 0, 
    lambda = 10^seq(2, -3, by = -.1)
)
reg_team <- train(team_score ~., data = train,
                  method = "glmnet",
                  trControl = ctrl,
                  tuneGrid = grid)

reg_team
summary(reg_team) # model components
autoplot(reg_team$finalModel) # viz - ggfortify
glance(reg_team$finalModel) # entire model - tidymodels
tidy(reg_team$finalModel) # model components - tidymodels

# predictions
team_pred <- predict(reg_team, test)

# model evaluation
postResample(pred = team_pred, obs = test$team_score) # caret eval

# variable importance
importance <- varImp(reg_team, scale = F)
plot(importance)

reg_team_imp <- rownames_to_column(importance[["importance"]], "Var") %>%
    arrange(desc(Overall)) %>%
    head(20)
reg_team_imp


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
grid <- expand.grid(
    alpha = 0, 
    lambda = 10^seq(2, -3, by = -.1)
)
reg_opp <- train(opp_score ~., data = train,
                 method = "glmnet",
                 trControl = ctrl,
                 tuneGrid = grid)

reg_opp
summary(reg_opp) # model components
autoplot(reg_opp$finalModel) # viz - ggfortify
glance(reg_opp$finalModel) # entire model - tidymodels
tidy(reg_opp$finalModel) # model components - tidymodels

# predictions
opp_pred <- predict(reg_opp, test)

# model evaluation
postResample(pred = opp_pred, obs = test$opp_score) # caret eval

# variable importance
importance <- varImp(lin_opp, scale = F)
plot(importance)

reg_opp_imp <- rownames_to_column(importance[["importance"]], "Var") %>%
    arrange(desc(Overall)) %>%
    head(20)
reg_opp_imp







