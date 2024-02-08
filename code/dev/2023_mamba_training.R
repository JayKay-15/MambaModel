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

options(scipen = 999999)
# Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)

# https://www.kaggle.com/code/pelkoja/visual-xgboost-tuning-with-caret/report

# pull all historical data
nba_final <- tbl(dbConnect(SQLite(), "../nba_sql_db/nba_db"), "mamba_stats") %>%
    collect() %>%
    rename(team_winner = wl,
           team_score = pts,
           opp_score = opp_pts) %>%
    mutate(game_date = as_date(game_date, origin ="1970-01-01"),
           team_winner = if_else(team_winner == "W", "win", "loss"),
           team_winner = factor(team_winner, levels = c("win", "loss")))


# nba_final <- read_rds("../NBAdb/mamba_stats_w5.rds")
# nba_final <- read_rds("../NBAdb/mamba_stats_w10.rds")
# nba_final <- read_rds("../NBAdb/mamba_stats_w15.rds")
# nba_final <- read_rds("../NBAdb/mamba_stats_w20.rds")

# saveRDS(model_outputs, "./backest_output/model_outputs_w5.rds")
# saveRDS(model_outputs, "./backest_output/model_outputs_w10.rds")
# saveRDS(model_outputs, "./backest_output/model_outputs_w15.rds")
# saveRDS(model_outputs, "./backest_output/model_outputs_w20.rds")

# model_outputs <- read_rds("./backest_output/model_outputs_w15.rds")

nba_final <- nba_final %>%
    filter(season_year >= 2019) %>%
    mutate(game_date = as_date(game_date, origin ="1970-01-01"),
           team_winner = factor(team_winner, levels = c("win", "loss")))

# correlations ----
set.seed(214)

# feature correlations
cor_df <- nba_final %>%
    select(is_b2b_first:opp_is_b2b_second, over_under, away_implied_prob,
           away_fgm:home_opp_pct_uast_fgm) %>%
    select(-contains("_rating"))

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
    select(team_winner, is_b2b_first:opp_is_b2b_second, over_under,
           away_implied_prob, away_fgm:home_opp_pct_uast_fgm) %>%
    select(-contains("_rating")) %>%
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
    select(team_score, is_b2b_first:opp_is_b2b_second, over_under,
           away_implied_prob, away_fgm:home_opp_pct_uast_fgm) %>%
    select(-contains("_rating"))

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
    select(opp_score, is_b2b_first:opp_is_b2b_second, over_under,
           away_implied_prob, away_fgm:home_opp_pct_uast_fgm) %>%
    select(-contains("_rating"))

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

# team winner models ----
# all features
train <- nba_final %>%
    filter(season_year <= 2021) %>%
    select(team_winner, is_b2b_first:opp_is_b2b_second, over_under,
           away_implied_prob, away_fgm:home_opp_pct_uast_fgm) %>%
    select(-contains("_rating"))

test <- nba_final %>%
    filter(season_year > 2021) %>%
    select(team_winner, is_b2b_first:opp_is_b2b_second, over_under,
           away_implied_prob, away_fgm:home_opp_pct_uast_fgm) %>%
    select(-contains("_rating"))

# model_outputs <- nba_final %>%
#     filter(season_year > 2021) %>%
#     select(season_year:home_implied_prob)

# highly correlated features removed
train <- train %>% select(-all_of(cor_cols))
test <- test %>% select(-all_of(cor_cols))

# normalize features
pre_proc_val <- preProcess(train[,-c(1:5)], method = c("center", "scale"))

train[,-c(1:5)] = predict(pre_proc_val, train[,-c(1:5)])
test[,-c(1:5)] = predict(pre_proc_val, test[,-c(1:5)])
 

# team winner logistic regression model ----
# model
ctrl <- trainControl(method = "cv", number = 5, verboseIter = T, 
                     classProbs = T, summaryFunction = twoClassSummary)

log_win <- train(team_winner ~., data = train,
                 trControl = ctrl,
                 method = "glm",
                 metric = "ROC",
                 family = "binomial")

saveRDS(log_win, "../NBAdb/models/models_trained/log_win_2021.rds")

# predictions
win_pred <- predict(log_win, test, type = "prob")

model_outputs <- model_outputs %>%
    mutate(log_win_away = as.numeric(win_pred[,1]),
           log_win_home = as.numeric(win_pred[,2]))


# team winner ridge regression model ----
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

saveRDS(reg_win, "../NBAdb/models/models_trained/reg_win_2021.rds")

# predictions
win_pred <- predict(reg_win, test, type = "prob")

model_outputs <- model_outputs %>%
    mutate(reg_win_away = as.numeric(win_pred[,1]),
           reg_win_home = as.numeric(win_pred[,2]))


# team winner knn model ----
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

saveRDS(knn_win, "../NBAdb/models/models_trained/knn_win_2021.rds")

# predictions
win_pred <- predict(knn_win, test, type = "prob")

model_outputs <- model_outputs %>%
    mutate(knn_win_away = as.numeric(win_pred[,1]),
           knn_win_home = as.numeric(win_pred[,2]))


# team winner random forest model ----
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

saveRDS(rf_win, "../NBAdb/models/models_trained/rf_win_2021.rds")

# predictions
win_pred <- predict(rf_win, test, type = "prob")

model_outputs <- model_outputs %>%
    mutate(rf_win_away = as.numeric(win_pred[,1]),
           rf_win_home = as.numeric(win_pred[,2]))


# team winner support vector machines model ----
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

saveRDS(svm_win, "../NBAdb/models/models_trained/svm_win_2021.rds")

# predictions
win_pred <- predict(svm_win, test, type = "prob")

model_outputs <- model_outputs %>%
    mutate(svm_win_away = as.numeric(win_pred[,1]),
           svm_win_home = as.numeric(win_pred[,2]))

# team winner neural net model ----
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

saveRDS(nn_win, "../NBAdb/models/models_trained/nn_win_2021.rds")

# predictions
win_pred <- predict(nn_win, test, type = "prob")

model_outputs <- model_outputs %>%
    mutate(nn_win_away = as.numeric(win_pred[,1]),
           nn_win_home = as.numeric(win_pred[,2]))

# team winner extreme gradient boosting model ----
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

saveRDS(xgb_win, "../NBAdb/models/models_trained/xgb_win_2021.rds")

# predictions
win_pred <- predict(xgb_win, test, type = "prob")

model_outputs <- model_outputs %>%
    mutate(xgb_win_away = as.numeric(win_pred[,1]),
           xgb_win_home = as.numeric(win_pred[,2]))


# team score models ----
# all features
train <- nba_final %>%
    filter(season_year <= 2021) %>%
    select(team_score, is_b2b_first:opp_is_b2b_second, over_under,
           away_implied_prob, away_fgm:home_opp_pct_uast_fgm) %>%
    select(-contains("_rating"))

test <- nba_final %>%
    filter(season_year > 2021) %>%
    select(team_score, is_b2b_first:opp_is_b2b_second, over_under,
           away_implied_prob, away_fgm:home_opp_pct_uast_fgm) %>%
    select(-contains("_rating"))

# model_outputs <- nba_final %>%
#     filter(season_year > 2021) %>%
#     select(season_year:home_implied_prob)

# highly correlated features removed
train <- train %>% select(-all_of(cor_cols))
test <- test %>% select(-all_of(cor_cols))

# normalize features
pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))

train[,-c(1:5)] = predict(pre_proc_val, train[,-c(1:5)])
test[,-c(1:5)] = predict(pre_proc_val, test[,-c(1:5)])


# team score linear regression model ----
# model
ctrl <- trainControl(method = "cv", number = 5, verboseIter = T)
lin_team <- train(team_score ~., data = train,
                  trControl = ctrl,
                  method = "lm")

saveRDS(lin_team, "../NBAdb/models/models_trained/lin_team_2021.rds")

# predictions
team_pred <- predict(lin_team, test)
model_outputs <- model_outputs %>%
    mutate(lin_team_score = as.numeric(team_pred))


# team score ridge regression model ----
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

saveRDS(reg_team, "../NBAdb/models/models_trained/reg_team_2021.rds")

# predictions
team_pred <- predict(reg_team, test)
model_outputs <- model_outputs %>%
    mutate(reg_team_score = as.numeric(team_pred))


# team score knn model ----
# model
ctrl <- trainControl(method = "cv", number = 5, verboseIter = T)
grid <- expand.grid(
    k = seq(2, 50, 2)
)
knn_team <- train(team_score ~., data = train, 
                  method = "knn",
                  trControl = ctrl,
                  tuneGrid = grid)

saveRDS(knn_team, "../NBAdb/models/models_trained/knn_team_2021.rds")

# predictions
team_pred <- predict(knn_team, test)
model_outputs <- model_outputs %>%
    mutate(knn_team_score = as.numeric(team_pred))


# team score random forest model ----
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

saveRDS(rf_team, "../NBAdb/models/models_trained/rf_team_2021.rds")

# predictions
team_pred <- predict(rf_team, test)
model_outputs <- model_outputs %>%
    mutate(rf_team_score = as.numeric(team_pred))


# team score support vector machines model ----
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

saveRDS(svm_team, "../NBAdb/models/models_trained/svm_team_2021.rds")

# predictions
team_pred <- predict(svm_team, test)
model_outputs <- model_outputs %>%
    mutate(svm_team_score = as.numeric(team_pred))


# team score neural net model ----
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

saveRDS(nn_team, "../NBAdb/models/models_trained/nn_team_2021.rds")

# predictions
team_pred <- predict(nn_team, test)
model_outputs <- model_outputs %>%
    mutate(nn_team_score = as.numeric(team_pred))


# team score extreme gradient boosting model ----
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

saveRDS(xgb_team, "../NBAdb/models/models_trained/xgb_team_2021.rds")

# predictions
team_pred <- predict(xgb_team, test)
model_outputs <- model_outputs %>%
    mutate(xgb_team_score = as.numeric(team_pred))


# opp score models ----
# all features
train <- nba_final %>%
    filter(season_year <= 2021) %>%
    select(opp_score, is_b2b_first:opp_is_b2b_second, over_under,
           away_implied_prob, away_fgm:home_opp_pct_uast_fgm) %>%
    select(-contains("_rating"))

test <- nba_final %>%
    filter(season_year > 2021) %>%
    select(opp_score, is_b2b_first:opp_is_b2b_second, over_under,
           away_implied_prob, away_fgm:home_opp_pct_uast_fgm) %>%
    select(-contains("_rating"))

# model_outputs <- nba_final %>%
#     filter(season_year > 2021) %>%
#     select(season_year:home_implied_prob)

# highly correlated features removed
train <- train %>% select(-all_of(cor_cols))
test <- test %>% select(-all_of(cor_cols))

# normalize features
pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))

train[,-c(1:5)] = predict(pre_proc_val, train[,-c(1:5)])
test[,-c(1:5)] = predict(pre_proc_val, test[,-c(1:5)])


# opp score linear regression model ----
# model
ctrl <- trainControl(method = "cv", number = 5, verboseIter = T)
lin_opp <- train(opp_score ~., data = train,
                 trControl = ctrl,
                 method = "lm")

saveRDS(lin_opp, "../NBAdb/models/models_trained/lin_opp_2021.rds")

# predictions
opp_pred <- predict(lin_opp, test)
model_outputs <- model_outputs %>%
    mutate(lin_opp_score = as.numeric(opp_pred))


# opp score ridge regression model ----
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

saveRDS(reg_opp, "../NBAdb/models/models_trained/reg_opp_2021.rds")

# predictions
opp_pred <- predict(reg_opp, test)
model_outputs <- model_outputs %>%
    mutate(reg_opp_score = as.numeric(opp_pred))


# opp score knn model ----
# model
ctrl <- trainControl(method = "cv", number = 5, verboseIter = T)
grid <- expand.grid(
    k = seq(2, 50, 2)
)
knn_opp <- train(opp_score ~., data = train, 
                 method = "knn",
                 trControl = ctrl,
                 tuneGrid = grid)

saveRDS(knn_opp, "../NBAdb/models/models_trained/knn_opp_2021.rds")

# predictions
opp_pred <- predict(knn_opp, test)
model_outputs <- model_outputs %>%
    mutate(knn_opp_score = as.numeric(opp_pred))


# opp score random forest model ----
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

saveRDS(rf_opp, "../NBAdb/models/models_trained/rf_opp_2021.rds")

# predictions
opp_pred <- predict(rf_opp, test)
model_outputs <- model_outputs %>%
    mutate(rf_opp_score = as.numeric(opp_pred))


# opp score support vector machines model ----
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

saveRDS(svm_opp, "../NBAdb/models/models_trained/svm_opp_2021.rds")

# predictions
opp_pred <- predict(svm_opp, test)
model_outputs <- model_outputs %>%
    mutate(svm_opp_score = as.numeric(opp_pred))


# opp score neural net model ----
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

saveRDS(nn_opp, "../NBAdb/models/models_trained/nn_opp_2021.rds")

# predictions
opp_pred <- predict(nn_opp, test)
model_outputs <- model_outputs %>%
    mutate(nn_opp_score = as.numeric(opp_pred))


# opp score extreme gradient boosting model ----
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

saveRDS(xgb_opp, "../NBAdb/models/models_trained/xgb_opp_2021.rds")

# predictions
opp_pred <- predict(xgb_opp, test)
model_outputs <- model_outputs %>%
    mutate(xgb_opp_score = as.numeric(opp_pred))

# pre-processed stats ----
saveRDS(pre_proc_val,
        "../NBAdb/models/models_trained/pre_proc_val_2019_2021.rds")






# xgb tuning grids ----
grid <- expand.grid(
    nrounds = seq(200, 1000, 50),
    eta = c(0.025, 0.05, 0.1, 0.3),
    max_depth = c(2, 3, 4, 5, 6),
    gamma = 0,
    colsample_bytree = 1,
    min_child_weight = 1,
    subsample = 1
)

grid <- expand.grid(
    nrounds = seq(50, 1000, 50),
    eta = xgb_tune$bestTune$eta,
    max_depth = c(2, 3, 4, 5, 6),
    gamma = 0,
    colsample_bytree = 1,
    min_child_weight = c(1, 2, 3),
    subsample = 1
)

grid <- expand.grid(
    nrounds = seq(50, 1000, 50),
    eta = xgb_tune$bestTune$eta,
    max_depth = xgb_tune$bestTune$max_depth,
    gamma = 0,
    colsample_bytree = c(0.4, 0.6, 0.8, 1.0),
    min_child_weight = xgb_tune$bestTune$min_child_weight,
    subsample = c(0.5, 0.75, 1.0)
)

grid <- expand.grid(
    nrounds = seq(50, 1000, 50),
    eta = xgb_tune$bestTune$eta,
    max_depth = xgb_tune$bestTune$max_depth,
    gamma = c(0, 0.05, 0.1, 0.5, 0.7, 0.9, 1.0),
    colsample_bytree = xgb_tune$bestTune$colsample_bytree,
    min_child_weight = xgb_tune$bestTune$min_child_weight,
    subsample = xgb_tune$bestTune$subsample
)

grid <- expand.grid(
    nrounds = seq(100, 10000, 100),
    eta = c(0.01, 0.015, 0.025, 0.05, 0.1),
    max_depth = xgb_tune$bestTune$max_depth,
    gamma = xgb_tune$bestTune$gamma,
    colsample_bytree = xgb_tune$bestTune$colsample_bytree,
    min_child_weight = xgb_tune$bestTune$min_child_weight,
    subsample = xgb_tune$bestTune$subsample
)

grid <- expand.grid(
    nrounds = xgb_tune$bestTune$nrounds,
    eta = xgb_tune$bestTune$eta,
    max_depth = xgb_tune$bestTune$max_depth,
    gamma = xgb_tune$bestTune$gamma,
    colsample_bytree = xgb_tune$bestTune$colsample_bytree,
    min_child_weight = xgb_tune$bestTune$min_child_weight,
    subsample = xgb_tune$bestTune$subsample
)





# results book
model_outputs <- read_rds("./backest_output/model_outputs_w15.rds")


results_book <- model_outputs %>%
    mutate(
        ens_win_away = rowMeans(select(.,log_win_away,reg_win_away,
                                       svm_win_away,nn_win_away,
                                       xgb_win_away), na.rm = TRUE),
        ens_win_home = 1 - ens_win_away,
        ens_team_score = rowMeans(select(.,lin_team_score,reg_team_score,
                                         svm_team_score,nn_team_score,
                                         xgb_team_score), na.rm = TRUE),
        ens_opp_score = rowMeans(select(.,lin_opp_score,reg_opp_score,
                                        svm_opp_score,nn_opp_score,
                                        xgb_opp_score), na.rm = TRUE),
        away_ml_result = case_when(
            away_moneyline > 0 & team_winner == "win" ~ away_moneyline/100,
            away_moneyline > 0 & team_winner == "loss" ~ -1,
            away_moneyline < 0 & team_winner == "win" ~ 1,
            away_moneyline < 0 & team_winner == "loss" ~ away_moneyline/100),
        home_ml_result = case_when(
            home_moneyline > 0 & team_winner == "loss" ~ home_moneyline/100,
            home_moneyline > 0 & team_winner == "win" ~ -1,
            home_moneyline < 0 & team_winner == "loss" ~ 1,
            home_moneyline < 0 & team_winner == "win" ~ home_moneyline/100),
        away_ats_result = if_else((plus_minus + away_spread) == 0, 0,
                                  if_else((plus_minus + away_spread) > 0,
                                          1, -1.1)),
        home_ats_result = if_else((plus_minus + home_spread) == 0, 0,
                                  if_else((-plus_minus + home_spread) > 0,
                                          1, -1.1)),
        over_game_result = if_else((team_score + opp_score) == 0, 0,
                                   if_else((team_score + opp_score) > over_under,
                                           1, -1.1)),
        under_game_result = if_else((team_score + opp_score) == 0, 0,
                                    if_else((team_score + opp_score) < over_under,
                                            1, -1.1))
    )

win_columns <- names(results_book %>% select(ends_with("win_away")))
team_columns <- names(results_book %>% select(ends_with("_team_score")))
opp_columns <- names(results_book %>% select(ends_with("_opp_score")))


for (i in seq_along(win_columns)) {
    model_edges <- results_book %>%
        mutate(
            win_edge = abs(.data[[win_columns[[i]]]] - away_implied_prob),
            spread_edge = abs((.data[[team_columns[[i]]]] - .data[[opp_columns[[i]]]]) + away_spread),
            over_edge = (.data[[team_columns[[i]]]] + .data[[opp_columns[[i]]]]) - over_under,
            under_edge = over_under - (.data[[team_columns[[i]]]] + .data[[opp_columns[[i]]]]),
            win_result = if_else(.data[[win_columns[[i]]]] - away_implied_prob > 0,
                                 away_ml_result, home_ml_result),
            spread_result = if_else((.data[[team_columns[[i]]]] - .data[[opp_columns[[i]]]]) + away_spread > 0,
                                    away_ats_result, home_ats_result),
            over_under_result = if_else(over_edge > 0,
                                        over_game_result, under_game_result),
            ml_wager = if_else(.data[[win_columns[[i]]]] - away_implied_prob > 0,
                               ifelse(away_moneyline < 100,
                                      away_moneyline/-100, 1),
                               ifelse(home_moneyline < 100,
                                      home_moneyline/-100, 1))
        ) %>%
        select(game_id,win_edge:ml_wager)
    
    colnames(model_edges) <- c(
        "game_id",
        paste0(sub("_win_away$", "", win_columns[i]),"_win_edge"),
        paste0(sub("_team_score$", "", team_columns[i]),"_spread_edge"),
        paste0(sub("_team_score$", "", team_columns[i]),"_over_edge"),
        paste0(sub("_team_score$", "", team_columns[i]),"_under_edge"),
        paste0(sub("_win_away$", "", win_columns[i]),"_win_result"),
        paste0(sub("_team_score$", "", team_columns[i]),"_spread_result"),
        paste0(sub("_team_score$", "", team_columns[i]),"_over_under_result"),
        paste0(sub("_win_away$", "", win_columns[i]),"_ml_wager")
    )
    
    edge_columns <- names(model_edges %>% select(ends_with("edge")))
    result_columns <- names(model_edges %>% select(ends_with("result")))
    result_columns <- c(result_columns, tail(result_columns, 1))
    wager <- names(model_edges %>% select(ends_with("wager")))
    
    # Loop through columns
    for (h in seq_along(edge_columns)) {
        model_stats <- model_edges %>%
            arrange(desc(select(., matches(edge_columns[h])))) %>%
            mutate(
                cume = round(cumsum(.data[[result_columns[[h]]]]), 2),
                games = row_number(),
                roi = case_when(
                    str_ends(edge_columns[[h]], "_win_edge") ~
                        round((cume / cumsum(.data[[wager]])) * 100, 2),
                    !str_ends(edge_columns[[h]], "_win_edge") ~
                        round((cume / (games * 1.1)) * 100, 2)
                ),
                win_pct = (cumsum(if_else(.data[[result_columns[[h]]]] > 0, 1, 0)))/
                    (cumsum(if_else(.data[[result_columns[[h]]]] != 0, 1, 0))),
                across((cume:win_pct), as.numeric)
            ) %>%
            select(cume, games, roi, win_pct)
        
        colnames(model_stats) <- c(
            paste0(sub("_edge$", "", edge_columns[h]),"_cume"),
            paste0(sub("_edge$", "", edge_columns[h]),"_games"),
            paste0(sub("_edge$", "", edge_columns[h]),"_roi"),
            paste0(sub("_edge$", "", edge_columns[h]),"_win_pct")
        )
        
        model_edges <- model_edges %>%
            arrange(desc(select(., matches(edge_columns[h])))) %>%
            bind_cols(model_stats)
    }
    
    results_book <- results_book %>%
        left_join(model_edges)
}





#### function to summarize results book ----
summarize_results_book <- function(results_book) {
    # Create an empty data frame to store results
    result_df <- data.frame()
    
    # Specify column names
    edge_columns <- names(results_book %>% select(ends_with("_edge")))
    cume_columns <- names(results_book %>% select(ends_with("_cume")))
    games_columns <- names(results_book %>% select(ends_with("_games")))
    roi_columns <- names(results_book %>% select(ends_with("_roi")))
    
    # Loop through columns
    for (i in seq_along(edge_columns)) {
        max_value <- results_book %>%
            arrange(desc(select(., matches(cume_columns[i])))) %>%
            slice(1) %>%
            mutate(model = edge_columns[i]) %>%
            select(model, edge = edge_columns[i], value = cume_columns[i],
                   games = games_columns[i], roi = roi_columns[i]) %>%
            mutate(across(c(edge, value, games, roi), as.numeric),
                   model = sub("_edge$", "", model))
        
        # Store values in the result data frame
        result_df <- bind_rows(result_df, max_value)
    }
    
    
    return(result_df)
}

# usage of the function
results_summary <- summarize_results_book(results_book)



#### function to produce model viz ----
model_viz <- function(models_edge, models_result, models_key) {
    
    for (j in seq_along(models_edge)) {
        plot <- results_book %>%
            select(season_year, game_date,
                   models_edge[j], models_result[j]) %>%
            filter(c_across(contains(models_edge[j])) >= models_key[j]) %>%
            group_by(season_year, game_date) %>%
            summarise(day_total_win = sum(c_across(contains(models_result[j])))) %>%
            ungroup() %>%
            mutate(cume_win = cumsum(day_total_win)) %>%
            add_row(season_year = min(model_outputs$season_year),
                    game_date = min(model_outputs$game_date) - 1,
                    day_total_win = 0,
                    cume_win = 0) %>%
            ggplot(aes(x = game_date, y = cume_win, group = season_year)) +
            geom_line() +
            geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "dodgerblue") +
            labs(x = "Game Date", y = "Units Won") +
            labs(title = "Betting Performance by Season",
                 subtitle = paste0(sub("_edge$", "", models_edge[j])),
                 x = "",
                 y = "Units Won") +
            theme_bw() +
            theme(
                text = element_text(size = 12),
                axis.text.x = element_text(angle = 45, hjust = 1),
                legend.position = "top"
            ) +
            facet_wrap(~season_year, scales = "free_x") +
            scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
        
        print(plot)
        
    }
    
}


# usage of the function
models_edge <- c("reg_win_edge", "ens_spread_edge",
                 "ens_over_edge", "ens_under_edge")
models_result <- c("reg_win_result", "ens_spread_result",
                   "ens_over_under_result", "ens_over_under_result")
models_key <- c(0.05781939, 4.30365229, 2.5421957, 1.9372166)

model_viz(models_edge, models_result, models_key)







moneyline_key <- 0.05781939 # regularization (ridge)
spread_key <- 4.30365229 # ensemble (lin, reg, svm, nn, xgb)
over_key <- 2.5421957 # ensemble (lin, reg, svm, nn, xgb)
under_key <- 1.9372166 # ensemble (lin, reg, svm, nn, xgb)




model_outputs %>%
    filter(reg_win_edge >= results_book) %>%
    group_by(season_year, game_date) %>%
    summarise(day_total_win = sum(reg_win_result)) %>%
    ungroup() %>%
    mutate(cume_win = cumsum(day_total_win)) %>%
    add_row(season_year = min(model_outputs$season_year),
            game_date = min(model_outputs$game_date) - 1,
            day_total_win = 0,
            cume_win = 0) %>%
    ggplot(aes(x = game_date, y = cume_win, group = season_year)) +
    geom_line() +
    geom_smooth(method = "lm", se = FALSE,
                linetype = "dashed", color = "dodgerblue") +
    labs(title = "Betting Performance by Season",
         x = "",
         y = "Units Won") +
    theme_bw() +
    theme(
        text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top"
    ) +
    facet_wrap(~season_year, scales = "free_x") +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10))



