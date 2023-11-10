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
nba_final <- tbl(dbConnect(SQLite(), "../nba_sql_db/nba_db"), "mamba_stats") %>%
    collect() %>%
    rename(team_winner = wl,
           team_score = pts,
           opp_score = opp_pts) %>%
    mutate(game_date = as_date(game_date, origin ="1970-01-01"),
           team_winner = if_else(team_winner == "W", "win", "loss"),
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
    select(team_winner, is_b2b_first:opp_is_b2b_second, over_under, away_implied_prob,
           away_fgm:home_opp_pct_uast_fgm) %>%
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
    select(team_score, is_b2b_first:opp_is_b2b_second, over_under, away_implied_prob,
           away_fgm:home_opp_pct_uast_fgm) %>%
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
    select(opp_score, is_b2b_first:opp_is_b2b_second, over_under, away_implied_prob,
           away_fgm:home_opp_pct_uast_fgm) %>%
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
    select(team_winner, is_b2b_first:opp_is_b2b_second, over_under, away_implied_prob,
           away_fgm:home_opp_pct_uast_fgm) %>%
    select(-contains("_rating"))

test <- nba_final %>%
    filter(season_year > 2021) %>%
    select(team_winner, is_b2b_first:opp_is_b2b_second, over_under, away_implied_prob,
           away_fgm:home_opp_pct_uast_fgm) %>%
    select(-contains("_rating"))

model_outputs <- nba_final %>%
    filter(season_year > 2021) %>%
    select(season_year:home_implied_prob)

# highly correlated features removed
train <- train %>% select(-all_of(cor_cols))
test <- test %>% select(-all_of(cor_cols))

# normalize features
pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))

train[,-1] = predict(pre_proc_val, train[,-1])
test[,-1] = predict(pre_proc_val, test[,-1])


# team winner logistic regression model ----
# model
ctrl <- trainControl(method = "cv", number = 5, verboseIter = T, 
                     classProbs = T, summaryFunction = twoClassSummary)

log_win <- train(team_winner ~., data = train,
                 trControl = ctrl,
                 method = "glm",
                 metric = "ROC",
                 family = "binomial")

saveRDS(log_win, "../NBAdb/models/models_2021/log_win_2021.rds")

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

saveRDS(reg_win, "../NBAdb/models/models_2021/reg_win_2021.rds")

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

saveRDS(knn_win, "../NBAdb/models/models_2021/knn_win_2021.rds")

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

saveRDS(rf_win, "../NBAdb/models/models_2021/rf_win_2021.rds")

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

saveRDS(svm_win, "../NBAdb/models/models_2021/svm_win_2021.rds")

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

saveRDS(nn_win, "../NBAdb/models/models_2021/nn_win_2021.rds")

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
    nrounds = seq(300, 800, 100),
    eta = c(0.015, 0.025, 0.035),
    max_depth = c(1,2,3),
    gamma = c(1,2,3,4),
    colsample_bytree = seq(0.5, 0.9, 0.1),
    min_child_weight = 1,
    subsample = seq(0.5, 0.8, 0.1)
)
xgb_win <- train(team_winner ~., data = train,
                 method = "xgbTree",
                 metric = "ROC",
                 trControl = ctrl,
                 tuneGrid = grid)

saveRDS(xgb_win, "../NBAdb/models/models_2021/xgb_win_2021.rds")

# predictions
win_pred <- predict(xgb_win, test, type = "prob")

model_outputs <- model_outputs %>%
    mutate(xgb_win_away = as.numeric(win_pred[,1]),
           xgb_win_home = as.numeric(win_pred[,2]))


# team score models ----
# all features
train <- nba_final %>%
    filter(season_year <= 2021) %>%
    select(team_score, is_b2b_first:opp_is_b2b_second, over_under, away_implied_prob,
           away_fgm:home_opp_pct_uast_fgm) %>%
    select(-contains("_rating"))

test <- nba_final %>%
    filter(season_year > 2021) %>%
    select(team_score, is_b2b_first:opp_is_b2b_second, over_under, away_implied_prob,
           away_fgm:home_opp_pct_uast_fgm) %>%
    select(-contains("_rating"))

model_outputs <- nba_final %>%
    filter(season_year > 2021) %>%
    select(season_year:home_implied_prob)

# highly correlated features removed
train <- train %>% select(-all_of(cor_cols))
test <- test %>% select(-all_of(cor_cols))

# normalize features
pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))

train[,-1] = predict(pre_proc_val, train[,-1])
test[,-1] = predict(pre_proc_val, test[,-1])


# team score linear regression model ----
# model
ctrl <- trainControl(method = "cv", number = 5, verboseIter = T)
lin_team <- train(team_score ~., data = train,
                  trControl = ctrl,
                  method = "lm")

saveRDS(nn_win, "../NBAdb/models/models_2021/lin_team_2021.rds")

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

saveRDS(nn_win, "../NBAdb/models/models_2021/reg_team_2021.rds")

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

saveRDS(nn_win, "../NBAdb/models/models_2021/knn_team_2021.rds")

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

saveRDS(rf_team, "../NBAdb/models/models_2021/rf_team_2021.rds")

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

saveRDS(svm_team, "../NBAdb/models/models_2021/svm_team_2021.rds")

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

saveRDS(nn_team, "../NBAdb/models/models_2021/nn_team_2021.rds")

# predictions
team_pred <- predict(nn_team, test)
model_outputs <- model_outputs %>%
    mutate(nn_team_score = as.numeric(team_pred))


# team score extreme gradient boosting model ----
# model
ctrl <- trainControl(method = "cv", number = 5, verboseIter = T)
grid <- expand.grid(
    nrounds = seq(300,800,50),
    eta = c(0.015, 0.025, 0.035),
    max_depth = c(1,2,3),
    gamma = c(1,2,3,4),
    colsample_bytree = seq(0.5, 0.9, 0.1),
    min_child_weight = 1,
    subsample = seq(0.5, 0.8, 0.1)
)
xgb_team <- train(team_score ~., data = train,
                  method = "xgbTree",
                  trControl = ctrl,
                  tuneGrid = grid)

saveRDS(xgb_team, "../NBAdb/models/models_2021/xgb_team_2021.rds")

# predictions
team_pred <- predict(xgb_team, test)
model_outputs <- model_outputs %>%
    mutate(xgb_team_score = as.numeric(team_pred))


# opp score models ----
# all features
train <- nba_final %>%
    filter(season_year <= 2021) %>%
    select(opp_score, is_b2b_first:opp_is_b2b_second, over_under, away_implied_prob,
           away_fgm:home_opp_pct_uast_fgm) %>%
    select(-contains("_rating"))

test <- nba_final %>%
    filter(season_year > 2021) %>%
    select(opp_score, is_b2b_first:opp_is_b2b_second, over_under, away_implied_prob,
           away_fgm:home_opp_pct_uast_fgm) %>%
    select(-contains("_rating"))

model_outputs <- nba_final %>%
    filter(season_year > 2021) %>%
    select(season_year:home_implied_prob)

# highly correlated features removed
train <- train %>% select(-all_of(cor_cols))
test <- test %>% select(-all_of(cor_cols))

# normalize features
pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))

train[,-1] = predict(pre_proc_val, train[,-1])
test[,-1] = predict(pre_proc_val, test[,-1])


# opp score linear regression model ----
# model
ctrl <- trainControl(method = "cv", number = 5, verboseIter = T)
lin_opp <- train(opp_score ~., data = train,
                 trControl = ctrl,
                 method = "lm")

saveRDS(lin_opp, "../NBAdb/models/models_2021/lin_opp_2021.rds")

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

saveRDS(reg_opp, "../NBAdb/models/models_2021/reg_opp_2021.rds")

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

saveRDS(knn_opp, "../NBAdb/models/models_2021/knn_opp_2021.rds")

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

saveRDS(rf_opp, "../NBAdb/models/models_2021/rf_opp_2021.rds")

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

saveRDS(svm_opp, "../NBAdb/models/models_2021/svm_opp_2021.rds")

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

saveRDS(nn_opp, "../NBAdb/models/models_2021/nn_opp_2021.rds")

# predictions
opp_pred <- predict(nn_opp, test)
model_outputs <- model_outputs %>%
    mutate(nn_opp_score = as.numeric(opp_pred))


# opp score extreme gradient boosting model ----
# model
ctrl <- trainControl(method = "cv", number = 5, verboseIter = T)
grid <- expand.grid(
    nrounds = seq(300,800,50),
    eta = c(0.015, 0.025, 0.035),
    max_depth = c(1,2,3),
    gamma = c(1,2,3,4),
    colsample_bytree = seq(0.5, 0.9, 0.1),
    min_child_weight = 1,
    subsample = seq(0.5, 0.8, 0.1)
)
xgb_opp <- train(opp_score ~., data = train,
                 method = "xgbTree",
                 trControl = ctrl,
                 tuneGrid = grid)

saveRDS(xgb_opp, "../NBAdb/models/models_2021/xgb_opp_2021.rds")

# predictions
opp_pred <- predict(xgb_opp, test)
model_outputs <- model_outputs %>%
    mutate(xgb_opp_score = as.numeric(opp_pred))




