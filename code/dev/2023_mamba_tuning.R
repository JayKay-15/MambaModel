### model tuning ----

library(tidyverse)
library(RSQLite) # db
library(DBI) # db
library(caret) # model training
library(tidymodels) # model eval
library(ggfortify) # autoplot
library(doParallel) # parallel
library(tictoc) # timer

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

# Check how many cores you have to work with
detectCores()

# Set the number of clusters tidymodels has to work with
cl <- makePSOCKcluster(10)  # Create 8 clusters
registerDoParallel(cl)
getDoParWorkers()

# Turn off the parallel processing once you're finished with it
stopCluster(cl)
registerDoSEQ()

options(scipen = 999999)
# Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)

saveRDS(pre_proc_cs_score, "../NBAdb/models/trained_models/pre_proc_cs_score.rds")
saveRDS(pre_proc_yj_score, "../NBAdb/models/trained_models/pre_proc_yj_score.rds")
saveRDS(pre_proc_cs_win, "../NBAdb/models/trained_models/pre_proc_cs_win.rds")
saveRDS(pre_proc_yj_win, "../NBAdb/models/trained_models/pre_proc_yj_win.rds")

saveRDS(cor_cols_win, "../NBAdb/models/trained_models/cor_cols_win.rds")
saveRDS(cor_cols_score, "../NBAdb/models/trained_models/cor_cols_score.rds")

# pull all historical data
nba_final <- tbl(dbConnect(SQLite(), "../nba_sql_db/nba_db"), "mamba_long_odds") %>%
    collect() %>%
    filter(season_year >= 2020) %>%
    rename(team_winner = wl,
           team_score = pts,
           opp_score = opp_pts) %>%
    mutate(
        game_date = as_date(game_date, origin ="1970-01-01"),
        team_winner = if_else(team_winner == "W", "win", "loss"),
        team_winner = factor(team_winner, levels = c("win", "loss")),
        location = if_else(location == "away", 1, 0)
    )

nba_final_train <- nba_final %>%
  filter(season_year < 2024 & location == 1) %>%
  select(game_id, team_winner,
         is_b2b_first:opp_is_b2b_second, over_under, team_implied_prob,
         team_fgm:opp_opp_pct_uast_fgm) %>%
  select(-contains("_rating"))

nba_final_test <- nba_final %>%
  filter(season_year == 2024 & location == 1) %>%
  select(team_winner,
         is_b2b_first:opp_is_b2b_second, over_under, team_implied_prob,
         team_fgm:opp_opp_pct_uast_fgm) %>%
  select(-contains("_rating"))

nba_final_win_outputs <- nba_final %>%
  filter(season_year == 2024 & location == 1) %>%
  select(game_id:opp_implied_prob)

nba_final_cor <- nba_final_train[1:112]

# correlations ----
set.seed(214)
# cor_index <- createDataPartition(nba_final_cor$team_winner,
#                                  p = .1, list = FALSE)
# 
# nba_cor <- nba_final_cor[cor_index,]
# nba_final_train <- nba_final_train[-cor_index,]

sampled_game_ids <- sample(unique(nba_final_cor$game_id),
                           size = 0.05 * nrow(nba_final_cor),
                           replace = FALSE)

nba_cor <- nba_final_cor %>%
  filter(game_id %in% sampled_game_ids) %>%
  select(-game_id)
nba_final_train <- nba_final_train %>%
  filter(!game_id %in% sampled_game_ids) %>%
  select(-game_id)

# feature correlations
cor_df <- nba_cor %>%
    select(over_under, team_implied_prob, team_fgm:team_opp_pct_uast_fgm) %>%
    select(-contains("_rating"))

# check for extreme correlation
cor_mx <- cor(cor_df)
extreme_cor <- sum(abs(cor_mx[upper.tri(cor_mx)]) > .999)
extreme_cor
summary(cor_mx[upper.tri(cor_mx)])

# find highly correlated features
cor_cols <- findCorrelation(cor_mx, cutoff = .4, exact = F, names = T)
cor_cols

team_cor_cols <- cor_cols
opp_cor_cols <- gsub("team_", "opp_", cor_cols)

cor_cols <- c(team_cor_cols, opp_cor_cols)

# filter highly correlated features
cor_df_new <- cor_df %>% select(-all_of(cor_cols))

# check new set of features for correlation
cor_mx_new <- cor(cor_df_new)
findCorrelation(cor_mx_new, cutoff = .5)
summary(cor_mx_new[upper.tri(cor_mx_new)])

# correlations - win
model_win_all <- nba_final %>%
    select(team_winner, is_b2b_first:opp_is_b2b_second, over_under,
           team_implied_prob, team_fgm:opp_opp_pct_uast_fgm) %>%
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
           team_implied_prob, team_fgm:opp_opp_pct_uast_fgm) %>%
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
           team_implied_prob, team_fgm:opp_opp_pct_uast_fgm) %>%
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
rm(list=ls()[! ls() %in% c("nba_final", "cor_cols", "cl",
                           "nba_final_train", "nba_final_test")])

# team winner models ----

# highly correlated features removed
train <- nba_final_train %>%
    select(-all_of(cor_cols)) %>%
    mutate(across(is_b2b_first:opp_is_b2b_second, factor))

test <- nba_final_test %>%
    select(-all_of(cor_cols)) %>%
    mutate(across(is_b2b_first:opp_is_b2b_second, factor))

# normalize features
pre_proc_cs <- preProcess(train[,-1], method = c("center", "scale"))
pre_proc_cs_win <- pre_proc_cs

train[,-1] <- predict(pre_proc_cs, train[,-1])
test[,-1] <- predict(pre_proc_cs, test[,-1])


# team winner logistic regression model ----
# model
# ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3,
#                      verboseIter = T, classProbs = T, 
#                      summaryFunction = twoClassSummary)
ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3,
                     verboseIter = T, classProbs = T)

set.seed(214)
log_win <- train(team_winner ~., data = train,
                 method = "glm",
                 # metric = "ROC",
                 # preProc = c("center", "scale"),
                 trControl = ctrl,
                 family = "binomial")

getTrainPerf(log_win)
log_win
log_win$resample
log_win$results
summary(log_win) # model components
confusionMatrix(log_win) # confusion matrix
glance(log_win$finalModel) # entire model - tidymodels
tidy(log_win$finalModel) # model components - tidymodels
augment(log_win$finalModel) # observations - tidymodels

saveRDS(log_win, "../NBAdb/models/trained_models/log_win_20_23.rds")
log_win <- read_rds("../NBAdb/models/trained_models/log_win_20_23.rds")

# predictions
win_pred <- predict(log_win, test, type = "prob")
confusionMatrix(test$team_winner,
                factor(ifelse(win_pred[,1] > 0.5, "win", "loss"), 
                       levels = c("win","loss")),
                positive = "win")

team_pred <- as.numeric(win_pred[,1])
opp_pred <- as.numeric(win_pred[,2])
obs <- test$team_winner
pred <- factor(ifelse(team_pred > 0.5, "win", "loss"), levels = c("win","loss"))
obs_pred <- data.frame(obs = obs,
                       pred = pred,
                       win = team_pred, 
                       loss = opp_pred)

# model evaluation
twoClassSummary(obs_pred, lev = levels(obs)) # roc
prSummary(obs_pred, lev = levels(obs)) # auc
mnLogLoss(obs_pred, lev = levels(obs)) # log loss

roc_score <- pROC::roc(test$team_winner, team_pred, plot = T, legacy.axes = T,
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


# team winner ridge regression model ----
# model
# ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3,
#                      verboseIter = T, classProbs = T,
#                      summaryFunction = twoClassSummary)
ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3,
                     verboseIter = T, classProbs = T)
grid <- expand.grid(
    alpha = 0, # ridge = 0 / lasso = 1
    lambda = 10^seq(2, -3, by = -.1)
)
set.seed(214)
reg_win <- train(team_winner ~., data = train,
                 method = "glmnet",
                 # metric = "ROC",
                 # preProc = c("center", "scale"),
                 trControl = ctrl,
                 tuneGrid = grid)

getTrainPerf(reg_win)
reg_win
reg_win$resample
reg_win$results
summary(reg_win) # model components
confusionMatrix(reg_win) # confusion matrix
glance(reg_win$finalModel) # entire model - tidymodels
tidy(reg_win$finalModel) # model components - tidymodels
plot(reg_win) # viz

saveRDS(reg_win, "../NBAdb/models/trained_models/reg_win_20_23.rds")
reg_win <- read_rds("../NBAdb/models/trained_models/reg_win_20_23.rds")

# predictions
win_pred <- predict(reg_win, test, type = "prob")
confusionMatrix(test$team_winner,
                factor(ifelse(win_pred[,1] > 0.5, "win", "loss"), 
                       levels = c("win","loss")),
                positive = "win")

team_pred <- as.numeric(win_pred[,1])
opp_pred <- as.numeric(win_pred[,2])
obs <- test$team_winner
pred <- factor(ifelse(team_pred > 0.5, "win", "loss"), levels = c("win","loss"))
obs_pred <- data.frame(obs = obs,
                       pred = pred,
                       win = team_pred, 
                       loss = opp_pred)

# model evaluation
twoClassSummary(obs_pred, lev = levels(obs)) # roc
prSummary(obs_pred, lev = levels(obs)) # auc
mnLogLoss(obs_pred, lev = levels(obs)) # log loss

roc_score <- pROC::roc(test$team_winner, team_pred, plot = T, legacy.axes = T,
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


# team winner knn model ----
# model
# ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3,
#                      verboseIter = T, classProbs = T,
#                      summaryFunction = twoClassSummary)
ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3,
                     verboseIter = T, classProbs = T)
grid <- expand.grid(
    k = seq(5, 50, 5)
)
set.seed(214)
knn_win <- train(team_winner ~., data = train, 
                 method = "knn",
                 # metric = "ROC",
                 # preProc = c("center", "scale"),
                 trControl = ctrl,
                 tuneGrid = grid)

getTrainPerf(knn_win)
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

team_pred <- as.numeric(win_pred[,1])
opp_pred <- as.numeric(win_pred[,2])
obs <- test$team_winner
pred <- factor(ifelse(team_pred > 0.5, "win", "loss"), levels = c("win","loss"))
obs_pred <- data.frame(obs = obs,
                       pred = pred,
                       win = team_pred, 
                       loss = opp_pred)

# model evaluation
twoClassSummary(obs_pred, lev = levels(obs)) # roc
prSummary(obs_pred, lev = levels(obs)) # auc
mnLogLoss(obs_pred, lev = levels(obs)) # log loss

eval <- MLeval::evalm(data.frame(win_pred, test$team_winner)) # roc chart
roc_score <- pROC::roc(test$team_winner, team_pred, plot = T, legacy.axes = T,
                       percent = T,
                       xlab="False Positive Percentage",
                       ylab="True Postive Percentage",
                       col="#377eb8", lwd = 4, print.auc = T) # roc chart

postResample(pred = pred, obs = obs) # caret eval
knn_win_metrics <- postResample(pred = pred, obs = obs)[1]


# team winner random forest model ----
# model
# ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3,
#                      verboseIter = T, classProbs = T,
#                      summaryFunction = twoClassSummary)
ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3,
                     verboseIter = T, classProbs = T)
grid <- expand.grid(
    .mtry = 1:8,
    .splitrule = "gini",
    .min.node.size = 1:3
)
set.seed(214)
rf_win <- train(team_winner ~., data = train,
                method = "ranger",
                # metric = "ROC",
                # preProc = c("center", "scale"),
                trControl = ctrl,
                tuneGrid = grid)

getTrainPerf(rf_win)
rf_win
rf_win$resample
rf_win$results
summary(rf_win) # model components
confusionMatrix(rf_win) # confusion matrix
plot(rf_win) # viz

saveRDS(rf_win, "../NBAdb/models/trained_models/rf_win_20_23.rds")
rf_win <- read_rds("../NBAdb/models/trained_models/rf_win_20_23.rds")

# predictions
win_pred <- predict(rf_win, test, type = "prob")
confusionMatrix(test$team_winner,
                factor(ifelse(win_pred[,1] > 0.5, "win", "loss"), 
                       levels = c("win","loss")),
                positive = "win")

team_pred <- as.numeric(win_pred[,1])
opp_pred <- as.numeric(win_pred[,2])
obs <- test$team_winner
pred <- factor(ifelse(team_pred > 0.5, "win", "loss"), levels = c("win","loss"))
obs_pred <- data.frame(obs = obs,
                       pred = pred,
                       win = team_pred, 
                       loss = opp_pred)

# model evaluation
postResample(pred = pred, obs = obs) # accuracy
twoClassSummary(obs_pred, lev = levels(obs)) # roc
prSummary(obs_pred, lev = levels(obs)) # auc
mnLogLoss(obs_pred, lev = levels(obs)) # log loss

roc_score <- pROC::roc(test$team_winner, team_pred, plot = T, legacy.axes = T,
                       percent = T,
                       xlab="False Positive Percentage",
                       ylab="True Postive Percentage",
                       col="#377eb8", lwd = 4, print.auc = T) # roc chart

postResample(pred = pred, obs = obs) # caret eval
rf_win_metrics <- postResample(pred = pred, obs = obs)[1]


# team winner extreme gradient boosting model ----
# model
# ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3,
#                      verboseIter = T, classProbs = T,
#                      summaryFunction = twoClassSummary)
ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3,
                     verboseIter = T, classProbs = T)
# ctrl <- trainControl(method = "adaptive_cv", number = 5, repeats = 3,
#                      verboseIter = T, classProbs = T,
#                      adaptive = list(min = 3,
#                                      alpha = 0.05,
#                                      method = "gls",
#                                      complete = TRUE))
# https://www.analyticsvidhya.com/blog/2016/03/complete-guide-parameter-tuning-xgboost-with-codes-python/#:~:text=What%20parameters%20should%20you%20use,minimum%20child%20weight%20(min_child_weight).
# https://ml-course.kazsakamoto.com/Labs/hyperparameterTuning.html
# https://www.kaggle.com/code/pelkoja/visual-xgboost-tuning-with-caret
# https://www.hackerearth.com/practice/machine-learning/machine-learning-algorithms/beginners-tutorial-on-xgboost-parameter-tuning-r/tutorial/

grid <- expand.grid(
  nrounds = seq(2000, 5000, 100),
  eta = c(0.005, 0.01, 0.1),
  max_depth = c(3),
  gamma = c(23),
  colsample_bytree = c(0.6),
  min_child_weight = c(5),
  subsample = c(0.8)
)

tic()
set.seed(214)
xgb_win <- train(team_winner ~., data = train,
                 method = "xgbTree",
                 # metric = "ROC",
                 # preProc = c("center", "scale"),
                 trControl = ctrl,
                 tuneGrid = grid)
toc()

# grid <- expand.grid(
#     nrounds = seq(100, 300, 100),
#     eta = c(0.1),
#     alpha = seq(30, 70, 5),
#     lambda = c(1)
# )
# tic()
# set.seed(214)
# xgb_win <- train(team_winner ~., data = train,
#                  method = "xgbLinear",
#                  # metric = "ROC",
#                  preProc = c("center", "scale", "YeoJohnson"),
#                  trControl = ctrl,
#                  tuneGrid = grid)
# toc()

getTrainPerf(xgb_win)
xgb_win
xgb_win$resample
xgb_win$results
summary(xgb_win) # model components
confusionMatrix(xgb_win) # confusion matrix
plot(xgb_win) # viz

saveRDS(xgb_win, "../NBAdb/models/trained_models/xgb_win_20_23.rds")
xgb_win <- read_rds("../NBAdb/models/trained_models/xgb_win_20_23.rds")

# predictions
win_pred <- predict(xgb_win, test, type = "prob")
confusionMatrix(test$team_winner,
                factor(ifelse(win_pred[,1] > 0.5, "win", "loss"), 
                       levels = c("win","loss")),
                positive = "win")

team_pred <- as.numeric(win_pred[,1])
opp_pred <- as.numeric(win_pred[,2])
obs <- test$team_winner
pred <- factor(ifelse(team_pred > 0.5, "win", "loss"), levels = c("win","loss"))
obs_pred <- data.frame(obs = obs,
                       pred = pred,
                       win = team_pred, 
                       loss = opp_pred)

# model evaluation
twoClassSummary(obs_pred, lev = levels(obs)) # roc
prSummary(obs_pred, lev = levels(obs)) # auc
mnLogLoss(obs_pred, lev = levels(obs)) # log loss

roc_score <- pROC::roc(test$team_winner, team_pred, plot = T, legacy.axes = T,
                       percent = T,
                       xlab="False Positive Percentage",
                       ylab="True Postive Percentage",
                       col="#377eb8", lwd = 4, print.auc = T) # roc chart

postResample(pred = pred, obs = obs) # caret eval

# feature importance
importance <- varImp(xgb_win, scale = F)
plot(importance)

xgb_win_imp <- rownames_to_column(importance[["importance"]], "Var") %>%
    arrange(desc(Overall)) %>%
    head(20)
xgb_win_imp


# team winner boosted generalized linear model ----
# model
# ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3,
#                      verboseIter = T, classProbs = T,
#                      summaryFunction = twoClassSummary)
ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3,
                     verboseIter = T, classProbs = T)
# ctrl <- trainControl(method = "adaptive_cv", number = 5, repeats = 3,
#                      verboseIter = T, classProbs = T,
#                      adaptive = list(min = 3,
#                                      alpha = 0.05,
#                                      method = "gls",
#                                      complete = TRUE))
grid <- expand.grid(
  mstop = seq(100, 1000, 50),
  prune = c("no")
)
set.seed(214)
glmb_win <- train(team_winner ~., data = train,
                 method = "glmboost",
                 # metric = "ROC",
                 # preProc = c("center", "scale"),
                 trControl = ctrl,
                 tuneGrid = grid)

getTrainPerf(glmb_win)
glmb_win
glmb_win$resample
glmb_win$results
summary(glmb_win) # model components
confusionMatrix(glmb_win) # confusion matrix
plot(glmb_win)

saveRDS(glmb_win, "../NBAdb/models/trained_models/glmb_win_20_23.rds")
glmb_win <- read_rds("../NBAdb/models/trained_models/glmb_win_20_23.rds")

# predictions
win_pred <- predict(glmb_win, test, type = "prob")
confusionMatrix(test$team_winner,
                factor(ifelse(win_pred[,1] > 0.5, "win", "loss"), 
                       levels = c("win","loss")),
                positive = "win")

team_pred <- as.numeric(win_pred[,1])
opp_pred <- as.numeric(win_pred[,2])
obs <- test$team_winner
pred <- factor(ifelse(team_pred > 0.5, "win", "loss"), levels = c("win","loss"))
obs_pred <- data.frame(obs = obs,
                       pred = pred,
                       win = team_pred, 
                       loss = opp_pred)

# model evaluation
twoClassSummary(obs_pred, lev = levels(obs)) # roc
prSummary(obs_pred, lev = levels(obs)) # auc
mnLogLoss(obs_pred, lev = levels(obs)) # log loss

roc_score <- pROC::roc(test$team_winner, team_pred, plot = T, legacy.axes = T,
                       percent = T,
                       xlab="False Positive Percentage",
                       ylab="True Postive Percentage",
                       col="#377eb8", lwd = 4, print.auc = T) # roc chart

postResample(pred = pred, obs = obs) # caret eval

# feature importance
importance <- varImp(glmboost_win, scale = F)
plot(importance)

glmb_win_imp <- rownames_to_column(importance[["importance"]], "Var") %>%
  arrange(desc(Overall)) %>%
  head(20)
glmb_win_imp


# team winner earth model ----
# model
# ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3,
#                      verboseIter = T, classProbs = T,
#                      summaryFunction = twoClassSummary)
ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3,
                     verboseIter = T, classProbs = T)
# ctrl <- trainControl(method = "adaptive_cv", number = 5, repeats = 3,
#                      verboseIter = T, classProbs = T,
#                      adaptive = list(min = 3,
#                                      alpha = 0.05,
#                                      method = "gls",
#                                      complete = TRUE))
grid <- expand.grid(
  nprune = seq(5, 50, 5),
  degree = c(1:4)
)
set.seed(214)
mars_win <- train(team_winner ~., data = train,
                  method = "earth",
                  # metric = "ROC",
                  # preProc = c("center", "scale"),
                  trControl = ctrl,
                  tuneGrid = grid)

getTrainPerf(mars_win)
mars_win
mars_win$resample
mars_win$results
summary(mars_win) # model components
confusionMatrix(mars_win) # confusion matrix
plot(mars_win)

saveRDS(mars_win, "../NBAdb/models/trained_models/mars_win_20_23.rds")
mars_win <- read_rds("../NBAdb/models/trained_models/mars_win_20_23.rds")

# predictions
win_pred <- predict(mars_win, test, type = "prob")
confusionMatrix(test$team_winner,
                factor(ifelse(win_pred[,1] > 0.5, "win", "loss"), 
                       levels = c("win","loss")),
                positive = "win")

team_pred <- as.numeric(win_pred[,1])
opp_pred <- as.numeric(win_pred[,2])
obs <- test$team_winner
pred <- factor(ifelse(team_pred > 0.5, "win", "loss"), levels = c("win","loss"))
obs_pred <- data.frame(obs = obs,
                       pred = pred,
                       win = team_pred, 
                       loss = opp_pred)

# model evaluation
twoClassSummary(obs_pred, lev = levels(obs)) # roc
prSummary(obs_pred, lev = levels(obs)) # auc
mnLogLoss(obs_pred, lev = levels(obs)) # log loss

roc_score <- pROC::roc(test$team_winner, team_pred, plot = T, legacy.axes = T,
                       percent = T,
                       xlab="False Positive Percentage",
                       ylab="True Postive Percentage",
                       col="#377eb8", lwd = 4, print.auc = T) # roc chart

postResample(pred = pred, obs = obs) # caret eval

# feature importance
importance <- varImp(mars_win, scale = F)
plot(importance)

mars_win_imp <- rownames_to_column(importance[["importance"]], "Var") %>%
  arrange(desc(Overall)) %>%
  head(20)
mars_win_imp


# normalize features
train <- nba_final_train %>%
  select(-all_of(cor_cols)) %>%
  mutate(across(is_b2b_first:opp_is_b2b_second, factor))

test <- nba_final_test %>%
  select(-all_of(cor_cols)) %>%
  mutate(across(is_b2b_first:opp_is_b2b_second, factor))

pre_proc_yj <- preProcess(train[,-1], method = c("center", "scale", "YeoJohnson"))
pre_proc_yj_win <- pre_proc_yj

train[,-1] <- predict(pre_proc_yj, train[,-1])
test[,-1] <- predict(pre_proc_yj, test[,-1])

# team winner support vector machines model ----
# model
# ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3,
#                      verboseIter = T, classProbs = T,
#                      summaryFunction = twoClassSummary)
ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3,
                     verboseIter = T, classProbs = T)
# ctrl <- trainControl(method = "adaptive_cv", number = 5, repeats = 3,
#                      verboseIter = T, classProbs = T,
#                      adaptive = list(min = 3,
#                                      alpha = 0.05,
#                                      method = "gls",
#                                      complete = TRUE))
grid <- expand.grid(
  sigma = c(0.001, 0.005, 0.01, 0.05),
  C = c(0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.95)
)
set.seed(214)
svm_win <- train(team_winner ~., data = train,
                 method = "svmRadial",
                 # metric = "ROC",
                 # preProc = c("center", "scale", "YeoJohnson"),
                 trControl = ctrl,
                 tuneGrid = grid)

# grid <- expand.grid(
#     C = c(0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.95)
# )
# set.seed(214)
# svm_win <- train(team_winner ~., data = train,
#                  method = "svmLinear",
#                  # metric = "ROC",
#                  preProc = c("center", "scale", "YeoJohnson"),
#                  trControl = ctrl,
#                  tuneGrid = grid)
#
# grid <- expand.grid(
#   degree = c(1, 2, 3),
#   scale = c(0.001, 0.01, 0.1, 1),
#   C = c(0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.95)
# )
# set.seed(214)
# svm_win <- train(team_winner ~., data = train,
#                  method = "svmPoly",
#                  # metric = "ROC",
#                  preProc = c("center", "scale", "YeoJohnson"),
#                  trControl = ctrl,
#                  tuneLength = 25)

getTrainPerf(svm_win)
svm_win
svm_win$resample
svm_win$results
summary(svm_win) # model components
confusionMatrix(svm_win) # confusion matrix
plot(svm_win) # viz

saveRDS(svm_win, "../NBAdb/models/trained_models/svm_win_20_23.rds")
svm_win <- read_rds("../NBAdb/models/trained_models/svm_win_20_23.rds")

# predictions
win_pred <- predict(svm_win, test, type = "prob")
confusionMatrix(test$team_winner,
                factor(ifelse(win_pred[,1] > 0.5, "win", "loss"), 
                       levels = c("win","loss")),
                positive = "win")

team_pred <- as.numeric(win_pred[,1])
opp_pred <- as.numeric(win_pred[,2])
obs <- test$team_winner
pred <- factor(ifelse(team_pred > 0.5, "win", "loss"), levels = c("win","loss"))
obs_pred <- data.frame(obs = obs,
                       pred = pred,
                       win = team_pred, 
                       loss = opp_pred)

# model evaluation
twoClassSummary(obs_pred, lev = levels(obs)) # roc
prSummary(obs_pred, lev = levels(obs)) # auc
mnLogLoss(obs_pred, lev = levels(obs)) # log loss

roc_score <- pROC::roc(test$team_winner, team_pred, plot = T, legacy.axes = T,
                       percent = T,
                       xlab="False Positive Percentage",
                       ylab="True Postive Percentage",
                       col="#377eb8", lwd = 4, print.auc = T) # roc chart

postResample(pred = pred, obs = obs) # caret eval
svm_win_metrics <- postResample(pred = pred, obs = obs)[1]


# team winner neural net model ----
# model
# ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3,
#                      verboseIter = T, classProbs = T,
#                      summaryFunction = twoClassSummary)
ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3,
                     verboseIter = T, classProbs = T)
# ctrl <- trainControl(method = "adaptive_cv", number = 5, repeats = 3,
#                      verboseIter = T, classProbs = T,
#                      adaptive = list(min = 3,
#                                      alpha = 0.05,
#                                      method = "gls",
#                                      complete = TRUE))
grid <- expand.grid(
  decay = c(0.95, 0.75, 0.5, 0.1, 1e-2, 1e-3),
  size = c(1, 3, 5)
)
set.seed(214)
nn_win <- train(team_winner ~., data = train,
                method = "nnet",
                # metric = "ROC",
                # preProc = c("center", "scale", "YeoJohnson"),
                trControl = ctrl,
                tuneGrid = grid,
                maxit = 1000)

# grid <- expand.grid(
#   decay = c(0.95, 0.75, 0.5, 0.1, 1e-2, 1e-3),
#   size = c(1, 2, 3, 4),
#   bag = c(FALSE)
# )
# set.seed(214)
# nn_win <- train(team_winner ~., data = train,
#                 method = "avNNet",
#                 # metric = "ROC",
#                 preProc = c("center", "scale", "YeoJohnson"),
#                 trControl = ctrl,
#                 tuneGrid = grid,
#                 maxit = 1000)
# 
# grid <- expand.grid(
#   decay = c(0.95, 0.75, 0.5, 0.1, 1e-2, 1e-3),
#   size = c(1, 2, 3, 4)
# )
# set.seed(214)
# nn_win <- train(team_winner ~., data = train,
#                 method = "pcaNNet",
#                 # metric = "ROC",
#                 preProc = c("center", "scale", "YeoJohnson"),
#                 trControl = ctrl,
#                 tuneGrid = grid,
#                 maxit = 1000)
# 
# grid <- expand.grid(
#   # decay = c(0.75, 0.5, 0.1, 1e-2, 1e-3),
#   layer1 = 2:4,
#   layer2 = 2:4,
#   layer3 = 2:4
# )
# set.seed(214)
# nn_win <- train(team_winner ~., data = train,
#                 method = "mlpML", # mlpML # mlpWeightDecayML
#                 # metric = "ROC",
#                 preProc = c("center", "scale", "YeoJohnson"),
#                 trControl = ctrl,
#                 tuneGrid = grid,
#                 maxit = 1000)

getTrainPerf(nn_win)
nn_win
nn_win$resample
nn_win$results
summary(nn_win) # model components
confusionMatrix(nn_win) # confusion matrix
plot(nn_win) # viz

saveRDS(nn_win, "../NBAdb/models/trained_models/nn_win_20_23.rds")
nn_win <- read_rds("../NBAdb/models/trained_models/nn_win_20_23.rds")

# predictions
win_pred <- predict(nn_win, test, type = "prob")
confusionMatrix(test$team_winner,
                factor(ifelse(win_pred[,1] > 0.5, "win", "loss"), 
                       levels = c("win","loss")),
                positive = "win")

team_pred <- as.numeric(win_pred[,1])
opp_pred <- as.numeric(win_pred[,2])
obs <- test$team_winner
pred <- factor(ifelse(team_pred > 0.5, "win", "loss"), levels = c("win","loss"))
obs_pred <- data.frame(obs = obs,
                       pred = pred,
                       win = team_pred, 
                       loss = opp_pred)

# model evaluation
twoClassSummary(obs_pred, lev = levels(obs)) # roc
prSummary(obs_pred, lev = levels(obs)) # auc
mnLogLoss(obs_pred, lev = levels(obs)) # log loss

roc_score <- pROC::roc(test$team_winner, team_pred, plot = T, legacy.axes = T,
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




# team score models ----
# pull all historical data
nba_final <- tbl(dbConnect(SQLite(), "../nba_sql_db/nba_db"), "mamba_long_odds") %>%
  collect() %>%
  filter(season_year >= 2020) %>%
  rename(team_winner = wl,
         team_score = pts,
         opp_score = opp_pts) %>%
  mutate(
    game_date = as_date(game_date, origin ="1970-01-01"),
    team_winner = if_else(team_winner == "W", "win", "loss"),
    team_winner = factor(team_winner, levels = c("win", "loss")),
    location = if_else(location == "away", 1, 0)
  )

nba_final_train <- nba_final %>%
  filter(season_year < 2024) %>%
  select(game_id, team_score, location,
         is_b2b_first:opp_is_b2b_second, over_under, team_implied_prob,
         team_fgm:opp_opp_pct_uast_fgm) %>%
  select(-contains("_rating"))

nba_final_test <- nba_final %>%
  filter(season_year == 2024) %>%
  select(team_score, location,
         is_b2b_first:opp_is_b2b_second, over_under, team_implied_prob,
         team_fgm:opp_opp_pct_uast_fgm) %>%
  select(-contains("_rating"))

nba_final_ts_outputs <- nba_final %>%
  filter(season_year == 2024) %>%
  select(game_id:opp_implied_prob)

nba_final_cor <- nba_final_train[1:113]

# correlations ----
set.seed(214)

sampled_game_ids <- sample(unique(nba_final_cor$game_id),
                           size = 0.05 * nrow(nba_final_cor),
                           replace = FALSE)

nba_cor <- nba_final_cor %>%
  filter(game_id %in% sampled_game_ids) %>%
  select(-game_id)
nba_final_train <- nba_final_train %>%
  filter(!game_id %in% sampled_game_ids) %>%
  select(-game_id)

# feature correlations
cor_df <- nba_cor %>%
  select(over_under, team_implied_prob, team_fgm:team_opp_pct_uast_fgm) %>%
  select(-contains("_rating"))

# check for extreme correlation
cor_mx <- cor(cor_df)
extreme_cor <- sum(abs(cor_mx[upper.tri(cor_mx)]) > .999)
extreme_cor
summary(cor_mx[upper.tri(cor_mx)])

# find highly correlated features
cor_cols <- findCorrelation(cor_mx, cutoff = .4, exact = F, names = T)
cor_cols

team_cor_cols <- cor_cols
opp_cor_cols <- gsub("team_", "opp_", cor_cols)

cor_cols <- c(team_cor_cols, opp_cor_cols)

# clear environment ----
rm(list=ls()[! ls() %in% c("nba_final", "cor_cols", "cl",
                           "nba_final_train", "nba_final_test")])

# highly correlated features removed
train <- nba_final_train %>%
  select(-all_of(cor_cols)) %>%
  mutate(across(location:opp_is_b2b_second, factor))

test <- nba_final_test %>%
  select(-all_of(cor_cols)) %>%
  mutate(across(location:opp_is_b2b_second, factor))

# normalize features
pre_proc_cs <- preProcess(train[,-1], method = c("center", "scale"))
pre_proc_cs_score <- pre_proc_cs

train[,-1] <- predict(pre_proc_cs, train[,-1])
test[,-1] <- predict(pre_proc_cs, test[,-1])

# team score linear regression model ----
# model
ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3,
                     verboseIter = T)
set.seed(214)
lin_team <- train(team_score ~., data = train,
                  method = "lm",
                  metric = "MAE",
                  # preProc = c("center", "scale"),
                  trControl = ctrl)

getTrainPerf(lin_team)
lin_team
lin_team$resample
lin_team$results
summary(lin_team) # model components
autoplot(lin_team$finalModel) # viz - ggfortify
glance(lin_team$finalModel) # entire model - tidymodels
tidy(lin_team$finalModel) # model components - tidymodels
augment(lin_team$finalModel) # observations - tidymodels

saveRDS(lin_team, "../NBAdb/models/trained_models/lin_team_20_23.rds")
lin_team <- read_rds("../NBAdb/models/trained_models/lin_team_20_23.rds")

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


# team score ridge regression model ----
# model
# ctrl <- trainControl(method = "cv", number = 5, verboseIter = T)
ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3,
                     verboseIter = T)
grid <- expand.grid(
    alpha = 0, 
    lambda = 10^seq(2, -3, by = -.1)
)
set.seed(214)
reg_team <- train(team_score ~., data = train,
                  method = "glmnet",
                  metric = "MAE",
                  # preProc = c("center", "scale"),
                  trControl = ctrl,
                  tuneGrid = grid)

getTrainPerf(reg_team)
reg_team
reg_team$resample
reg_team$results
summary(reg_team) # model components
autoplot(reg_team$finalModel) # viz - ggfortify
glance(reg_team$finalModel) # entire model - tidymodels
tidy(reg_team$finalModel) # model components - tidymodels
plot(reg_team) # viz

saveRDS(reg_team, "../NBAdb/models/trained_models/reg_team_20_23.rds")
reg_team <- read_rds("../NBAdb/models/trained_models/reg_team_20_23.rds")

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


# team score knn model ----
# model
# ctrl <- trainControl(method = "cv", number = 5, verboseIter = F)
ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3,
                     verboseIter = T)
grid <- expand.grid(
    k = seq(2, 24, 2)
)
set.seed(214)
knn_team <- train(team_score ~., data = train, 
                  method = "knn",
                  metric = "MAE",
                  # preProc = c("center", "scale"),
                  trControl = ctrl,
                  tuneGrid = grid)

getTrainPerf(knn_team)
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


# team score random forest model ----
# model
# ctrl <- trainControl(method = "cv", number = 5, verboseIter = F)
ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3,
                     verboseIter = T)
grid <- expand.grid(
    .mtry = seq(2, 20, 2),
    .splitrule = "variance",
    .min.node.size = 1:3
)
set.seed(214)
rf_team <- train(team_score ~., data = train,
                 method = "ranger",
                 metric = "MAE",
                 # preProc = c("center", "scale"),
                 trControl = ctrl,
                 tuneGrid = grid)

getTrainPerf(rf_team)
rf_team
rf_team$resample
rf_team$results
plot(rf_team) # viz

saveRDS(rf_team, "../NBAdb/models/trained_models/rf_team_20_23.rds")
rf_team <- read_rds("../NBAdb/models/trained_models/rf_team_20_23.rds")

# predictions
team_pred <- predict(rf_team, test)

# model evaluation
postResample(pred = team_pred, obs = test$team_score) # caret eval
rf_team_metrics <- postResample(pred = team_pred, obs = test$team_score)[1]


# team score extreme gradient boosting model ----
# model
# ctrl <- trainControl(method = "cv", number = 5, verboseIter = F)
ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3,
                     verboseIter = T)
grid <- expand.grid(
  nrounds = seq(100, 2000, 100),
  eta = c(0.005, 0.01),
  max_depth = c(3),
  gamma = c(5),
  colsample_bytree = c(0.8),
  min_child_weight = c(1),
  subsample = c(0.9)
)
set.seed(214)
xgb_team <- train(team_score ~., data = train,
                  method = "xgbTree",
                  metric = "MAE",
                  # preProc = c("center", "scale"),
                  trControl = ctrl,
                  tuneGrid = grid)

getTrainPerf(xgb_team)
xgb_team
xgb_team$resample
xgb_team$results
summary(xgb_team) # model components
plot(xgb_team) # viz

saveRDS(xgb_team, "../NBAdb/models/trained_models/xgb_team_20_23.rds")
xgb_team <- read_rds("../NBAdb/models/trained_models/xgb_team_20_23.rds")

# predictions
team_pred <- predict(xgb_team, test)

# model evaluation
postResample(pred = team_pred, obs = test$team_score) # caret eval
xgb_team_metrics <- postResample(pred = team_pred, obs = test$team_score)[1]

# variable importance
importance <- varImp(xgb_team, scale = F)
plot(importance)

xgb_team_imp <- rownames_to_column(importance[["importance"]], "Var") %>%
    arrange(desc(Overall)) %>%
    head(20)
xgb_team_imp


# team score glm boost model ----
# model
# ctrl <- trainControl(method = "cv", number = 5, verboseIter = F)
ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3,
                     verboseIter = T)
grid <- expand.grid(
    mstop = seq(50, 500, 50),
    prune = c("yes", "no")
)
set.seed(214)
glmb_team <- train(team_score ~., data = train,
                  method = "glmboost",
                  metric = "MAE",
                  # preProc = c("center", "scale"),
                  trControl = ctrl,
                  tuneGrid = grid)

getTrainPerf(glmb_team)
glmb_team
glmb_team$resample
glmb_team$results
summary(glmb_team) # model components
plot(glmb_team) # viz

saveRDS(glmb_team, "../NBAdb/models/trained_models/glmb_team_20_23.rds")
glmb_team <- read_rds("../NBAdb/models/trained_models/glmb_team_20_23.rds")

# predictions
team_pred <- predict(glmb_team, test)

# model evaluation
postResample(pred = team_pred, obs = test$team_score) # caret eval
glmb_team_metrics <- postResample(pred = team_pred, obs = test$team_score)[1]


# team score earth model ----
# model
# ctrl <- trainControl(method = "cv", number = 5, verboseIter = F)
ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3,
                     verboseIter = T)
grid <- expand.grid(
  nprune = seq(5, 50, 5),
  degree = c(1:4)
)
set.seed(214)
mars_team <- train(team_score ~., data = train,
                 method = "earth",
                 metric = "MAE",
                 # preProc = c("center", "scale"),
                 trControl = ctrl,
                 tuneGrid = grid)

getTrainPerf(mars_team)
mars_team
mars_team$resample
mars_team$results
summary(mars_team) # model components
plot(mars_team) # viz

saveRDS(mars_team, "../NBAdb/models/trained_models/mars_team_20_23.rds")
mars_team <- read_rds("../NBAdb/models/trained_models/mars_team_20_23.rds")

# predictions
team_pred <- predict(mars_team, test)

# model evaluation
postResample(pred = team_pred, obs = test$team_score) # caret eval
mars_team_metrics <- postResample(pred = team_pred, obs = test$team_score)[1]


# normalize features
train <- nba_final_train %>%
  select(-all_of(cor_cols)) %>%
  mutate(across(location:opp_is_b2b_second, factor))

test <- nba_final_test %>%
  select(-all_of(cor_cols)) %>%
  mutate(across(location:opp_is_b2b_second, factor))

# normalize features
pre_proc_yj <- preProcess(train[,-1], method = c("center", "scale", "YeoJohnson"))
pre_proc_yj_score <- pre_proc_yj

train[,-1] <- predict(pre_proc_yj, train[,-1])
test[,-1] <- predict(pre_proc_yj, test[,-1])

# team score support vector machines model ----
# model
# ctrl <- trainControl(method = "cv", number = 5, verboseIter = F)
ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3,
                     verboseIter = T)
grid <- expand.grid(
  sigma = c(0.001, 0.005, 0.01, 0.05, 0.1),
  C = c(0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.95)
)
set.seed(214)
svm_team <- train(team_score ~., data = train,
                  method = "svmRadial",
                  metric = "MAE",
                  # preProc = c("center", "scale", "YeoJohnson"),
                  trControl = ctrl,
                  tuneGrid = grid)

# grid <- expand.grid(
#     C = c(0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.95)
# )
# set.seed(214)
# svm_team <- train(team_score ~., data = train,
#                   method = "svmLinear",
#                   metric = "MAE",
#                   # preProc = c("center", "scale", "YeoJohnson"),
#                   trControl = ctrl,
#                   tuneGrid = grid)

getTrainPerf(svm_team)
svm_team
svm_team$resample
svm_team$results
summary(svm_team) # model components
plot(svm_team) # viz

saveRDS(svm_team, "../NBAdb/models/trained_models/svm_team_20_23.rds")
svm_team <- read_rds("../NBAdb/models/trained_models/svm_team_20_23.rds")

# predictions
team_pred <- predict(svm_team, test)

# model evaluation
postResample(pred = team_pred, obs = test$team_score) # caret eval
svm_team_metrics <- postResample(pred = team_pred, obs = test$team_score)[1]


# team score neural net model ----
# model
# ctrl <- trainControl(method = "cv", number = 5, verboseIter = F)
ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3,
                     verboseIter = T)
grid <- expand.grid(
  decay = c(0.75, 0.5, 0.25, 0.1, 1e-2),
  size = c(1, 2, 3, 4)
)
set.seed(214)
nn_team <- train(team_score ~., data = train,
                 method = "nnet",
                 metric = "MAE",
                 # preProc = c("center", "scale", "YeoJohnson"),
                 trControl = ctrl,
                 tuneGrid = grid,
                 maxit = 1000,
                 linout = 1)

# grid <- expand.grid(
#     decay = c(0.75, 0.5, 0.25, 0.1, 1e-2),
#     size = c(1, 2, 3, 4),
#     bag = c(FALSE, TRUE)
# )
# set.seed(214)
# nn_team <- train(team_score ~., data = train,
#                 method = "avNNet",
#                 metric = "MAE",
#                 preProc = c("center", "scale", "YeoJohnson"),
#                 trControl = ctrl,
#                 tuneGrid = grid,
#                 maxit = 1000,
#                 linout = 1)

# grid <- expand.grid(
#   decay = c(0.75, 0.5, 0.25, 0.1, 1e-2),
#   size = c(1, 2, 3, 4)
# )
# set.seed(214)
# nn_team <- train(team_score ~., data = train,
#                 method = "pcaNNet",
#                 metric = "MAE",
#                 # preProc = c("center", "scale", "YeoJohnson"),
#                 trControl = ctrl,
#                 tuneGrid = grid,
#                 maxit = 1000,
#                 linout = 1)

getTrainPerf(nn_team)
nn_team
nn_team$resample
nn_team$results
summary(nn_team) # model components
plot(nn_team) # viz

saveRDS(nn_team, "../NBAdb/models/trained_models/nn_team_20_23.rds")
nn_team <- read_rds("../NBAdb/models/trained_models/nn_team_20_23.rds")

# predictions
team_pred <- predict(nn_team, test)

# model evaluation
postResample(pred = team_pred, obs = test$team_score) # caret eval
nn_team_metrics <- postResample(pred = team_pred, obs = test$team_score)[1]

# variable importance
importance <- varImp(nn_team, scale = F)
plot(importance)

nn_team_imp <- rownames_to_column(importance[["importance"]], "Var") %>%
  arrange(desc(Overall)) %>%
  head(20)
nn_team_imp





























# opp score models ----
# all features
train <- nba_final %>%
    filter(season_year <= 2022) %>%
    select(opp_score, location, is_b2b_first:opp_is_b2b_second, over_under,
           team_implied_prob, team_fgm:opp_opp_pct_uast_fgm) %>%
    select(-contains("_rating"))

test <- nba_final %>%
    filter(season_year > 2022) %>%
    select(opp_score, location, is_b2b_first:opp_is_b2b_second, over_under,
           team_implied_prob, team_fgm:opp_opp_pct_uast_fgm) %>%
    select(-contains("_rating"))

# highly correlated features removed
train <- train %>%
    select(-all_of(cor_cols)) %>%
    mutate(across(location:opp_is_b2b_second, factor))

test <- test %>%
    select(-all_of(cor_cols)) %>%
    mutate(across(location:opp_is_b2b_second, factor))

# # normalize features
# pre_proc_val <- preProcess(train[,-c(1:6)], method = c("center", "scale"))
# 
# train[,-c(1:6)] = predict(pre_proc_val, train[,-c(1:6)])
# test[,-c(1:6)] = predict(pre_proc_val, test[,-c(1:6)])


# opp score linear regression model ----
# model
ctrl <- trainControl(method = "cv", number = 5, verboseIter = T)
set.seed(214)
lin_opp <- train(opp_score ~., data = train,
                 method = "lm",
                 metric = "MAE",
                 preProc = c("center", "scale"),
                 trControl = ctrl)

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


# opp score ridge regression model ----
# model
ctrl <- trainControl(method = "cv", number = 5, verboseIter = T)
grid <- expand.grid(
    alpha = 0, 
    lambda = 10^seq(2, -3, by = -.1)
)
set.seed(214)
reg_opp <- train(opp_score ~., data = train,
                 method = "glmnet",
                 metric = "MAE",
                 preProc = c("center", "scale"),
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


# opp score knn model ----
# model
ctrl <- trainControl(method = "cv", number = 5, verboseIter = T)
grid <- expand.grid(
    k = seq(2, 50, 2)
)
set.seed(214)
knn_opp <- train(opp_score ~., data = train, 
                 method = "knn",
                 metric = "MAE",
                 preProc = c("center", "scale"),
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


# opp score random forest model ----
# model
ctrl <- trainControl(method = "cv", number = 5, verboseIter = T)
grid <- expand.grid(
    .mtry = seq(2, 24, 2),
    .splitrule = "variance",
    .min.node.size = 1
)
set.seed(214)
rf_opp <- train(opp_score ~., data = train,
                method = "ranger",
                metric = "MAE",
                preProc = c("center", "scale"),
                trControl = ctrl,
                tuneGrid = grid)

grid <- expand.grid(
    .mtry = seq(2, 24, 2)
)
set.seed(214)
rf_opp <- train(opp_score ~., data = train,
                method = "rf",
                metric = "MAE",
                preProc = c("center", "scale"),
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


# opp score support vector machines model ----
# model
ctrl <- trainControl(method = "cv", number = 5, verboseIter = T)
grid <- expand.grid(
    sigma = c(0.0005, 0.001, 0.005, 0.01),
    C = c(0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.95)
)
set.seed(214)
svm_opp <- train(opp_score ~., data = train,
                  method = "svmRadial",
                  metric = "MAE",
                  preProc = c("center", "scale"),
                  trControl = ctrl,
                  tuneGrid = grid)

grid <- expand.grid(
    C = c(0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.95)
)
set.seed(214)
svm_opp <- train(opp_score ~., data = train,
                  method = "svmLinear",
                  metric = "MAE",
                  preProc = c("center", "scale"),
                  trControl = ctrl,
                  tuneGrid = grid)

svm_opp
svm_opp$resample
svm_opp$results
summary(svm_opp) # model components
plot(svm_opp) # viz

# predictions
opp_pred <- predict(svm_opp, test)

# model evaluation
postResample(pred = opp_pred, obs = test$opp_score) # caret eval
svm_opp_metrics <- postResample(pred = opp_pred, obs = test$opp_score)[1]


# opp score neural net model ----
# model
ctrl <- trainControl(method = "cv", number = 5, verboseIter = T)
grid <- expand.grid(
    decay = c(0.75, 0.5, 0.1, 1e-2, 1e-3, 1e-4, 1e-5, 1e-6, 1e-7),
    size = c(1, 3, 5, 10)
)
nn_opp <- train(opp_score ~., data = train,
                method = "nnet",
                metric = "MAE",
                preProc = c("center", "scale"),
                trControl = ctrl,
                tuneGrid = grid,
                linout = 1)

nn_opp
nn_opp$resample
nn_opp$results
summary(nn_opp) # model components
plot(nn_opp) # viz

# predictions
opp_pred <- predict(nn_opp, test)

# model evaluation
postResample(pred = opp_pred, obs = test$opp_score) # caret eval
nn_opp_metrics <- postResample(pred = opp_pred, obs = test$opp_score)[1]

# variable importance
importance <- varImp(nn_opp, scale = F)
plot(importance)

nn_opp_imp <- rownames_to_column(importance[["importance"]], "Var") %>%
    arrange(desc(Overall)) %>%
    head(20)
nn_opp_imp


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

xgb_opp
xgb_opp$resample
xgb_opp$results
summary(xgb_opp) # model components
plot(xgb_opp) # viz

# predictions
opp_pred <- predict(xgb_opp, test)

# model evaluation
postResample(pred = opp_pred, obs = test$opp_score) # caret eval
xgb_opp_metrics <- postResample(pred = opp_pred, obs = test$opp_score)[1]

# variable importance
importance <- varImp(xgb_opp, scale = F)
plot(importance)

xgb_opp_imp <- rownames_to_column(importance[["importance"]], "Var") %>%
    arrange(desc(Overall)) %>%
    head(20)
xgb_opp_imp


# team score glm boost model ----
# model
ctrl <- trainControl(method = "cv", number = 5, verboseIter = T)
grid <- expand.grid(
    mstop = seq(50, 500, 50),
    prune = c("yes", "yes")
)
set.seed(214)
glm_opp <- train(opp_score ~., data = train,
                  method = "glmboost",
                  metric = "MAE",
                  preProc = c("center", "scale"),
                  trControl = ctrl,
                  tuneGrid = grid)

glm_opp
glm_opp$resample
glm_opp$results
summary(glm_opp) # model components
plot(glm_opp) # viz

# predictions
opp_pred <- predict(glm_opp, test)

# model evaluation
postResample(pred = opp_pred, obs = test$opp_score) # caret eval
glm_opp_metrics <- postResample(pred = opp_pred, obs = test$opp_score)[1]






# # team winner ensemble model ----
# # https://www.stepbystepdatascience.com/hyperparameter-tuning-and-model-stacking-with-caret
# # model
# library(caretEnsemble)
# ensem_control <- trainControl(method = "repeatedcv", number = 5, repeats = 3,
#                               verboseIter = T, classProbs = T,
#                               savePredictions = "final")
# 
# glmnet_grid <- expand.grid(
#   alpha = 0, # ridge = 0 / lasso = 1
#   lambda = 10^seq(2, -3, by = -.1)
# )
# rf_grid <- expand.grid(
#   .mtry = 1:7,
#   .splitrule = "gini",
#   .min.node.size = 1
# )
# glmboost_grid <- expand.grid(
#   mstop = seq(50, 500, 50),
#   prune = c("no")
# )
# earth_grid <- expand.grid(
#   nprune = c(1:5),
#   degree = c(1:5)
# )
# set.seed(214)
# all_in_one <- caretList(team_winner ~., data = train,
#                         trControl = ensem_control,
#                         preProc = c("center", "scale"),
#                         tuneList=list(lm = caretModelSpec(method = "glm",
#                                                           family = "binomial"),
#                                       glmnet = caretModelSpec(method = "glmnet",
#                                                               tuneGrid = glmnet_grid),
#                                       rf = caretModelSpec(method = "ranger",
#                                                           tuneGrid = rf_grid),
#                                       glmboost = caretModelSpec(method = "glmboost",
#                                                            tuneGrid = glmboost_grid),
#                                       mars = caretModelSpec(method = "earth",
#                                                             tuneGrid = earth_grid)
#                         )
# )
# 
# # Have a look at the models
# rbind(getTrainPerf(all_in_one$"lm"),
#       getTrainPerf(all_in_one$"glmnet"),
#       getTrainPerf(all_in_one$"rf"),
#       getTrainPerf(all_in_one$"glmboost"),
#       getTrainPerf(all_in_one$"mars")) %>% 
#     arrange(desc(TrainAccuracy))
# 
# # Plot the results
# bwplot(resamples(all_in_one))
# 
# # Have a look at the correlation amongst the models
# modelCor(resamples(all_in_one))
# 
# # Add on average correlation amongst models
# rbind(as_tibble(modelCor(resamples(all_in_one)), rownames = "models"),
#       summarise_all(as_tibble(modelCor(resamples(all_in_one))), mean) %>% 
#           mutate(models = "average") %>% 
#           select(models, everything()))
# 
# # Caret ensemble will ensemble all models in your list
# all_models_ensemble <- caretEnsemble(all_in_one)
# all_models_ensemble # print the performance of the stack
# 
# summary(all_models_ensemble) # print a bit more detail
# 
# # We can pass a trControl to our caretEnsemble too
# ensembleCtrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3)
# 
# cv_ensemble <- caretEnsemble(all_in_one, trControl = ensembleCtrl)
# 
# summary(cv_ensemble) 
# 
# 
# # Create the trainControl - don't reuse the one from the model training
# stackControl <- trainControl(method = "repeatedcv", number = 5, repeats = 3,
#                              verboseIter = T, classProbs = T,
#                              savePredictions = "final")
# 
# set.seed(214)
# stack_glmboost <- caretStack(all_in_one,
#                              method = "glmboost",
#                              tuneLength = 50,
#                              trControl = stackControl)
# 
# set.seed(214)
# stack_mars <- caretStack(all_in_one,
#                          method = "earth",
#                          tuneLength = 50,
#                          trControl = stackControl)
# 
# # Which model performed the best?
# as_tibble(rbind(lm = getTrainPerf(all_in_one$"lm")$TrainAccuracy,
#                 glmnet = getTrainPerf(all_in_one$"glmnet")$TrainAccuracy,
#                 rf = getTrainPerf(all_in_one$"rf")$TrainAccuracy,
#                 glmboost = getTrainPerf(all_in_one$"glmboost")$TrainAccuracy,
#                 mars = getTrainPerf(all_in_one$"mars")$TrainAccuracy,
#                 stack_glm = mean(stack_glmboost$error$Accuracy),
#                 stack_mars = mean(stack_mars$error$Accuracy)),
#           rownames="models") %>%
#   rename(accuracy = V1) %>%
#   arrange(desc(accuracy))
# 
# 
# # predictions
# win_pred <- predict(all_in_one$"glm", test, type = "prob")
# confusionMatrix(test$team_winner,
#                 factor(ifelse(win_pred[,1] > 0.5, "win", "loss"), 
#                        levels = c("win","loss")),
#                 positive = "win")
# # confusionMatrix(test$team_winner,
# #                 factor(ifelse(win_pred > 0.5, "win", "loss"), 
# #                        levels = c("win","loss")),
# #                 positive = "win")







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









