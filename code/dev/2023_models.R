### model tuning ----

library(tidyverse)
library(RSQLite) # db
library(DBI) # db
library(caret) # model training
library(tidymodels) # model eval
# library(MLeval) # model eval
# library(pROC) # model eval
library(ggfortify) # autoplot
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

eval <- MLeval::evalm(data.frame(win_pred, test$team_winner)) # roc chart
roc_score <- pROC::roc(test$team_winner, away_pred, plot = T, legacy.axes = T,
                percent = T,
                xlab="False Positive Percentage",
                ylab="True Postive Percentage",
                col="#377eb8", lwd = 4, print.auc = T) # roc chart

postResample(pred = pred, obs = obs) # caret eval
obs_pred %>% metrics(obs, pred) # accuracy and kappa
obs_pred %>% roc_auc(obs, win) # auc

# feature importance
importance <- varImp(log_win, scale = F)
print(importance)
plot(importance)

log_win_imp <- rownames_to_column(importance[["importance"]], "Var") %>%
    arrange(desc(Overall)) %>%
    head(20)
log_win_imp


# results testing ----

results <- bind_cols(nba_final %>%
                         filter(season > 2021) %>%
                         select(season,game_id,
                                away_moneyline,home_moneyline),
                     obs_pred)


results <- results %>%
    mutate(
        away_odds = if_else(away_moneyline<0,
                            (-away_moneyline/(-away_moneyline+100)),
                            (100/(away_moneyline+100))),
        home_odds = if_else(home_moneyline<0,
                            (-home_moneyline/(-home_moneyline+100)),
                            (100/(home_moneyline+100))),
        away_edge = if_else(win - away_odds > 0, 1, 0),
        home_edge = if_else(loss - home_odds > 0, 1, 0),
        away_ml_win = if_else(away_moneyline > 0, away_moneyline/100, 1),
        home_ml_win = if_else(home_moneyline > 0, home_moneyline/100, 1),
        away_ml_loss = if_else(away_moneyline < 0, (away_moneyline/100), -1),
        home_ml_loss = if_else(home_moneyline < 0, (home_moneyline/100), -1),
        bet_result = case_when(away_edge == 1 & obs == "win" ~ away_ml_win,
                         home_edge == 1 & obs == "loss" ~ home_ml_win,
                         away_edge == 1 & obs == "loss" ~ away_ml_loss,
                         home_edge == 1 & obs == "win" ~ home_ml_loss,
                         TRUE ~ 0),
        cume = cumsum(bet_result)
    )


results_2022 <- results %>%
    filter(season == 2022) %>%
    mutate(
        away_odds = if_else(away_moneyline<0,
                            (-away_moneyline/(-away_moneyline+100)),
                            (100/(away_moneyline+100))),
        home_odds = if_else(home_moneyline<0,
                            (-home_moneyline/(-home_moneyline+100)),
                            (100/(home_moneyline+100))),
        away_edge = if_else(win - away_odds > 0, 1, 0),
        home_edge = if_else(loss - home_odds > 0, 1, 0),
        away_ml_win = if_else(away_moneyline > 0, away_moneyline/100, 1),
        home_ml_win = if_else(home_moneyline > 0, home_moneyline/100, 1),
        away_ml_loss = if_else(away_moneyline < 0, (away_moneyline/100), -1),
        home_ml_loss = if_else(home_moneyline < 0, (home_moneyline/100), -1),
        bet_result = case_when(away_edge == 1 & obs == "win" ~ away_ml_win,
                               home_edge == 1 & obs == "loss" ~ home_ml_win,
                               away_edge == 1 & obs == "loss" ~ away_ml_loss,
                               home_edge == 1 & obs == "win" ~ home_ml_loss,
                               TRUE ~ 0),
        cume = cumsum(bet_result)
    )

max(results_2022$cume)
tail(results_2022$cume, 1)
plot(results_2022$cume, type = "line")


results_2023 <- results %>%
    filter(season == 2023) %>%
    mutate(
        away_odds = if_else(away_moneyline<0,
                            (-away_moneyline/(-away_moneyline+100)),
                            (100/(away_moneyline+100))),
        home_odds = if_else(home_moneyline<0,
                            (-home_moneyline/(-home_moneyline+100)),
                            (100/(home_moneyline+100))),
        away_edge = if_else(win - away_odds > 0, 1, 0),
        home_edge = if_else(loss - home_odds > 0, 1, 0),
        away_ml_win = if_else(away_moneyline > 0, away_moneyline/100, 1),
        home_ml_win = if_else(home_moneyline > 0, home_moneyline/100, 1),
        away_ml_loss = if_else(away_moneyline < 0, (away_moneyline/100), -1),
        home_ml_loss = if_else(home_moneyline < 0, (home_moneyline/100), -1),
        bet_result = case_when(away_edge == 1 & obs == "win" ~ away_ml_win,
                               home_edge == 1 & obs == "loss" ~ home_ml_win,
                               away_edge == 1 & obs == "loss" ~ away_ml_loss,
                               home_edge == 1 & obs == "win" ~ home_ml_loss,
                               TRUE ~ 0),
        cume = cumsum(bet_result)
    )

max(results_2023$cume)
tail(results_2023$cume, 1)
plot(results_2023$cume, type = "line")




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
# plot(lin_team$finalModel) # viz

# predictions
team_pred <- predict(lin_team, test)

# model evaluation
postResample(pred = team_pred, obs = test$team_score) # caret eval

rmse <- RMSE(team_pred, test$team_score)
mae <- MAE(team_pred, test$team_score)
r2 <- R2(team_pred, test$team_score)

cat("RMSE: ", rmse, "MAE: ", mae, " R2: ", r2)

# variable importance
importance <- varImp(lin_team, scale = F)
print(importance)
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
# plot(lin_opp$finalModel) # viz

# predictions
opp_pred <- predict(lin_opp, test)

# model evaluation
postResample(pred = opp_pred, obs = test$opp_score) # caret eval

rmse <- RMSE(opp_pred, test$opp_score)
mae <- MAE(opp_pred, test$opp_score)
r2 <- R2(opp_pred, test$opp_score)

cat("RMSE: ", rmse, "MAE: ", mae, " R2: ", r2)

# variable importance
importance <- varImp(lin_opp, scale = F)
print(importance)
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

eval <- MLeval::evalm(data.frame(win_pred, test$team_winner)) # roc chart
roc_score <- pROC::roc(test$team_winner, away_pred, plot = T, legacy.axes = T,
                       percent = T,
                       xlab="False Positive Percentage",
                       ylab="True Postive Percentage",
                       col="#377eb8", lwd = 4, print.auc = T) # roc chart

postResample(pred = pred, obs = obs) # caret eval
obs_pred %>% metrics(obs, pred) # accuracy and kappa
obs_pred %>% roc_auc(obs, win) # auc

# feature importance
importance <- varImp(reg_win, scale = F)
print(importance)
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
summary(reg_team) # model components
autoplot(reg_team$finalModel) # viz - ggfortify
glance(reg_team$finalModel) # entire model - tidymodels
tidy(reg_team$finalModel) # model components - tidymodels
# plot(reg_team$finalModel) # viz

# predictions
team_pred <- predict(reg_team, test)

# model evaluation
postResample(pred = team_pred, obs = test$team_score) # caret eval

rmse <- RMSE(team_pred, test$team_score)
mae <- MAE(team_pred, test$team_score)
r2 <- R2(team_pred, test$team_score)

cat("RMSE: ", rmse, "MAE: ", mae, " R2: ", r2)

# variable importance
importance <- varImp(reg_team, scale = F)
print(importance)
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
summary(reg_opp) # model components
autoplot(reg_opp$finalModel) # viz - ggfortify
glance(reg_opp$finalModel) # entire model - tidymodels
tidy(reg_opp$finalModel) # model components - tidymodels
# plot(reg_opp$finalModel) # viz

# predictions
opp_pred <- predict(reg_opp, test)

# model evaluation
postResample(pred = opp_pred, obs = test$opp_score) # caret eval

rmse <- RMSE(opp_pred, test$opp_score)
mae <- MAE(opp_pred, test$opp_score)
r2 <- R2(opp_pred, test$opp_score)

cat("RMSE: ", rmse, "MAE: ", mae, " R2: ", r2)

# variable importance
importance <- varImp(lin_opp, scale = F)
print(importance)
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
obs_pred %>% metrics(obs, pred) # accuracy and kappa
obs_pred %>% roc_auc(obs, win) # auc


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
summary(knn_team) # model components
plot(knn_team) # viz

# predictions
team_pred <- predict(knn_team, test)

# model evaluation
postResample(pred = team_pred, obs = test$team_score) # caret eval

rmse <- RMSE(team_pred, test$team_score)
mae <- MAE(team_pred, test$team_score)
r2 <- R2(team_pred, test$team_score)

cat("RMSE: ", rmse, "MAE: ", mae, " R2: ", r2)


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
    k = seq(2, 50, 2)
)
knn_opp <- train(opp_score ~., data = train, 
                  method = "knn",
                  trControl = ctrl,
                  tuneGrid = grid)
knn_opp
summary(knn_opp) # model components
plot(knn_opp) # viz

# predictions
opp_pred <- predict(knn_opp, test)

# model evaluation
postResample(pred = opp_pred, obs = test$opp_score) # caret eval

rmse <- RMSE(opp_pred, test$opp_score)
mae <- MAE(opp_pred, test$opp_score)
r2 <- R2(opp_pred, test$opp_score)

cat("RMSE: ", rmse, "MAE: ", mae, " R2: ", r2)


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

eval <- MLeval::evalm(data.frame(win_pred, test$team_winner)) # roc chart
roc_score <- pROC::roc(test$team_winner, away_pred, plot = T, legacy.axes = T,
                       percent = T,
                       xlab="False Positive Percentage",
                       ylab="True Postive Percentage",
                       col="#377eb8", lwd = 4, print.auc = T) # roc chart

postResample(pred = pred, obs = obs) # caret eval


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
xgb_win
xgb_win$resample
xgb_win$results
summary(xgb_win) # model components
confusionMatrix(xgb_win) # confusion matrix
glance(xgb_win$finalModel) # entire model - tidymodels
tidy(xgb_win$finalModel) # model components - tidymodels

# predictions
win_pred <- predict(xgb_win, test, type = "prob")
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

# feature importance
importance <- varImp(xgb_win, scale = F)
print(importance)
plot(importance)

xgb_win_imp <- rownames_to_column(importance[["importance"]], "Var") %>%
    arrange(desc(Overall)) %>%
    head(20)
xgb_win_imp


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
xgb_team
xgb_team$resample
xgb_team$results
summary(xgb_team) # model components
plot(xgb_team) # viz

# predictions
team_pred <- predict(xgb_team, test)

# model evaluation
postResample(pred = team_pred, obs = test$team_score) # caret eval

rmse <- RMSE(team_pred, test$team_score)
mae <- MAE(team_pred, test$team_score)
r2 <- R2(team_pred, test$team_score)

cat("RMSE: ", rmse, "MAE: ", mae, " R2: ", r2)

# variable importance
importance <- varImp(xgb_team, scale = F)
print(importance)
plot(importance)

xgb_team_imp <- rownames_to_column(importance[["importance"]], "Var") %>%
    arrange(desc(Overall)) %>%
    head(20)
xgb_team_imp


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
xgb_opp
xgb_opp$resample
xgb_opp$results
summary(xgb_opp) # model components
plot(xgb_opp) # viz

# predictions
team_pred <- predict(xgb_opp, test)

# model evaluation
postResample(pred = team_pred, obs = test$opp_score) # caret eval

rmse <- RMSE(team_pred, test$opp_score)
mae <- MAE(team_pred, test$opp_score)
r2 <- R2(team_pred, test$opp_score)

cat("RMSE: ", rmse, "MAE: ", mae, " R2: ", r2)

# variable importance
importance <- varImp(xgb_opp, scale = F)
print(importance)
plot(importance)

xgb_opp_imp <- rownames_to_column(importance[["importance"]], "Var") %>%
    arrange(desc(Overall)) %>%
    head(20)
xgb_opp_imp











# soccer work ----

football <- readRDS("/Users/jesse/Desktop/football_file.RDS")

football <- football %>%
    mutate(bet = pmax(edge_ho, edge_dr, edge_aw),
           bet_type = case_when(
               edge_ho == pmax(edge_ho, edge_dr, edge_aw) ~ "edge_ho",
               edge_dr == pmax(edge_ho, edge_dr, edge_aw) ~ "edge_dr",
               edge_aw == pmax(edge_ho, edge_dr, edge_aw) ~ "edge_aw"),
           filter = case_when(
               pmax(B365H, B365D, B365A) > 5 ~ "rm",
               TRUE ~ "keep")) %>%
    filter(filter == "keep") %>%
    select(Date, HomeTeam, AwayTeam, bet, bet_type) %>%
    head(15)








