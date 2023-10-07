### model tuning ----
library(tidyverse)
library(RSQLite)
library(DBI)

# library(nbastatR)

# library(caTools)
library(tidymodels)
library(caret) # model training
# library(ggfortify) # autoplot
# library(glmnet) # regularization
# library(ranger) # rf
# library(xgboost) # xgb
# library(e1071) # svm
# library(nnet) # nn

# library(RSQLite) # db
# library(DBI) # db

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


# correlations - win
cor_win_df <- nba_final %>%
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

cor_win <- cor(cor_win_df, cor_win_df$team_winner)
cor_win <- as.matrix(cor_win[order(abs(cor_win[,1]), decreasing = T),])
cor_win


# correlations - team score
cor_team_df <- nba_final %>%
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

cor_team <- cor(cor_team_df, cor_team_df$team_score)
cor_team <- as.matrix(cor_team[order(abs(cor_team[,1]), decreasing = T),])
cor_team


# correlations - opp score
cor_opp_df <- nba_final %>%
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

cor_opp <- cor(cor_opp_df, cor_opp_df$opp_score)
cor_opp <- as.matrix(cor_opp[order(abs(cor_opp[,1]), decreasing = T),])
cor_opp





nba_final_win <- nba_final %>%
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

nearZeroVar(nba_final_win, saveMetrics = T)

base_cor <-  cor(nba_final_win[, 2:ncol(nba_final_win)])
extreme_cor <- sum(abs(base_cor[upper.tri(base_cor)]) > .999)
extreme_cor

col_rm <- findCorrelation(base_cor, cutoff = .5, exact = F, names = T)

nba_final_win_filtered <- nba_final_win %>% select(-col_rm)

new_cor <- cor(nba_final_win_filtered[, 2:ncol(nba_final_win_filtered)])

findCorrelation(new_cor, cutoff = .5)

summary(new_cor[upper.tri(new_cor)])



set.seed(214)
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

train <- train %>% select(-col_rm)
test <- test %>% select(-col_rm)


pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))

train[,-1] = predict(pre_proc_val, train[,-1])
test[,-1] = predict(pre_proc_val, test[,-1])

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
summary(log_win) # Model Components
confusionMatrix(log_win) # Confusion Matrix
glance(log_win$finalModel) # Entire Model - tidymodels
tidy(log_win$finalModel) # Model Components - tidymodels
augment(log_win$finalModel) # Observations - tidymodels

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

twoClassSummary(obs_pred, lev = levels(obs)) # ROC
prSummary(obs_pred, lev = levels(obs)) # AUC
mnLogLoss(obs_pred, lev = levels(obs)) # Log Loss


library(MLeval)
eval <- evalm(data.frame(win_pred, test$team_winner))

library(pROC)
roc_score = roc(test$team_winner, away_pred, plot = T, legacy.axes = T,
                percent = T,
                xlab="False Positive Percentage",
                ylab="True Postive Percentage",
                col="#377eb8", lwd = 4, print.auc = T)


obs_pred %>% metrics(obs, pred)
obs_pred %>% roc_auc(obs, win)


importance <- varImp(log_win, scale = F)
print(importance)
plot(importance)


log_win_imp <- rownames_to_column(varImp(log_win, scale = F)[["importance"]], "Var") %>%
    arrange(desc(Overall)) %>%
    head(20)
log_win_imp





