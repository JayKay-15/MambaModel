### model tuning ----
library(tidyverse)
library(RSQLite)
library(DBI)

# library(lubridate)
# library(nbastatR)

# library(caTools)
# library(tidymodels)
# library(caret) # model training
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
    mutate(game_date = as_date(game_date, origin ="1970-01-01"))

cor_win_df <- nba_final %>%
    select(team_winner,
           away_implied_prob:away_opp_pf_pct,away_pace,
           home_fg2m:home_pf_pct,home_pace) %>%
    mutate(team_winner = if_else(team_winner == "win", 1, 0))

cor_win <- cor(cor_win_df, cor_win_df$team_winner)
cor_win <- as.matrix(cor_win[order(abs(cor_win[,1]), decreasing = T),])
cor_win


cor_win_df_away <- nba_final %>%
    select(team_winner,
           away_implied_prob,away_fg2m:away_opp_pf_pct,away_pace) %>%
    mutate(team_winner = if_else(team_winner == "win", 1, 0))

cor_win_away <- cor(cor_win_df_away, cor_win_df_away$team_winner)
cor_win_away <- as.matrix(cor_win_away[order(abs(cor_win_away[,1]), decreasing = T),])
cor_win_away

cor_win_df_home <- nba_final %>%
    select(team_winner,
           home_implied_prob,home_fg2m:home_opp_pf_pct,home_pace) %>%
    mutate(team_winner = if_else(team_winner == "win", 1, 0))

cor_win_home <- cor(cor_win_df_home, cor_win_df_home$team_winner)
cor_win_home <- as.matrix(cor_win_home[order(abs(cor_win_home[,1]), decreasing = T),])
cor_win_home










cor_team_df <- nba_final %>%
    select(team_score,
           away_implied_prob:away_opp_pf_pct,away_pace,
           home_fg2m:home_pf_pct,home_pace)

cor_team <- cor(cor_team_df, cor_team_df$team_score)
cor_team <- as.matrix(cor_team[order(abs(cor_team[,1]), decreasing = T),])
cor_team


cor_opp_df <- nba_final %>%
    select(opp_score,
           away_implied_prob:away_opp_pf_pct,away_pace,
           home_fg2m:home_pf_pct,home_pace)

cor_opp <- cor(cor_opp_df, cor_opp_df$opp_score)
cor_opp <- as.matrix(cor_opp[order(abs(cor_opp[,1]), decreasing = T),])
cor_opp
















