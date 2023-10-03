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
nba_final <- tbl(dbConnect(SQLite(), "../nba_sql_db/nba_db"), "game_logs_adj") %>% 
    collect() %>%
    mutate(game_date = as_date(game_date, origin ="1970-01-01"))

cor_train <- nba_final %>%
    filter(season <= 2021) %>%
    select(team_winner,team_margin)

cor_test <- nba_final %>%
    filter(season > 2021) %>%
    select()















