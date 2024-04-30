### Results Book ----

library(tidyverse)
library(data.table)
library(nbastatR)

options(scipen = 999999)

#### need function to get current season game info and model outputs


mamba_lag_long <- tbl(dbConnect(SQLite(), "../nba_sql_db/nba_db"),
                      "mamba_lag_long") %>%
    collect() %>%
    mutate(game_date = as_date(game_date, origin ="1970-01-01"))

slate_final <- read_csv("/Users/jesse/Desktop/slate_final.csv")


#### This should match nba_final in eval file
mamba_long_odds <- mamba_lag_long %>%
    left_join(slate_final %>% select(game_date, team_name,
                                 team_spread:opp_implied_prob),
              by = c("game_date", "team_name")) %>%
    select(season_year:opp_is_b2b_second,
           team_spread:opp_implied_prob,
           team_fgm:opp_opp_pct_uast_fgm) %>%
    na.exclude()

nba_final <- mamba_long_odds %>%
    # filter(season_year >= 2020) %>%
    rename(team_winner = wl,
           team_score = pts,
           opp_score = opp_pts) %>%
    mutate(
        game_date = as_date(game_date, origin ="1970-01-01"),
        team_winner = if_else(team_winner == "W", "win", "loss"),
        team_winner = factor(team_winner, levels = c("win", "loss")),
        location = if_else(location == "away", 1, 0)
    )
    

# nba_final <- tbl(dbConnect(SQLite(), "../nba_sql_db/nba_db"), "mamba_long_odds") %>%
#     collect()


#### This should match model_outputs in eval file
nba_model_outputs <- nba_final %>%
    # filter(season_year == 2024) %>%
    select(season_year:opp_implied_prob) %>%
    mutate(location = if_else(location == 1, "away", "home")) %>%
    left_join(
        slate_final %>%
            select(game_id, team_name, log_win_team:nn_win_opp),
        by = c("game_id" = "game_id", "team_name" = "team_name")
    ) %>%
    mutate(
        across(log_win_team:nn_win_opp, ~if_else(is.na(.), 1 - lag(.), .))
    ) %>%
    left_join(
        slate_final %>%
            select(game_id, team_name, lin_score_team:nn_score_opp),
        by = c("game_id" = "game_id", "team_name" = "team_name")
    )
    
    
    
    





