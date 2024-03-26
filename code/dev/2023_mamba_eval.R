### model training ----

library(tidyverse)
library(RSQLite) # db
library(DBI) # db
library(caret) # model training
library(tidymodels) # model eval

options(scipen = 999999)
# Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)

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

nba_final_win_outputs <- nba_final %>%
    filter(season_year == 2024 & location == 1) %>%
    select(season_year:opp_implied_prob) %>%
    mutate(location = if_else(location == 1, "away", "home"))

nba_final_ts_outputs <- nba_final %>%
    filter(season_year == 2024) %>%
    select(season_year:opp_implied_prob) %>%
    mutate(location = if_else(location == 1, "away", "home"))

model_outputs <- nba_final %>%
    filter(season_year == 2024) %>%
    select(season_year:opp_implied_prob) %>%
    mutate(location = if_else(location == 1, "away", "home"))


# team winner models ----
nba_final_test <- nba_final %>%
    filter(season_year == 2024 & location == 1) %>%
    select(team_winner,
           is_b2b_first:opp_is_b2b_second, over_under, team_implied_prob,
           team_fgm:opp_opp_pct_uast_fgm) %>%
    select(-contains("_rating"))

cor_cols <- read_rds("../NBAdb/models/trained_models/cor_cols_win.rds")
pre_proc_cs_win <- read_rds("../NBAdb/models/trained_models/pre_proc_cs_win.rds")

test <- nba_final_test %>%
    select(-all_of(cor_cols)) %>%
    mutate(across(is_b2b_first:opp_is_b2b_second, factor))

test[,-1] <- predict(pre_proc_cs_win, test[,-1])

# team winner logistic regression model ----
log_win <- read_rds("../NBAdb/models/trained_models/log_win_20_23.rds")

# predictions
win_pred <- predict(log_win, test, type = "prob")

confusionMatrix(test$team_winner,
                factor(ifelse(win_pred[,1] > 0.5, "win", "loss"), 
                       levels = c("win","loss")),
                positive = "win")

nba_final_win_outputs <- nba_final_win_outputs %>%
    mutate(log_win_team = as.numeric(win_pred[,1]),
           log_win_opp = as.numeric(win_pred[,2]))


# team winner ridge regression model ----
reg_win <- read_rds("../NBAdb/models/trained_models/reg_win_20_23.rds")

# predictions
win_pred <- predict(reg_win, test, type = "prob")
confusionMatrix(test$team_winner,
                factor(ifelse(win_pred[,1] > 0.5, "win", "loss"), 
                       levels = c("win","loss")),
                positive = "win")

nba_final_win_outputs <- nba_final_win_outputs %>%
    mutate(reg_win_team = as.numeric(win_pred[,1]),
           reg_win_opp = as.numeric(win_pred[,2]))


# team winner random forest model ----
rf_win <- read_rds("../NBAdb/models/trained_models/rf_win_20_23.rds")

# predictions
win_pred <- predict(rf_win, test, type = "prob")
confusionMatrix(test$team_winner,
                factor(ifelse(win_pred[,1] > 0.5, "win", "loss"), 
                       levels = c("win","loss")),
                positive = "win")

nba_final_win_outputs <- nba_final_win_outputs %>%
    mutate(rf_win_team = as.numeric(win_pred[,1]),
           rf_win_opp = as.numeric(win_pred[,2]))

# team winner extreme gradient boosting model ----
xgb_win <- read_rds("../NBAdb/models/trained_models/xgb_win_20_23.rds")

# predictions
win_pred <- predict(xgb_win, test, type = "prob")
confusionMatrix(test$team_winner,
                factor(ifelse(win_pred[,1] > 0.5, "win", "loss"), 
                       levels = c("win","loss")),
                positive = "win")

nba_final_win_outputs <- nba_final_win_outputs %>%
    mutate(xgb_win_team = as.numeric(win_pred[,1]),
           xgb_win_opp = as.numeric(win_pred[,2]))

# team winner boosted generalized linear model ----
glmb_win <- read_rds("../NBAdb/models/trained_models/glmb_win_20_23.rds")

# predictions
win_pred <- predict(glmb_win, test, type = "prob")
confusionMatrix(test$team_winner,
                factor(ifelse(win_pred[,1] > 0.5, "win", "loss"), 
                       levels = c("win","loss")),
                positive = "win")

nba_final_win_outputs <- nba_final_win_outputs %>%
    mutate(glmb_win_team = as.numeric(win_pred[,1]),
           glmb_win_opp = as.numeric(win_pred[,2]))

# team winner earth model ----
mars_win <- read_rds("../NBAdb/models/trained_models/mars_win_20_23.rds")

# predictions
win_pred <- predict(mars_win, test, type = "prob")
confusionMatrix(test$team_winner,
                factor(ifelse(win_pred[,1] > 0.5, "win", "loss"), 
                       levels = c("win","loss")),
                positive = "win")

nba_final_win_outputs <- nba_final_win_outputs %>%
    mutate(mars_win_team = as.numeric(win_pred[,1]),
           mars_win_opp = as.numeric(win_pred[,2]))

# Yeo Johnson pre-processed
pre_proc_yj_win <- read_rds("../NBAdb/models/trained_models/pre_proc_yj_win.rds")

test <- nba_final_test %>%
    select(-all_of(cor_cols)) %>%
    mutate(across(is_b2b_first:opp_is_b2b_second, factor))

test[,-1] <- predict(pre_proc_yj_win, test[,-1])

# team winner support vector machines model ----
svm_win <- read_rds("../NBAdb/models/trained_models/svm_win_20_23.rds")

# predictions
win_pred <- predict(svm_win, test, type = "prob")
confusionMatrix(test$team_winner,
                factor(ifelse(win_pred[,1] > 0.5, "win", "loss"), 
                       levels = c("win","loss")),
                positive = "win")

nba_final_win_outputs <- nba_final_win_outputs %>%
    mutate(svm_win_team = as.numeric(win_pred[,1]),
           svm_win_opp = as.numeric(win_pred[,2]))

# team winner neural net model ----
nn_win <- read_rds("../NBAdb/models/trained_models/nn_win_20_23.rds")

# predictions
win_pred <- predict(nn_win, test, type = "prob")
confusionMatrix(test$team_winner,
                factor(ifelse(win_pred[,1] > 0.5, "win", "loss"), 
                       levels = c("win","loss")),
                positive = "win")

nba_final_win_outputs <- nba_final_win_outputs %>%
    mutate(nn_win_team = as.numeric(win_pred[,1]),
           nn_win_opp = as.numeric(win_pred[,2]))



# team score models ----
nba_final_test <- nba_final %>%
    filter(season_year == 2024) %>%
    select(team_score, location,
           is_b2b_first:opp_is_b2b_second, over_under, team_implied_prob,
           team_fgm:opp_opp_pct_uast_fgm)

cor_cols <- read_rds("../NBAdb/models/trained_models/cor_cols_score.rds")
pre_proc_cs_score <- read_rds("../NBAdb/models/trained_models/pre_proc_cs_score.rds")

test <- nba_final_test %>%
    select(-all_of(cor_cols)) %>%
    mutate(across(location:opp_is_b2b_second, factor))

test[,-1] <- predict(pre_proc_cs_score, test[,-1])


# team score linear regression model ----
lin_team <- read_rds("../NBAdb/models/trained_models/lin_team_20_23.rds")

# predictions
team_pred <- predict(lin_team, test)
postResample(pred = team_pred, obs = test$team_score)

team_pred <- data.frame(team_pred)
team_pred <- team_pred %>%
    mutate(opp_pred = if_else(row_number() %% 2 == 0, lag(team_pred), lead(team_pred)))
nba_final_ts_outputs <- nba_final_ts_outputs %>%
    mutate(lin_team_score = as.numeric(team_pred$team_pred),
           lin_opp_score = as.numeric(team_pred$opp_pred))


# team score ridge regression model ----
reg_team <- read_rds("../NBAdb/models/trained_models/reg_team_20_23.rds")

# predictions
team_pred <- predict(reg_team, test)
postResample(pred = team_pred, obs = test$team_score)

team_pred <- data.frame(team_pred)
team_pred <- team_pred %>%
    mutate(opp_pred = if_else(row_number() %% 2 == 0, lag(team_pred), lead(team_pred)))
nba_final_ts_outputs <- nba_final_ts_outputs %>%
    mutate(reg_team_score = as.numeric(team_pred$team_pred),
           reg_opp_score = as.numeric(team_pred$opp_pred))


# team score random forest model ----
rf_team <- read_rds("../NBAdb/models/trained_models/rf_team_20_23.rds")


# predictions
team_pred <- predict(rf_team, test)
postResample(pred = team_pred, obs = test$team_score)

team_pred <- data.frame(team_pred)
team_pred <- team_pred %>%
    mutate(opp_pred = if_else(row_number() %% 2 == 0, lag(team_pred), lead(team_pred)))
nba_final_ts_outputs <- nba_final_ts_outputs %>%
    mutate(rf_team_score = as.numeric(team_pred$team_pred),
           rf_opp_score = as.numeric(team_pred$opp_pred))


# team score extreme gradient boosting model ----
xgb_team <- read_rds("../NBAdb/models/trained_models/xgb_team_20_23.rds")

# predictions
team_pred <- predict(xgb_team, test)
postResample(pred = team_pred, obs = test$team_score)

team_pred <- data.frame(team_pred)
team_pred <- team_pred %>%
    mutate(opp_pred = if_else(row_number() %% 2 == 0, lag(team_pred), lead(team_pred)))
nba_final_ts_outputs <- nba_final_ts_outputs %>%
    mutate(xgb_team_score = as.numeric(team_pred$team_pred),
           xgb_opp_score = as.numeric(team_pred$opp_pred))


# team score boosted generalized linear model ----
glmb_team <- read_rds("../NBAdb/models/trained_models/glmb_team_20_23.rds")

# predictions
team_pred <- predict(glmb_team, test)
postResample(pred = team_pred, obs = test$team_score)

team_pred <- data.frame(team_pred)
team_pred <- team_pred %>%
    mutate(opp_pred = if_else(row_number() %% 2 == 0, lag(team_pred), lead(team_pred)))
nba_final_ts_outputs <- nba_final_ts_outputs %>%
    mutate(glmb_team_score = as.numeric(team_pred$team_pred),
           glmb_opp_score = as.numeric(team_pred$opp_pred))


# team score earth model ----
mars_team <- read_rds("../NBAdb/models/trained_models/mars_team_20_23.rds")

# predictions
team_pred <- predict(mars_team, test)
postResample(pred = team_pred, obs = test$team_score)

team_pred <- data.frame(team_pred) %>% rename(team_pred = y)
team_pred <- team_pred %>%
    mutate(opp_pred = if_else(row_number() %% 2 == 0, lag(team_pred), lead(team_pred)))
nba_final_ts_outputs <- nba_final_ts_outputs %>%
    mutate(mars_team_score = as.numeric(team_pred$team_pred),
           mars_opp_score = as.numeric(team_pred$opp_pred))


# Yeo Johnson pre-processed
pre_proc_yj_score <- read_rds("../NBAdb/models/trained_models/pre_proc_yj_score.rds")

test <- nba_final_test %>%
    select(-all_of(cor_cols)) %>%
    mutate(across(location:opp_is_b2b_second, factor))

test[,-1] <- predict(pre_proc_yj_score, test[,-1])

# team score support vector machines model ----
svm_team <- read_rds("../NBAdb/models/trained_models/svm_team_20_23.rds")

# predictions
team_pred <- predict(svm_team, test)
postResample(pred = team_pred, obs = test$team_score)

team_pred <- data.frame(team_pred)
team_pred <- team_pred %>%
    mutate(opp_pred = if_else(row_number() %% 2 == 0, lag(team_pred), lead(team_pred)))
nba_final_ts_outputs <- nba_final_ts_outputs %>%
    mutate(svm_team_score = as.numeric(team_pred$team_pred),
           svm_opp_score = as.numeric(team_pred$opp_pred))


# team score neural net model ----
nn_team <- read_rds("../NBAdb/models/trained_models/nn_team_20_23.rds")

# predictions
team_pred <- predict(nn_team, test)
postResample(pred = team_pred, obs = test$team_score)

team_pred <- data.frame(team_pred)
team_pred <- team_pred %>%
    mutate(opp_pred = if_else(row_number() %% 2 == 0, lag(team_pred), lead(team_pred)))
nba_final_ts_outputs <- nba_final_ts_outputs %>%
    mutate(nn_team_score = as.numeric(team_pred$team_pred),
           nn_opp_score = as.numeric(team_pred$opp_pred))


model_outputs <- model_outputs %>%
    left_join(
        nba_final_win_outputs %>%
            select(game_id, team_id, log_win_team:nn_win_opp),
        by = c("game_id" = "game_id", "team_id" = "team_id")
    ) %>%
    mutate(
        across(log_win_team:nn_win_opp, ~if_else(is.na(.), 1 - lag(.), .))
    ) %>%
    left_join(
        nba_final_ts_outputs %>%
            select(game_id, team_id, lin_team_score:nn_opp_score),
        by = c("game_id" = "game_id", "team_id" = "team_id")
    )


# clear environment ----
rm(list=ls()[! ls() %in% c("model_outputs")])

#### function to created results book ---- combined over/under
process_results_book <- function(model_outputs) {
    
    # results book ----
    results_book <- model_outputs %>%
        mutate(
            team_ml_result = case_when(
                team_moneyline > 0 & team_winner == "win" ~ team_moneyline/100,
                team_moneyline > 0 & team_winner == "loss" ~ -1,
                team_moneyline < 0 & team_winner == "win" ~ 1,
                team_moneyline < 0 & team_winner == "loss" ~ team_moneyline/100),
            opp_ml_result = case_when(
                opp_moneyline > 0 & team_winner == "loss" ~ opp_moneyline/100,
                opp_moneyline > 0 & team_winner == "win" ~ -1,
                opp_moneyline < 0 & team_winner == "loss" ~ 1,
                opp_moneyline < 0 & team_winner == "win" ~ opp_moneyline/100),
            team_ats_result = if_else((plus_minus + team_spread) == 0, 0,
                                      if_else((plus_minus + team_spread) > 0,
                                              1, -1.1)),
            opp_ats_result = if_else((plus_minus + opp_spread) == 0, 0,
                                     if_else((-plus_minus + opp_spread) > 0,
                                             1, -1.1)),
            over_game_result = if_else((team_score + opp_score) == 0, 0,
                                       if_else((team_score + opp_score) > over_under,
                                               1, -1.1)),
            under_game_result = if_else((team_score + opp_score) == 0, 0,
                                        if_else((team_score + opp_score) < over_under,
                                                1, -1.1))
        )
    
    win_columns <- names(results_book %>% select(ends_with("win_team")))
    team_columns <- names(results_book %>% select(ends_with("_team_score")))
    opp_columns <- names(results_book %>% select(ends_with("_opp_score")))
    
    
    for (i in seq_along(win_columns)) {
        
        ## over = away -- under = home
        model_edges <- results_book %>%
            mutate(
                win_edge = .data[[win_columns[[i]]]] - team_implied_prob,
                spread_edge = (.data[[team_columns[[i]]]] - .data[[opp_columns[[i]]]]) + team_spread,
                over_under_edge = if_else(
                    location == "away",
                    (.data[[team_columns[[i]]]] + .data[[opp_columns[[i]]]]) - over_under,
                    over_under - (.data[[team_columns[[i]]]] + .data[[opp_columns[[i]]]])
                ),
                win_result = if_else(win_edge > 0, team_ml_result, 0),
                spread_result = if_else(spread_edge > 0, team_ats_result, 0),
                over_under_result = if_else(over_under_edge > 0 & location == "away", 
                                            over_game_result,
                                            if_else(over_under_edge > 0 & location == "home",
                                                    under_game_result, 0)
                ),
                ml_wager = if_else(team_moneyline < 100, team_moneyline/-100, 1)
            ) %>%
            select(game_id,team_id,win_edge:ml_wager)

        colnames(model_edges) <- c(
            "game_id","team_id",
            paste0(sub("_win_team$", "", win_columns[i]),"_win_edge"),
            paste0(sub("_team_score$", "", team_columns[i]),"_spread_edge"),
            paste0(sub("_team_score$", "", team_columns[i]),"_over_under_edge"),
            paste0(sub("_win_team$", "", win_columns[i]),"_win_result"),
            paste0(sub("_team_score$", "", team_columns[i]),"_spread_result"),
            paste0(sub("_team_score$", "", team_columns[i]),"_over_under_result"),
            paste0(sub("_win_team$", "", win_columns[i]),"_ml_wager")
        )
        
        edge_columns <- names(model_edges %>% select(ends_with("edge")))
        result_columns <- names(model_edges %>% select(ends_with("result")))
        wager <- names(model_edges %>% select(ends_with("wager")))
        
        # Loop through columns
        for (h in seq_along(edge_columns)) {
            model_stats <- model_edges %>%
                arrange(desc(select(., matches(edge_columns[h])))) %>%
                filter(select(., matches(edge_columns[h])) > 0) %>%
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
                select(game_id, team_id, cume, games, roi, win_pct)
            
            colnames(model_stats) <- c(
                "game_id","team_id",
                paste0(sub("_edge$", "", edge_columns[h]),"_cume"),
                paste0(sub("_edge$", "", edge_columns[h]),"_games"),
                paste0(sub("_edge$", "", edge_columns[h]),"_roi"),
                paste0(sub("_edge$", "", edge_columns[h]),"_win_pct")
            )
            
            model_edges <- model_edges %>%
                left_join(model_stats, by = c("game_id", "team_id"))
            
        }
        
        results_book <- results_book %>%
            left_join(model_edges)
    }
    
    assign(x = "results_book", results_book, envir = .GlobalEnv)
}

process_results_book(model_outputs)


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
    for (j in seq_along(edge_columns)) {
        max_value <- results_book %>%
            arrange(desc(!!sym(cume_columns[j]))) %>%
            slice(1) %>%
            mutate(model = edge_columns[j]) %>%
            select(model, edge = edge_columns[j], value = cume_columns[j],
                   games = games_columns[j], roi = roi_columns[j]) %>%
            mutate(across(c(edge, value, games, roi), as.numeric),
                   model = sub("_edge$", "", model))
        
        # Store values in the result data frame
        result_df <- bind_rows(result_df, max_value)
    }
    
    assign(x = "results_summary", result_df, envir = .GlobalEnv)
}

summarize_results_book(results_book)

gt::gt(results_summary) %>% gtExtras::gt_theme_538()
DT::datatable(results_summary)


#### function to produce model viz for all models ----
model_viz_all <- function() {
    
    edge_columns_all <- paste0(results_summary$model, "_edge")
    result_columns_all <- paste0(results_summary$model, "_result")
    models_key_all <- results_summary$edge
    
    for (j in seq_along(edge_columns_all)) {
        plot <- results_book %>%
            select(season_year, game_date,
                   edge_columns_all[j], result_columns_all[j]) %>%
            filter(c_across(contains(edge_columns_all[j])) >= models_key_all[j]) %>%
            # filter(c_across(contains(edge_columns_all[j])) > 0) %>%
            group_by(season_year, game_date) %>%
            summarise(day_total_win = sum(c_across(contains(result_columns_all[j])),
                                          na.rm = T)) %>%
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
                 subtitle = paste0(sub("_edge$", "", edge_columns_all[j])),
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

model_viz_all()


#### function to produce model viz for best models ----
model_viz <- function(models_edge, models_result, models_key) {
    
    for (j in seq_along(models_edge)) {
        plot <- results_book %>%
            select(season_year, game_date,
                   models_edge[j], models_result[j]) %>%
            filter(c_across(contains(models_edge[j])) >= models_key[j]) %>%
            group_by(season_year, game_date) %>%
            summarise(day_total_win = sum(c_across(contains(models_result[j])),
                                          na.rm = T)) %>%
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

models_edge <- c("svm_win_edge", "xgb_spread_edge", "glmb_over_under_edge")
models_result <- c("svm_win_result", "xgb_spread_result", "glmb_over_under_result")
models_key <- c(0.1222, 2.7, 1.3)

model_viz(models_edge, models_result, models_key)





#### function to created results book ---- separate over/under
process_results_book <- function(model_outputs) {
    
    # results book ----
    results_book <- model_outputs %>%
        mutate(
            team_ml_result = case_when(
                team_moneyline > 0 & team_winner == "win" ~ team_moneyline/100,
                team_moneyline > 0 & team_winner == "loss" ~ -1,
                team_moneyline < 0 & team_winner == "win" ~ 1,
                team_moneyline < 0 & team_winner == "loss" ~ team_moneyline/100),
            opp_ml_result = case_when(
                opp_moneyline > 0 & team_winner == "loss" ~ opp_moneyline/100,
                opp_moneyline > 0 & team_winner == "win" ~ -1,
                opp_moneyline < 0 & team_winner == "loss" ~ 1,
                opp_moneyline < 0 & team_winner == "win" ~ opp_moneyline/100),
            team_ats_result = if_else((plus_minus + team_spread) == 0, 0,
                                      if_else((plus_minus + team_spread) > 0,
                                              1, -1.1)),
            opp_ats_result = if_else((plus_minus + opp_spread) == 0, 0,
                                     if_else((-plus_minus + opp_spread) > 0,
                                             1, -1.1)),
            over_game_result = if_else((team_score + opp_score) == 0, 0,
                                       if_else((team_score + opp_score) > over_under,
                                               1, -1.1)),
            under_game_result = if_else((team_score + opp_score) == 0, 0,
                                        if_else((team_score + opp_score) < over_under,
                                                1, -1.1))
        )
    
    win_columns <- names(results_book %>% select(ends_with("win_team")))
    team_columns <- names(results_book %>% select(ends_with("_team_score")))
    opp_columns <- names(results_book %>% select(ends_with("_opp_score")))
    
    
    for (i in seq_along(win_columns)) {
        # over/under each line -- need to filter one out
        model_edges <- results_book %>%
            mutate(
                win_edge = .data[[win_columns[[i]]]] - team_implied_prob,
                spread_edge = (.data[[team_columns[[i]]]] - .data[[opp_columns[[i]]]]) + team_spread,
                over_edge = (.data[[team_columns[[i]]]] + .data[[opp_columns[[i]]]]) - over_under,
                under_edge = over_under - (.data[[team_columns[[i]]]] + .data[[opp_columns[[i]]]]),
                win_result = if_else(win_edge > 0, team_ml_result, 0),
                spread_result = if_else(spread_edge > 0, team_ats_result, 0),
                over_result = if_else(over_edge > 0 & location == "away",
                                      over_game_result, 0),
                under_rresult = if_else(under_edge > 0 & location == "away",
                                        under_game_result, 0),
                ml_wager = if_else(team_moneyline < 100, team_moneyline/-100, 1)
            ) %>%
            select(game_id,team_id,win_edge:ml_wager)

        colnames(model_edges) <- c(
            "game_id","team_id",
            paste0(sub("_win_team$", "", win_columns[i]),"_win_edge"),
            paste0(sub("_team_score$", "", team_columns[i]),"_spread_edge"),
            paste0(sub("_team_score$", "", team_columns[i]),"_over_edge"),
            paste0(sub("_team_score$", "", team_columns[i]),"_under_edge"),
            paste0(sub("_win_team$", "", win_columns[i]),"_win_result"),
            paste0(sub("_team_score$", "", team_columns[i]),"_spread_result"),
            paste0(sub("_team_score$", "", team_columns[i]),"_over_result"),
            paste0(sub("_team_score$", "", team_columns[i]),"_under_result"),
            paste0(sub("_win_team$", "", win_columns[i]),"_ml_wager")
        )
        
        edge_columns <- names(model_edges %>% select(ends_with("edge")))
        result_columns <- names(model_edges %>% select(ends_with("result")))
        wager <- names(model_edges %>% select(ends_with("wager")))
        
        # Loop through columns
        for (h in seq_along(edge_columns)) {
            model_stats <- model_edges %>%
                arrange(desc(select(., matches(edge_columns[h])))) %>%
                filter(select(., matches(edge_columns[h])) > 0) %>%
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
                select(game_id, team_id, cume, games, roi, win_pct)
            
            colnames(model_stats) <- c(
                "game_id","team_id",
                paste0(sub("_edge$", "", edge_columns[h]),"_cume"),
                paste0(sub("_edge$", "", edge_columns[h]),"_games"),
                paste0(sub("_edge$", "", edge_columns[h]),"_roi"),
                paste0(sub("_edge$", "", edge_columns[h]),"_win_pct")
            )
            
            model_edges <- model_edges %>%
                left_join(model_stats, by = c("game_id", "team_id"))
            
        }
        
        results_book <- results_book %>%
            left_join(model_edges)
    }
    
    assign(x = "results_book", results_book, envir = .GlobalEnv)
}

process_results_book(model_outputs)

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
    for (j in seq_along(edge_columns)) {
        max_value <- results_book %>%
            arrange(desc(!!sym(cume_columns[j]))) %>%
            slice(1) %>%
            mutate(model = edge_columns[j]) %>%
            select(model, edge = edge_columns[j], value = cume_columns[j],
                   games = games_columns[j], roi = roi_columns[j]) %>%
            mutate(across(c(edge, value, games, roi), as.numeric),
                   model = sub("_edge$", "", model))
        
        # Store values in the result data frame
        result_df <- bind_rows(result_df, max_value)
    }
    
    assign(x = "results_summary", result_df, envir = .GlobalEnv)
}

summarize_results_book(results_book)


#### function to produce model viz for all models ----
model_viz_all <- function() {
    
    edge_columns_all <- paste0(results_summary$model, "_edge")
    result_columns_all <- paste0(results_summary$model, "_result")
    models_key_all <- results_summary$edge
    
    for (j in seq_along(edge_columns_all)) {
        plot <- results_book %>%
            select(season_year, game_date,
                   edge_columns_all[j], result_columns_all[j]) %>%
            filter(c_across(contains(edge_columns_all[j])) >= models_key_all[j]) %>%
            # filter(c_across(contains(edge_columns_all[j])) > 0) %>%
            group_by(season_year, game_date) %>%
            summarise(day_total_win = sum(c_across(contains(result_columns_all[j])),
                                          na.rm = T)) %>%
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
                 subtitle = paste0(sub("_edge$", "", edge_columns_all[j])),
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

model_viz_all()


#### function to produce model viz ----
model_viz <- function(models_edge, models_result, models_key) {
    
    for (j in seq_along(models_edge)) {
        plot <- results_book %>%
            select(season_year, game_date,
                   models_edge[j], models_result[j]) %>%
            filter(c_across(contains(models_edge[j])) >= models_key[j]) %>%
            group_by(season_year, game_date) %>%
            summarise(day_total_win = sum(c_across(contains(models_result[j])),
                                          na.rm = T)) %>%
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

models_edge <- c("svm_win_edge", "xgb_spread_edge",
                 "nn_over_edge", "reg_under_edge")
models_result <- c("svm_win_result", "xgb_spread_result",
                   "nn_over_result", "reg_under_result")
models_key <- c(0.1167694, 2.6282730, 5.0565789, 0.4745254)

model_viz(models_edge, models_result, models_key)








# best keys
moneyline_key <- 0.1167694 # svm
spread_key <- 2.6282730 # xgb
over_key <- 5.0565789 # nn
under_key <- 0.4745254 # reg

# single metrics chart
results_book %>%
    filter(xgb_spread_edge >= spread_key) %>% # edge column & key
    group_by(season_year, game_date) %>%
    summarise(day_total_win = sum(xgb_spread_result)) %>% # result column
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



