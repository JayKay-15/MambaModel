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
        team_winner = factor(team_winner, levels = c("win", "loss"))
    )

nba_final_df <- nba_final %>%
    filter(season_year == 2024) %>%
    select(season_year:opp_implied_prob)

nba_final_test <- read_rds("../NBAdb/models/trained_models/nba_final_test.rds")
# cor_cols <- read_rds("../NBAdb/models/trained_models/cor_cols.rds")

test <- nba_final_test %>%
    select(-team_score) %>%
    mutate(across(location:opp_is_b2b_second, factor))

model_outputs <- nba_final_df

# team winner models ----

# team winner logistic regression model ----
log_win <- read_rds("../NBAdb/models/trained_models/log_win_20_23.rds")

# predictions
win_pred <- predict(log_win, test, type = "prob")

confusionMatrix(test$team_winner,
                factor(ifelse(win_pred[,1] > 0.5, "win", "loss"), 
                       levels = c("win","loss")),
                positive = "win")

model_outputs <- model_outputs %>%
    mutate(log_win_team = as.numeric(win_pred[,1]),
           log_win_opp = as.numeric(win_pred[,2]))


# team winner ridge regression model ----

# saveRDS(reg_win, "../NBAdb/models/models_trained/reg_win_2020_2022.rds")
reg_win <- read_rds("../NBAdb/models/models_trained/reg_win_2020_2022.rds")

# predictions
win_pred <- predict(reg_win, test, type = "prob")

model_outputs <- model_outputs %>%
    mutate(reg_win_team = as.numeric(win_pred[,1]),
           reg_win_opp = as.numeric(win_pred[,2]))


# team winner knn model ----

# saveRDS(knn_win, "../NBAdb/models/models_trained/knn_win_2020_2022.rds")
knn_win <- read_rds("../NBAdb/models/models_trained/knn_win_2020_2022.rds")

# predictions
win_pred <- predict(knn_win, test, type = "prob")

model_outputs <- model_outputs %>%
    mutate(knn_win_team = as.numeric(win_pred[,1]),
           knn_win_opp = as.numeric(win_pred[,2]))


# team winner random forest model ----

# saveRDS(rf_win, "../NBAdb/models/models_trained/rf_win_2020_2022.rds")
rf_win <- read_rds("../NBAdb/models/models_trained/rf_win_2020_2022.rds")

# predictions
win_pred <- predict(rf_win, test, type = "prob")

model_outputs <- model_outputs %>%
    mutate(rf_win_team = as.numeric(win_pred[,1]),
           rf_win_opp = as.numeric(win_pred[,2]))


# team winner support vector machines model ----

# saveRDS(svm_win, "../NBAdb/models/models_trained/svm_win_2020_2022.rds")
svm_win <- read_rds("../NBAdb/models/models_trained/svm_win_2020_2022.rds")

# predictions
win_pred <- predict(svm_win, test, type = "prob")

model_outputs <- model_outputs %>%
    mutate(svm_win_team = as.numeric(win_pred[,1]),
           svm_win_opp = as.numeric(win_pred[,2]))

# team winner neural net model ----

# saveRDS(nn_win, "../NBAdb/models/models_trained/nn_win_2020_2022.rds")
nn_win <- read_rds("../NBAdb/models/models_trained/nn_win_2020_2022.rds")

# predictions
win_pred <- predict(nn_win, test, type = "prob")

model_outputs <- model_outputs %>%
    mutate(nn_win_team = as.numeric(win_pred[,1]),
           nn_win_opp = as.numeric(win_pred[,2]))

# team winner extreme gradient boosting model ----

# saveRDS(xgb_win, "../NBAdb/models/models_trained/xgb_win_2020_2022.rds")
xgb_win <- read_rds("../NBAdb/models/models_trained/xgb_win_2020_2022.rds")

# predictions
win_pred <- predict(xgb_win, test, type = "prob")

model_outputs <- model_outputs %>%
    mutate(xgb_win_team = as.numeric(win_pred[,1]),
           xgb_win_opp = as.numeric(win_pred[,2]))


# team score models ----

# team score linear regression model ----

# saveRDS(lin_team, "../NBAdb/models/models_trained/lin_team_2020_2022.rds")
lin_team <- read_rds("../NBAdb/models/models_trained/lin_team_2020_2022.rds")

# predictions
team_pred <- predict(lin_team, test)
model_outputs <- model_outputs %>%
    mutate(lin_team_score = as.numeric(team_pred))


# team score ridge regression model ----

# saveRDS(reg_team, "../NBAdb/models/models_trained/reg_team_2020_2022.rds")
reg_team <- read_rds("../NBAdb/models/models_trained/reg_team_2020_2022.rds")

# predictions
team_pred <- predict(reg_team, test)
model_outputs <- model_outputs %>%
    mutate(reg_team_score = as.numeric(team_pred))


# team score knn model ----

# saveRDS(knn_team, "../NBAdb/models/models_trained/knn_team_2020_2022.rds")
knn_team <- read_rds("../NBAdb/models/models_trained/knn_team_2020_2022.rds")

# predictions
team_pred <- predict(knn_team, test)
model_outputs <- model_outputs %>%
    mutate(knn_team_score = as.numeric(team_pred))


# team score random forest model ----

# saveRDS(rf_team, "../NBAdb/models/models_trained/rf_team_2020_2022.rds")
rf_team <- read_rds("../NBAdb/models/models_trained/rf_team_2020_2022.rds")


# predictions
team_pred <- predict(rf_team, test)
model_outputs <- model_outputs %>%
    mutate(rf_team_score = as.numeric(team_pred))


# team score support vector machines model ----

# saveRDS(svm_team, "../NBAdb/models/models_trained/svm_team_2020_2022.rds")
svm_team <- read_rds("../NBAdb/models/models_trained/svm_team_2020_2022.rds")

# predictions
team_pred <- predict(svm_team, test)
model_outputs <- model_outputs %>%
    mutate(svm_team_score = as.numeric(team_pred))


# team score neural net model ----

# saveRDS(nn_team, "../NBAdb/models/models_trained/nn_team_2020_2022.rds")
nn_team <- read_rds("../NBAdb/models/models_trained/nn_team_2020_2022.rds")


# predictions
team_pred <- predict(nn_team, test)
model_outputs <- model_outputs %>%
    mutate(nn_team_score = as.numeric(team_pred))


# team score extreme gradient boosting model ----

# saveRDS(xgb_team, "../NBAdb/models/models_trained/xgb_team_2020_2022.rds")
xgb_team <- read_rds("../NBAdb/models/models_trained/xgb_team_2020_2022.rds")

# predictions
team_pred <- predict(xgb_team, test)
model_outputs <- model_outputs %>%
    mutate(xgb_team_score = as.numeric(team_pred))


# opp score models ----

# opp score linear regression model ----

# saveRDS(lin_opp, "../NBAdb/models/models_trained/lin_opp_2020_2022.rds")
lin_opp <- read_rds("../NBAdb/models/models_trained/lin_opp_2020_2022.rds")


# predictions
opp_pred <- predict(lin_opp, test)
model_outputs <- model_outputs %>%
    mutate(lin_opp_score = as.numeric(opp_pred))


# opp score ridge regression model ----

# saveRDS(reg_opp, "../NBAdb/models/models_trained/reg_opp_2020_2022.rds")
reg_opp <- read_rds("../NBAdb/models/models_trained/reg_opp_2020_2022.rds")


# predictions
opp_pred <- predict(reg_opp, test)
model_outputs <- model_outputs %>%
    mutate(reg_opp_score = as.numeric(opp_pred))


# opp score knn model ----

# saveRDS(knn_opp, "../NBAdb/models/models_trained/knn_opp_2020_2022.rds")
knn_opp <- read_rds("../NBAdb/models/models_trained/knn_opp_2020_2022.rds")


# predictions
opp_pred <- predict(knn_opp, test)
model_outputs <- model_outputs %>%
    mutate(knn_opp_score = as.numeric(opp_pred))


# opp score random forest model ----

# saveRDS(rf_opp, "../NBAdb/models/models_trained/rf_opp_2020_2022.rds")
rf_opp <- read_rds("../NBAdb/models/models_trained/rf_opp_2020_2022.rds")


# predictions
opp_pred <- predict(rf_opp, test)
model_outputs <- model_outputs %>%
    mutate(rf_opp_score = as.numeric(opp_pred))


# opp score support vector machines model ----

# saveRDS(svm_opp, "../NBAdb/models/models_trained/svm_opp_2020_2022.rds")
svm_opp <- read_rds("../NBAdb/models/models_trained/svm_opp_2020_2022.rds")


# predictions
opp_pred <- predict(svm_opp, test)
model_outputs <- model_outputs %>%
    mutate(svm_opp_score = as.numeric(opp_pred))


# opp score neural net model ----

# saveRDS(nn_opp, "../NBAdb/models/models_trained/nn_opp_2020_2022.rds")
nn_opp <- read_rds("../NBAdb/models/models_trained/nn_opp_2020_2022.rds")


# predictions
opp_pred <- predict(nn_opp, test)
model_outputs <- model_outputs %>%
    mutate(nn_opp_score = as.numeric(opp_pred))


# opp score extreme gradient boosting model ----

# saveRDS(xgb_opp, "../NBAdb/models/models_trained/xgb_opp_2020_2022.rds")
xgb_opp <- read_rds("../NBAdb/models/models_trained/xgb_opp_2020_2022.rds")

# predictions
opp_pred <- predict(xgb_opp, test)
model_outputs <- model_outputs %>%
    mutate(xgb_opp_score = as.numeric(opp_pred))





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


# best keys
moneyline_key <- 0.05781939 # regularization (ridge)
spread_key <- 4.30365229 # ensemble (lin, reg, svm, nn, xgb)
over_key <- 2.5421957 # ensemble (lin, reg, svm, nn, xgb)
under_key <- 1.9372166 # ensemble (lin, reg, svm, nn, xgb)


# single metrics chart
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



