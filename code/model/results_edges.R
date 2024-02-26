### Results Book ----

library(tidyverse)
library(data.table)
library(nbastatR)

options(scipen = 999999)

#### need function to get current season game info and model outputs


#### Results Book ####
model_outputs <- read_rds("./backest_output/model_outputs_w15.rds")

## ensemble and result columns
results_book <- model_outputs %>%
    mutate(
        ens_win_team = rowMeans(select(.,log_win_away,reg_win_away,
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


## calculate edges
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








