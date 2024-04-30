### Model Performance ----
library(tidyverse)
library(data.table)
library(gt)



#### visualize model performance


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
    team_columns <- names(results_book %>% select(ends_with("_score_team")))
    opp_columns <- names(results_book %>% select(ends_with("_score_opp")))
    
    
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
            paste0(sub("_score_team$", "", team_columns[i]),"_spread_edge"),
            paste0(sub("_score_team$", "", team_columns[i]),"_over_under_edge"),
            paste0(sub("_win_team$", "", win_columns[i]),"_win_result"),
            paste0(sub("_score_team$", "", team_columns[i]),"_spread_result"),
            paste0(sub("_score_team$", "", team_columns[i]),"_over_under_result"),
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

models_edge <- c("mars_win_edge", "mars_spread_edge", "glmb_over_under_edge")
models_result <- c("mars_win_result", "mars_spread_result", "glmb_over_under_result")
models_key <- c(0.05750776, 1.24326434, 1.29048131)

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
    team_columns <- names(results_book %>% select(ends_with("_score_team")))
    opp_columns <- names(results_book %>% select(ends_with("_score_opp")))
    
    
    for (i in seq_along(win_columns)) {
        # over/under each line
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






