library(tidyverse)
library(janitor)
library(httr)
library(data.table)

options(scipen = 999999)





df <- readxl::read_xlsx("/Users/kartesj/OneDrive - Trinity Industries Inc/Personal/model_outputs.xlsx")
model_outputs <- df[1:53]



process_results_book <- function(model_outputs) {
    
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
    
    # return(result_df)
    assign(x = "results_summary", result_df, envir = .GlobalEnv)
    
}

# usage of the function
summarize_results_book(results_book)


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



max_date <- as_date(max(model_outputs$game_date))

model_outputs <- model_outputs %>%
    filter(game_date >= as_date(max_date - 14))




process_results_book <- function(model_outputs) {
    
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
                over_under_edge = abs(.data[[team_columns[[i]]]] + .data[[opp_columns[[i]]]] - over_under),
                # under_edge = over_under - (.data[[team_columns[[i]]]] + .data[[opp_columns[[i]]]]),
                win_result = if_else(.data[[win_columns[[i]]]] - away_implied_prob > 0,
                                     away_ml_result, home_ml_result),
                spread_result = if_else((.data[[team_columns[[i]]]] - .data[[opp_columns[[i]]]]) + away_spread > 0,
                                        away_ats_result, home_ats_result),
                over_under_result = if_else(.data[[team_columns[[i]]]] + .data[[opp_columns[[i]]]] - over_under > 0,
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
            paste0(sub("_team_score$", "", team_columns[i]),"_over_under_edge"),
            # paste0(sub("_team_score$", "", team_columns[i]),"_under_edge"),
            paste0(sub("_win_away$", "", win_columns[i]),"_win_result"),
            paste0(sub("_team_score$", "", team_columns[i]),"_spread_result"),
            paste0(sub("_team_score$", "", team_columns[i]),"_over_under_result"),
            paste0(sub("_win_away$", "", win_columns[i]),"_ml_wager")
        )
        
        edge_columns <- names(model_edges %>% select(ends_with("edge")))
        result_columns <- names(model_edges %>% select(ends_with("result")))
        # result_columns <- c(result_columns, tail(result_columns, 1))
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
    
    assign(x = "results_book", results_book, envir = .GlobalEnv)
    
}


process_results_book(model_outputs)

























### model tuning ----  

library(tidyverse)
library(caret) # model training
library(tidymodels) # model eval
library(finetune)   # package for more advanced hyperparameter tuning 
library(ggfortify) # autoplot
library(doParallel) # parallel

library(embed)      # use the create embeddings for categorical features
library(stacks)     # used for stacking models 
library(rules)      # contains the cubist modelling algorithm
library(vip)        # variable importance plots
library(brulee)     # torch model
library(bonsai)
library(tictoc)     # measure how long processes take

# Check how many cores you have to work with
detectCores()

# Set the number of clusters tidymodels has to work with
cl <- makePSOCKcluster(6)  # Create 8 clusters
registerDoParallel(cl)
getDoParWorkers()

# Turn off the parallel processing once you're finished with it
stopCluster(cl)
registerDoSEQ()


options(scipen = 999999)
set.seed(214)

# https://www.stepbystepdatascience.com/ml-with-tidymodels


nba_final_cor <- read_rds("/Users/kartesj/OneDrive - Trinity Industries Inc/Personal/nba_final_w10.rds")
# model_outputs <- read_rds("/Users/kartesj/OneDrive - Trinity Industries Inc/Personal/model_outputs.rds")

nba_final <- nba_final_cor %>%
    filter(season_year >= 2020) %>%
    mutate(
        game_date = as_date(game_date, origin ="1970-01-01"),
        team_winner = factor(team_winner, levels = c("win", "loss")),
        location = if_else(location == "away", 1, 0),
        across(c(location, is_b2b_first:opp_is_b2b_second), factor)
        # location = if_else(location == "away", 1, 0)
    )


# # correlations ----
# feature correlations
cor_df <- nba_final_cor %>%
    select(is_b2b_first:opp_is_b2b_second, over_under, team_implied_prob,
           team_fgm:opp_opp_pct_uast_fgm) %>%
    select(-contains("_rating"))

# check for extreme correlation
cor_mx <- cor(cor_df)

# find highly correlated features
cor_cols <- findCorrelation(cor_mx, cutoff = .5, exact = F, names = T)
cor_cols


# clear environment ----
rm(list=ls()[! ls() %in% c("nba_final", "cor_cols", "cl")])

## team winner models ----
# all features
train <- nba_final %>%
    filter(season_year <= 2022) %>%
    select(team_winner, location, is_b2b_first:opp_is_b2b_second, over_under,
           team_implied_prob, team_fgm:opp_opp_pct_uast_fgm) %>%
    select(-contains("_rating"))

test <- nba_final %>%
    filter(season_year > 2022) %>%
    select(team_winner, location, is_b2b_first:opp_is_b2b_second, over_under,
           team_implied_prob, team_fgm:opp_opp_pct_uast_fgm) %>%
    select(-contains("_rating"))

# highly correlated features removed
train <- train %>%
    select(-all_of(cor_cols)) %>%
    mutate(across(location:opp_is_b2b_second, factor))

test <- test %>%
    select(-all_of(cor_cols)) %>%
    mutate(across(location:opp_is_b2b_second, factor))



modelLookup()

# team winner extreme gradient boosting model ----
# model
# ctrl <- trainControl(method = "cv", number = 5, verboseIter = F, 
#                      classProbs = T, summaryFunction = twoClassSummary)
ctrl <- trainControl(method = "cv", number = 3, verboseIter = T, 
                     classProbs = T)
# grid <- expand.grid(
#     nrounds = seq(100, 10000, 100),
#     eta = c(0.01, 0.015, 0.025, 0.05, 0.1),
#     max_depth = c(2, 3, 4, 5, 6),
#     gamma = c(0, 0.05, 0.1, 0.5, 0.7, 0.9, 1.0),
#     colsample_bytree = c(0.4, 0.6, 0.8, 1.0),
#     min_child_weight = c(1, 2, 3),
#     subsample = c(0.5, 0.75, 1.0)
# )
xgb_win <- train(team_winner ~., data = train,
                 method = "xgbTree",
                 # metric = "ROC",
                 preProc = c("center", "scale"),
                 trControl = ctrl,
                 tuneGrid = grid)
xgb_tune <- xgb_win

unregister_dopar <- function() {
    env <- foreach:::.foreachGlobals
    rm(list=ls(name=env), pos=env)
}

unregister_dopar()

grid <- expand.grid(
    nrounds = xgb_tune$bestTune$nrounds,
    eta = xgb_tune$bestTune$eta,
    max_depth = xgb_tune$bestTune$max_depth,
    gamma = xgb_tune$bestTune$gamma,
    colsample_bytree = xgb_tune$bestTune$colsample_bytree,
    min_child_weight = xgb_tune$bestTune$min_child_weight,
    subsample = xgb_tune$bestTune$subsample
)

grid <- expand.grid(
    nrounds = 1000,
    eta = c(0.01, 0.015, 0.025, 0.05, 0.1),
    lambda = 1,
    alpha = 0
)
xgb_win <- train(team_winner ~., data = train,
                 method = "xgbLinear",
                 # metric = "ROC",
                 preProc = c("center", "scale"),
                 trControl = ctrl,
                 tuneGrid = grid)
xgb_tune <- xgb_win


# ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 5,
#                      verboseIter = T, classProbs = T, search = "random")

ctrl <- trainControl(method = "adaptive_cv", number = 5, repeats = 1,
                     verboseIter = T, classProbs = T,
                     adaptive = list(min = 3,     # minimum number of resamples before elimination is possible
                                     alpha = 0.05,     # confidence level used to eliminate hyperparameter combos
                                     method = "gls",  
                                     complete = TRUE))




ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 1,
                     verboseIter = T, classProbs = T)

# https://www.analyticsvidhya.com/blog/2016/03/complete-guide-parameter-tuning-xgboost-with-codes-python/#:~:text=What%20parameters%20should%20you%20use,minimum%20child%20weight%20(min_child_weight).
# https://ml-course.kazsakamoto.com/Labs/hyperparameterTuning.html
# https://www.kaggle.com/code/pelkoja/visual-xgboost-tuning-with-caret
# https://www.hackerearth.com/practice/machine-learning/machine-learning-algorithms/beginners-tutorial-on-xgboost-parameter-tuning-r/tutorial/

# grid <- expand.grid(
#     nrounds = 100,
#     eta = c(0.1),
#     max_depth = c(2),
#     gamma = 0,
#     colsample_bytree = c(0.9),
#     min_child_weight = c(3),
#     subsample = c(0.9)
# )

grid <- expand.grid(
    nrounds = c(200),
    eta = c(0.025),
    max_depth = c(2),
    gamma = c(5),
    colsample_bytree = c(1),
    min_child_weight = c(2),
    subsample = c(1)
)

# random
# ctrl <- trainControl(method = "adaptive_cv", number = 5, repeats = 1,
#                      verboseIter = T, classProbs = T, search = "random",
#                      adaptive = list(min = 3,     # minimum number of resamples before elimination is possible
#                                      alpha = 0.05,     # confidence level used to eliminate hyperparameter combos
#                                      method = "gls",  
#                                      complete = TRUE))
tic()
set.seed(214)
# xgb_win <- train(team_winner ~., data = train,
#                  method = "xgbTree",
#                  # metric = "ROC",
#                  # preProc = c("center", "scale", "YeoJohnson"),
#                  trControl = ctrl,
#                  tuneLength = 25)

# xgb_win <- train(team_winner ~., data = train,
#                  method = "xgbTree",
#                  # metric = "ROC",
#                  # preProc = c("center", "scale", "YeoJohnson"),
#                  trControl = ctrl,
#                  tuneGrid = grid)

# grid <- expand.grid(
#     trials  = seq(1, 7, 1),
#     winnow  = c(TRUE, FALSE),
#     model = c("rules", "trees")
# )

# grid <- expand.grid(
#     trials  = c(5, 10, 15),
#     winnow  = c(TRUE, FALSE),
#     model = c("rules", "trees"),
#     cost = c(1, 2, 3)
# )


# xgb_win <- train(team_winner ~., data = train,
#                  method = "C5.0",
#                  # metric = "ROC",
#                  # preProc = c("center", "scale", "YeoJohnson"),
#                  trControl = ctrl,
#                  tuneGrid = grid)


# xgb_win <- train(team_winner ~., data = train,
#                  method = "C5.0Cost",
#                  # metric = "ROC",
#                  # preProc = c("center", "scale", "YeoJohnson"),
#                  trControl = ctrl,
#                  tuneGrid = grid)

xgb_win <- train(team_winner ~., data = train,
                 method = "earth",
                 # metric = "ROC",
                 preProc = c("center", "scale", "YeoJohnson"),
                 trControl = ctrl,
                 tuneLength = 15)


toc()
getTrainPerf(xgb_win)
xgb_win
xgb_win$resample
xgb_win$results
summary(xgb_win) # model components
confusionMatrix(xgb_win) # confusion matrix
plot(xgb_win) # viz

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
ctrl <- trainControl(method = "cv", number = 5, verboseIter = T, 
                     classProbs = T)
grid <- expand.grid(
    mstop = seq(50, 500, 50),
    prune = c("no")
)
set.seed(214)
glm_win <- train(team_winner ~., data = train,
                 method = "glmboost",
                 # metric = "ROC",
                 preProc = c("center", "scale"),
                 trControl = ctrl,
                 tuneGrid = grid)

glm_win
glm_win$resample
glm_win$results
summary(glm_win) # model components
confusionMatrix(glm_win) # confusion matrix
plot(glm_win)

# predictions
win_pred <- predict(glm_win, test, type = "prob")
confusionMatrix(test$team_winner,
                factor(ifelse(win_pred[,1] > 0.5, "win", "loss"), 
                       levels = c("win","loss")),
                positive = "win")



# team winner ensemble model ---- 
# https://www.stepbystepdatascience.com/hyperparameter-tuning-and-model-stacking-with-caret
# model
library(caretEnsemble)
# ensem_control <- trainControl(method = "adaptive_cv", number = 5, repeats = 5,
#                               verboseIter = T, classProbs = T,
#                               adaptive = list(min = 5,
#                                               alpha = 0.05,
#                                               method = "gls",
#                                               complete = TRUE),
#                               search = "random",
#                               savePredictions = "final") # this is needed so we can ensemble later

ensem_control <- trainControl(method = "cv", number = 5,
                              verboseIter = T, classProbs = T,
                              index = createFolds(train$team_winner),
                              savePredictions = "final") # this is needed so we can ensemble later

glmnet_grid <- expand.grid(
    alpha = 0, # ridge = 0 / lasso = 1
    lambda = 10^seq(2, -3, by = -.1)
)
nn_grid <- expand.grid(
    decay = c(0.75, 0.5, 0.1, 1e-2, 1e-3, 1e-4, 1e-5),
    size = c(1, 3, 5, 7, 9)
)
svm_grid <- expand.grid(
    sigma = c(0.001, 0.005, 0.01, 0.05),
    C = c(0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.95)
)

set.seed(214)
all_in_one <- caretList(team_winner ~., data = train,
                        trControl = ensem_control,
                        preProc = c("center", "scale"),
                        tuneList=list(glmnet = caretModelSpec(method = "glmnet",
                                                              tuneGrid = glmnet_grid),
                                      nn = caretModelSpec(method = "pcaNNet",
                                                          tuneGrid = nn_grid,
                                                          maxit = 1000),
                                      svm = caretModelSpec(method = "svmRadial",
                                                           tuneGrid = svm_grid)))
# Have a look at the models
rbind(getTrainPerf(all_in_one$"glmnet"),
      getTrainPerf(all_in_one$"nn"),
      getTrainPerf(all_in_one$"svm")) %>% 
    arrange(desc(TrainAccuracy))

# Plot the results
bwplot(resamples(all_in_one))

# Have a look at the correlation amongst the models
modelCor(resamples(all_in_one))

# Add on average correlation amongst models
rbind(as_tibble(modelCor(resamples(all_in_one)), rownames = "models"),
      summarise_all(as_tibble(modelCor(resamples(all_in_one))), mean) %>% 
          mutate(models = "average") %>% 
          select(models, everything()))

# Caret ensemble will ensemble all models in your list
all_models_ensemble <- caretEnsemble(all_in_one)
all_models_ensemble # print the performance of the stack

summary(all_models_ensemble) # print a bit more detail

# We can pass a trControl to our caretEnsemble too
ensembleCtrl <- trainControl(method = "repeatedcv", # what method of resampling we want to use
                             number = 5, # 10 folds
                             repeats = 5) # repeated ten times

cv_ensemble <- caretEnsemble(all_in_one, trControl = ensembleCtrl)

summary(cv_ensemble) 


# Create the trainControl - don't reuse the one from the model training
stackControl <- trainControl(method = "adaptive_cv",
                             number = 10, repeats = 5, classProbs = T,
                             adaptive = list(min = 5,
                                             alpha = 0.05,
                                             method = "gls",
                                             complete = TRUE),
                             search = "random",
                             savePredictions="final") 

# Ensemble the predictions of `models` to form a new combined prediction based on glm
set.seed(214)
stack_glmnet <- caretStack(all_in_one,  # caretList
                           method="glmnet",   # algorithm to try for our super learner
                           # metric="MAE",
                           tuneLength = 50,
                           trControl = stackControl)

stack_glmnet

# Which model performed the best?
as_tibble(rbind(glmnet = getTrainPerf(all_in_one$"glmnet")$TrainAccuracy,
                nn = getTrainPerf(all_in_one$"nn")$TrainAccuracy,
                svm = getTrainPerf(all_in_one$"svm")$TrainAccuracy,            	
                ensemble = all_models_ensemble$error$Accuracy,
                glmnet_stack = mean(stack_glmnet$ens_model$resample$Accuracy)),
          rownames="models") %>% 
    rename(Accuracy = V1) %>% 
    arrange(desc(Accuracy))


# We'll use the best performing model to score up our Test set
final_model <- stack_glmnet


win_pred <- predict(all_in_one$svm, test, type = "prob")

team_pred <- as.numeric(win_pred[,1])
opp_pred <- as.numeric(win_pred[,2])
obs <- test$team_winner
pred <- factor(ifelse(team_pred > 0.5, "win", "loss"), levels = c("win","loss"))

# Compare performance on the Test set vs the resamples
tibble(train = mean(all_in_one$svm$resample$Accuracy), 
       test = postResample(pred = pred, obs = obs)["Accuracy"]) %>%
    mutate(difference = train-test)











##### tidymodels ####
### model tuning ----  

library(RSQLite) # db
library(DBI) # db

library(tidyverse)
library(caret) # model training
library(tidymodels) # model eval
library(finetune)   # package for more advanced hyperparameter tuning 
library(ggfortify) # autoplot
library(doParallel) # parallel

library(embed)      # use the create embeddings for categorical features
library(stacks)     # used for stacking models 
library(rules)      # contains the cubist modelling algorithm
library(vip)        # variable importance plots
library(brulee)     # torch model
library(bonsai)
library(tictoc)     # measure how long processes take

# Check how many cores you have to work with
detectCores()

# Set the number of clusters tidymodels has to work with
cl <- makePSOCKcluster(6)  # Create 8 clusters
registerDoParallel(cl)
getDoParWorkers()

# Turn off the parallel processing once you're finished with it
stopCluster(cl)
registerDoSEQ()

options(scipen = 999999)
set.seed(214)

# https://www.stepbystepdatascience.com/ml-with-tidymodels
# https://www.stepbystepdatascience.com/tuning-hyperparameters-tidymodels


# pull all historical data
nba_final_cor <- tbl(dbConnect(SQLite(), "../nba_sql_db/nba_db"), "mamba_long_odds") %>%
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


nba_final_cor <- read_rds("/Users/kartesj/OneDrive - Trinity Industries Inc/Personal/nba_final_w10.rds")
# model_outputs <- read_rds("/Users/kartesj/OneDrive - Trinity Industries Inc/Personal/model_outputs.rds")

nba_final <- nba_final_cor %>%
    filter(season_year >= 2020) %>%
    mutate(
        game_date = as_date(game_date, origin ="1970-01-01"),
        team_winner = factor(team_winner, levels = c("win", "loss")),
        across(c(location, is_b2b_first:opp_is_b2b_second), factor)
        # location = if_else(location == "away", 1, 0)
    )


# # correlations ----
# feature correlations
cor_df <- nba_final_cor %>%
    select(is_b2b_first:opp_is_b2b_second, over_under, team_implied_prob,
           team_fgm:opp_opp_pct_uast_fgm) %>%
    select(-contains("_rating"))

# check for extreme correlation
cor_mx <- cor(cor_df)

# find highly correlated features
cor_cols <- findCorrelation(cor_mx, cutoff = .5, exact = F, names = T)
cor_cols


# clear environment ----
rm(list=ls()[! ls() %in% c("nba_final", "cor_cols", "cl")])


# team score models ----
# all features
train <- nba_final %>%
    filter(season_year <= 2022) %>%
    select(team_score, location, is_b2b_first:opp_is_b2b_second, over_under,
           team_implied_prob, team_fgm:opp_opp_pct_uast_fgm) %>%
    select(-contains("_rating"))

test <- nba_final %>%
    filter(season_year > 2022) %>%
    select(team_score, location, is_b2b_first:opp_is_b2b_second, over_under,
           team_implied_prob, team_fgm:opp_opp_pct_uast_fgm) %>%
    select(-contains("_rating"))

# highly correlated features removed
train <- train %>%
    select(-all_of(cor_cols)) %>%
    mutate(across(location:opp_is_b2b_second, factor))

test <- test %>%
    select(-all_of(cor_cols)) %>%
    mutate(across(location:opp_is_b2b_second, factor))


# Create train-test set
train_test_split <- make_splits(x = train, assessment = test)
# train_test_split <- initial_split(nba_final,
#                                   prop = 0.8,      # take a 10% sample for Train
#                                   strata = team_winner) # tell tidymodels to keep the distribution of team_winner the same in each sample
train_test_split

test  <- testing(train_test_split)  # create the test set
train <- training(train_test_split) # create the train set

# Examine data
skimr::skim(train)

# team_score histogram
ggplot(train, aes(x = team_score)) +
    geom_histogram()  +
    ggtitle(paste0("Histogram of team_score"))

#### tidymodels tuning ----
## Team Score Linear Model ----
# k-fold cross-validation - vfold_cv()
train_cv <- vfold_cv(train, v = 5, repeats = 1) # create resampling scheme on train

# Create recipe
ts_recipe <- recipe(team_score ~ ., data = train) %>%
    step_dummy(location, is_b2b_first:opp_is_b2b_second) %>%
    step_corr(all_numeric_predictors()) %>%
    step_normalize(all_numeric_predictors()) %>%
    step_nzv(all_predictors())

# Model specification
lm_spec <- linear_reg() %>%
    set_engine("lm") %>%
    set_mode("regression")

lm_workflow <- workflow() %>%
    add_recipe(ts_recipe) %>% # add in our newly created recipe
    add_model(lm_spec) # add the model spec we made earlier

# lm_model <- fit_resamples(lm_workflow,
#                           resamples = train_cv,
#                           control = control_resamples(save_pred = T,
#                                                       save_workflow = T,
#                                                       verbose = T),
#                           metrics = metric_set(mae))
# 
# # Helper functions
# show_best(lm_model, metric = "mae")
# collect_metrics(lm_model)
# collect_predictions(lm_model)

# Fit model
lm_res <- lm_workflow %>%
    last_fit(train_test_split,  metrics = metric_set(mae))

# See how it performs on the Test set
collect_metrics(lm_res)
collect_predictions(lm_res)
lm_res %>% extract_fit_parsnip() %>% tidy()



# team score regularization model ----
# k-fold cross-validation
train_cv <- vfold_cv(train, v = 5, repeats = 3)

# Model specification
glmnet_spec <- linear_reg(
    penalty = tune(),
    mixture = tune()
) %>%
    set_engine("glmnet") %>%
    set_mode("regression")

# Create recipe
ts_recipe <- recipe(team_score ~ ., data = train) %>%
    step_dummy(location, is_b2b_first:opp_is_b2b_second) %>%
    step_corr(all_numeric_predictors()) %>%
    step_YeoJohnson(all_numeric_predictors()) %>%
    step_nzv(all_predictors())

# Create workflow
glmnet_workflow <- workflow() %>%
    add_recipe(ts_recipe) %>%
    add_model(glmnet_spec)

# Grid options
manual_grid <- crossing(penalty = 10^seq(2, -3, by = -.1),
                        mixture = c(0, 1))

latin_grid <- glmnet_spec %>%
    extract_parameter_set_dials() %>%
    grid_latin_hypercube(size = 50)

entropy_grid <- glmnet_spec %>%
    extract_parameter_set_dials() %>%
    grid_max_entropy(size = 50)

# Test different grids
diff_params <- function(name, x){
    tune_grid(glmnet_workflow,
              resamples = train_cv,
              grid = x,
              metrics = metric_set(mae),
              control = control_grid(verbose = T)) %>% 
        show_best(metric = 'mae', n = 1) %>% 
        mutate(param_set = name)
}

latin <- diff_params("latin", latin_grid)
entropy <- diff_params("entropy", entropy_grid)
manual <- diff_params("manual", manual_grid)

tibble(bind_rows(latin, entropy, manual))[-8]

# Control
ctrl <- control_grid(verbose = T)

# Initial tune
glmnet_initial <- tune_grid(glmnet_workflow,
                            resamples = train_cv,
                            grid = latin_grid,
                            metrics = metric_set(mae),
                            control = ctrl)
glmnet_initial %>% show_best(metric = 'mae', n = 5)

# Anneal tune
glmnet_anneal <- tune_sim_anneal(glmnet_workflow,
                                 resamples = train_cv,
                                 initial = glmnet_initial,
                                 iter = 15,
                                 metrics = metric_set(mae),
                                 control = control_sim_anneal(restart = 5L))  
glmnet_anneal %>% show_best(metric = 'mae', n = 1)

# Select best model
glmnet_best_mod <- glmnet_anneal %>% select_best(metric = "mae")

# Finalize model
glmnet_final_res <- glmnet_workflow %>%
    finalize_workflow(best_mod) %>%
    last_fit(train_test_split, metrics = metric_set(mae))

glmnet_final_mod <- extract_workflow(final_res)

# Test metrics
glmnet_final_res %>% collect_metrics()
glmnet_final_res %>% collect_predictions()

# Variable importance
vip::vi(extract_fit_parsnip(glmnet_final_res))  # extract the variable importance scores
vip::vip(extract_fit_parsnip(glmnet_final_res)) + # plot them
    theme(text = element_text(size = 15)) 


# team score random forest model ----
# k-fold cross-validation
train_cv <- vfold_cv(train, v = 5, repeats = 1)

# Model specification
rf_spec <- rand_forest(
    mtry = tune(),
    trees = 1000,
    min_n = 5
) %>%  
    set_engine("ranger") %>% 
    set_mode("regression")

# Create recipe
ts_recipe <- recipe(team_score ~ ., data = train) %>%
    step_dummy(location, is_b2b_first:opp_is_b2b_second) %>%
    step_corr(all_numeric_predictors()) %>%
    step_YeoJohnson(all_numeric_predictors()) %>%
    step_nzv(all_predictors())

# Create workflow
rf_workflow <- workflow() %>%
    add_recipe(ts_recipe) %>%
    add_model(rf_spec)

# Grid options
latin_grid <- rf_spec %>%
    extract_parameter_set_dials() %>%
    update(mtry=finalize(mtry(), prep(ts_recipe) %>% bake(train))) %>%
    grid_latin_hypercube(size = 10)

entropy_grid <- rf_spec %>%
    extract_parameter_set_dials() %>%
    update(mtry=finalize(mtry(), prep(ts_recipe) %>% bake(train))) %>%
    grid_max_entropy(size = 10)

# Test different grids
diff_params <- function(name, x){
    tune_grid(rf_workflow,
              resamples = train_cv,
              grid = x,
              metrics = metric_set(mae),
              control = control_grid(verbose = T)) %>% 
        show_best(metric = 'mae', n = 1) %>% 
        mutate(param_set = name)
}

latin <- diff_params("latin", latin_grid)
entropy <- diff_params("entropy", entropy_grid)

tibble(bind_rows(latin, entropy))[-8]

# Control
ctrl <- control_grid(verbose = TRUE)

# Racing tune
racing_results <- tune_race_anova(rf_workflow,
                                  resamples = train_cv,
                                  grid = latin_grid,
                                  metrics = metric_set(mae),
                                  control = control_race(verbose = TRUE))
racing_results %>% show_best(metric = 'mae', n = 5) 

# Initial tune
rf_initial <- tune_grid(rf_workflow,
                        resamples = train_cv,
                        grid = latin_grid,
                        metrics = metric_set(mae),
                        control = control_grid(verbose = TRUE))
rf_initial %>% show_best(metric = 'mae', n = 5)

# Anneal tune
latin_grid <- rf_workflow %>%
    extract_parameter_set_dials() %>%
    update(mtry=finalize(mtry(), prep(ts_recipe) %>% bake(train)))

rf_anneal <- tune_sim_anneal(rf_workflow,
                             resamples = train_cv,
                             initial = rf_initial,
                             iter = 15,
                             metrics = metric_set(mae),
                             param_info = 
                                 control = control_sim_anneal(restart = 5L))  
rf_anneal %>% show_best(metric = 'mae', n = 1)

# Select best model
rf_best_mod <- rf_anneal %>% select_best(metric = "mae")

# Finalize model
rf_final_res <- rf_workflow %>%
    finalize_workflow(rf_best_mod) %>%
    last_fit(train_test_split, metrics = metric_set(mae))

rf_final_mod <- extract_workflow(rf_final_res)

# Test metrics
rf_final_res %>% collect_metrics()
rf_final_res %>% collect_predictions()

# Variable importance
vip::vi(extract_fit_parsnip(rf_final_res))  # extract the variable importance scores
vip::vip(extract_fit_parsnip(rf_final_res)) + # plot them
    theme(text = element_text(size = 15)) 



# team score svm model ----
# k-fold cross-validation
train_cv <- vfold_cv(train, v = 5, repeats = 1)

# Model specification
svm_spec <- svm_linear(
    cost = tune(),
    margin = tune()
) %>%  
    set_engine("kernlab") %>% 
    set_mode("regression")

# Create recipe
ts_recipe <- recipe(team_score ~ ., data = train) %>%
    step_dummy(location, is_b2b_first:opp_is_b2b_second) %>%
    step_corr(all_numeric_predictors()) %>%
    step_YeoJohnson(all_numeric_predictors()) %>%
    step_nzv(all_predictors())

# Create workflow
svm_workflow <- workflow() %>%
    add_recipe(ts_recipe) %>%
    add_model(svm_spec)

# Grid options
latin_grid <- svm_spec %>%
    extract_parameter_set_dials() %>%
    grid_latin_hypercube(size = 25)

entropy_grid <- svm_spec %>%
    extract_parameter_set_dials() %>%
    grid_max_entropy(size = 25)

# Test different grids
diff_params <- function(name, x){
    tune_grid(svm_workflow,
              resamples = train_cv,
              grid = x,
              metrics = metric_set(mae),
              control = control_grid(verbose = T)) %>% 
        show_best(metric = 'mae', n = 1) %>% 
        mutate(param_set = name)
}

latin <- diff_params("latin", latin_grid)
entropy <- diff_params("entropy", entropy_grid)

tibble(bind_rows(latin, entropy))[-8]

# Control
ctrl <- control_grid(verbose = TRUE)

# Racing tune
racing_results <- tune_race_anova(svm_workflow,
                                  resamples = train_cv,
                                  grid = latin_grid,
                                  metrics = metric_set(mae),
                                  control = ctrl)
racing_results %>% show_best(metric = 'mae', n = 5) 

# Initial tune
svm_initial <- tune_grid(svm_workflow,
                         resamples = train_cv,
                         grid = latin_grid,
                         metrics = metric_set(mae),
                         control = ctrl)
svm_initial %>% show_best(metric = 'mae', n = 5)

# Anneal tune
svm_anneal <- tune_sim_anneal(svm_workflow,
                              resamples = train_cv,
                              initial = svm_initial,
                              iter = 15,
                              metrics = metric_set(mae),
                              control = control_sim_anneal(restart = 5L))  
svm_anneal %>% show_best(metric = 'mae', n = 1)

# Select best model
svm_best_mod <- svm_anneal %>% select_best(metric = "mae")

# Finalize model
svm_final_res <- svm_workflow %>%
    finalize_workflow(svm_best_mod) %>%
    last_fit(train_test_split, metrics = metric_set(mae))

svm_final_mod <- extract_workflow(svm_final_res)

# Test metrics
svm_final_res %>% collect_metrics()
svm_final_res %>% collect_predictions()

# Variable importance
vip::vi(extract_fit_parsnip(svm_final_res))  # extract the variable importance scores
vip::vip(extract_fit_parsnip(svm_final_res)) + # plot them
    theme(text = element_text(size = 15)) 







##### from code file ######


# Create train-test set
train_test_split <- make_splits(x = train, assessment = test)
# train_test_split <- initial_split(nba_final,
#                                   prop = 0.8,      # take a 10% sample for Train
#                                   strata = team_winner) # tell tidymodels to keep the distribution of team_winner the same in each sample
train_test_split

test  <- testing(train_test_split)  # create the test set
train <- training(train_test_split) # create the train set

skimr::skim(train)

# team_score histogram
ggplot(train, aes(x = team_score)) +
    geom_histogram()  +
    ggtitle(paste0("Histogram of team_score"))

# Check the correlation amongst the columns
cor(train %>% 
        select(team_fgm:opp_opp_pct_uast_fgm) %>%
        select(-contains("_rating")) %>% # only keep numeric columns
        filter(if_all(everything(), ~!is.na(.x)))) # filter out any rows that have missing data

# k-fold cross-validation - vfold_cv()
# train_cv <- vfold_cv(train, v = 5, repeats = 3, strata = price) # create resampling scheme on train
train_cv <- vfold_cv(train, v = 5, repeats = 3) # create resampling scheme on train
train_cv

# Create a recipe for our linear model
ts_recipe <- recipe(team_score ~ ., data = train) %>%
    # step_unorder(location, is_b2b_first:opp_is_b2b_second) %>%
    step_dummy(location, is_b2b_first:opp_is_b2b_second) %>%
    step_corr(all_numeric_predictors()) %>%
    step_normalize(all_numeric_predictors()) %>%
    step_nzv(all_predictors())
ts_recipe

# Create the specification for a glmnet regression model
glmnet_spec <- linear_reg(mode = "regression", 
                          engine = "glmnet", 
                          penalty = 0.0001, 
                          mixture = 0)
glmnet_spec

# Create a workflow that combines the recipe and the linear model specification
lm_workflow <- workflow() %>%
    add_recipe(ts_recipe) %>% # add in our newly created recipe
    add_model(glmnet_spec) # add the model spec we made earlier
lm_workflow

# Fit the model with a workflow
lm_model <- fit_resamples(lm_workflow,         # use our newly made workflow
                          resamples = train_cv,    # use our k-fold cross-validation we made earlier
                          control = control_resamples(save_pred = T,   # save the predictions for each of the resamples
                                                      save_workflow = F,  # append the workflow ID to the outcome
                                                      verbose = T),           # print extra info as the model trains
                          metrics = metric_set(mae))   # use MAE as the assessment metric
lm_model

# Helper functions
collect_metrics(lm_model) # collect the model performance (average across resamples)

# Other helper functions
show_best(lm_model, metric = "mae")       # bring back the best performing model
collect_notes(lm_model)   # collect any notes

# Have a look at the results
collect_predictions(lm_model)

# Plot the results
collect_predictions(lm_model) %>% 
    filter(.config == pull(show_best(lm_model, metric = 'mae', n =1 ),.config)) %>% # only bring back predictions for the best model
    ggplot(aes(x = .pred, y = team_score )) + 
    geom_point(shape = 1)  +
    geom_abline(slope = 1, linetype = "dashed", colour = 'blue') +
    coord_obs_pred() +
    ggtitle("Actuals vs Prediction on Resamples") + 
    theme(text = element_text(size = 20))

# Plot the residuals
collect_predictions(lm_model) %>% 
    filter(.config == pull(show_best(lm_model, metric = 'mae', n = 1),.config)) %>% 
    mutate(residuals = team_score-.pred) %>% 
    ggplot(aes(x = team_score, y = residuals )) + 
    geom_point(shape = 1)  +
    geom_abline(slope = 0, linetype = "dashed", colour = 'blue') +
    ggtitle("Actuals vs Prediction on Resamples") + 
    theme(text = element_text(size = 20))


# Fit the final model
last_fit <- lm_workflow %>% # workflow that creates our chosen model
    last_fit(split = train_test_split, metrics = metric_set(mae)) # fit the model on 100% Train and predict on the Test set

# See how it performs on the Test set
collect_metrics(last_fit)

# Compare Test v Resamples
inner_join(collect_metrics(lm_model) %>%
               select(.metric, mean) %>%
               rename(.estimate = mean),
           collect_metrics(last_fit) %>%
               select(.metric, .estimate),
           by = ".metric",
           suffix = c("_train", "_test")) %>% 
    data.frame() # tibbles round values so they can appear the same

# Plot predictions on Test
collect_predictions(last_fit) %>% 
    ggplot(aes(x = .pred, y = team_score )) + 
    geom_point(shape = 1)  +
    geom_abline(slope = 1, linetype = "dashed", colour = 'blue') +
    coord_obs_pred() +
    ggtitle("Actuals vs Prediction on Test") + 
    theme(text = element_text(size=20))

# Variable importance
vip::vi(extract_fit_parsnip(last_fit))  # extract the variable importance scores
vip::vip(extract_fit_parsnip(last_fit)) + # plot them
    theme(text = element_text(size = 20)) 



# Create default model specs for lots of different algorithms
create_spec <- function(x){
    x(mode="regression")
}

bag_mars_spec <- create_spec(bag_mars)
bag_tree_spec <- create_spec(bag_tree)
bart_spec <- create_spec(bart)
boost_tree_spec <- create_spec(boost_tree)
cubist_rules_spec <- create_spec(cubist_rules)
decision_tree_spec <- create_spec(decision_tree)
linear_reg_spec <- create_spec(linear_reg)
mars_spec <- create_spec(mars)
mlp_spec <- create_spec(mlp)
knn_spec <- create_spec(nearest_neighbor)
rand_forest_spec <- create_spec(rand_forest)
svm_linear <- create_spec(svm_linear)
svm_poly_spec <- create_spec(svm_poly)
svm_rbf_spec <- create_spec(svm_rbf)

# Recipe
dummy_norm <- recipe(team_score ~ ., data=train) %>% 
    step_unorder(location, is_b2b_first:opp_is_b2b_second) %>%
    step_dummy(location, is_b2b_first:opp_is_b2b_second) %>%
    step_corr(all_numeric_predictors()) %>%
    step_normalize(all_numeric_predictors()) %>%
    step_nzv(all_predictors())

# Have a look at the recipe
dummy_norm %>% prep(train) %>% bake(train)

# Workflow sets
all_models_set <- workflow_set(preproc = list(dummy_norm = dummy_norm),
                               models = list(bag_mars_spec,
                                             bag_tree_spec,
                                             bart_spec,
                                             boost_tree_spec,
                                             cubist_rules_spec,
                                             decision_tree_spec,
                                             glmnet = glmnet_spec, # name glmnet so tidymodels can tell it apart from the linear model
                                             linear_reg_spec,
                                             mars_spec,
                                             mlp_spec,
                                             knn_spec,
                                             rand_forest_spec,
                                             svm_linear,
                                             svm_poly_spec,
                                             svm_rbf_spec),
                               cross = TRUE)

# Have a look at our workflow set
all_models_set

# Run the workflow set
all_models <- all_models_set %>%   # use our workflowset
    workflow_map("fit_resamples", # tell the workflow map what function to run
                 resamples = train_cv, # use our k-fold cross-validation scheme for resampling
                 verbose = T,   # print a more detailed output in the console
                 metrics = metric_set(mae))

# Have a look at the results
collect_metrics(all_models) %>% filter(.metric == "mae") 

# Rank the best models and only return the top X
rank_results(all_models, rank_metric = "mae") %>% 
    filter(.metric == "mae")

# Plot our results
autoplot(all_models) + theme(text = element_text(size = 20))




#### tidymodels tuning ----
glmnet_spec <- linear_reg(mode = "regression", 
                          engine = "glmnet", 
                          penalty = tune(), 
                          mixture = tune())

svm_spec <- svm_rbf(cost = tune(), rbf_sigma = tune()) %>%
    set_engine("kernlab") %>%
    set_mode("classification")

# Manually create our tuning grid
manual_grid <- crossing(penalty = 10^seq(2, -3, by = -.1),
                        mixture = c(0, 1))

latin_grid <- svm_spec %>%
    parameters() %>%
    grid_latin_hypercube(size = 155)

# Create a simple recipe
ts_recipe <- recipe(team_score ~ ., data = train) %>%
    step_dummy(location, is_b2b_first:opp_is_b2b_second) %>%
    step_corr(all_numeric_predictors()) %>%
    step_normalize(all_numeric_predictors()) %>%
    step_nzv(all_predictors())

# Create our workflow for our tunable glmnet
workflow <- workflow() %>%
    add_recipe(ts_recipe) %>% # add in our newly created recipe
    add_model(glmnet_spec) # add the model spec we made earlier

# Have tidymodels generate hyperparameter combinations for us
glmnet_auto_tune <- tune_grid(workflow,
                              resamples = train_cv,
                              metrics = metric_set(mae)) 

collect_metrics(glmnet_auto_tune)   # have a look at how the models performed

# Specify how many hyperparameter combinations to try using 'grid='
glmnet_auto_tune <- tune_grid(workflow,
                              resamples = train_cv,
                              grid = 25,
                              metrics = metric_set(mae))

glmnet_auto_tune <- tune_grid(workflow,
                              resamples = train_cv,
                              grid = manual_grid,
                              metrics = metric_set(mae))

glmnet_auto_tune <- tune_grid(workflow,
                              resamples = train_cv,
                              grid = latin_grid,
                              metrics = metric_set(mae)) 

show_best(glmnet_auto_tune, metric = 'mae', n = 10)  # only show the results for the best 5 models
autoplot(glmnet_auto_tune)   # plot the performance of each model


# Which way of generating hyperparameters gave us the best set?
diff_params <- function(name, x){
    tune_grid(workflow,
              resamples = train_cv,
              grid = x,
              metrics = metric_set(mae)) %>% 
        show_best(metric = 'mae', n = 1) %>% 
        mutate(param_set = name)
}
grid <- diff_params("grid", grid_regular(penalty(), mixture(), levels = 5))
random <- diff_params("random", grid_random(penalty(), mixture(), size = 25))
latin <- diff_params("latin", grid_latin_hypercube(penalty(), mixture(), size = 25))
entropy <- diff_params("entropy", grid_max_entropy(penalty(), mixture(), size = 25))
manual <- diff_params("manual", manual_grid) # manually created grid from earlier

tibble(bind_rows(grid, random, latin, entropy, manual))[-8]


# Run the same hyperparameter search but as a racing method
racing_results <- tune_race_anova(workflow,
                                  resamples = train_cv,
                                  grid = 25,
                                  metrics = metric_set(mae))

racing_results %>% show_best(metric = 'mae', n = 1) 

# See how many hyperparameters made it until the end
plot_race(racing_results)

cl <- makePSOCKcluster(4) 
registerDoParallel(cl)
getDoParWorkers()

set.seed(214)
bayes <- tune_bayes(workflow,
                    resamples = train_cv,
                    initial = 5, # generate five at semi-random to start
                    iter = 15,   # run the search procedure for 15 iterations
                    metrics = metric_set(mae),
                    control = control_bayes(no_improve = 10, verbose = TRUE)) # stop if no improvement after 10

stopCluster(cl)
registerDoSEQ()

show_best(bayes, metric = 'mae', n = 1) 
autoplot(bayes, metric = "mae", type = "performance")

# Can also pass it the results of an existing tune grid run
initial_latin <- tune_grid(workflow,
                           resamples = train_cv,
                           grid = 50, # latin hypercube of 50 values
                           metrics = metric_set(mae))

extra_bayes <- tune_bayes(workflow,
                          resamples = train_cv,
                          initial = initial_latin, # pass the output from the initial run
                          iter = 15,
                          metrics = metric_set(mae),
                          control = control_bayes(no_improve = 10, verbose = TRUE))  

show_best(initial_latin, metric = 'mae', n = 1) # initial results
show_best(extra_bayes, metric = 'mae', n = 1)   # after Bayes



# Annealing
sim_anneal <- tune_sim_anneal(workflow,
                              resamples = train_cv,
                              initial = 5, # generate five at semi-random to start
                              iter = 15,   # run the search procedure for 15 iterations
                              metrics = metric_set(mae),
                              control = control_bayes(no_improve = 10, verbose = TRUE)) # stop if no improvement after 10

show_best(sim_anneal, metric = 'mae', n = 1) 
autoplot(sim_anneal, metric = "mae", type = "performance")



sim_anneal_warm <- tune_sim_anneal(workflow,
                                   resamples = train_cv,
                                   initial = initial_latin, # pass the output from the initial run
                                   iter = 15,
                                   metrics = metric_set(mae),
                                   control = control_sim_anneal(restart = 5L))  

show_best(sim_anneal_warm, metric = 'mae', n = 1)


# Create our model specs and recipes
boost_tree_tune_spec <- parsnip::boost_tree(mode = "regression",
                                            engine = "xgboost",
                                            mtry = tune(),
                                            trees = 1000,
                                            min_n = tune(),
                                            tree_depth = tune(),
                                            learn_rate = tune(),
                                            loss_reduction = tune(),
                                            sample_size = tune(),
                                            stop_iter = tune())

lightGBM_tune_spec <- parsnip::boost_tree(mode = "regression",
                                          engine = "lightgbm",
                                          mtry = tune(),
                                          trees = 1000,
                                          min_n = tune(),
                                          tree_depth = tune(),
                                          learn_rate = tune(),
                                          loss_reduction = tune())

cubist_rules_tune_spec <- parsnip::cubist_rules(mode = "regression",
                                                engine = "Cubist",
                                                committees = tune(),
                                                neighbors = tune(),
                                                max_rules = tune())

mars_tune_spec <- parsnip::mars(mode = "regression",
                                engine = "earth",
                                num_terms = tune(),
                                prod_degree = tune(),
                                prune_method = tune())

mlp_tune_spec <- parsnip::mlp(mode = "regression",
                              engine = "nnet",
                              hidden_units = tune(),
                              penalty = tune(),
                              epochs = tune(),
                              learn_rate = tune(),
                              activation = tune()) 

glmnet_tune_spec <- parsnip::linear_reg(mode="regression",
                                        engine = "glmnet",
                                        penalty = tune(),
                                        mixture = tune())

knn_tune_spec <- parsnip::nearest_neighbor(mode = "regression",
                                           engine = "kknn",
                                           neighbors = tune(),
                                           weight_func = tune(),
                                           dist_power = tune())

rand_forest_tune_spec <- parsnip::rand_forest(mode="regression", 
                                              engine = "ranger",
                                              mtry = tune(),
                                              trees = 1000,
                                              min_n = tune())

svm_linear_tune_spec <- parsnip::svm_linear(mode = "regression",
                                            engine = "LiblineaR", 
                                            cost = tune(), 
                                            margin = tune())

svm_poly_tune_spec <- parsnip::svm_poly(mode = "regression",
                                        engine = "kernlab",
                                        cost = tune(),
                                        degree = tune(),
                                        scale_factor = tune(),
                                        margin = tune())

svm_rbf_tune_spec <- parsnip::svm_rbf(mode = "regression",
                                      engine = "kernlab",
                                      cost = tune(),
                                      rbf_sigma = tune(),
                                      margin = tune())

# Define a set of recipes
bare_recipe <- recipe(team_score ~ ., data = train)

dummy_imp <- recipe(team_score ~ ., data = train) %>%
    step_dummy(location, is_b2b_first:opp_is_b2b_second) %>%    # create dummy variables
    step_nzv(all_predictors())             # remove zero and near-zero variance features

dummy_imp_trans <- recipe(team_score ~ ., data = train) %>% 
    step_YeoJohnson(all_numeric_predictors()) %>% # transform numeric features  
    step_dummy(location, is_b2b_first:opp_is_b2b_second) %>%    # create dummy variables
    step_normalize(all_numeric_predictors()) %>% # normalise all numeric features
    step_nzv(all_predictors())            # remove zero and near-zero variance features

# Define our workflow set
tune_workflows <- rbind(workflow_set(preproc = list(bare_recipe = bare_recipe),
                                     models = list(cubist = cubist_rules_tune_spec),
                                     cross = TRUE),
                        workflow_set(preproc = list(dummy_imp = dummy_imp),
                                     models = list(xgboost = boost_tree_tune_spec,  
                                                   lightGBM = lightGBM_tune_spec,
                                                   rand_forest = rand_forest_tune_spec),
                                     cross = TRUE),
                        workflow_set(preproc = list(dummy_imp_trans = dummy_imp_trans),
                                     models = list(glmnet = glmnet_tune_spec,
                                                   knn = knn_tune_spec,
                                                   mars = mars_tune_spec,   
                                                   NN = mlp_tune_spec,
                                                   svm_linear = svm_linear_tune_spec,  
                                                   svm_poly = svm_poly_tune_spec, 
                                                   svm_rbf = svm_rbf_tune_spec),
                                     cross = TRUE))

# Use a faster 5-fold cross-validation scheme
train_cv <- vfold_cv(train, v = 5, repeats = 1)

# Run and tune all our models
cl <- makePSOCKcluster(6)  # Create clusters
registerDoParallel(cl)
getDoParWorkers()

tune_workflows_models <- tune_workflows %>%
    workflow_map("tune_grid", # tell workflow map what function to use
                 resamples = train_cv, # resampling scheme 
                 grid = 25, # size of grid
                 metrics = metric_set(mae),
                 verbose = TRUE)

# Rank the best models
rank_results(tune_workflows_models, rank_metric = "mae") %>% 
    filter(.metric == "mae")

# Show the best sub-model per workflow
rank_results(tune_workflows_models, rank_metric = "mae", select_best = T)

# Plot the results
autoplot(tune_workflows_models, select_best = TRUE) + theme(text = element_text(size = 20))

# Save the workflow ID of the best performing model
best_tuned_workflow <- rank_results(tune_workflows_models, rank_metric = "mae", select_best = TRUE) %>%
    select(wflow_id, model, rank, mean, .config) %>% 
    filter(rank == 1) 
best_tuned_workflow

# Find the hyperparameter combo from the best model's workflow
final_model <- extract_workflow_set_result(tune_workflows_models, pull(best_tuned_workflow, wflow_id)) %>% 
    select_best(metric = "mae") 
final_model

# Fit final model on Train and predict on Test set
final_model_pred <- extract_workflow(tune_workflows_models, pull(best_tuned_workflow, wflow_id)) %>% # extract the workflow
    finalize_workflow(final_model) %>% # finalise the workflow with the hyperparameters of the best model
    last_fit(train_test_split) # fit the model on Train and score on Test

# How did we do?
collect_predictions(final_model_pred) %>% # gets Test set scores
    mae(.pred, target) # calculate mae

# Plot actual vs prediction on Test set
collect_predictions(final_model_pred) %>% # gets Test set scores
    ggplot(aes(x = .pred,y = team_score)) + 
    geom_point(shape = 1) +
    geom_abline(slope = 1, colour = 'blue') +
    coord_obs_pred()+ 
    ggtitle("Predictions vs Actuals on Test Set") +
    theme(text = element_text(size = 20))


# Generate our candidate models for stacking
stack_workflow <-
    tune_workflows %>% # filter for only the best models
    filter(wflow_id %in% c("bare_recipe_cubist", "dummy_imp_xgboost",
                           "dummy_imp_trans_NN", "dummy_imp_trans_svm_poly")) %>% 
    workflow_map("tune_grid", 
                 resamples = train_cv, 
                 grid = 50, # size of grid
                 metrics = metric_set(mae),
                 control = control_grid(save_pred = TRUE, # tell tidymodels to save the resample predictions
                                        save_workflow = TRUE),
                 verbose = TRUE)

rank_results(stack_workflow, rank_metric = "mae", select_best = T)

# Create our stacks object and add our models to it
stack_model <- 
    stacks() %>%
    add_candidates(stack_workflow) 
stack_model

# Blend the outputs. Fits a LASSO model and keep any candidate models with >0 coefficients
stack_model_blend <-
    stack_model %>%
    blend_predictions(metric = metric_set(mae))
stack_model_blend

# Plot the stack performance for different penalty values
autoplot(stack_model_blend) 

# Plot the different weights of each candidate
autoplot(stack_model_blend, type = "weights") 

# Fit the model
stack_model_fit <-
    stack_model_blend %>%
    fit_members()

# Get the Test set results of the stack and each individual model
results <-
    bind_cols(test %>% select(target), 
              predict(stack_model_fit, test, members = TRUE))

# See how the stack does against the individual candidates
map_dfr(results, mae, truth = target, data = results) %>%
    mutate(member = colnames(results)) %>% 
    arrange(.estimate)
















