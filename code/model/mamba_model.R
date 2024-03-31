library(tidyverse)
library(data.table)
library(janitor)
library(RSQLite)
library(DBI)
library(theoddsapi)

options(scipen = 999999)

#### all functions for mamba model ----
# function for odds
get_odds <- function(book_name, dates) {
    Sys.setenv(THEODDS_API_KEY = "9f158680c234e152e9df4027f4eccf1f")
    
    get_sports()
    print(get_remaining_requests())
    
    nba_schedule_current <- tbl(DBI::dbConnect(RSQLite::SQLite(),
                                               "../nba_sql_db/nba_db"),
                                "nba_schedule_current") %>%
        collect() %>%
        mutate(game_date = as_date(game_date, origin ="1970-01-01"))
    
    all_spread <- theoddsapi::get_odds("basketball_nba", mkt = "spreads")
    all_total <- theoddsapi::get_odds("basketball_nba", mkt = "totals")
    all_ml <- theoddsapi::get_odds("basketball_nba", mkt = "h2h") %>%
        mutate(h2h = round(if_else(h2h >= 2, (h2h-1)*100, (-100)/(h2h-1)),0))
    
    book_spread <- all_spread %>%
        mutate(commence_time = as_date(commence_time)) %>%
        filter(book == book_name) %>%
        rename(line = spreads) %>%
        mutate(bet_type = "spread")
    
    book_total <- all_total %>%
        mutate(commence_time = as_date(commence_time)) %>%
        filter(book == book_name) %>%
        rename(line = totals) %>%
        mutate(bet_type = "total")
    
    book_ml <- all_ml %>%
        mutate(commence_time = as_date(commence_time)) %>%
        filter(book == book_name) %>%
        rename(line = h2h) %>%
        mutate(bet_type = "ml")
    
    all_odds <- bind_rows(book_spread, book_total, book_ml) %>%
        filter(commence_time %in% c(dates)) %>%
        pivot_wider(names_from = bet_type,
                    values_from = line) %>%
        select(commence_time, team, spread, ml, total) %>%
        mutate(team = str_replace(team, "Los Angeles Clippers", "LA Clippers"))
    
    team_df <- nba_schedule_current %>%
        filter(game_date %in% c(dates)) %>%
        select(team_name, game_date) %>%
        left_join(all_odds, by = c("team_name" = "team",
                                   "game_date" = "commence_time")) %>%
        rename(team_spread = spread,
               team_moneyline = ml,
               over_under = total)
    
    opp_df <- nba_schedule_current %>%
        filter(game_date %in% c(dates)) %>%
        select(opp_team_name, game_date) %>%
        left_join(all_odds, by = c("opp_team_name" = "team",
                                   "game_date" = "commence_time")) %>%
        rename(opp_spread = spread,
               opp_moneyline = ml) %>%
        select(-game_date)
    
    final_odds <- bind_cols(team_df, opp_df) %>%
        select(game_date, team_name, opp_team_name, team_spread, opp_spread,
               team_moneyline, opp_moneyline, over_under)
    
    odds_wpo <- final_odds %>%
        mutate(team_moneyline = odds.converter::odds.us2dec(team_moneyline),
               opp_moneyline = odds.converter::odds.us2dec(opp_moneyline)) %>%
        select(team_moneyline, opp_moneyline)
    
    odds_wpo <- implied::implied_probabilities(odds_wpo, method = 'wpo')
    
    final_odds <- final_odds %>%
        mutate(team_implied_prob = odds_wpo$probabilities[,1],
               opp_implied_prob = odds_wpo$probabilities[,2])
    
    return(final_odds)
}

# functions for mamba cleanR
mamba_nba <- function(seasons) {
    
    generate_headers <- function() {
        headers <- c(
            `Sec-Fetch-Site` = "same-site",
            `Accept` = "*/*",
            `Origin` = "https://www.nba.com",
            `Sec-Fetch-Dest` = "empty",
            `Accept-Language` = "en-US,en;q=0.9",
            `Sec-Fetch-Mode` = "cors",
            `Host` = "stats.nba.com",
            `User-Agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.0 Safari/605.1.15",
            `Referer` = "https://www.nba.com/",
            `Accept-Encoding` = "gzip, deflate, br",
            `Connection` = "keep-alive"
        )
        return(headers)
    }
    
    generate_parameters <- function(year, measure_type) {
        year <- (year - 1)
        season <- sprintf("%d-%02d", year, (year + 1) %% 100)
        params <- list(
            `DateFrom` = "",
            `DateTo` = "",
            `GameSegment` = "",
            `ISTRound` = "",
            `LastNGames` = "0",
            `LeagueID` = "00",
            `Location` = "",
            `MeasureType` = measure_type,
            `Month` = "0",
            `OpponentTeamID` = "0",
            `Outcome` = "",
            `PORound` = "0",
            `PaceAdjust` = "N",
            `PerMode` = "Totals",
            `Period` = "0",
            `PlusMinus` = "N",
            `Rank` = "N",
            `Season` = season,
            `SeasonSegment` = "",
            `SeasonType` = "Regular Season",
            `ShotClockRange` = "",
            `VsConference` = "",
            `VsDivision` = ""
        )
        return(params)
    }
    
    headers <- generate_headers()
    all_data_list <- list()
    
    # Define available measure types
    available_measure_types <- c("Base", "Advanced", "Four Factors", "Misc", "Scoring")
    
    for (measure_type in available_measure_types) {
        all_data <- data.frame()
        
        for (year in seasons) {
            params <- generate_parameters(year, measure_type)
            
            res <- httr::GET(url = "https://stats.nba.com/stats/teamgamelogs",
                             httr::add_headers(.headers = headers), query = params)
            data <- httr::content(res) %>% .[['resultSets']] %>% .[[1]]
            column_names <- data$headers %>% as.character()
            dt <- rbindlist(data$rowSet) %>% setnames(column_names)
            
            all_data <- bind_rows(all_data, dt)
            
            print(paste0(params$Season, " ", params$MeasureType))
        }
        
        all_data_list[[measure_type]] <- all_data
    }
    
    # Data transformation code
    cleaned_team_list <- lapply(all_data_list, function(df) {
        df <- clean_names(df) %>%
            select(-starts_with(c("e_","opp_")), -ends_with(c("_rank","_flag")))
        return(df)
    })
    
    combined_df <- cleaned_team_list[[1]]
    for (i in 2:length(cleaned_team_list)) {
        df_to_join <- cleaned_team_list[[i]]
        existing_cols <- intersect(names(combined_df), names(df_to_join))
        existing_cols <- setdiff(existing_cols, c("game_id", "team_name"))
        df_to_join <- df_to_join %>% select(-any_of(existing_cols))
        combined_df <- left_join(combined_df, df_to_join, by = c("game_id", "team_name"))
    }
    
    team_all_stats <- combined_df %>%
        arrange(game_date, game_id) %>%
        mutate(location = if_else(grepl("@", matchup) == T, "away", "home"),
               game_date = as_date(game_date),
               season_year = as.numeric(substr(season_year, 1, 4)) + 1) %>%
        select(season_year:matchup, location, wl:pct_uast_fgm)
    
    opp_all_stats <- team_all_stats %>%
        select(game_id, team_id, team_abbreviation, team_name,
               fgm:pct_uast_fgm) %>%
        rename_with(~paste0("opp_", .), -c(game_id)) %>%
        select(-opp_plus_minus)
    
    team_games <- team_all_stats %>%
        distinct(season_year, game_id, game_date, team_id, team_name) %>%
        group_by(season_year, team_id) %>%
        mutate(
            game_count_season = 1:n(),
            days_rest_team = ifelse(game_count_season > 1,
                                    (game_date - lag(game_date) - 1),
                                    120),
            days_next_game_team =
                ifelse(game_count_season < 82,
                       ((
                           lead(game_date) - game_date
                       ) - 1),
                       120),
            days_next_game_team = days_next_game_team %>% as.numeric(),
            days_rest_team = days_rest_team %>% as.numeric(),
            is_b2b = if_else(days_next_game_team == 0 |
                                 days_rest_team == 0, TRUE, FALSE),
            is_b2b_first = if_else(lead(days_next_game_team) == 0, TRUE, FALSE),
            is_b2b_second = if_else(lag(days_next_game_team) == 0, TRUE, FALSE)
        ) %>%
        ungroup() %>%
        mutate_if(is.logical, ~ ifelse(is.na(.), FALSE, .)) %>%
        select(game_id, team_name,
               is_b2b_first, is_b2b_second, game_count_season)
    
    opp_team_games <- team_games %>%
        select(game_id, team_name,
               is_b2b_first, is_b2b_second, game_count_season) %>%
        rename_with(~paste0("opp_", .), -c(game_id))
    
    raw_stats <- team_all_stats %>%
        inner_join(opp_all_stats, by = c("game_id"), relationship = "many-to-many") %>%
        filter(team_name != opp_team_name) %>%
        left_join(team_games, by = c("game_id", "team_name")) %>%
        left_join(opp_team_games, by = c("game_id", "opp_team_name")) %>%
        select(season_year, team_id, team_abbreviation, team_name,
               opp_team_id, opp_team_abbreviation, opp_team_name,
               game_id:min, pts, opp_pts, plus_minus,
               game_count_season, opp_game_count_season,
               is_b2b_first, is_b2b_second, opp_is_b2b_first, opp_is_b2b_second,
               fgm:opp_pct_uast_fgm) %>%
        arrange(game_date, game_id, location)
    
    # mamba_pace_adj <- raw_stats %>%
    #     select(season_year, game_date, pace) %>%
    #     group_by(game_date) %>%
    #     summarize(sum_pace = sum(pace),
    #               num_row = n()) %>%
    #     mutate(pace_adj = cumsum(sum_pace)/cumsum(num_row)) %>%
    #     ungroup() %>%
    #     select(game_date, pace_adj)
    
    stats_mov_avg <- raw_stats %>%
        mutate(pts_2pt_mr = round(pct_pts_2pt_mr*pts,0),
               ast_2pm = round(pct_ast_2pm*(fgm-fg3m),0),
               ast_3pm = round(pct_ast_3pm*fg3m,0),
               opp_pts_2pt_mr = round(opp_pct_pts_2pt_mr*opp_pts,0),
               opp_ast_2pm = round(opp_pct_ast_2pm*(opp_fgm-opp_fg3m),0),
               opp_ast_3pm = round(opp_pct_ast_3pm*opp_fg3m,0)
        ) %>%
        # left_join(mamba_pace_adj) %>%
        group_by(season_year, team_id, location) %>%
        mutate(across(c(fgm:opp_pct_uast_fgm),
                      \(x) pracma::movavg(x, n = 10, type = 'e'))
               # mutate(across(c(fgm:opp_pct_uast_fgm), ~ . * (pace_adj/pace)),
               #        across(c(fgm:opp_pct_uast_fgm),
               #               \(x) pracma::movavg(x, n = 10, type = 'e'))
        ) %>%
        ungroup() %>%
        mutate(fg_pct = fgm/fga,
               fg3_pct = fg3m/fg3a,
               ft_pct = ftm/fta,
               ast_pct = ast/fgm,
               oreb_pct = oreb/(oreb+opp_dreb),
               dreb_pct = dreb/(dreb+opp_oreb),
               reb_pct = reb/(reb+opp_reb),
               tm_tov_pct = tov/poss,
               efg_pct = (fgm+(0.5*fg3m))/fga,
               ts_pct = pts/(2*(fga+0.44*fta)),
               fta_rate = fta/fga,
               pct_fga_2pt = (fga-fg3a)/fga,
               pct_fga_3pt = 1-pct_fga_2pt,
               pct_pts_2pt = ((fgm-fg3m)*2)/pts,
               pct_pts_2pt_mr = pts_2pt_mr/pts,
               pct_pts_3pt = (fg3m*3)/pts,
               pct_pts_fb = pts_fb/pts,
               pct_pts_ft = ftm/pts,
               pct_pts_off_tov = pts_off_tov/pts,
               pct_pts_paint = pts_paint/pts,
               pct_ast_2pm = ast_2pm/(fgm-fg3m),
               pct_uast_2pm = 1-pct_ast_2pm,
               pct_ast_3pm = ast_3pm/fg3m,
               pct_uast_3pm = 1-pct_ast_3pm,
               pct_ast_fgm = ast_pct,
               pct_uast_fgm = 1-ast_pct,
               opp_fg_pct = opp_fgm/opp_fga,
               opp_fg3_pct = opp_fg3m/opp_fg3a,
               opp_ft_pct = opp_ftm/opp_fta,
               opp_ast_pct = opp_ast/opp_fgm,
               opp_oreb_pct = opp_oreb/(opp_oreb+dreb),
               opp_dreb_pct = opp_dreb/(opp_dreb+oreb),
               opp_reb_pct = opp_reb/(reb+opp_reb),
               opp_tm_tov_pct = opp_tov/opp_poss,
               opp_efg_pct = (opp_fgm+(0.5*opp_fg3m))/opp_fga,
               opp_ts_pct = opp_pts/(2*(opp_fga+0.44*opp_fta)),
               opp_fta_rate = opp_fta/opp_fga,
               opp_pct_fga_2pt = (opp_fga-opp_fg3a)/opp_fga,
               opp_pct_fga_3pt = 1-opp_pct_fga_2pt,
               opp_pct_pts_2pt = ((opp_fgm-opp_fg3m)*2)/opp_pts,
               opp_pct_pts_2pt_mr = opp_pts_2pt_mr/opp_pts,
               opp_pct_pts_3pt = (opp_fg3m*3)/opp_pts,
               opp_pct_pts_fb = opp_pts_fb/opp_pts,
               opp_pct_pts_ft = opp_ftm/opp_pts,
               opp_pct_pts_off_tov = opp_pts_off_tov/opp_pts,
               opp_pct_pts_paint = opp_pts_paint/opp_pts,
               opp_pct_ast_2pm = opp_ast_2pm/(opp_fgm-opp_fg3m),
               opp_pct_uast_2pm = 1-opp_pct_ast_2pm,
               opp_pct_ast_3pm = opp_ast_3pm/opp_fg3m,
               opp_pct_uast_3pm = 1-opp_pct_ast_3pm,
               opp_pct_ast_fgm = opp_ast_pct,
               opp_pct_uast_fgm = 1-opp_ast_pct) %>%
        select(-c(pts_2pt_mr, ast_2pm, ast_3pm,
                  opp_pts_2pt_mr, opp_ast_2pm, opp_ast_3pm))
    
    stats_lag <- stats_mov_avg
    
    # stats_lag <- stats_mov_avg %>%
    #     group_by(season_year, team_name, location) %>%
    #     mutate(across(fgm:opp_pct_uast_fgm, \(x) lag(x, n = 1))) %>%
    #     ungroup() %>%
    #     na.exclude()
    
    base_stats_full <- stats_lag %>%
        select(season_year:opp_is_b2b_second)
    
    team_stats <- stats_lag %>%
        select(season_year, game_id, team_name, fgm:opp_pct_uast_fgm) %>%
        rename_with(~paste0("team_", .), fgm:opp_pct_uast_fgm) %>%
        select(-season_year)
    
    opp_stats <- stats_lag %>%
        select(season_year, game_id, team_name, fgm:opp_pct_uast_fgm) %>%
        rename_with(~paste0("opp_", .), team_name:opp_pct_uast_fgm) %>%
        select(-season_year)
    
    nba_final_full <- base_stats_full %>%
        left_join(team_stats, by = c("game_id" = "game_id",
                                     "team_name" = "team_name")) %>%
        left_join(opp_stats, by = c("game_id" = "game_id",
                                    "opp_team_name" = "opp_team_name")) %>%
        arrange(game_date, game_id, location) %>%
        na.exclude()
    
    
    base_stats <- stats_lag %>%
        filter(location == "away") %>%
        select(season_year:opp_is_b2b_second) %>%
        rename_with(~gsub("^opp_", "home_", .), starts_with("opp_")) %>%
        rename_with(~paste0("away_", .), c(team_id:team_name,
                                           pts, game_count_season, plus_minus,
                                           is_b2b_first, is_b2b_second))
    
    away_stats <- stats_lag %>%
        filter(location == "away") %>%
        select(season_year, game_id, team_name, fgm:opp_pct_uast_fgm) %>%
        rename_with(~paste0("away_", .), team_name:opp_pct_uast_fgm) %>%
        select(-season_year)
    
    home_stats <- stats_lag %>%
        filter(location == "home") %>%
        select(season_year, game_id, team_name, fgm:opp_pct_uast_fgm) %>%
        rename_with(~paste0("home_", .), team_name:opp_pct_uast_fgm) %>%
        select(-season_year)
    
    nba_final <- base_stats %>%
        left_join(away_stats, by = c("game_id" = "game_id",
                                     "away_team_name" = "away_team_name")) %>%
        left_join(home_stats, by = c("game_id" = "game_id",
                                     "home_team_name" = "home_team_name")) %>%
        arrange(game_date, game_id) %>%
        na.exclude()
    
    # # game by game stats in mamba format
    # assign(x = "mamba_raw_stats", raw_stats, envir = .GlobalEnv)
    
    # lagged stats in mamba format - long
    assign(x = "mamba_daily", nba_final_full, envir = .GlobalEnv)
    
    # # lagged stats in mamba format - wide
    # assign(x = "mamba_lag_wide", nba_final, envir = .GlobalEnv)
}

add_stats <- function() {
    
    team_stats <- mamba_daily %>%
        group_by(team_name) %>%
        slice(tail(row_number(), 1)) %>%
        select(team_name, team_fgm:team_opp_pct_uast_fgm)
    
    opps_stats <- mamba_daily %>%
        group_by(team_name) %>%
        slice(tail(row_number(), 1)) %>%
        select(team_name, team_fgm:team_opp_pct_uast_fgm) %>%
        rename_with(~gsub("^team_", "opp_", .), starts_with("team_")) %>%
        rename(opp_team_name = opp_name)
    
    slate <- slate_today
    
    mamba_today <- slate %>%
        left_join(team_stats %>% select(team_name,
                                        team_fgm:team_opp_pct_uast_fgm),
                  by = c("team_name")) %>%
        left_join(opps_stats %>% select(opp_team_name,
                                        opp_fgm:opp_opp_pct_uast_fgm),
                  by = c("opp_team_name")) %>%
        select(game_date:opp_implied_prob, team_fgm:opp_opp_pct_uast_fgm) %>%
        na.exclude()
    
    assign(x = "mamba_today", mamba_today, envir = .GlobalEnv)
}

# function to read in models for predictions
load_model_rds_files <- function(directory_path) {
    # Get a list of all RDS files in the specified directory
    rds_files <- list.files(path = directory_path, pattern = "\\.rds$", full.names = TRUE)
    
    # Check if there are any RDS files in the directory
    if (length(rds_files) == 0) {
        stop("No RDS files found in the specified directory.")
    }
    
    # Initialize an empty list to store loaded data
    loaded_data <- list()
    
    # Use a for loop to read each RDS file and store the results in a list
    for (file in rds_files) {
        # Extract the desired part of the file name
        file_name <- sub("_20_23.*", "", tools::file_path_sans_ext(basename(file)))
        # Read the RDS file and assign to the list using the extracted file_name as the list element name
        loaded_data[[file_name]] <- readRDS(file)
    }
    
    # Create separate lists based on names
    win_models <- loaded_data[grep("win", names(loaded_data), ignore.case = TRUE)]
    score_models <- loaded_data[grep("score", names(loaded_data), ignore.case = TRUE)]
    
    win_models <- win_models[c("cor_cols_win", "glmb_win", "log_win", "mars_win",
                               "nn_win", "pre_proc_cs_win", "pre_proc_yj_win",
                               "reg_win", "rf_win", "svm_win", "xgb_win")]
    score_models <- score_models[c("cor_cols_score", "glmb_score", "lin_score", "mars_score",
                                 "nn_score", "pre_proc_cs_score", "pre_proc_yj_score",
                                 "reg_score", "rf_score", "svm_score", "xgb_score")]

    # Return a list containing the three sublists
    return(list(win_models = win_models,
                score_models = score_models))
}

# function to make predictions
make_predictions <- function(model_type, model_list) {
    model_preds <- character(0)
    
    for (i in seq_along(model_list_pred[[model_type]])) {
        model <- model_list_pred[[model_type]][[i]]
        
        if (model_type == "win_models") {
            if (!grepl("svm|nn", model$method)) {
                pred <- predict(model, mamba_win_cs, type = "prob")[1]
                pred <- pred %>%
                    mutate(win = if_else(row_number() %% 2 == 0,
                                         1 - lag(win), win),
                           opp_pred = 1 - win)
            } else {
                pred <- predict(model, mamba_win_yj, type = "prob")[1]
                pred <- pred %>%
                    mutate(win = if_else(row_number() %% 2 == 0,
                                         1 - lag(win), win),
                           opp_pred = 1 - win)
            }
        } else if (model_type == "score_models") {
            if (!grepl("svm|nn", model$method)) {
                pred <- as.data.frame(predict(model, mamba_score_cs))
                colnames(pred)[1] <- "score"
                pred <- pred %>%
                    mutate(opp_pred = if_else(row_number() %% 2 == 0,
                                              lag(pred$score), lead(pred$score)))
            } else {
                pred <- as.data.frame(predict(model, mamba_score_yj))
                colnames(pred)[1] <- "score"
                pred <- pred %>%
                    mutate(opp_pred = if_else(row_number() %% 2 == 0,
                                              lag(pred$score), lead(pred$score)))
            }
        } else {
            stop("Unknown model type")
        }
        
        new_cols <- c(paste0(names(model_list_pred[[model_type]][i]), "_team"),
                      paste0(names(model_list_pred[[model_type]][i]), "_opp"))
        colnames(pred) <- new_cols
        model_preds <- c(model_preds, pred)
    }
    
    return(model_preds)
}


#### schedule and odds ----
# get current schedule from database
nba_schedule_current <- tbl(dbConnect(SQLite(), "../nba_sql_db/nba_db"),
                            "nba_schedule_current") %>%
    collect() %>%
    mutate(game_date = as_date(game_date, origin ="1970-01-01"))

# get current odds
book_name <- "DraftKings"
dates <- today()
today_odds <- get_odds(book_name, dates)

# today's slate
slate_today <- nba_schedule_current %>%
    filter(game_date %in% c(dates)) %>%
    left_join(today_odds)


#### prepare ytd data ----
mamba_nba(2024)

add_stats()

mamba_inputs <- mamba_today %>%
    select(location, is_b2b_first:opp_is_b2b_second, over_under, team_implied_prob,
           team_fgm:opp_opp_pct_uast_fgm)

#### prepare for predictions ----
directory_path <- "../NBAdb/models/trained_models/"
model_list <- invisible(load_model_rds_files(directory_path))

model_list$win_models <- model_list$win_models[c(3,8,9,11,2,4,10,5,6,7,1)]
model_list$score_models <- model_list$score_models[c(3,8,9,11,2,4,10,5,6,7,1)]

#### prepare model inputs
mamba_win_cs <- mamba_inputs %>%
    select(-location, -all_of(model_list$win_models$cor_cols_win)) %>%
    mutate(across(is_b2b_first:opp_is_b2b_second, factor))
mamba_win_cs <- predict(model_list$win_models$pre_proc_cs_win, mamba_win_cs)

mamba_win_yj <- mamba_inputs %>%
    select(-location, -all_of(model_list$win_models$cor_cols_win)) %>%
    mutate(across(is_b2b_first:opp_is_b2b_second, factor))
mamba_win_yj <- predict(model_list$win_models$pre_proc_yj_win, mamba_win_yj)

mamba_score_cs <- mamba_inputs %>%
    select(-all_of(model_list$score_models$cor_cols_score)) %>%
    mutate(location = if_else(location == "away", 1, 0)) %>%
    mutate(across(location:opp_is_b2b_second, factor))
mamba_score_cs <- predict(model_list$score_models$pre_proc_cs_score, mamba_score_cs)

mamba_score_yj <- mamba_inputs %>%
    select(-all_of(model_list$score_models$cor_cols_score)) %>%
    mutate(location = if_else(location == "away", 1, 0)) %>%
    mutate(across(location:opp_is_b2b_second, factor))
mamba_score_yj <- predict(model_list$score_models$pre_proc_yj_score, mamba_score_yj)


# Remove specific elements from win_models and score_models lists
model_list_pred <- model_list

# Remove elements from win_models
model_list_pred$win_models$cor_cols_win <- NULL
model_list_pred$win_models$pre_proc_cs_win <- NULL
model_list_pred$win_models$pre_proc_yj_win <- NULL

# Remove elements from score_models
model_list_pred$score_models$cor_cols_score <- NULL
model_list_pred$score_models$pre_proc_cs_score <- NULL
model_list_pred$score_models$pre_proc_yj_score <- NULL


#### make predictions ----
# initialize slate_final
slate_final <- slate_today %>%
    select(game_date:opp_team_name, team_spread:team_implied_prob) %>%
    mutate(opp_implied_prob = 1 - team_implied_prob) %>%
    arrange(game_id, location)

# loop through models
for (model_type in c("win_models", "score_models")) {
    slate_final <- bind_cols(slate_final, make_predictions(model_type, model_list_pred))
}


# clear environment ----
rm(list=ls()[! ls() %in% c("model_outputs", "slate_final")])


### left off 3/30 --- evaluate bets -- align w/ eval file









moneyline_key <- 0.05781939 # regularization (ridge)
spread_key <- 4.30365229 # ensemble (lin, reg, svm, nn, xgb)
over_key <- 2.5421957 # ensemble (lin, reg, svm, nn, xgb)
under_key <- 1.9372166 # ensemble (lin, reg, svm, nn, xgb)

# add ensemble
bets_final <- slate_final %>%
    mutate(
        ens_win_away = rowMeans(select(.,log_win_away,reg_win_away,
                                          svm_win_away,nn_win_away,
                                          xgb_win_away), na.rm = TRUE),
        ens_team_score = rowMeans(select(.,lin_team_score,reg_team_score,
                                            svm_team_score,nn_team_score,
                                            xgb_team_score), na.rm = TRUE),
        ens_opp_score = rowMeans(select(.,lin_opp_score,reg_opp_score,
                                           svm_opp_score,nn_opp_score,
                                           xgb_opp_score), na.rm = TRUE),
        reg_win_edge_away = reg_win_away - away_implied_prob,
        ens_spread_edge_away = (ens_team_score - ens_opp_score) + away_spread,
        moneyline_bet = if_else(abs(reg_win_edge_away) >= moneyline_key &
                                    reg_win_edge_away > 0,
                                   paste0(away_team_name," ", away_moneyline),
                                if_else(abs(reg_win_edge_away) >= moneyline_key
                                        & reg_win_edge_away < 0,
                                        paste0(home_team_name, " ",
                                               home_moneyline), NA), NA),
        spread_bet = if_else(abs(ens_spread_edge_away) >= spread_key &
                                    ens_spread_edge_away > 0,
                                paste0(away_team_name," ", away_spread),
                             if_else(abs(ens_spread_edge_away) >= spread_key &
                                         ens_spread_edge_away < 0,
                                     paste0(home_team_name, " ", home_spread),
                                     NA), NA),
        over_under_bet = case_when(
            (ens_team_score + ens_opp_score) > over_under &
                ((ens_team_score + ens_opp_score) - over_under) >
                over_key ~ paste0("over ", over_under),
            (ens_team_score + ens_opp_score) < over_under &
                (over_under - (ens_team_score + ens_opp_score)) >
                under_key ~ paste0("under ", over_under),
            .default = as.character(NA))
    ) %>%
    select(game_date:over_under, reg_win_away, ens_team_score, ens_opp_score,
           reg_win_edge_away, ens_spread_edge_away,
           moneyline_bet, spread_bet, over_under_bet) %>%
    filter(!is.na(moneyline_bet) | !is.na(spread_bet) |
               !is.na(over_under_bet)) %>%
    select(game_date:home_team_name, moneyline_bet, spread_bet, over_under_bet)

bets_final


results_book <- slate_final %>%
    mutate(
        log_win_home = 1-log_win_away,
        reg_win_home = 1-reg_win_away,
        knn_win_home = 1-knn_win_away,
        rf_win_home = 1-rf_win_away,
        svm_win_home = 1-svm_win_away,
        nn_win_home = 1-nn_win_away,
        xgb_win_home = 1-xgb_win_away,
        ens_win_away = rowMeans(select(.,log_win_away,reg_win_away,
                                       svm_win_away,nn_win_away,
                                       xgb_win_away), na.rm = TRUE),
        ens_win_home = 1-ens_win_away,
        ens_team_score = rowMeans(select(.,lin_team_score,reg_team_score,
                                         svm_team_score,nn_team_score,
                                         xgb_team_score), na.rm = TRUE),
        ens_opp_score = rowMeans(select(.,lin_opp_score,reg_opp_score,
                                        svm_opp_score,nn_opp_score,
                                        xgb_opp_score), na.rm = TRUE)
    ) %>%
    select(game_id, log_win_away, log_win_home, reg_win_away, reg_win_home,
           knn_win_away, knn_win_home, rf_win_away, rf_win_home,
           svm_win_away, svm_win_home, nn_win_away, nn_win_home,
           xgb_win_away, xgb_win_home, lin_team_score:xgb_team_score,
           lin_opp_score:xgb_opp_score)


    



















