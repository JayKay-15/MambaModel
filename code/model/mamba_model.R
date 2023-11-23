library(tidyverse)
library(data.table)
library(janitor)
library(RSQLite)
library(DBI)
library(theoddsapi)


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
    
    teams_away <- nba_schedule_current %>%
        filter(game_date %in% c(dates)) %>%
        select(away_team_name, game_date) %>%
        left_join(all_odds, by = c("away_team_name" = "team",
                                   "game_date" = "commence_time")) %>%
        rename(away_spread = spread,
               away_moneyline = ml)
    
    teams_home <- nba_schedule_current %>%
        filter(game_date %in% c(dates)) %>%
        select(home_team_name, game_date) %>%
        left_join(all_odds, by = c("home_team_name" = "team",
                                   "game_date" = "commence_time")) %>%
        rename(home_spread = spread,
               home_moneyline = ml,
               over_under = total) %>%
        select(-game_date)
    
    final_odds <- bind_cols(teams_away, teams_home) %>%
        select(game_date, away_team_name, home_team_name, away_spread, home_spread,
               away_moneyline, home_moneyline, over_under)
    
    odds_wpo <- final_odds %>%
        mutate(away_moneyline = odds.converter::odds.us2dec(away_moneyline),
               home_moneyline = odds.converter::odds.us2dec(home_moneyline)) %>%
        select(away_moneyline, home_moneyline)
    
    odds_wpo <- implied::implied_probabilities(odds_wpo, method = 'wpo')
    
    final_odds <- final_odds %>%
        mutate(away_implied_prob = odds_wpo$probabilities[,1])
    
    return(final_odds)
}

# functions for mamba cleanR
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
mamba_nba_cleanR <- function(seasons) {
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
    
    opp_all_stats <- team_all_stats %>%
        select(game_id, team_name, fgm:pct_uast_fgm) %>%
        rename_with(~paste0("opp_", .), -c(game_id)) %>%
        select(-opp_plus_minus)
    
    min_games <<- min(team_all_stats %>%
                          select(team_name, location) %>%
                          group_by(team_name, location) %>%
                          tally() %>%
                          ungroup() %>%
                          select(n))
    
    nba_final <- team_all_stats %>%
        inner_join(opp_all_stats, by = c("game_id"), relationship = "many-to-many") %>%
        filter(team_name != opp_team_name) %>%
        select(season_year:team_name, opp_team_name,
               game_id:min, pts, opp_pts, plus_minus, fgm:opp_pct_uast_fgm) %>%
        mutate(pts_2pt_mr = round(pct_pts_2pt_mr*pts,0),
               ast_2pm = round(pct_ast_2pm*(fgm-fg3m),0),
               ast_3pm = round(pct_ast_3pm*fg3m,0),
               opp_pts_2pt_mr = round(opp_pct_pts_2pt_mr*opp_pts,0),
               opp_ast_2pm = round(opp_pct_ast_2pm*(opp_fgm-opp_fg3m),0),
               opp_ast_3pm = round(opp_pct_ast_3pm*opp_fg3m,0)
        ) %>%
        group_by(season_year, team_id, location) %>%
        mutate(across(c(fgm:opp_pct_uast_fgm),
                      ~ if (min_games > 15) {
                          pracma::movavg(.x, n = 15, type = 'e')
                      } else {
                          pracma::movavg(.x, n = min_games-1, type = 'e')
                      })
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
    
    return(nba_final)
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
        file_name <- sub("_2019_2021.*", "", tools::file_path_sans_ext(basename(file)))
        # Read the RDS file and assign to the list using the extracted file_name as the list element name
        loaded_data[[file_name]] <- readRDS(file)
    }
    
    # Create separate lists based on names
    win_models <- loaded_data[grep("win", names(loaded_data), ignore.case = TRUE)]
    team_models <- loaded_data[grep("team", names(loaded_data), ignore.case = TRUE)]
    opp_models <- loaded_data[grep("opp", names(loaded_data), ignore.case = TRUE)]
    pre_proc_val <- loaded_data[["pre_proc_val"]]
    
    # Return a list containing the three sublists
    return(list(win_models = win_models,
                team_models = team_models,
                opp_models = opp_models,
                pre_proc_val = pre_proc_val))
}

# function to make predictions
make_predictions <- function(model_type, model_list, input_data, output_col_suffix) {
    cols <- character(0)
    
    for (i in seq_along(model_list[[model_type]])) {
        model <- model_list[[model_type]][[i]]
        
        if (model_type == "win_models") {
            pred <- predict(model, input_data, type = "prob")[1]
        } else {
            pred <- as.data.frame(predict(model, input_data))
        }
        
        new_cols <- paste0(names(model_list[[model_type]][i]), output_col_suffix)
        colnames(pred) <- new_cols
        cols <- c(cols, pred)
    }
    
    return(cols)
}



#### schedule and odds ----
# get current schedule from database
nba_schedule_current <- tbl(dbConnect(SQLite(), "../nba_sql_db/nba_db"),
                            "nba_schedule_current") %>%
    collect() %>%
    mutate(game_date = as_date(game_date, origin ="1970-01-01"))

# get current odds
book_name <- "BetMGM"
dates <- "2023-11-24"
today_odds <- get_odds(book_name, dates)

# today's slate
slate_today <- nba_schedule_current %>%
    filter(game_date %in% c(dates)) %>%
    left_join(today_odds)


#### prepare ytd data ----
# mamba cleanR
mamba_clean <- mamba_nba_cleanR(2024)

# filter for today's away teams
mamba_away <- mamba_clean %>%
    filter(location == "away" & team_name %in% slate_today$away_team_name) %>%
    select(-c(season_year, location)) %>%
    rename_with(~paste0("away_", .), -team_name) %>%
    group_by(team_name) %>%
    slice(tail(row_number(), 1))

# filter for today's home teams
mamba_home <- mamba_clean %>%
    filter(location == "home" & team_name %in% slate_today$home_team_name) %>%
    select(-c(season_year, location)) %>%
    rename_with(~paste0("home_", .), -team_name) %>%
    group_by(team_name) %>%
    slice(tail(row_number(), 1))


#### prepare for predictions ----
directory_path <- "../NBAdb/models/models_trained/"
model_list <- invisible(load_model_rds_files(directory_path))

# mamba slate for predictions
slate_mamba <- slate_today %>%
    left_join(mamba_away, by = c("away_team_name" = "team_name")) %>%
    left_join(mamba_home, by = c("home_team_name" = "team_name")) %>%
    select(away_team_name, all_of(model_list$pre_proc_val$method$center))

mamba_input <- predict(model_list$pre_proc_val,  slate_mamba[-1])


#### make predictions ----
# initialize slate_final
slate_final <- slate_today %>% select(-c(is_b2b_first:opp_is_b2b_second))

# loop through models
for (model_type in c("win_models", "team_models", "opp_models")) {
    col_suffix <- ifelse(model_type == "win_models", "_away", "_score")
    slate_final <- bind_cols(slate_final,
                             make_predictions(model_type, model_list,
                                              mamba_input, col_suffix))
}

moneyline_key <- 0.05781939
spread_key <- 4.30365229
totals_key <- 0

# add ensemble
bets_final <- slate_final %>%
    mutate(ens_win_away = rowMeans(select(.,log_win_away,reg_win_away,
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
                                   if_else(abs(reg_win_edge_away) >= moneyline_key &
                                               reg_win_edge_away < 0,
                                           paste0(home_team_name, " ", home_moneyline), NA), NA),
           spread_bet = if_else(abs(ens_spread_edge_away) >= spread_key &
                                    ens_spread_edge_away > 0,
                                paste0(away_team_name," ", away_spread),
                                if_else(abs(ens_spread_edge_away) >= spread_key &
                                            ens_spread_edge_away < 0,
                                        paste0(home_team_name, " ", home_spread), NA), NA)) %>%
    select(game_date:over_under, reg_win_away, ens_team_score, ens_opp_score,
           reg_win_edge_away, ens_spread_edge_away, moneyline_bet, spread_bet) %>%
    filter(!is.na(moneyline_bet) | !is.na(spread_bet)) %>%
    select(game_date:home_team_name, moneyline_bet, spread_bet)






















