library(tidyverse)

load("/Users/jesse/Desktop/data_frames.rda")



# Function to remove duplicate columns across categories within each season
remove_duplicate_columns <- function(player_df_v2) {
    # Process each season using map
    processed_seasons <- imap(player_df_v2, function(season_data, season_name) {
        # Initialize seen columns vector
        seen_columns <- character(0)
        
        # Process each category using map
        processed_categories <- imap(season_data, function(category_df, category_name) {
            current_cols <- colnames(category_df)
            
            # Keep only columns we haven't seen before
            unique_cols <- current_cols[!(current_cols %in% seen_columns)]
            # Update seen columns (using <<- to modify the parent environment's variable)
            seen_columns <<- c(seen_columns, unique_cols)
            
            # Return dataframe with only unique columns
            category_df[, unique_cols, drop = FALSE]
        })
        
        return(processed_categories)
    })
    
    return(processed_seasons)
}

# Helper function to print columns by category for a specific season
print_columns_by_category <- function(data, season) {
    if (season %in% names(data)) {
        cat(sprintf("\nColumns for %s:\n", season))
        iwalk(data[[season]], function(category_df, category_name) {
            cat(sprintf("\n%s columns:\n", category_name))
            print(colnames(category_df))
        })
    } else {
        cat("Season not found in data\n")
    }
}

# Usage:
processed_data <- remove_duplicate_columns(player_df_v2)
print_columns_by_category(processed_data, "season_2024")

# Use map to combine all data frames for each season
combined_list <- map(processed_data, bind_cols)

# Combine all seasons into a single data frame
combined_df <- bind_rows(combined_list, .id = "season")



player_tbl <- combined_df %>%
    mutate(
        location = if_else(grepl("@", matchup) == T, "away", "home"),
        game_date = as_date(game_date),
        season_year = as.numeric(substr(season_year, 1, 4)) + 1
    ) %>%
    arrange(game_date, game_id, location) %>%
    select(season_year:matchup, location, wl:ncol(.)) %>%
    left_join(
        combined_df %>%
            arrange(game_date, game_id) %>%
            distinct(season_year, game_id, game_date, player_id) %>%
            group_by(season_year, player_id) %>%
            mutate(
                game_date = as_date(game_date),
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
            mutate(across(where(is.logical), ~ coalesce(., FALSE))) %>%
            select(game_id, player_id, is_b2b_first, is_b2b_second, game_count_season)
    ) %>%
    select(
        season_year:wl, is_b2b_first, is_b2b_second, game_count_season,
        min:ncol(.)
    )

player_tbl_filtered <- player_tbl %>%
    mutate(
        pts_2pt_mr = round(pct_pts_2pt_mr*pts,0),
        ast_2pm = round(pct_ast_2pm*(fgm-fg3m),0),
        ast_3pm = round(pct_ast_3pm*fg3m,0)
    ) %>%
    select(-c(
        fg_pct, fg3_pct, ft_pct, dd2, td3,
        min_sec:pie,
        fgm_pg:pct_pts,
        opp_pts_off_tov:opp_pts_paint,
        pct_fga_2pt:pct_uast_fgm
    ))



# 0.5*((fga + 0.4*fta – 1.07*(oreb/(oreb + opp_dreb))*(fga – fgm) + tov) + (opp_fga + 0.4*(opp_fta) – 1.07*(opp_oreb)/(opp_oreb + dreb))*(opp_fga – opp_fgm) + opp_tov))









