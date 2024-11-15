library(tidyverse)
library(janitor)

#### load database ----
# NBAdb <- DBI::dbConnect(RSQLite::SQLite(), "../nba_sql_db/nba_db")

## query db
# pbp <- tbl(NBAdb, "play_by_play") %>%
#     collect() %>%
#     filter(game_id %in% c(
#         "0021800001", "0021800002", "0021800003", "0021800004"
#     ))

pbp <- readRDS("./pbp_df.rds")

#### MAMBA Possessions ----
# eventmsgtype 1 = made shot *
# eventmsgtype 2 = missed shot *
# eventmsgtype 3 & 10 = 1 of 1 common foul *
# eventmsgtype 3 & 11 = 1 of 2 common foul
# eventmsgtype 3 & 12 = 2 of 2 common foul *
# eventmsgtype 3 & 13 = 1 of 3 common foul
# eventmsgtype 3 & 14 = 2 of 3 common foul
# eventmsgtype 3 & 15 = 3 of 3 common foul *
# eventmsgtype 3 & 16 = 1 of 1 tech
# eventmsgtype 3 & 18 = 1 of 2 flagrant
# eventmsgtype 3 & 19 = 2 of 2 flagrant *
# eventmsgtype 3 & 20 = 1 of 1 flagrant *
# eventmsgtype 3 & 21 = 1 of 2 tech
# eventmsgtype 3 & 22 = 2 of 2 tech
# eventmsgtype 3 & 25 = 1 of 2 clear path
# eventmsgtype 3 & 26 = 2 of 2 clear path
# eventmsgtype 3 & 27 = 1 of 3 flagrant
# eventmsgtype 3 & 28 = 2 of 3 flagrant
# eventmsgtype 3 & 29 = 3 of 3 flagrant *
# eventmsgtype 4 & 0 = rebound off fg
# eventmsgtype 4 & 1 = rebound off ft
# eventmsgtype 5 = turnover *
# eventmsgtype 6 = foul
# eventmsgtype 7 = 
# eventmsgtype 8 = sub
# eventmsgtype 9 = timeout

# Vectorized convert_to_seconds
convert_to_seconds <- function(time_str) {
    time_parts <- do.call(rbind, strsplit(time_str, ":"))
    minutes <- as.numeric(time_parts[, 1])
    seconds <- as.numeric(time_parts[, 2])
    
    # Total seconds remaining
    return((minutes * 60) + seconds)
}

# Vectorized seconds_passed
seconds_passed <- function(time_str, period) {
    period_seconds <- 720
    time_remaining <- convert_to_seconds(time_str)
    secs_passed_current_period <- ifelse(period %in% 1:4, period_seconds - time_remaining, 300 - time_remaining)
    
    # Calculate seconds passed in previous periods
    seconds_in_previous_periods <- (as.numeric(period) - 1) * period_seconds
    
    # Total seconds passed in the game
    return(seconds_in_previous_periods + secs_passed_current_period)
}

#### pbp code starts here ----
new_pbp <- pbp %>% # mine
    distinct() %>%
    filter(eventmsgtype != 18) %>%
    mutate(
        season_year = case_when(
            substr(game_id, 1, 5) == "00218" ~ 2019,
            substr(game_id, 1, 5) == "00219" ~ 2020,
            substr(game_id, 1, 5) == "00220" ~ 2021,
            substr(game_id, 1, 5) == "00221" ~ 2022,
            substr(game_id, 1, 5) == "00222" ~ 2023,
            TRUE ~ NA_integer_
        ),
        team_location = case_when(
            person1type == "4" ~ "home", # home player
            person1type == "5" ~ "away", # away player
            person1type == "2" ~ "home", # home team
            person1type == "3" ~ "away", # away team
            TRUE ~ NA_character_
        ),
        secs_left_quarter = convert_to_seconds(pctimestring),
        secs_passed_quarter = if_else(period %in% 1:4, 720 - secs_left_quarter, 300 - secs_left_quarter),
        secs_passed_game = seconds_passed(pctimestring, period),
        possession = case_when(
            eventmsgtype %in% c(1, 2, 5) ~ 1,
            eventmsgtype == 3 & eventmsgactiontype %in% c(10, 12, 15, 19, 20, 29) ~ 1,
            TRUE ~ 0),
        shot_pts_home = case_when(
            eventmsgtype == 3 & !str_detect(homedescription, "MISS") ~ 1,                               
            eventmsgtype == 1 & str_detect(homedescription, "3PT") ~ 3,                                 
            eventmsgtype == 1 & !str_detect(homedescription, "3PT") ~ 2,
            TRUE ~ 0),
        shot_pts_away = case_when(
            eventmsgtype == 3 & !str_detect(visitordescription, "MISS") ~ 1,
            eventmsgtype == 1 & str_detect(visitordescription, "3PT") ~ 3,
            eventmsgtype == 1 & !str_detect(visitordescription, "3PT") ~ 2,
            TRUE ~ 0)
    ) %>%
    group_by(game_id) %>%
    mutate(
        pts_home = cumsum(shot_pts_home),
        pts_away = cumsum(shot_pts_away),
        event_index = row_number(),
    ) %>%
    ungroup() %>%
    arrange(game_id, secs_passed_game) %>%
    select(
        -c(ends_with("_city"), ends_with("_nickname"),
           score, scoremargin, video_available_flag)
    )

subs_made <- new_pbp %>% # players subbed in quarter
    filter(eventmsgtype == 8) %>%
    select(
        game_id, period, secs_passed_game, team_location,
        team_player = player1_team_abbreviation,
        player_out = player1_name,
        player_in = player2_name
    ) %>%
    pivot_longer(
        cols = starts_with("player_"),
        names_to = "sub",
        names_prefix = "player_",
        values_to = "name_player"
    ) %>%
    group_by(game_id, period, team_player, name_player) %>%
    filter(row_number() == 1) %>%
    ungroup()

others_qtr <- new_pbp %>% # finds players not subbed out in a quarter
    filter(eventmsgtype != 8,
           !(eventmsgtype == 6 & eventmsgactiontype %in% c(10, 11, 16, 18, 25)),
           !(eventmsgtype == 11 & eventmsgactiontype %in% c(1, 4))) %>%
    pivot_longer(
        cols = c(player1_name, player2_name, player3_name),
        names_to = "player_number",
        names_prefix = "name_player",
        values_to = "name_player"
    ) %>%
    mutate(
        team_player = case_when(
            player_number == "player1_name" ~ player1_team_abbreviation,
            player_number == "player2_name" ~ player2_team_abbreviation,
            player_number == "player3_name" ~ player3_team_abbreviation,
            TRUE ~ NA_character_
        )
    ) %>%
    filter(!is.na(name_player), !is.na(team_player)) %>%
    anti_join(
        subs_made %>%
            select(game_id, period, team_player, name_player),
        by = c("game_id", "period", "team_player", "name_player")
    ) %>%
    distinct(game_id, period, name_player, team_player) %>%
    left_join(
        subs_made %>% select(game_id, team_player, team_location) %>% distinct(),
        by = c("game_id", "team_player")
    )


lineups_quarters <- subs_made %>% # lineups to start quarter
    filter(sub == "out") %>%
    select(game_id, period, team_player, name_player, team_location) %>%
    union_all(others_qtr %>%
                  select(game_id, period, team_player, name_player, team_location)) %>%
    arrange(game_id, period, team_player)

lineups_errors <- lineups_quarters %>%
    count(game_id, period, team_player) %>%
    filter(n != 5)

lineups_quarters <- missing_starters %>%
    mutate(period = as.character(period)) %>%
    left_join(
        subs_made %>% select(game_id, team_player, team_location) %>% distinct(),
        by = c("game_id", "team_player")
    ) %>%
    bind_rows(lineups_quarters) %>%
    filter(!(game_id == "0022200140" & name_player == "Dennis Schroder" & period == 4))

lineup_subs <- new_pbp %>%
    filter(eventmsgtype == 8) %>%
    select(
        game_id, period, event_index, team_location,
        team_player = player1_team_abbreviation,
        player_out = player1_name,
        player_in = player2_name
    ) %>%
    left_join(
        lineups_quarters %>%
            group_by(game_id, period, team_player) %>%
            summarise(lineup_initial = paste(sort(unique(name_player)), collapse = ", "), .groups = "drop"),
        by = c("game_id", "period", "team_player")
    ) %>%
    mutate(lineup_initial = str_split(lineup_initial, ", ")) %>%
    group_by(game_id, period, team_player) %>%
    mutate(
        lineup_after = accumulate2(
            .x = player_in,
            .y = player_out,
            .f = ~ setdiff(c(..1, ..2), ..3),
            .init = lineup_initial[[1]]
        )[-1],
        lineup_before = coalesce(lineup_initial, lag(lineup_after))
    ) %>%
    ungroup() %>%
    mutate(across(c(lineup_initial, lineup_after), ~ map_chr(.x, ~ paste(.x, collapse = ", ")))) %>%
    mutate(
        lineup_before_home = ifelse(team_location == "home", lineup_initial, NA),
        lineup_after_home = ifelse(team_location == "home", lineup_after, NA),
        lineup_before_away = ifelse(team_location == "away", lineup_initial, NA),
        lineup_after_away = ifelse(team_location == "away", lineup_after, NA)
    ) %>%
    select(game_id, event_index, contains("home"), contains("away"))

lineup_game <- new_pbp %>%
    left_join(
        lineups_quarters %>%
            group_by(game_id, period, team_location) %>%
            summarize(lineup_initial = paste(sort(unique(name_player)), collapse = ", "), .groups = "drop") %>%
            pivot_wider(
                names_from = team_location,
                names_prefix = "lineup_initial_",
                values_from = lineup_initial
            ) %>%
            mutate(eventmsgtype = "12"),
        by = c("game_id", "period", "eventmsgtype"),
    ) %>%
    left_join(
        lineup_subs,
        by = c("game_id", "event_index")
    ) %>%
    mutate(
        lineup_before_home = coalesce(lineup_before_home, lineup_initial_home),
        lineup_after_home = coalesce(lineup_after_home, lineup_initial_home),
        lineup_before_away = coalesce(lineup_before_away, lineup_initial_away),
        lineup_after_away = coalesce(lineup_after_away, lineup_initial_away)
    ) %>%
    select(-starts_with("lineup_initial"))

# filter out timeouts and only keep last lineup -- get rid of consecutive lineups
lineup_game <- lineup_game %>%
    group_by(eventmsgtype != 9) %>%
    mutate(across(lineup_before_home:lineup_after_away, ~ {
        last_na <- lead(is.na(.), default = TRUE)
        replace(., !last_na & !is.na(.), NA)
    })) %>%
    ungroup() %>%
    select(-"eventmsgtype != 9")

lineup_game <- lineup_game %>%
    group_by(game_id, period) %>%
    mutate(
        lineup_home = zoo::na.locf(lineup_after_home, na.rm = FALSE),
        lineup_away = zoo::na.locf(lineup_after_away, na.rm = FALSE),
        lineup_home = coalesce(lineup_home, zoo::na.locf(lineup_before_home, fromLast = TRUE, na.rm = FALSE)),
        lineup_away = coalesce(lineup_away, zoo::na.locf(lineup_before_away, fromLast = TRUE, na.rm = FALSE)),
        lineup_home = str_split(lineup_home, ", ") %>% map_chr(~ paste(sort(.), collapse = ", ")),
        lineup_away = str_split(lineup_away, ", ") %>% map_chr(~ paste(sort(.), collapse = ", "))
    ) %>%
    ungroup() %>%
    select(-starts_with("lineup_before"), -starts_with("lineup_after"))



game_events <- lineup_game %>%
    filter(game_id == "0021800128")



game_df <- lineup_game %>%
    filter(game_id == "0021800128") %>%
    mutate(
        fga = if_else(eventmsgtype %in% c(1, 2), player1_name, NA_character_),
        fgm = if_else(eventmsgtype == 1, player1_name, NA_character_),
        fg3a = if_else(eventmsgtype %in% c(1, 2) & (str_detect(homedescription, "3PT") | str_detect(visitordescription, "3PT")), player1_name, NA_character_),
        fg3m = if_else(eventmsgtype == 1 & (str_detect(homedescription, "3PT") | str_detect(visitordescription, "3PT")), player1_name, NA_character_),
        fta = if_else(eventmsgtype == 3, player1_name, NA_character_),
        ftm = if_else(eventmsgtype == 3 & (!str_detect(homedescription, "MISS") | !str_detect(visitordescription, "MISS")), player1_name, NA_character_),
        oreb = if_else(eventmsgtype == 4 & player1_team_id == lag(player1_team_id), player1_name, NA_character_),
        dreb = if_else(eventmsgtype == 4 & player1_team_id != lag(player1_team_id), player1_name, NA_character_),
        reb = if_else(eventmsgtype == 4, player1_name, NA_character_),
        ast = if_else(eventmsgtype == 1, player2_name, NA_character_),
        stl = if_else(eventmsgtype == 5 & eventmsgactiontype %in% c(1, 2), player1_name, NA_character_),
        blk = if_else(eventmsgtype == 2, player3_name, NA_character_),
        tov = if_else(eventmsgtype == 5, player1_name, NA_character_),
        foul = if_else(eventmsgtype == 6, player1_name, NA_character_)
    ) %>%
    group_by(
        game_id,
        team_location,
        player_name = coalesce(player1_name, player2_name, player3_name),
        team_name = coalesce(player1_team_abbreviation, player2_team_abbreviation, player3_team_abbreviation)
    ) %>%
    summarize(
        fga = sum(!is.na(fga)),
        fgm = sum(!is.na(fgm)),
        fg3a = sum(!is.na(fg3a)),
        fg3m = sum(!is.na(fg3m)),
        fta = sum(!is.na(fta)),
        ftm = sum(!is.na(ftm)),
        oreb = sum(!is.na(oreb)),
        dreb = sum(!is.na(dreb)),
        reb = sum(!is.na(reb)),
        ast = sum(!is.na(ast)),
        stl = sum(!is.na(stl)),
        blk = sum(!is.na(blk)),
        tov = sum(!is.na(tov)),
        foul = sum(!is.na(foul)),
        .groups = "drop"
    ) %>%
    mutate(
        pts = (fgm-fg3m)*2 + fg3m*3 + ftm,
        fg_pct = round(fgm/fga, 3),
        fg3_pct = round(fg3m/fg3a, 3),
        ft_pct = round(ftm/fta, 3)
    ) %>%
    filter(!is.na(player_name)) %>%
    arrange(team_location)



lineup_minutes <- lineup_game %>%
    filter(game_id == "0021800128") %>%
    arrange(game_id, secs_passed_game) %>%
    mutate(
        sub_home = if_else(lineup_home == lead(lineup_home), FALSE, TRUE),
        sub_away = if_else(lineup_away == lead(lineup_away), FALSE, TRUE),
        next_home = lead(lineup_home),
        next_away = lead(lineup_away)
    ) %>%
    mutate(
        lineup_home = str_split(lineup_home, ", "),
        lineup_away = str_split(lineup_away, ", "),
        next_home = str_split(next_home, ", "),
        next_away = str_split(next_away, ", ")
    ) %>%
    mutate(
        exited_home = map2(lineup_home, next_home, ~ setdiff(.x, .y)),
        exited_away = map2(lineup_away, next_away, ~ setdiff(.x, .y)),
        entered_home = if_else(eventmsgtype == 12 & period == 1,
                               lineup_home,
                               map2(lineup_home, next_home, ~ setdiff(.y, .x))),
        entered_away = if_else(eventmsgtype == 12 & period == 1,
                               lineup_away,
                               map2(lineup_away, next_away, ~ setdiff(.y, .x)))
        
    )




convert_to_minutes_seconds <- function(time_numeric) {
    minutes <- floor(time_numeric)
    seconds <- round((time_numeric - minutes) * 60)
    sprintf("%d:%02d", minutes, seconds)
}

calculate_playtime <- function(data, lineup_col, secs_col) {
    last_secs <- tail(data[[secs_col]], 1)  # Get the last value of secs_passed_game
    last_lineup <- tail(data[[lineup_col]], 1)  # Get the last recorded lineup
    
    data %>%
        mutate(
            prev_lineup = lag(!!sym(lineup_col), default = ""),
            entered = map2(!!sym(lineup_col), prev_lineup, ~ setdiff(str_split(.x, ", ")[[1]], str_split(.y, ", ")[[1]])),
            exited = map2(prev_lineup, !!sym(lineup_col), ~ setdiff(str_split(.x, ", ")[[1]], str_split(.y, ", ")[[1]]))
        ) %>%
        select(secs_passed_game = !!sym(secs_col), entered, exited) %>%
        bind_rows(
            tibble(
                secs_passed_game = last_secs,
                entered = list(character()),  # No new players entering
                exited = list(str_split(last_lineup, ", ")[[1]])  # Players in the final lineup exiting
            )
        )
}

# Function to calculate entry and exit events
# calculate_playtime <- function(data, lineup_col, secs_col) {
#     data %>%
#         mutate(
#             prev_lineup = lag(!!sym(lineup_col), default = ""),
#             entered = map2(!!sym(lineup_col), prev_lineup, ~ setdiff(str_split(.x, ", ")[[1]], str_split(.y, ", ")[[1]])),
#             exited = map2(prev_lineup, !!sym(lineup_col), ~ setdiff(str_split(.x, ", ")[[1]], str_split(.y, ", ")[[1]]))
#         ) %>%
#         select(secs_passed_game = !!sym(secs_col), entered, exited)
# }

# Process home and away lineups
home_events <- calculate_playtime(lineup_minutes, "lineup_home", "secs_passed_game")
away_events <- calculate_playtime(lineup_minutes, "lineup_away", "secs_passed_game")

all_events <- bind_rows(
    home_events %>% mutate(team = "home"),
    away_events %>% mutate(team = "away")
) %>%
    pivot_longer(cols = c(entered, exited), names_to = "event_type", values_to = "player") %>%
    unnest(player) %>%  # Unnest the players column
    arrange(secs_passed_game)

# Track cumulative playtime for each player
player_playtime <- all_events %>%
    group_by(player) %>%
    mutate(
        time_on_court = ifelse(event_type == "exited",
                               secs_passed_game - lag(secs_passed_game, default = 0),
                               0)
    ) %>%
    filter(event_type == "exited") %>%  # Only consider exit events for playtime
    summarise(total_seconds = sum(time_on_court, na.rm = TRUE)) %>%
    mutate(total_minutes = total_seconds / 60,
           minutes_played = convert_to_minutes_seconds(total_minutes)) %>%
    arrange(desc(total_minutes)) %>%
    filter(total_seconds != 0)

# Display the results
view(player_playtime)














#### possessions code ----
# Initial possessions with optimized code
poss_initial <- pbp %>%
    distinct() %>%
    group_by(game_id) %>%
    mutate(
        home_team = nth(player1_team_id, 2),
        away_team = nth(player2_team_id, 2)
    ) %>%
    ungroup() %>%
    mutate(
        secs_left_quarter = convert_to_seconds(pctimestring),
        secs_passed_quarter = if_else(period %in% 1:4, 720 - secs_left_quarter, 300 - secs_left_quarter),
        secs_passed_game = seconds_passed(pctimestring, period),
        possession = case_when(
            eventmsgtype %in% c(1, 2, 5) ~ 1,
            eventmsgtype == 3 & eventmsgactiontype %in% c(10, 12, 15, 19, 20, 29) ~ 1,
            TRUE ~ 0
        )
    ) %>%
    ungroup()

# Change end of possession to ft of and one
fgs_and1 <- poss_initial %>%
    filter(
        eventmsgtype == 1 |
            (eventmsgtype == 6 & !eventmsgactiontype %in% c(4, 10, 11, 12, 16, 18)) |
            (eventmsgtype == 3 & eventmsgactiontype == 10)
    ) %>%
    group_by(game_id, secs_passed_game) %>%
    filter(
        eventmsgtype == 1 &
            lead(eventmsgtype) == 6 &
            player1_team_id != lead(player1_team_id)
    ) %>%
    ungroup() %>%
    mutate(possession = 0)

# Use left join for conditional updating of the possession column
poss_initial_2 <- poss_initial %>%
    left_join(fgs_and1 %>% select(game_id, eventnum, possession), by = c("game_id", "eventnum")) %>%
    mutate(possession = coalesce(possession.y, possession.x)) %>%
    select(-possession.y, -possession.x)

# Identify and change consecutive possessions
change_consec <- poss_initial_2 %>%
    distinct() %>%
    filter(
        possession == 1 |
            (eventmsgtype == 6 & eventmsgactiontype == 30)
    ) %>%
    group_by(game_id, period) %>%
    filter(
        possession == lead(possession) &
            player1_team_id == lead(player1_team_id) &
            !(eventmsgtype == 3 & eventmsgactiontype %in% c(19,20,29))
    ) %>%
    ungroup() %>%
    mutate(possession = 0)

poss_non_consec <- poss_initial_2 %>%
    left_join(change_consec %>% select(game_id, eventnum, possession), by = c("game_id", "eventnum")) %>%
    mutate(possession = coalesce(possession.y, possession.x)) %>%
    select(-possession.y, -possession.x)

# Find the start of the possession
start_possessions <- poss_non_consec %>%
    mutate(
        start_poss = case_when(possession == 1 & eventmsgtype != 4 ~ lag(secs_left_quarter)),
        poss_team = case_when(possession == 1 & player1_team_id == home_team ~ "home",
                              possession == 1 & player1_team_id == away_team ~ "away")
    ) %>%
    select(game_id:period, pctimestring, possession, start_poss, poss_team,
           homedescription:visitordescription,
           player1_name, player2_name, player3_name)




# ------------------
poss_grouped <- poss_non_consec %>%
    group_by(season_year, game_id, player1_team_id) %>%
    summarise(sum(possession))

game_poss <- start_possessions %>%
    filter(game_id == '0022201216' & player1_team_id == '1610612737' & period == 2) %>%
    mutate(poss_team = ifelse(player1_team_id == home_team, "home", "away"),
           total_poss = cumsum(possession))

game_poss_count <- start_possessions %>%
    filter(game_id == '0022201216' & period == 2) %>%
    group_by(player1_team_id) %>%
    summarise(sum(possession))



# ----------------
game_poss <- poss_non_consec %>%
    filter(game_id == "0021800149") %>%
    select(game_id:period, pctimestring, possession, player1_team_id, homedescription:visitordescription) %>%
    group_by(player1_team_id) %>%
    summarize(sum(possession))

game_poss_count <- poss_non_consec %>%
    filter(season_year == 2023) %>%
    select(game_id:period, pctimestring, possession, player1_team_id, homedescription:visitordescription) %>%
    group_by(player1_team_id) %>%
    summarize(sum(possession))

game_events <- poss_non_consec %>%
    group_by(eventmsgtype, eventmsgactiontype) %>%
    summarize(sum(possession))

game_events <- poss_non_consec %>%
    filter(eventmsgtype == 6 & eventmsgactiontype %in% c(30))







game_poss <- poss_non_consec %>%
    filter(game_id == "0022200060" & period == 1) %>%
    select(game_id:period, pctimestring, possession, player1_team_id, homedescription:visitordescription)


game_poss_count <- poss_non_consec %>%
    filter(season_year == 2023 & eventmsgtype == 3 & eventmsgactiontype == 19) %>%
    select(game_id:period, pctimestring, possession, player1_team_id, homedescription:visitordescription) %>%
    group_by(player1_team_id) %>%
    summarize(sum(possession))

game_poss_count <- poss_non_consec %>%
    filter(season_year == 2023 & eventmsgtype == 6 & !eventmsgactiontype %in% c(1)) %>%
    select(game_id:period, pctimestring, possession, player1_team_id, homedescription:visitordescription)


# https://squared2020.com/2017/07/10/analyzing-nba-possession-models/#:~:text=A%20possession%20is%20the%20number,in%20the%20number%20of%20Turnovers.
# https://www.pbpstats.com/totals/nba/team?Season=2022-23&SeasonType=Regular%2BSeason&StartType=All&GroupBy=Season
# all possessions - https://github.com/ramirobentes/NBA-in-R/blob/master/all_possessions
# https://github.com/ramirobentes/NBA-in-R/blob/04ef36bb6da424ee808209875534ad77198319f3/2016_17/pbp_lineups.R#L183
# pbp poss number - https://github.com/ramirobentes/NBA-in-R/blob/master/pbp_poss_number.R
# https://github.com/ramirobentes/NBA-in-R/blob/95915510edc107ee1bea0e81aa2798401d73736d/every%20poss.R#L22
# https://github.com/ramirobentes/NBA-in-R/blob/95915510edc107ee1bea0e81aa2798401d73736d/pbp%20steps.R#L246
# https://github.com/ramirobentes/NBA-in-R/blob/04ef36bb6da424ee808209875534ad77198319f3/hoopR%20pbp%20past%20seasons.R#L190
# https://github.com/JayKay-15/MambaModel/blob/main/code/model/win_prob.R


library(tidyverse)
library(lubridate)
library(zoo)
library(nbastatR)
library(future)
library(readxl)


team_logs <- nbastatR::game_logs(seasons = c(2019), result_types = "team")
source("https://raw.githubusercontent.com/ramirobentes/NBA-in-R/master/data%20add%20pbp.R")

games <- team_logs %>%
    mutate(slugTeamHome = ifelse(locationGame == "H", slugTeam, slugOpponent),
           slugTeamAway = ifelse(locationGame == "A", slugTeam, slugOpponent)) %>%
    distinct(idGame, slugTeamHome, slugTeamAway)

play_logs_all <- nbastatR::play_by_play_v2(game_ids = unique(0021800001:0021800011))

new_pbp <- play_logs_all %>%
    mutate(numberOriginal = numberEvent) %>%
    distinct(idGame, numberEvent, .keep_all = TRUE) %>%   # remove duplicate events
    mutate(secsLeftQuarter = (minuteRemainingQuarter * 60) + secondsRemainingQuarter) %>%
    mutate(secsStartQuarter = case_when(
        numberPeriod %in% c(1:5) ~ (numberPeriod - 1) * 720,
        TRUE ~ 2880 + (numberPeriod - 5) * 300
    )) %>%
    mutate(secsPassedQuarter = ifelse(numberPeriod %in% c(1:4), 720 - secsLeftQuarter, 300 - secsLeftQuarter),
           secsPassedGame = secsPassedQuarter + secsStartQuarter) %>%
    arrange(idGame, secsPassedGame) %>%
    filter(numberEventMessageType != 18) %>%     # instant replay
    group_by(idGame) %>%
    mutate(numberEvent = row_number()) %>%  # new numberEvent column with events in the right order
    ungroup() %>%
    select(idGame, numberOriginal, numberEventMessageType, numberEventActionType, namePlayer1, namePlayer2, namePlayer3,
           slugTeamPlayer1, slugTeamPlayer2,  slugTeamPlayer3, numberPeriod, timeQuarter, secsPassedGame,
           descriptionPlayHome, numberEvent, descriptionPlayVisitor, descriptionPlayNeutral) %>%
    mutate(shotPtsHome = case_when(
        numberEventMessageType == 3 & !str_detect(descriptionPlayHome, "MISS") ~ 1,
        numberEventMessageType == 1 & str_detect(descriptionPlayHome, "3PT") ~ 3,
        numberEventMessageType == 1 & !str_detect(descriptionPlayHome, "3PT") ~ 2,
        TRUE ~ 0
    )) %>%
    mutate(shotPtsAway = case_when(
        numberEventMessageType == 3 & !str_detect(descriptionPlayVisitor, "MISS") ~ 1,
        numberEventMessageType == 1 & str_detect(descriptionPlayVisitor, "3PT") ~ 3,
        numberEventMessageType == 1 & !str_detect(descriptionPlayVisitor, "3PT") ~ 2,
        TRUE ~ 0
    )) %>%
    group_by(idGame) %>%
    mutate(ptsHome = cumsum(shotPtsHome),
           ptsAway = cumsum(shotPtsAway)) %>%
    ungroup() %>%
    left_join(games %>%
                  select(idGame, slugTeamHome, slugTeamAway)) %>%
    select(idGame, numberOriginal, numberEventMessageType, numberEventActionType, slugTeamHome, slugTeamAway, slugTeamPlayer1,
           slugTeamPlayer2, slugTeamPlayer3, numberPeriod, timeQuarter, secsPassedGame, numberEvent, namePlayer1, namePlayer2,
           namePlayer3, descriptionPlayHome, descriptionPlayVisitor, ptsHome, ptsAway, shotPtsHome, shotPtsAway,
           descriptionPlayNeutral) %>%
    mutate(marginBeforeHome = ptsHome - ptsAway - shotPtsHome + shotPtsAway,
           marginBeforeAway = ptsAway - ptsHome - shotPtsAway + shotPtsHome,
           timeQuarter = str_pad(timeQuarter, width = 5, pad = 0)) %>%
    mutate(numberEventNew = numberEvent) %>%
    mutate(numberEvent = numberEventNew) %>%
    select(-numberEventNew) %>%
    arrange(idGame, numberEvent)

new_pbp <- pbp %>% # mine
    distinct() %>%
    filter(eventmsgtype != 18) %>%
    group_by(game_id) %>%
    mutate(
        event_num = row_number(),
        home_team = nth(player1_team_abbreviation, 2),
        away_team = nth(player2_team_abbreviation, 2)
    ) %>%
    ungroup() %>%
    mutate(
        season_year = case_when(
            substr(game_id, 1, 5) == "00218" ~ 2019,
            substr(game_id, 1, 5) == "00219" ~ 2020,
            substr(game_id, 1, 5) == "00220" ~ 2021,
            substr(game_id, 1, 5) == "00221" ~ 2022,
            substr(game_id, 1, 5) == "00222" ~ 2023,
            TRUE ~ NA_integer_
        ),
        secs_left_quarter = convert_to_seconds(pctimestring),
        secs_passed_quarter = if_else(period %in% 1:4, 720 - secs_left_quarter, 300 - secs_left_quarter),
        secs_passed_game = seconds_passed(pctimestring, period),
        possession = case_when(
            eventmsgtype %in% c(1, 2, 5) ~ 1,
            eventmsgtype == 3 & eventmsgactiontype %in% c(10, 12, 15, 19, 20, 29) ~ 1,
            TRUE ~ 0),
        shot_pts_home = case_when(
            eventmsgtype == 3 & !str_detect(homedescription, "MISS") ~ 1,                               
            eventmsgtype == 1 & str_detect(homedescription, "3PT") ~ 3,                                 
            eventmsgtype == 1 & !str_detect(homedescription, "3PT") ~ 2,
            TRUE ~ 0),
        shot_pts_away = case_when(
            eventmsgtype == 3 & !str_detect(visitordescription, "MISS") ~ 1,
            eventmsgtype == 1 & str_detect(visitordescription, "3PT") ~ 3,
            eventmsgtype == 1 & !str_detect(visitordescription, "3PT") ~ 2,
            TRUE ~ 0)
    ) %>%
    arrange(game_id, secs_passed_game) %>%
    group_by(game_id) %>%
    mutate(
        pts_home = cumsum(shot_pts_home),
        pts_away = cumsum(shot_pts_away)
    ) %>%
    ungroup() %>%
    mutate(
        margin_before_home = pts_home - pts_away - shot_pts_home + shot_pts_away,
        margin_before_away = pts_away - pts_home - shot_pts_away + shot_pts_home,
    ) %>%
    arrange(game_id, event_num)

subs_made <- new_pbp2 %>%
    filter(numberEventMessageType == 8) %>%
    mutate(slugTeamLocation = ifelse(slugTeamPlayer1 == slugTeamHome, "Home", "Away")) %>%
    select(idGame, numberPeriod, timeQuarter, secsPassedGame, slugTeamPlayer = slugTeamPlayer1,
           slugTeamLocation, playerOut = namePlayer1, playerIn = namePlayer2) %>%
    pivot_longer(cols = starts_with("player"),
                 names_to = "inOut",
                 names_prefix = "player",
                 values_to = "namePlayer") %>%
    group_by(idGame, numberPeriod, slugTeamPlayer, namePlayer) %>%
    filter(row_number() == 1) %>%
    ungroup()

subs_made <- new_pbp %>% # mine
    filter(eventmsgtype == 8) %>%
    mutate(team_location = ifelse(player1_team_abbreviation == home_team, "home", "away")) %>%
    select(game_id, period, secs_passed_quarter, secs_passed_game, team_player = player1_team_abbreviation,
           team_location, player_out = player1_name, player_in = player2_name) %>%
    pivot_longer(cols = starts_with("player_"),
                 names_to = "in_out",
                 names_prefix = "player_",
                 values_to = "name_player") %>%
    group_by(game_id, period, team_player, name_player) %>%
    filter(row_number() == 1) %>%
    ungroup()

others_qtr <- new_pbp2 %>%
    filter(numberEventMessageType != 8) %>%                             
    filter(!(numberEventMessageType == 6 & numberEventActionType %in% c(10, 11, 16, 18, 25))) %>% 
    filter(!(numberEventMessageType == 11 & numberEventActionType %in% c(1, 4))) %>%
    pivot_longer(cols = starts_with("namePlayer"),
                 names_to = "playerNumber",
                 names_prefix = "namePlayer",
                 values_to = "namePlayer") %>%
    mutate(slugTeamPlayer = case_when(playerNumber == 1 ~ slugTeamPlayer1,
                                      playerNumber == 2 ~ slugTeamPlayer2,
                                      playerNumber == 3 ~ slugTeamPlayer3,
                                      TRUE ~ "None")) %>%
    mutate(slugTeamLocation = ifelse(slugTeamPlayer == slugTeamHome, "Home", "Away")) %>%
    filter(!is.na(namePlayer),
           !is.na(slugTeamPlayer)) %>%
    anti_join(subs_made %>%
                  select(idGame, numberPeriod, slugTeamPlayer, namePlayer)) %>%    # remove players that were subbed in the quarter
    distinct(idGame, numberPeriod, namePlayer, slugTeamPlayer, slugTeamLocation)

others_qtr <- new_pbp %>% # mine
    filter(eventmsgtype != 8) %>%                             
    filter(!(eventmsgtype == 6 & eventmsgactiontype %in% c(10, 11, 16, 18, 25))) %>% 
    filter(!(eventmsgtype == 11 & eventmsgactiontype %in% c(1, 4))) %>%
    pivot_longer(cols = c(player1_name, player2_name, player3_name),
                 names_to = "player_number",
                 names_prefix = "name_player",
                 values_to = "name_player") %>%
    mutate(team_player = case_when(player_number == "player1_name" ~ player1_team_abbreviation,
                                   player_number == "player2_name" ~ player2_team_abbreviation,
                                   player_number == "player3_name" ~ player3_team_abbreviation,
                                   TRUE ~ "none")) %>%
    mutate(team_location = ifelse(team_player == home_team, "home", "away")) %>%
    filter(!is.na(name_player),
           !is.na(team_player)) %>%
    anti_join(subs_made %>%
                  select(game_id, period, team_player, name_player)) %>%
    distinct(game_id, period, name_player, team_player, team_location)

lineups_quarters <- subs_made2 %>%
    filter(inOut == "Out") %>%
    select(idGame, numberPeriod, slugTeamPlayer, namePlayer, slugTeamLocation) %>%
    bind_rows(others_qtr) %>%
    arrange(idGame, numberPeriod, slugTeamPlayer)

lineups_quarters <- subs_made %>% # mine
    filter(in_out == "out") %>%
    select(game_id, period, team_player, name_player, team_location) %>%
    bind_rows(others_qtr) %>%
    arrange(game_id, period, team_player)

lineups_quarters %>%
    count(idGame, numberPeriod, slugTeamPlayer) %>%
    filter(n != 5)

lineups_errors <- lineups_quarters %>% # mine
    count(game_id, period, team_player) %>%
    filter(n != 5)

missing_players_ot <- data_missing_players %>%
    left_join(games %>%
                  select(idGame, slugTeamHome, slugTeamAway)) %>%
    mutate(slugTeamLocation = ifelse(slugTeamHome == slugTeamPlayer, "Home", "Away")) %>%
    select(-c(slugTeamHome, slugTeamAway))

lineups_quarters <- lineups_quarters %>%
    bind_rows(missing_players_ot) %>%
    arrange(idGame, numberPeriod, slugTeamPlayer)

lineup_subs <- new_pbp %>%
    filter(numberEventMessageType == 8) %>%
    select(idGame, numberPeriod, timeQuarter, secsPassedGame, slugTeamPlayer = slugTeamPlayer1, playerOut = namePlayer1, 
           playerIn = namePlayer2, numberEvent) %>%
    arrange(idGame, numberEvent) %>%
    group_by(idGame, numberPeriod, slugTeamPlayer) %>%
    mutate(row1 = row_number()) %>%
    ungroup() %>%
    left_join(lineups_quarters %>%
                  group_by(idGame, numberPeriod, slugTeamPlayer) %>%
                  summarise(lineupBefore = paste(sort(unique(namePlayer)), collapse = ", ")) %>%
                  ungroup() %>%
                  mutate(row1 = 1)) %>%
    select(-row1)

lineup_subs <- new_pbp %>% # mine
    filter(eventmsgtype == 8) %>%
    select(game_id, period, secs_passed_quarter, secs_passed_game, team_player = player1_team_abbreviation,
           player_out = player1_name, player_in = player2_name, event_num) %>%
    arrange(game_id, event_num) %>%
    group_by(game_id, period, team_player) %>%
    mutate(row1 = row_number()) %>%
    ungroup() %>%
    left_join(lineups_quarters %>%
                  group_by(game_id, period, team_player) %>%
                  summarise(lineup_before = paste(sort(unique(name_player)), collapse = ", ")) %>%
                  ungroup() %>%
                  mutate(row1 = 1)) %>%
    select(-row1)

lineup_subs <- lineup_subs %>%
    mutate(lineupBefore = str_split(lineupBefore, ", ")) %>% 
    arrange(idGame, numberEvent) %>%
    group_by(idGame, numberPeriod, slugTeamPlayer) %>%
    mutate(lineupAfter = accumulate2(playerIn, playerOut, ~setdiff(c(..1, ..2), ..3), .init = lineupBefore[[1]])[-1],
           lineupBefore = coalesce(lineupBefore, lag(lineupAfter))) %>%
    ungroup() %>% 
    mutate_all(~map_chr(., ~paste(.x, collapse = ", "))) %>%
    mutate_at(vars("numberEvent", "numberPeriod", "idGame"), ~ as.integer(.)) %>%
    mutate(secsPassedGame = as.numeric(secsPassedGame)) %>%
    arrange(idGame, numberEvent) %>%
    left_join(lineups_quarters %>%
                  distinct(idGame, slugTeamPlayer, slugTeamLocation)) %>%
    filter(!is.na(slugTeamLocation))


lineup_subs <- lineup_subs %>% # mine
    mutate(lineup_before = str_split(lineup_before, ", ")) %>% 
    arrange(game_id, event_num) %>%
    group_by(game_id, period, team_player) %>%
    mutate(lineup_after = accumulate2(player_in, player_out, ~setdiff(c(..1, ..2), ..3), .init = lineup_before[[1]])[-1],
           lineup_before = coalesce(lineup_before, lag(lineup_after))) %>%
    ungroup() %>% 
    mutate_all(~map_chr(., ~paste(.x, collapse = ", "))) %>%
    # mutate_at(vars("event_num", "period", "game_id"), ~ as.integer(.)) %>%
    # mutate(secs_passed_game = as.numeric(secs_passed_game)) %>%
    arrange(game_id, event_num) %>%
    left_join(lineups_quarters %>%
                  distinct(game_id, team_player, team_location)) %>%
    filter(is.na(team_location))

lineup_game <- new_pbp %>%
    group_by(idGame, numberPeriod) %>%
    mutate(row1 = row_number()) %>%
    ungroup() %>%
    left_join(lineups_quarters %>%
                  group_by(idGame, numberPeriod, slugTeamLocation) %>%
                  summarise(lineupBefore = paste(sort(unique(namePlayer)), collapse = ", ")) %>%
                  ungroup() %>%
                  pivot_wider(names_from = slugTeamLocation,
                              names_prefix = "lineupInitial",
                              values_from = lineupBefore) %>%
                  mutate(row1 = 1)) %>%
    select(-row1) %>%
    left_join(lineup_subs %>%
                  mutate(lineupBeforeHome = ifelse(slugTeamLocation == "Home", lineupBefore, NA),
                         lineupAfterHome = ifelse(slugTeamLocation == "Home", lineupAfter, NA),
                         lineupBeforeAway = ifelse(slugTeamLocation == "Away", lineupBefore, NA),
                         lineupAfterAway = ifelse(slugTeamLocation == "Away", lineupAfter, NA)) %>%
                  select(idGame, numberPeriod, timeQuarter, secsPassedGame, numberEvent, slugTeamPlayer1 = slugTeamPlayer,
                         contains("Home"), contains("Away"))) %>%
    mutate_at(vars(c(lineupBeforeHome, lineupAfterHome)), ~ ifelse(!is.na(lineupInitialHome), lineupInitialHome, .)) %>%
    mutate_at(vars(c(lineupBeforeAway, lineupAfterAway)), ~ ifelse(!is.na(lineupInitialAway), lineupInitialAway, .)) %>%
    select(-starts_with("lineupInitial"))

lineup_game <- new_pbp %>% # mine
    group_by(game_id, period) %>%
    mutate(row1 = row_number(),
           event_num = as.character(event_num),
           secs_passed_quarter = as.character(secs_passed_quarter),
           secs_passed_game = as.character(secs_passed_game)) %>%
    ungroup() %>%
    left_join(lineups_quarters %>%
                  group_by(game_id, period, team_location) %>%
                  summarise(lineup_before = paste(sort(unique(name_player)), collapse = ", ")) %>%
                  ungroup() %>%
                  pivot_wider(names_from = team_location,
                              names_prefix = "lineup_initial_",
                              values_from = lineup_before) %>%
                  mutate(row1 = 1)) %>%
    select(-row1) %>%
    left_join(lineup_subs %>%
                  mutate(lineup_before_home = ifelse(team_location == "home", lineup_before, NA),
                         lineup_after_home = ifelse(team_location == "home", lineup_after, NA),
                         lineup_before_away = ifelse(team_location == "away", lineup_before, NA),
                         lineup_after_away = ifelse(team_location == "away", lineup_after, NA)) %>%
                  select(game_id, period, secs_passed_quarter, secs_passed_game, event_num, team_player1 = team_player,
                         contains("home"), contains("away"))) %>%
    mutate_at(vars(c(lineup_before_home, lineup_after_home)), ~ ifelse(!is.na(lineup_initial_home), lineup_initial_home, .)) %>%
    mutate_at(vars(c(lineup_before_away, lineup_after_away)), ~ ifelse(!is.na(lineup_initial_away), lineup_initial_away, .)) %>%
    select(-starts_with("lineup_initial"))

lineup_game <- lineup_game %>%
    group_by(idGame, numberPeriod) %>%
    mutate(lineupHome = na.locf(lineupAfterHome, na.rm = FALSE),
           lineupAway = na.locf(lineupAfterAway, na.rm = FALSE),
           lineupHome = ifelse(is.na(lineupHome), na.locf(lineupBeforeHome, fromLast = TRUE, na.rm = FALSE), lineupHome),
           lineupAway = ifelse(is.na(lineupAway), na.locf(lineupBeforeAway, fromLast = TRUE, na.rm = FALSE), lineupAway),
           lineupHome = str_split(lineupHome, ", "),
           lineupAway = str_split(lineupAway, ", "),
           lineupHome = map_chr(lineupHome, ~ paste(sort(.), collapse = ", ")),
           lineupAway = map_chr(lineupAway, ~ paste(sort(.), collapse = ", "))) %>%
    ungroup() %>%
    select(-c(starts_with("lineupBefore"), starts_with("lineupAfter")))

lineup_game <- lineup_game %>% # mine
    group_by(game_id, period) %>%
    mutate(lineup_home = zoo::na.locf(lineup_after_home, na.rm = FALSE),
           lineup_away = zoo::na.locf(lineup_after_away, na.rm = FALSE),
           lineup_home = ifelse(is.na(lineup_home), zoo::na.locf(lineup_before_home, fromLast = TRUE, na.rm = FALSE), lineup_home),
           lineup_away = ifelse(is.na(lineup_away), zoo::na.locf(lineup_before_away, fromLast = TRUE, na.rm = FALSE), lineup_away),
           lineup_home = str_split(lineup_home, ", "),
           lineup_away = str_split(lineup_away, ", "),
           lineup_home = map_chr(lineup_home, ~ paste(sort(.), collapse = ", ")),
           lineup_away = map_chr(lineup_away, ~ paste(sort(.), collapse = ", "))) %>%
    ungroup() %>%
    select(-c(starts_with("lineup_before"), starts_with("lineup_after")))

lineup_game_stats <- lineup_game %>%
    mutate(canSub = case_when(numberEventMessageType == 5 & !numberEventActionType %in% c(1, 2) ~ 1,    # dead ball turnovers
                              numberEventMessageType == 6 & numberEventActionType != 16 ~ 1,            # fouls
                              numberEventMessageType == 11 & numberEventActionType != 4 ~ 1,
                              numberEventMessageType == 7 & numberEventActionType == 5 ~ 1,             # kickballs
                              numberEventMessageType == 4 & numberEventActionType == 0 & !str_detect(str_to_upper(descriptionPlayHome), "OFF:") ~ 1,
                              numberEventMessageType == 4 & numberEventActionType == 0 & !str_detect(str_to_upper(descriptionPlayVisitor), "OFF:") ~ 1,
                              TRUE ~ 0)) %>%
    mutate(secsPassedGame2 = ifelse(timeQuarter == "12:00" &
                                        (str_detect(str_to_lower(descriptionPlayHome), "technical") |
                                             str_detect(str_to_lower(descriptionPlayVisitor), "technical")),
                                    secsPassedGame + 0.005, secsPassedGame)) %>%    # Note 4
    mutate(secsPassedGame2 = ifelse(timeQuarter == "00:00" & numberEventMessageType == 3 & numberEventActionType != 10,
                                    secsPassedGame2 - 0.1,
                                    secsPassedGame2)) %>%
    group_by(idGame, numberPeriod, secsPassedGame) %>%
    mutate(numberNew = ifelse(numberEventMessageType == 3 & numberEventActionType == 12, 
                              paste(numberEvent[numberEventMessageType == 3 & numberEventActionType == 11], collapse = ", "), 
                              as.character(numberEvent)),
           numberNew = ifelse(numberEventMessageType == 3 & numberEventActionType %in% c(14, 15), 
                              paste(numberEvent[numberEventMessageType == 3 & numberEventActionType == 13], collapse = ", "),
                              numberNew)) %>%
    mutate(techs_and1 = sum(numberEventMessageType == 3 & numberEventActionType == 16) > 0 &
               sum(numberEventMessageType == 3 & numberEventActionType == 10) > 0 & 
               sum(numberEventMessageType == 8) > 0) %>%
    mutate(numberNew = ifelse(numberEventMessageType == 3 & numberEventActionType == 10 & techs_and1, 
                              paste(numberEvent[numberEventMessageType == 6 & numberEventActionType == 2 & techs_and1], collapse = ", "), 
                              as.character(numberNew))) %>%
    mutate(numberNew = str_split(numberNew, ", "),
           numberNew = map(numberNew, ~as.numeric(.)),
           numberNew = map2_dbl(numberNew, numberEvent, ~ max(.x[.x <= .y]))) %>%
    ungroup() %>%
    arrange(idGame, numberNew, numberEvent) %>%
    group_by(idGame) %>%
    mutate(newptsHome = cumsum(shotPtsHome),
           newptsAway = cumsum(shotPtsAway)) %>%
    group_by(idGame, numberPeriod, secsPassedGame2) %>%
    mutate(subOpp = cumsum(canSub)) %>%
    group_by(idGame = as.character(idGame), 
             numberPeriod = as.character(numberPeriod), 
             subOpp, 
             secsPassedGame2 = as.character(secsPassedGame2)) %>%
    mutate(hasFouls = sum(numberEventMessageType == 3)) %>%
    mutate(newptsHome = ifelse(hasFouls > 0,
                               newptsHome[row_number() == max(row_number()[numberEventMessageType == 3])],
                               newptsHome),
           newptsAway = ifelse(hasFouls > 0,
                               newptsAway[row_number() == max(row_number()[numberEventMessageType == 3])],
                               newptsAway)) %>%
    ungroup() %>%
    select(-c(secsPassedGame2, numberNew, techs_and1, hasFouls)) %>%
    mutate(across(starts_with("description"), ~ coalesce(., "")))

# Adding possession when fg attempts, ft 1 of 2 and 1 of 3 and turnovers
possession_initial <- lineup_game_stats %>%
    mutate(possession = case_when(numberEventMessageType %in% c(1, 2, 5) ~ 1,
                                  numberEventMessageType == 3 & numberEventActionType %in% c(12, 15) ~ 1,
                                  TRUE ~ 0),
           team_possession = case_when(is.na(slugTeamPlayer1) & possession == 1 & descriptionPlayHome == "" ~ slugTeamAway,
                                       is.na(slugTeamPlayer1) & possession == 1 & descriptionPlayVisitor == "" ~ slugTeamHome,
                                       TRUE ~ slugTeamPlayer1))

# lane violation when there's no description of turnover (don't shoot last free throw and consider 1st free throw 1 of 1)
lane_description_missing <- possession_initial %>%
    group_by(idGame, secsPassedGame) %>%
    filter(sum(numberEventMessageType == 3 & numberEventActionType == 10) > 0,
           sum(numberEventMessageType == 6 & numberEventActionType == 2) > 0,
           sum(numberEventMessageType == 7 & numberEventActionType == 3) > 0,
           sum(numberEventMessageType == 1) == 0) %>%
    ungroup() %>%
    mutate(possession = ifelse(numberEventMessageType == 3 & numberEventActionType == 10, 1, possession)) %>%
    select(idGame, numberEvent, team_possession, possession)

# adding turnover to opponent of team when the challenger gets the jumpball
jumpball_turnovers <- possession_initial %>%
    group_by(idGame, numberPeriod) %>%
    mutate(prev_poss = zoo::na.locf0(ifelse(possession == 1, team_possession, NA)),
           next_poss = zoo::na.locf0(ifelse(possession == 1, team_possession, NA), fromLast = TRUE)) %>%
    ungroup() %>%
    mutate(slugTeamPlayer1 = case_when(numberEventMessageType == 9 & descriptionPlayHome == "" ~ slugTeamAway,
                                       numberEventMessageType == 9 & descriptionPlayVisitor == "" ~ slugTeamHome,
                                       TRUE ~ slugTeamPlayer1)) %>%
    group_by(idGame, secsPassedGame) %>%
    mutate(team_reb_chall = sum(numberEventMessageType == 9 & numberEventActionType == 7) > 0 &
               sum(numberEventMessageType == 4 & is.na(namePlayer1)) > 0) %>% 
    ungroup() %>%
    filter(numberEventMessageType == 10 & numberEventActionType == 1 & 
               lag(numberEventMessageType) == 9 & lag(numberEventActionType) == 7 &
               slugTeamPlayer3 == lag(slugTeamPlayer1) &
               prev_poss == next_poss &
               lag(team_reb_chall) == FALSE) %>%
    mutate(team_possession = ifelse(slugTeamPlayer3 == slugTeamPlayer1, slugTeamPlayer2, slugTeamPlayer1),
           possession = 1) %>%
    select(idGame, numberEvent, team_possession, possession)

# finding when there are consecutive poss and changing the first one to zero
change_consec <- possession_initial %>%
    # rows_update(lane_description_missing, by = c("idGame", "numberEvent")) %>%
    # rows_update(jumpball_turnovers, by = c("idGame", "numberEvent")) %>%
    filter(possession == 1 | (numberEventMessageType == 6 & numberEventActionType == 30)) %>%
    group_by(idGame, numberPeriod) %>%
    filter(possession == lead(possession) & team_possession == lead(team_possession)) %>%
    ungroup() %>%
    mutate(possession = 0) %>%
    select(idGame, numberEvent, possession)

# replacing in original data
poss_pack <- possession_initial %>%
    # rows_update(lane_description_missing, by = c("idGame", "numberEvent")) %>%
    # rows_update(jumpball_turnovers, by = c("idGame", "numberEvent")) %>%
    rows_update(change_consec, by = c("idGame","numberEvent"))

# identifying start of possession
start_possessions <- poss_pack %>%
    mutate(slugTeamPlayer1 = case_when(is.na(slugTeamPlayer1) & descriptionPlayHome == "" ~ slugTeamAway,
                                       is.na(slugTeamPlayer1) & descriptionPlayVisitor == "" ~ slugTeamHome,
                                       TRUE ~ slugTeamPlayer1)) %>% 
    select(idGame, numberPeriod, timeQuarter, numberEventMessageType,  slugTeamPlayer1, 
           descriptionPlayHome, descriptionPlayVisitor, numberEvent) %>%
    filter(numberEventMessageType %in% c(1:5)) %>%
    group_by(idGame, numberPeriod) %>%
    mutate(start_poss = case_when(slugTeamPlayer1 != lag(slugTeamPlayer1) & numberEventMessageType == 4 ~ timeQuarter, 
                                  slugTeamPlayer1 != lag(slugTeamPlayer1) & numberEventMessageType != 4 ~ lag(timeQuarter))) %>%
    mutate(start_poss = case_when(is.na(start_poss) & row_number() == 1 & numberPeriod <= 4 ~ "12:00", 
                                  is.na(start_poss) & row_number() == 1 & numberPeriod > 4 ~ "05:00",
                                  TRUE ~ start_poss)) %>%
    ungroup()

# add column with start of possession to the original table and identify heaves
poss_pack_start <- poss_pack %>%
    left_join(start_possessions %>%
                  select(idGame, numberEvent, start_poss)) %>%
    group_by(idGame, numberPeriod) %>%
    mutate(start_poss = ifelse(possession == 1, na.locf0(start_poss), start_poss),
           start_poss = ifelse(numberEventMessageType == 4 & numberEventActionType == 1, na.locf0(start_poss), start_poss),
           start_poss = na.locf0(start_poss, fromLast = TRUE)) %>%
    ungroup() %>%
    mutate(heave = ifelse(numberEventMessageType %in% c(2, 5) & possession == 1 & as.integer(str_sub(start_poss, 4, 5)) <= 2 & str_starts(start_poss, "00:") & (lead(shotPtsHome) + lead(shotPtsAway) == 0), 1, 0),
           possession = ifelse(heave == 1, 0, possession))


# adding extra possessions at end of quarter when team gets the ball with more than 2 secs
last_possessions <- poss_pack_start %>%
    group_by(idGame, numberPeriod) %>%
    filter(cumsum(possession) >= max(cumsum(possession)) & possession == 1) %>%
    ungroup()

last_rebounds <- poss_pack_start %>%
    group_by(idGame, numberPeriod) %>%
    filter(numberEventMessageType == 4 & !(lag(numberEventMessageType) == 3 & lag(numberEventActionType) %in% c(18:20, 27:29))) %>%
    filter(row_number() == max(row_number())) %>%
    ungroup() %>%
    mutate(rebound_team = case_when(is.na(slugTeamPlayer1) & descriptionPlayHome == "" ~ slugTeamAway,
                                    is.na(slugTeamPlayer1) & descriptionPlayVisitor == "" ~ slugTeamHome,
                                    TRUE ~ slugTeamPlayer1)) %>%
    select(idGame, numberPeriod, rebound_team, timeQuarterReb = timeQuarter)

missedft_and1_last <- poss_pack_start %>%
    semi_join(last_possessions %>%
                  select(idGame, secsPassedGame)) %>%
    group_by(idGame, secsPassedGame) %>%
    filter(sum(numberEventMessageType == 1) > 0 & sum(numberEventMessageType == 3 & numberEventActionType == 10) > 0 & sum(str_detect(descriptionPlayHome, "MISS") | str_detect(descriptionPlayVisitor, "MISS")) > 0) %>%
    ungroup() %>%
    filter(numberEventMessageType == 1) %>%
    select(idGame, numberEvent)

addit_poss_reb <- last_possessions %>%
    left_join(last_rebounds, by = c("idGame", "numberPeriod")) %>%
    left_join(missedft_and1_last %>%
                  mutate(and1_ft = 1)) %>%
    filter(numberEventMessageType == 2 | (numberEventMessageType == 3 & (str_detect(descriptionPlayHome, "MISS") | str_detect(descriptionPlayVisitor, "MISS"))) | and1_ft == 1) %>%
    filter(rebound_team != team_possession,
           as.integer(str_sub(timeQuarterReb, 4, 5)) >= 3) %>%
    transmute(idGame, numberPeriod, start_poss = timeQuarterReb, 
              team_possession = rebound_team, possession)

addit_poss_made <- last_possessions %>%
    filter(numberEventMessageType %in% c(1, 5) | (numberEventMessageType == 3 & !str_detect(descriptionPlayHome, "MISS") & !str_detect(descriptionPlayVisitor, "MISS"))) %>%
    anti_join(missedft_and1_last) %>%
    left_join(team_logs %>%
                  distinct(idGame = as.character(idGame), .keep_all = TRUE) %>%
                  select(idGame, slugTeam, slugOpponent)) %>%
    mutate(team_possession_next = ifelse(team_possession == slugTeam, slugOpponent, slugTeam)) %>%
    filter(as.integer(str_sub(timeQuarter, 4, 5)) >= 3) %>%
    transmute(idGame, numberPeriod, start_poss = timeQuarter, 
              team_possession = team_possession_next, possession)

additional_possessions <- bind_rows(addit_poss_reb,  addit_poss_made) %>%
    mutate(numberEventMessageType = 0,
           numberEventActionType = 0,
           numberOriginal = 0,
           descriptionPlayNeutral = "Last possession of quarter") %>%
    left_join(poss_pack %>%
                  filter(numberEventMessageType == 13) %>%
                  select(-c(numberOriginal, numberEventMessageType, numberEventActionType,
                            descriptionPlayNeutral, possession, team_possession))) %>%
    mutate(numberEvent = numberEvent - 0.5)

final_poss_pack <- poss_pack_start %>%
    bind_rows(additional_possessions) %>%
    arrange(idGame, numberEvent) %>%
    select(-c(subOpp, canSub)) %>%
    mutate(across(starts_with("description"), ~ coalesce(., "")))

# changing possession when it ends in free throw (make it end at foul that led to fts)
fouls_possessions <- final_poss_pack %>%
    filter(numberEventMessageType == 3 & possession == 1) %>%
    select(idGame, secsPassedGame, player_foul = namePlayer1, team_possession, numberEvent_ft = numberEvent) %>%
    left_join(final_poss_pack %>%
                  filter(numberEventMessageType == 6 & !numberEventActionType %in% c(6, 9, 11, 13, 14, 15, 16, 17)) %>%
                  mutate(description = ifelse(slugTeamPlayer1 == slugTeamHome, descriptionPlayHome, descriptionPlayVisitor)) %>%
                  select(idGame, secsPassedGame, player_foul = namePlayer2, numberEvent_foul = numberEvent, description)) %>%
    add_count(idGame, secsPassedGame, player_foul, name = "number_plays") %>%
    filter(!(number_plays > 1 & !str_detect(description, " S.FOUL |\\.PN\\)")))

missing_comp <- fouls_possessions %>%
    filter(is.na(numberEvent_foul)) %>%
    left_join(final_poss_pack %>%
                  filter(numberEventMessageType == 6 & !numberEventActionType %in% c(6, 9, 11, 13, 14, 15, 16, 17)) %>%
                  mutate(description = ifelse(slugTeamPlayer1 == slugTeamHome, descriptionPlayHome, descriptionPlayVisitor)) %>%
                  select(idGame, secsPassedGame, numberEvent_foul = numberEvent, description),
              by = c("idGame", "secsPassedGame"),
              suffix = c("", "_new")) %>%
    mutate(numberEvent_foul = numberEvent_foul_new,
           description = description_new) %>%
    select(-c(numberEvent_foul_new, description_new))

fouls_possessions <- fouls_possessions %>%
    rows_update(missing_comp, by = c("idGame", "secsPassedGame", "player_foul", "team_possession", "numberEvent_ft", "number_plays")) %>%
    select(idGame, secsPassedGame, team_possession, numberEvent_ft, numberEvent_foul) %>%
    pivot_longer(cols = starts_with("numberEvent"),
                 names_to = "type_play",
                 values_to = "numberEvent",
                 names_prefix = "numberEvent_") %>%
    mutate(possession_players = ifelse(type_play == "foul", 1, 0)) %>%
    select(-type_play)

final_poss_pack <- final_poss_pack %>%
    mutate(possession_players = possession) %>%
    rows_update(fouls_possessions, by = c("idGame", "numberEvent"))

lineup_stats <- final_poss_pack %>%
    select(idGame, numberEvent, slugTeamHome, slugTeamAway, numberPeriod, timeQuarter, secsPassedGame, 
           newptsHome, newptsAway, lineupHome, lineupAway, possession_players, team_possession) %>%
    mutate(possession_home = ifelse(team_possession == slugTeamHome & possession_players == 1, 1, 0),
           possession_away = ifelse(team_possession == slugTeamAway & possession_players == 1, 1, 0)) %>%
    pivot_longer(cols = starts_with("lineup"),
                 names_to = "lineupLocation",
                 names_prefix = "lineup",
                 values_to = "lineup") %>%
    mutate(ptsTeam = ifelse(lineupLocation == "Home", newptsHome, newptsAway),
           ptsOpp = ifelse(lineupLocation == "Away", newptsHome, newptsAway),
           possTeam = ifelse(lineupLocation == "Home", possession_home, possession_away),
           possOpp = ifelse(lineupLocation == "Away", possession_home, possession_away),
           slugTeam = ifelse(lineupLocation == "Home", slugTeamHome, slugTeamAway),
           slugOpp = ifelse(lineupLocation == "Away", slugTeamHome, slugTeamAway)) %>%
    distinct(idGame, slugTeam, slugOpp, numberPeriod, timeQuarter, secsPassedGame, ptsTeam, ptsOpp,
             possTeam, possOpp, lineup, teamLocation = lineupLocation, numberEvent) %>%
    arrange(idGame, numberEvent) %>%
    group_by(idGame, slugTeam) %>%
    mutate(lineupChange = lineup != lag(lineup),
           lineupChange = coalesce(lineupChange, FALSE)) %>%
    group_by(idGame, slugTeam) %>%
    mutate(lineupStint = cumsum(lineupChange)) %>%
    ungroup() %>%
    arrange(idGame, lineupStint, numberEvent) %>%
    group_by(idGame, slugTeam, lineup, lineupStint, numberPeriod) %>%
    summarise(totalPossTeam = sum(possTeam),
              totalPossOpp = sum(possOpp),
              initialScoreTeam = ptsTeam[row_number() == min(row_number())],
              initialScoreOpp = ptsOpp[row_number() == min(row_number())],
              finalScoreTeam = ptsTeam[row_number() == max(row_number())],
              finalScoreOpp =  ptsOpp[row_number() == max(row_number())],
              initialTime = secsPassedGame[row_number() == min(row_number())],
              finalTime = secsPassedGame[row_number() == max(row_number())]) %>%
    ungroup() %>%
    arrange(idGame, lineupStint) %>%
    group_by(idGame, slugTeam) %>%                              
    mutate(finalTime = ifelse(row_number() == max(row_number()), finalTime, lead(initialTime))) %>%  
    ungroup() %>%
    mutate(across(c(contains("Score")), ~ as.numeric(.), .names = "{col}")) %>%
    mutate(totalScoreTeam = finalScoreTeam - initialScoreTeam,
           totalScoreOpp = finalScoreOpp - initialScoreOpp,
           netScoreTeam = totalScoreTeam - totalScoreOpp,
           totalTime = finalTime - initialTime) %>%
    arrange(idGame, lineupStint)

lineup_stats <- lineup_stats %>%
    left_join(lineup_stats %>%
                  filter(lineupStint == 0) %>%
                  distinct(idGame, slugTeam, starters = lineup)) %>%
    mutate(across(c(lineup, starters), ~ str_split(., ", "), .names = "{.col}_list")) %>%
    mutate(reserves = map_int(map2(lineup_list, starters_list, setdiff), length)) %>%
    select(-c(contains("list"), starters))


rm(games, event_changes, play_logs_all, new_pbp, subs_made, others_qtr,
   lineups_quarters, data_missing_players, missing_players_ot, lineup_subs, 
   lineup_game, lineup_game_stats, possession_initial, jumpball_turnovers, lane_description_missing,
   change_consec, poss_pack, start_possessions, poss_pack_start, last_possessions, fouls_possessions,
   missing_comp, last_rebounds, missedft_and1_last, addit_poss_reb, addit_poss_made, additional_possessions)


missing_starters <- tribble(
    ~game_id,      ~period,      ~team_player,      ~name_player,
    "0022200025",            5,           "MIN",  "Jaden McDaniels",
    "0022200039",            5,           "WAS",     "Delon Wright",
    "0022200040",            5,           "UTA",      "Mike Conley",
    "0022200072",            5,           "BOS",       "Al Horford",
    "0022200117",            5,           "NOP",    "Naji Marshall",
    "0022200117",            5,           "LAL",    "Austin Reaves",
    "0022200325",            5,           "DET",   "Isaiah Stewart",
    "0022200440",            5,           "DAL",  "Tim Hardaway Jr.",
    "0022200519",            5,           "CHI",       "Zach LaVine",
    "0022200659",            5,           "TOR",    "Gary Trent Jr.",
    "0022200748",            5,           "SAS",  "Keita Bates-Diop",
    "0022200758",            5,           "SAC",   "Harrison Barnes",
    "0022200892",            5,           "OKC",    "Jalen Williams",
    "0022201007",            5,           "MIA",         "Max Strus",
    "0022201194",            5,           "NOP",       "CJ McCollum",
    "0022201205",            5,           "ATL",        "Saddiq Bey",
    "0022100041",            5,           "CHA",    "Gordon Hayward",
    "0022100291",            6,           "LAL",        "Malik Monk",
    "0022100353",            5,           "PHI",       "Danny Green",
    "0022100413",            5,           "BKN",   "Kessler Edwards",
    "0022100688",            3,           "POR",  "Robert Covington",
    "0022100860",            5,           "OKC",     "Darius Bazley",
    "0022100967",            5,           "NOP",        "Tony Snell",
    "0022000023",            5,           "DET",      "Delon Wright",
    "0022000100",            5,           "IND",    "Justin Holiday",
    "0022000120",            5,           "DEN",       "Gary Harris",
    "0022000440",            5,           "MIN",   "Anthony Edwards",
    "0022000465",            5,           "NOP",        "Lonzo Ball",
    "0022000485",            1,           "DAL", "Dorian Finney-Smith",
    "0022000637",            5,           "CHI",        "Coby White",
    "0022000645",            5,           "IND",    "T.J. McConnell",
    "0022001012",            5,           "WAS",         "Raul Neto",
    "0022001064",            5,           "CHA",   "Jalen McDaniels",
    "0021900023",            5,           "DEN",      "Malik Beasley",
    "0021900120",            5,           "MIN",    "Treveon Graham",
    "0021900272",            5,           "ATL",   "De'Andre Hunter",
    "0021900409",            5,           "WAS",         "Ish Smith",
    "0021900502",            5,           "GSW",        "Damion Lee",
    "0021900550",            5,           "OKC", "Terrance Ferguson",
    "0021900563",            5,           "DET",        "Tony Snell",
    "0021900696",            5,           "SAC",   "Harrison Barnes",
    "0021900787",            5,           "ATL",   "De'Andre Hunter",
    "0021900892",            5,           "HOU",       "Eric Gordon",
    "0021901281",            6,           "DEN",      "Monte Morris",
    "0021800143",            6,           "CHI",    "Justin Holiday",
    "0021800143",            6,           "NYK",       "Noah Vonleh",
    "0021800216",            5,           "BOS", "Marcus Morris Sr.",
    "0021800276",            3,           "DEN","Juancho Hernangomez",
    "0021800371",            5,           "BKN",        "Joe Harris",
    "0021800565",            5,           "HOU",       "P.J. Tucker",
    "0021800619",            5,           "OKC", "Terrance Ferguson",
    "0021800881",            5,           "UTA",        "Joe Ingles",
    "0021801070",            5,           "MEM",     "Bruno Caboclo",
    "0021801132",            5,           "GSW",    "Andre Iguodala",
    "0021801229",            5,           "UTA",   "Tyler Cavanaugh",
    "0021800569",            5,           "CHI","Wendell Carter Jr.",
    "0022200234",            2,           "LAL",    " Kendrick Nunn")

