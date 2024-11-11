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

pbp <- pbp %>%
    mutate(
        season_year = case_when(
            substr(game_id, 1, 5) == "00218" ~ 2019,
            substr(game_id, 1, 5) == "00219" ~ 2020,
            substr(game_id, 1, 5) == "00220" ~ 2021,
            substr(game_id, 1, 5) == "00221" ~ 2022,
            substr(game_id, 1, 5) == "00222" ~ 2023,
            TRUE ~ NA_integer_  # Handles cases not in the specified range
        )
    )

pbp <- pbp %>%
    filter(game_id %in% c(
        "0021800001", "0021800002", "0021800003", "0021800004"
    ))

# saveRDS(pbp, "./pbp_df.rds")

pbp_df <- pbp %>% select(game_id, period, pctimestring, eventnum:eventmsgactiontype, homedescription:visitordescription)

pbp_df_types <- pbp_df %>%
    filter(eventmsgtype %in% c(1:5) & !(eventmsgtype == 3 & eventmsgactiontype %in% c(16, 18:19, 20, 27:29, 25:26)) & !(eventmsgtype == 4 & eventmsgactiontype == 1))


# eventmsgtype 1 = made shot
# eventmsgtype 2 = missed shot
# eventmsgtype 3 & 10 = 1 of 1 common foul *
# eventmsgtype 3 & 11 = 1 of 2 common foul
# eventmsgtype 3 & 12 = 2 of 2 common foul *
# eventmsgtype 3 & 13 = 1 of 3 common foul
# eventmsgtype 3 & 14 = 2 of 3 common foul
# eventmsgtype 3 & 15 = 3 of 3 common foul *
# eventmsgtype 3 & 16 = 1 of 1 tech *
# eventmsgtype 3 & 18 = 1 of 2 flagrant
# eventmsgtype 3 & 19 = 2 of 2 flagrant *
# eventmsgtype 3 & 20 = 1 of 1 flagrant *
# eventmsgtype 3 & 25 = 1 of 2 clear path
# eventmsgtype 3 & 26 = 2 of 2 clear path *
# eventmsgtype 3 & 27 = 1 of 3 flagrant
# eventmsgtype 3 & 28 = 2 of 3 flagrant
# eventmsgtype 3 & 29 = 3 of 3 flagrant *
# eventmsgtype 4 & 0 = rebound off fg
# eventmsgtype 4 & 1 = rebound off ft
# eventmsgtype 5 = turnover




# all possessions - https://github.com/ramirobentes/NBA-in-R/blob/master/all_possessions

convert_to_seconds <- function(time_str) {
    time_parts <- strsplit(time_str, ":")[[1]]
    minutes <- as.numeric(time_parts[1])
    seconds <- as.numeric(time_parts[2])
    
    # Total seconds remaining
    total_seconds <- (minutes * 60) + seconds
    return(total_seconds)
}

# Function to calculate seconds passed in the game
seconds_passed <- function(time_str, period) {
    # Convert time_str to seconds remaining in the period
    time_parts <- strsplit(time_str, ":")[[1]]
    minutes <- as.numeric(time_parts[1])
    seconds <- as.numeric(time_parts[2])
    
    # Total seconds in a 12-minute period
    period_seconds <- 720
    
    # Calculate seconds passed in the current period
    seconds_remaining <- (minutes * 60) + seconds
    seconds_in_current_period <- period_seconds - seconds_remaining
    
    # Calculate total seconds passed in previous periods
    previous_periods <- (as.numeric(period) - 1)
    seconds_in_previous_periods <- previous_periods * period_seconds
    
    # Total seconds passed in the game
    total_seconds_passed <- seconds_in_previous_periods + seconds_in_current_period
    return(total_seconds_passed)
}

# https://github.com/ramirobentes/NBA-in-R/blob/04ef36bb6da424ee808209875534ad77198319f3/2016_17/pbp_lineups.R#L183

# initial possessions
poss_initial <- pbp %>%
    mutate(
        home_team = slice(pbp, 2)$player1_team_id,
        away_team = slice(pbp, 2)$player2_team_id
    ) %>%
    filter(eventmsgtype != 18) %>%
    rowwise() %>%
    mutate(
        secs_left_quarter = convert_to_seconds(pctimestring),
        secs_passed_quarter = if_else(period %in% c(1:4), 720 - secs_left_quarter, 300 - secs_left_quarter),  
        secs_passed_game = seconds_passed(pctimestring, period),
        possession = case_when(eventmsgtype %in% c(1, 2, 5) ~ 1,
                               eventmsgtype == 3 & eventmsgactiontype %in% c(10, 12, 15) ~ 1,
                               TRUE ~ 0)
    ) %>%
    ungroup()

 # change end of possession to ft of and one
fgs_and1 <- poss_initial %>%
    distinct() %>%
    filter(eventmsgtype == 1 | (eventmsgtype == 6 & !eventmsgactiontype %in% c(4, 10, 11, 12, 16, 18)) | (eventmsgtype == 3 & eventmsgactiontype == 10)) %>%
    group_by(game_id, secs_passed_game) %>%
    filter(eventmsgtype == 1 & lead(eventmsgtype) == 6 & player1_team_id != lead(player1_team_id)) %>%
    ungroup() %>%
    mutate(possession = 0)

poss_initial_2 <- poss_initial %>%
    rows_update(fgs_and1, by = c("game_id", "eventnum"))

# identify and change consecutive possessions
change_consec <- poss_initial_2 %>%
    distinct() %>%
    filter(possession == 1 | (eventmsgtype == 6 & eventmsgactiontype == 30)) %>%
    group_by(game_id, period) %>%
    filter(possession == lead(possession) & player1_team_id == lead(player1_team_id)) %>%
    ungroup() %>%
    mutate(possession = 0) %>%
    select(game_id, eventnum, possession)

# replace in data
poss_non_consec <- poss_initial_2 %>%
    rows_update(change_consec, by = c("game_id","eventnum"))

start_possessions <- poss_non_consec %>%
    mutate(
        start_poss = case_when(possession == 1 & eventmsgtype != 4 ~ lag(secs_left_quarter))
    ) %>%
    select(game_id:period, pctimestring, possession, start_poss, homedescription:visitordescription)














fouls_and1 <- poss_non_consec %>% 
    filter(eventmsgtype == 1 | (eventmsgtype == 6 & !eventmsgactiontype %in% c(4, 10, 11, 12, 16, 18)) | (eventmsgtype == 3 & eventmsgactiontype == 10)) %>%
    group_by(game_id, secs_passed_game) %>%
    filter(lag(eventmsgtype) == 1 & eventmsgtype == 6 & lag(player1_team_id) != player1_team_id) %>%
    ungroup()

fgs_and1 <- poss_non_consec %>% 
    filter(eventmsgtype == 1 | (eventmsgtype == 6 & !eventmsgactiontype %in% c(4, 10, 11, 12, 16, 18)) | (eventmsgtype == 3 & eventmsgactiontype == 10)) %>%
    group_by(game_id, secs_passed_game) %>%
    filter(eventmsgtype == 1 & lead(eventmsgtype) == 6 & player1_team_id != lead(player1_team_id)) %>%
    ungroup() %>%
    mutate(possession = 0)

fouls_and1_updt <- fouls_and1 %>%
    rows_update(fgs_and1, by = c("game_id", "secs_passed_game"))

fgs_and1_updt <- fgs_and1 %>%
    rows_update(fouls_and1, by = c("game_id", "secs_passed_game"))

poss_non_consec_2 <- poss_non_consec %>%
    rows_update(fouls_and1_updt, by = c("game_id", "eventnum")) %>%
    rows_update(fgs_and1_updt, by = c("game_id", "eventnum"))






# # identifying start of possession - https://github.com/ramirobentes/NBA-in-R/blob/master/pbp%20steps.R
# start_possessions <- poss_non_consec %>%
#     mutate(
#         player1_team_id = case_when(is.na(player1_team_id) & homedescription == "" ~ away_team,
#                                     is.na(player1_team_id) & visitordescription == "" ~ home_team,
#                                     TRUE ~ player1_team_id)
#     ) %>% 
#     filter(eventmsgtype %in% c(1:5)) %>%
#     group_by(game_id, period) %>%
#     mutate(start_poss = case_when(player1_team_id != lag(player1_team_id) & eventmsgtype == 4 ~ secs_left_quarter, 
#                                   player1_team_id != lag(player1_team_id) & eventmsgtype != 4 ~ lag(secs_left_quarter)),
#            start_poss = case_when(is.na(start_poss) & row_number() == 1 & period <= 4 ~ 720, 
#                                   is.na(start_poss) & row_number() == 1 & period > 4 ~ 300,
#                                   TRUE ~ start_poss)
#     ) %>%
#     ungroup()

# # find start of possessions
# start_possessions <- poss_non_consec %>%
#     group_by(
#         game_id,
#         secs_passed_game,
#         slug_team_foul = ifelse(eventmsgtype == 6,
#                                 ifelse(team_home == slug_team,
#                                        team_away, team_home),
#                                 slug_team)
#     ) %>%
#     mutate(and1 = sum(eventmsgtype == 1) > 0 &
#                sum(eventmsgtype == 3) > 0 &
#                sum(eventmsgtype == 6 & act_type == 2) > 0 &
#                (eventmsgtype == 1 | (eventmsgtype == 3 & act_type == 10))) %>%
#     ungroup() %>%
#     mutate(start_poss = case_when(eventmsgtype == 4 & act_type == 0 & desc_value == 0 ~ secs_passed_game,
#                                   eventmsgtype == 3 & act_type %in% c(10, 12, 15) & shot_pts > 0 ~ secs_passed_game,
#                                   eventmsgtype %in% c(1, 5) & !and1 ~ secs_passed_game),
#            eventnum = ifelse(eventmsgtype == 4, eventnum, eventnum + 1)) %>%
#     filter(!is.na(start_poss))






# add start of possession column to table
poss_non_consec <- poss_non_consec %>%
    left_join(start_possessions %>%
                  select(game_id, eventnum, start_poss)) %>%
    group_by(game_id, period) %>%
    mutate(start_poss = ifelse(row_number() == 1, clock, start_poss),
           start_poss = na.locf(start_poss)) %>%
    ungroup()












    
    
fouls_and1 <- pbp %>%
    filter(eventmsgtype == 1 | (eventmsgtype == 6 & !eventmsgactiontype %in% c(4, 10, 11, 12, 16, 18)) | (eventmsgtype == 3 & eventmsgactiontype == 10)) %>%
    group_by(game_id, secs_passed_game) %>%
    filter(lag(eventmsgtype) == 1 & eventmsgtype == 6 & lag(player1_team_id) != player1_team_id) %>%
    ungroup() %>%
    select(game_id, team_home, team_away, period, clock, secs_passed_game, number_original, msg_type, act_type, slug_team, 
           player1, player3, description, possession, poss_home, poss_away)

fgs_and1 <- pbp2023 %>% 
    filter(msg_type == 1 | (msg_type == 6 & !act_type %in% c(4, 10, 11, 12, 16, 18)) | (msg_type == 3 & act_type == 10)) %>%
    group_by(game_id, secs_passed_game) %>%
    filter(msg_type == 1 & lead(msg_type) == 6 & slug_team != lead(slug_team)) %>%
    ungroup() %>%
    select(game_id, team_home, team_away, period, clock, secs_passed_game, number_original, msg_type, act_type, 
           slug_team, player1, player3, description, possession, poss_home, poss_away)

fouls_and1_updt <- fouls_and1 %>%
    rows_update(fgs_and1 %>%
                    select(game_id, secs_passed_game, possession, poss_home, poss_away),
                by = c("game_id", "secs_passed_game")) %>%
    select(game_id, secs_passed_game, number_original, possession, poss_home, poss_away)

fgs_and1_updt <- fgs_and1 %>%
    rows_update(fouls_and1 %>%
                    select(game_id, secs_passed_game, possession, poss_home, poss_away),
                by = c("game_id", "secs_passed_game")) %>%
    select(game_id, team_home, team_away, number_original, possession, poss_home, poss_away)

pbp2023_updt <- pbp2023 %>%
    rows_update(fouls_and1_updt %>% select(-secs_passed_game), by = c("game_id", "number_original")) %>%
    rows_update(fgs_and1_updt %>% select(-c(team_home, team_away)), by = c("game_id", "number_original")) %>%
    mutate(poss_team = case_when(poss_home == 1 ~ team_home,
                                 poss_away == 1 ~ team_away,
                                 TRUE ~ NA))

fts_poss <- pbp2023_updt %>%
    filter(msg_type == 3 & !act_type %in% c(16, 21, 22)) %>% 
    semi_join(pbp2023_updt %>%
                  filter(msg_type == 6 & poss_home + poss_away > 0) %>%
                  transmute(game_id, secs_passed_game, slug_team = ifelse(slug_team == team_home, team_away, team_home))) %>%
    group_by(game_id, start_poss, secs_passed_game, slug_team) %>%
    filter(row_number() == max(row_number())) %>%
    ungroup() %>%
    transmute(game_id, secs_passed_game, number_original, poss_team = slug_team)  

poss_all <- pbp2023_updt %>%
    rows_update(pbp2023_updt %>%
                    filter(msg_type == 6 & poss_home + poss_away > 0) %>%
                    transmute(game_id, number_original, secs_passed_game, poss_team = ifelse(poss_away == 1, team_away, team_home)) %>%
                    semi_join(fts_poss %>% select(-number_original)) %>%
                    transmute(game_id, number_original, poss_team = NA),
                by = c("game_id", "number_original")) %>%
    rows_update(fts_poss %>%
                    select(-secs_passed_game),
                by = c("game_id", "number_original")) %>%
    select(game_id, team_home, team_away, period, clock, start_poss, number_original, msg_type, act_type, slug_team, poss_team, description, poss_home,
           poss_away, shot_pts_home, shot_pts_away, lineup_home, lineup_away, hs, vs) %>%
    group_by(game_id, period, poss_team) %>%
    mutate(poss_number = ifelse(!is.na(poss_team), cumsum(!is.na(poss_team)), NA)) %>%
    group_by(game_id, period) %>%
    mutate(across(c(poss_team, poss_number), ~ zoo::na.locf0(., fromLast = TRUE))) %>%
    ungroup()

poss_summary <- poss_all %>%
    group_by(game_id, team_home, team_away, period, poss_team, poss_number) %>%
    summarise(across(c(shot_pts_home, shot_pts_away), sum)) %>%
    ungroup() %>%
    mutate(pts = ifelse(team_home == poss_team, shot_pts_home, shot_pts_away)) %>%
    select(-starts_with("shot_pts")) %>%
    left_join(poss_all %>%
                  filter(poss_home + poss_away > 0) %>%
                  mutate(lineup_team = ifelse(poss_team == team_home, lineup_home, lineup_away),
                         lineup_opp = ifelse(poss_team == team_home, lineup_away, lineup_home)) %>%
                  select(game_id, period, clock, poss_team, poss_number, lineup_team, lineup_opp)) %>%
    mutate(opp_team = ifelse(poss_team == team_home, team_away, team_home)) %>%
    select(game_id, poss_team, opp_team, period, clock, poss_number, pts, lineup_team, lineup_opp) %>%
    arrange(game_id, period, desc(clock), poss_number) %>%
    filter(!is.na(poss_team))






# pbp poss number - https://github.com/ramirobentes/NBA-in-R/blob/master/pbp_poss_number.R


and_ones <- pbp2023 %>%
    group_by(game_id, secs_passed_game, 
             slug_team_foul = ifelse(msg_type == 6, ifelse(team_home == slug_team, team_away, team_home), slug_team)) %>%
    mutate(and1 = sum(msg_type == 1) > 0 &
               sum(msg_type == 3) > 0 &
               sum(msg_type == 6 & act_type == 2) > 0 &
               (msg_type == 1 | (msg_type == 3 & act_type == 10))) %>%
    ungroup() %>%
    filter(and1) %>%
    select(game_id, period, clock, number_event, msg_type, possession) %>%
    group_by(game_id, period, clock) %>%
    mutate(possession = sum(possession) - possession) %>%
    ungroup()

pbp_changed <- pbp2023 %>%
    rows_update(and_ones, by = c("game_id", "period", "clock", "number_event", "msg_type")) %>%  
    mutate(possession = case_when(poss_home == 1 & possession == 0 & msg_type != 1 ~ 1,
                                  poss_away == 1 & possession == 0 & msg_type != 1 ~ 1,
                                  possession == 1 & msg_type == 6 ~ 0,
                                  TRUE ~ possession),
           slug_team = case_when(poss_home == 1 ~ team_home,
                                 poss_away == 1 ~ team_away,
                                 TRUE ~ slug_team)) %>%
    group_by(game_id, period, poss_team = ifelse(possession == 1, slug_team, NA)) %>%
    mutate(poss_number = ifelse(possession == 1, cumsum(possession), NA)) %>%
    group_by(game_id, period) %>%
    mutate(across(c(poss_team, poss_number), ~ zoo::na.locf0(., fromLast = TRUE))) %>%
    ungroup() %>%
    mutate(lineup_off = ifelse(poss_team == team_home, lineup_home, lineup_away),
           lineup_def = ifelse(poss_team == team_away, lineup_home, lineup_away)) %>%
    select(game_id, period, clock, number_event, msg_type, act_type, slug_team, poss_team, description, lineup_off, lineup_def, possession,
           poss_number, shot_pts) %>%
    group_by(game_id, period, poss_team, poss_number) %>%
    mutate(pts_poss = ifelse(possession == 1, sum(shot_pts), NA)) %>%
    ungroup() 




# https://github.com/ramirobentes/NBA-in-R/blob/95915510edc107ee1bea0e81aa2798401d73736d/every%20poss.R#L22


# https://github.com/ramirobentes/NBA-in-R/blob/95915510edc107ee1bea0e81aa2798401d73736d/pbp%20steps.R#L246

# https://github.com/ramirobentes/NBA-in-R/blob/04ef36bb6da424ee808209875534ad77198319f3/hoopR%20pbp%20past%20seasons.R#L190

# https://github.com/JayKay-15/MambaModel/blob/main/code/model/win_prob.R





