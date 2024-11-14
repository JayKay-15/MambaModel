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
    bind_rows(lineups_quarters)

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


lineup_game <- lineup_game %>%
    mutate(
        fga = case_when(
            eventmsgtype %in% c(1,2) ~ player1_name,
            TRUE ~ NA_character_
        ),
        fgm = case_when(
            eventmsgtype == 1 ~ player1_name,
            TRUE ~ NA_character_
        ),
        fg3a = case_when(
            eventmsgtype %in% c(1,2) & (str_detect(homedescription, "3PT") | str_detect(visitordescription, "3PT")) ~ player1_name,
            TRUE ~ NA_character_
        ),
        fg3m = case_when(
            eventmsgtype == 1 & (str_detect(homedescription, "3PT") | str_detect(visitordescription, "3PT")) ~ player1_name,
            TRUE ~ NA_character_
        ),
        fta = case_when(
            eventmsgtype == 3 ~ player1_name,
            TRUE ~ NA_character_
        ),
        ftm = case_when(
            eventmsgtype == 3 & !str_detect(homedescription, "MISS") ~ player1_name,
            TRUE ~ NA_character_
        ),
        oreb = case_when(
            eventmsgtype == 4 & player1_team_id == lag(player1_team_id) ~ player1_name,
            TRUE ~ NA_character_
        ),
        dreb = case_when(
            eventmsgtype == 4 & player1_team_id != lag(player1_team_id) ~ player1_name,
            TRUE ~ NA_character_
        ),
        reb = case_when(
            eventmsgtype == 4 ~ player1_name,
            TRUE ~ NA_character_
        ),
        ast = case_when(
            eventmsgtype == 1 ~ player2_name,
            TRUE ~ NA_character_
        ),
        stl = case_when(
            eventmsgtype == 5 & eventmsgactiontype %in% c(1,2) ~ player1_name,
            TRUE ~ NA_character_
        ),
        blk = case_when(
            eventmsgtype == 2 ~ player3_name,
            TRUE ~ NA_character_
        ),
        tov = case_when(
            eventmsgtype == 5 ~ player1_name,
            TRUE ~ NA_character_
        ),
        foul = case_when(
            eventmsgtype == 6 ~ player1_name,
            TRUE ~ NA_character_
        )
    )





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
    "0021801229",            5,           "UTA",   "Tyler Cavanaugh")


corrections <- tribble(
    ~game_id,     ~number_event, ~msg_type, ~act_type,           ~description,
    22200094,               380,         3,        16,           "[PHX 93-88] Lee Free Throw Technical (5 PTS)",
    22201142,               437,         6,         3,           "[OKC] Jay Williams Foul: Loose Ball (2 PF) (2 FTA) (C Kirkland)",
    22201218,               448,         6,         1,           "[CHA] Bouknight Foul: Personal (3 PF) (2 FTA) (K Cutler)",
    22200583,               420,         6,         6,           "[MIL] Antetokounmpo Foul: Away From Play (3 PF) (1 FTA) (T Maddox)",
    22200583,               421,         3,        10,           "[CHA] Plumlee Free Throw 1 of 1 Missed")

corrections <- tribble(
    ~game_id,     ~clock,   ~secs_passed_game, ~number_event, ~msg_type, ~act_type,                                              ~description,
    22100295,    "04:26",                1894,           283,         6,         2,    "[ORL] Lopez Foul: Shooting (3 PF) (1 FTA) (J Capers)",
    22100199,    "04:39",                1881,           271,         6,         1,           "[CLE] Wade Foul: Personal (3 PF) (T Brothers)",
    22100249,    "09:18",                1602,           265,         3,        18,         "[DET] Jackson Free Throw Flagrant 1 of 2 Missed",
    22100249,    "09:18",                1602,           267,         3,        19,  "[DET 79-67] Jackson Free Throw Flagrant 2 of 2 (5 PTS)",
    22100512,  "00:20.8",              2859.2,           473,         6,         1,            "[NYK] Burks Foul: Personal (4 PF) (M Boland)")

corrections <- tribble(
    ~game_id,     ~clock,   ~secs_passed_game,  ~period, ~number_event, ~msg_type, ~act_type,  ~desc_value,                                         ~description,
    21800534,    "12:00",                1440,        3,           237,         6,        11,            0,         "[GSW] Iguodala Technical (1 FTA) (J Tiven)",
    21800534,    "12:00",                1440,        3,           238,        11,         4,            0,         "[GSW] Iguodala Ejection:Other",
    21800036,    "02:17",                1303,        2,           222,         3,        28,            1,         "[ATL 58-58] Bazemore Free Throw Flagrant 2 of 3 (7 PTS)",
    21800036,    "02:17",                1303,        2,           223,         3,        29,            1,         "[ATL 59-58] Bazemore Free Throw Flagrant 3 of 3 (8 PTS)")



change_order <- tribble(
    ~game_id,     ~number_original,     ~number_event,
    22101139,                  649,               466,
    22101139,                  648,               467,
    22101139,                  650,               469,
    22101139,                  654,               470,
    22101196,                  601,               400,
    22101196,                  577,               401)

change_order <- tribble(
    ~game_id,     ~number_original,     ~number_event,
    21900811,                  597,               431,
    21900811,                  605,               432,
    21900811,                  612,               433,
    21900811,                  601,               434,
    21900811,                  606,               435,
    21900811,                  599,               436,
    21900811,                  600,               437,
    21900507,                  454,               307,
    21900507,                  456,               308,
    21900507,                  457,               309,
    21900650,                  513,               349,
    21900650,                  517,               350,
    21900875,                  596,               420,
    21900875,                  598,               421,
    21900875,                  601,               423)

change_order <- tribble(
    ~game_id,     ~number_original,     ~number_event,
    21800578,                  246,               176,
    21800578,                  249,               177,
    21800578,                  250,               178,
    21800484,                  221,               158,
    21800484,                  223,               159,
    21800484,                  224,               160,
    21800484,                  225,               161,
    21800484,                  226,               162)


manual_changes <- tribble(
    ~game_id,     ~number_event,    ~shot_pts_home,     ~shot_pts_away,
    21900622,               328,                 0,                  2,
    21900742,               216,                 0,                  0,
    21900742,               219,                 0,                  1)

del_rows <- tribble(
    ~game_id,                   ~evt,
    "0022200731",                    648,
    "0022200741",                    267)
