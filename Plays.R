### Plays Book ----

### Keys ----

spread_model <- "naomi_spread_edge"
spread_key <- 5.041

ml_model <- "cindy_ml_edge"
ml_key <- 0.135

over_model <- "adriana_over_edge"
over_key <- 2.5

under_model <- "heidi_under_edge"
under_key <- 1.00


### Plays ----
plays_a <- slate %>%
    mutate(loc = "A") %>%
    select(1,4,6,3,2)
colnames(plays_a) <- c("game_id", "date", "loc", "opp_team", "team")

plays_a <- plays_a %>%
    left_join(kendall_predict, by = c("team" = "away")) %>%
    left_join(., tyra_predict, by = c("team" = "away")) %>%
    left_join(., gisele_predict, by = c("team" = "away")) %>%
    left_join(., chrissy_predict, by = c("team" = "away")) %>%
    left_join(., kate_predict, by = c("team" = "away")) %>%
    left_join(., cindy_predict, by = c("team" = "away")) %>%
    left_join(., naomi_predict, by = c("team" = "away")) %>%
    left_join(., heidi_predict, by = c("team" = "away")) %>%
    left_join(., adriana_predict, by = c("team" = "away"))

plays_a <- plays_a %>%
    select(1:5, 
           7, 9, 11,
           16, 18, 20, 
           25, 27, 29, 
           34, 36, 38, 
           43, 45, 47, 
           52, 54, 56,
           61, 63, 65,
           70, 72, 74,
           79, 81, 83)

colnames(plays_a) <- c("game_id", "date", "loc", "opp_team", "team", 
                       "kendall_margin", "kendall_win", "kendall_total",
                       "tyra_margin", "tyra_win", "tyra_total",
                       "gisele_margin", "gisele_win", "gisele_total",
                       "chrissy_margin", "chrissy_win", "chrissy_total",
                       "kate_margin", "kate_win", "kate_total",
                       "cindy_margin", "cindy_win", "cindy_total",
                       "naomi_margin", "naomi_win", "naomi_total",
                       "heidi_margin", "heidi_win", "heidi_total",
                       "adriana_margin", "adriana_win", "adriana_total")

plays_h <- slate %>%
    mutate(loc = "H") %>%
    select(1,4,6,2,3)
colnames(plays_h) <- c("game_id", "date", "loc", "opp_team", "team")

plays_h <- plays_h %>%
    left_join(kendall_predict, by = c("opp_team" = "away")) %>%
    left_join(., tyra_predict, by = c("opp_team" = "away")) %>%
    left_join(., gisele_predict, by = c("opp_team" = "away")) %>%
    left_join(., chrissy_predict, by = c("opp_team" = "away")) %>%
    left_join(., kate_predict, by = c("opp_team" = "away")) %>%
    left_join(., cindy_predict, by = c("opp_team" = "away")) %>%
    left_join(., naomi_predict, by = c("opp_team" = "away")) %>%
    left_join(., heidi_predict, by = c("opp_team" = "away")) %>%
    left_join(., adriana_predict, by = c("opp_team" = "away"))

plays_h <- plays_h %>%
    select(1:5, 
           8, 10, 11,
           17, 19, 20,
           26, 28, 29,
           35, 37, 38,
           44, 46, 47,
           53, 55, 56,
           62, 64, 65,
           71, 73, 74,
           80, 82, 83)

colnames(plays_h) <- c("game_id", "date", "loc", "opp_team", "team", 
                       "kendall_margin", "kendall_win", "kendall_total",
                       "tyra_margin", "tyra_win", "tyra_total",
                       "gisele_margin", "gisele_win", "gisele_total",
                       "chrissy_margin", "chrissy_win", "chrissy_total",
                       "kate_margin", "kate_win", "kate_total",
                       "cindy_margin", "cindy_win", "cindy_total",
                       "naomi_margin", "naomi_win", "naomi_total",
                       "heidi_margin", "heidi_win", "heidi_total",
                       "adriana_margin", "adriana_win", "adriana_total")

plays_today <- bind_rows(plays_a, plays_h) %>% arrange(game_id)

### Odds ----
odds_db <- dplyr::tbl(DBI::dbConnect(RSQLite::SQLite(),
                                      "/Users/Jesse/Documents/MyStuff/NBA Betting/NBAdb/NBAdb.sqlite"),"Odds")

odds <- odds_db %>% 
    collect() %>% 
    mutate(commence_time = as_date(commence_time, origin ="1970-01-01")) %>% 
    filter(commence_time == Sys.Date()) %>%
    mutate(across(where(is.character), str_replace_all, pattern = "Los Angeles Clippers", replacement = "LA Clippers")) %>%
    select(3,6,5,4) %>%
    pivot_wider(names_from = bet_type, values_from = line) %>%
    drop_na() %>%
    select(1:3,5,4)

odds_mainbook <- odds %>%
    filter(book == "DraftKings") %>%
    select(-book)

plays <- plays_today %>%
    left_join(odds_mainbook, by = "team") %>%
    select(1:5, 33, 34, 35, 6:32)

plays_raw <- plays


### Edges ----
plays$kendall_spread_edge <- with(plays, kendall_margin + spread)
plays$kendall_ml_edge <- with(plays, kendall_win - round((if_else(ml<0,((ml*-1)/((ml*-1)+100)),(100/(ml+100)))), 3))
plays$kendall_over_edge <- with(plays, kendall_total - total)
plays$kendall_under_edge <- with(plays, total - kendall_total)

plays$tyra_spread_edge <- with(plays, tyra_margin + spread)
plays$tyra_ml_edge <- with(plays, tyra_win - round((if_else(ml<0,((ml*-1)/((ml*-1)+100)),(100/(ml+100)))), 3))
plays$tyra_over_edge <- with(plays, tyra_total - total)
plays$tyra_under_edge <- with(plays, total - tyra_total)

plays$gisele_spread_edge <- with(plays, gisele_margin + spread)
plays$gisele_ml_edge <- with(plays, gisele_win - round((if_else(ml<0,((ml*-1)/((ml*-1)+100)),(100/(ml+100)))), 3))
plays$gisele_over_edge <- with(plays, gisele_total - total)
plays$gisele_under_edge <- with(plays, total - gisele_total)

plays$chrissy_spread_edge <- with(plays, chrissy_margin + spread)
plays$chrissy_ml_edge <- with(plays, chrissy_win - round((if_else(ml<0,((ml*-1)/((ml*-1)+100)),(100/(ml+100)))), 3))
plays$chrissy_over_edge <- with(plays, chrissy_total - total)
plays$chrissy_under_edge <- with(plays, total - chrissy_total)

plays$kate_spread_edge <- with(plays, kate_margin + spread)
plays$kate_ml_edge <- with(plays, kate_win - round((if_else(ml<0,((ml*-1)/((ml*-1)+100)),(100/(ml+100)))), 3))
plays$kate_over_edge <- with(plays, kate_total - total)
plays$kate_under_edge <- with(plays, total - kate_total)

plays$cindy_spread_edge <- with(plays, cindy_margin + spread)
plays$cindy_ml_edge <- with(plays, cindy_win - round((if_else(ml<0,((ml*-1)/((ml*-1)+100)),(100/(ml+100)))), 3))
plays$cindy_over_edge <- with(plays, cindy_total - total)
plays$cindy_under_edge <- with(plays, total - cindy_total)

plays$naomi_spread_edge <- with(plays, naomi_margin + spread)
plays$naomi_ml_edge <- with(plays, naomi_win - round((if_else(ml<0,((ml*-1)/((ml*-1)+100)),(100/(ml+100)))), 3))
plays$naomi_over_edge <- with(plays, naomi_total - total)
plays$naomi_under_edge <- with(plays, total - naomi_total)

plays$heidi_spread_edge <- with(plays, heidi_margin + spread)
plays$heidi_ml_edge <- with(plays, heidi_win - round((if_else(ml<0,((ml*-1)/((ml*-1)+100)),(100/(ml+100)))), 3))
plays$heidi_over_edge <- with(plays, heidi_total - total)
plays$heidi_under_edge <- with(plays, total - heidi_total)

plays$adriana_spread_edge <- with(plays, adriana_margin + spread)
plays$adriana_ml_edge <- with(plays, adriana_win - round((if_else(ml<0,((ml*-1)/((ml*-1)+100)),(100/(ml+100)))), 3))
plays$adriana_over_edge <- with(plays, adriana_total - total)
plays$adriana_under_edge <- with(plays, total - adriana_total)

plays_edges <- plays %>% select(1:8,36:71)


### Bets ----
plays$spread_play <- with(plays, if_else(plays[spread_model] >= spread_key, 1, 0))
plays$ml_play <- with(plays, if_else(plays[ml_model] >= ml_key, 1, 0))
plays$over_play <- with(plays, if_else(plays[over_model] >= over_key, 0, 0)) # not playing overs atm
plays$under_play <- with(plays, if_else(plays[under_model] >= under_key, 1, 0))


bets <- plays %>%
    filter(spread_play == 1 | ml_play == 1 | over_play == 1 | under_play == 1) %>%
    mutate(spread_bet = if_else(spread_play == 1, naomi_spread_edge, 0)) %>% # change model here ****
    mutate(ml_bet = if_else(ml_play == 1, cindy_spread_edge, 0)) %>% # change model here ****
    mutate(over_bet = if_else(over_play == 1, adriana_over_edge, 0)) %>% # change model here ****
    mutate(under_bet = if_else(under_play == 1, heidi_under_edge, 0)) %>% # change model here ****
    rename(spread_odds = spread, ml_odds = ml, total_odds = total) %>%
    select(1:8, 76:79) %>%
    pivot_longer(cols = 9:12, names_to = c("bet_type", ".value"),  names_sep="_") %>%
    filter(bet > 0) %>%
    mutate(bet_type = str_to_title(bet_type)) %>%
    mutate(bet_type = str_replace(bet_type, pattern = "Ml", replacement = "ML")) %>%
    mutate(
        line = case_when(
            bet_type == "Spread" ~ spread_odds,
            bet_type == "ML" ~ ml_odds,
            bet_type == "Over" ~ total_odds,
            bet_type == "Under" ~ total_odds
        )) %>%
    select(1:5, 9, 11, 10) %>%
    rename(edge = bet) %>%
    filter(bet_type %in% c("Spread", "ML") | (bet_type == "Over" | bet_type == "Under") & loc == "H")


### Official Plays ----
# official_plays <- bets %>%
#     mutate(remove_col = paste0(team, "_", bet_type)) %>%
#     filter(!remove_col %in% c("Portland Trail Blazers_ML")) %>%
#     select(-9)

official_plays <- bets


plays_raw
plays_edges
plays
bets
official_plays


### Plays to DB ----
NBAdb <- DBI::dbConnect(RSQLite::SQLite(),
                        "/Users/Jesse/Documents/MyStuff/NBA Betting/NBAdb/NBAdb.sqlite")

DBI::dbWriteTable(NBAdb, "Plays", plays, append = T)

DBI::dbDisconnect(NBAdb)
    

### Clean Environment ----
rm(list=ls()[! ls() %in% c("away_final_wt","home_final_wt","league_avg","standings",
                           "away_rank","home_rank","season_final","away_final","home_final",
                           "slate", "all_models","score_imp", "win_imp",
                           "kendall_predict", "tyra_predict", "gisele_predict",
                           "chrissy_predict","kate_predict", "cindy_predict", 
                           "naomi_predict", "heidi_predict", "adriana_predict",
                           "plays_raw", "plays_edges", "bets", "plays",
                           "official_plays")])

print("Plays Complete")