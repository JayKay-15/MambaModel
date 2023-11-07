### OddsR ----

library(tidyverse)
library(theoddsapi)
# library(oddsapiR) # https://oddsapir.sportsdataverse.org/reference/index.html

Sys.setenv(THEODDS_API_KEY = "9f158680c234e152e9df4027f4eccf1f")

get_sports()
get_remaining_requests()
all_spread <- get_odds("basketball_nba", mkt = "spreads")
all_total <- get_odds("basketball_nba", mkt = "totals")
all_ml <- get_odds("basketball_nba", mkt = "h2h") %>%
    mutate(h2h = round(if_else(h2h >= 2, (h2h-1)*100, (-100)/(h2h-1)),0))

book_spread <- all_spread %>%
    mutate(commence_time = as_date(commence_time)) %>%
    filter(commence_time == Sys.Date() & book == "DraftKings") %>%
    rename(line = spreads) %>%
    mutate(bet_type = "spread")

book_total <- all_total %>%
    mutate(commence_time = as_date(commence_time)) %>%
    filter(commence_time == Sys.Date() & book == "DraftKings") %>%
    rename(line = totals) %>%
    mutate(bet_type = "total")

book_ml <- all_ml %>%
    mutate(commence_time = as_date(commence_time)) %>%
    filter(commence_time == Sys.Date() & book == "DraftKings") %>%
    rename(line = h2h) %>%
    mutate(bet_type = "ml")

all_odds <- bind_rows(book_spread, book_total, book_ml)  %>%
    pivot_wider(names_from = bet_type,
                values_from = line) %>%
    select(team, spread, ml, total)

teams_away <- nba_schedule_current %>%
    filter(game_date == Sys.Date()) %>%
    select(away_team_name) %>%
    left_join(all_odds, by = c("away_team_name" = "team")) %>%
    rename(away_spread = spread,
           away_moneyline = ml)

teams_home <- nba_schedule_current %>%
    filter(game_date == Sys.Date()) %>%
    select(home_team_name) %>%
    left_join(all_odds, by = c("home_team_name" = "team")) %>%
    rename(home_spread = spread,
           home_moneyline = ml,
           over_under = total)

final_odds <- bind_cols(teams_away, teams_home) %>%
    select(away_team_name, home_team_name, away_spread, home_spread,
           away_moneyline, home_moneyline, over_under)

odds_wpo <- final_odds %>%
    mutate(away_moneyline = odds.converter::odds.us2dec(away_moneyline),
           home_moneyline = odds.converter::odds.us2dec(home_moneyline)) %>%
    select(away_moneyline, home_moneyline)

odds_wpo <- implied::implied_probabilities(odds_wpo, method = 'wpo')

final_odds <- final_odds %>%
    mutate(away_implied_prob = odds_wpo$probabilities[,1])



hist_odds %>%
    filter(book == "DraftKings" & bet_type == "spread")
hist_odds %>%
    filter(book == "DraftKings" & bet_type == "ml")
hist_odds %>%
    filter(book == "DraftKings" & bet_type == "total")


### Add to DB ----
NBAdb <- DBI::dbConnect(RSQLite::SQLite(), "../nba_sql_db/nba_db")
DBI::dbWriteTable(NBAdb, "Odds", hist_odds, append = T)
DBI::dbDisconnect(NBAdb)

print("OddsR Complete")




get_odds <- function(book_name) {
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
        filter(commence_time == Sys.Date() & book == book_name) %>%
        rename(line = spreads) %>%
        mutate(bet_type = "spread")
    
    book_total <- all_total %>%
        mutate(commence_time = as_date(commence_time)) %>%
        filter(commence_time == Sys.Date() & book == book_name) %>%
        rename(line = totals) %>%
        mutate(bet_type = "total")
    
    book_ml <- all_ml %>%
        mutate(commence_time = as_date(commence_time)) %>%
        filter(commence_time == Sys.Date() & book == book_name) %>%
        rename(line = h2h) %>%
        mutate(bet_type = "ml")
    
    all_odds <- bind_rows(book_spread, book_total, book_ml)  %>%
        pivot_wider(names_from = bet_type,
                    values_from = line) %>%
        select(team, spread, ml, total)
    
    teams_away <- nba_schedule_current %>%
        filter(game_date == Sys.Date()) %>%
        select(away_team_name) %>%
        left_join(all_odds, by = c("away_team_name" = "team")) %>%
        rename(away_spread = spread,
               away_moneyline = ml)
    
    teams_home <- nba_schedule_current %>%
        filter(game_date == Sys.Date()) %>%
        select(home_team_name) %>%
        left_join(all_odds, by = c("home_team_name" = "team")) %>%
        rename(home_spread = spread,
               home_moneyline = ml,
               over_under = total)
    
    final_odds <- bind_cols(teams_away, teams_home) %>%
        select(away_team_name, home_team_name, away_spread, home_spread,
               away_moneyline, home_moneyline, over_under)
    
    odds_wpo <- final_odds %>%
        mutate(away_moneyline = odds.converter::odds.us2dec(away_moneyline),
               home_moneyline = odds.converter::odds.us2dec(home_moneyline)) %>%
        select(away_moneyline, home_moneyline)
    
    odds_wpo <- implied::implied_probabilities(odds_wpo, method = 'wpo')
    
    final_odds <- final_odds %>%
        mutate(away_implied_prob = odds_wpo$probabilities[, 1])
    
    return(final_odds)
}

# Usage:
book_name <- "DraftKings"
final_odds <- get_odds(book_name)





