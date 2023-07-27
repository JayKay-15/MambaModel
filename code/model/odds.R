### OddsR ----

library(tidyverse)
library(lubridate)
library(theoddsapi)

Sys.setenv(THEODDS_API_KEY = "9f158680c234e152e9df4027f4eccf1f")

get_sports()
get_remaining_requests()
all_spread <- get_odds("basketball_nba", mkt = "spreads")
all_total <- get_odds("basketball_nba", mkt = "totals")
all_ml <- get_odds("basketball_nba", mkt = "h2h") %>%
    mutate(h2h = round(if_else(h2h >= 2, (h2h-1)*100, (-100)/(h2h-1)),0))

hist_spread <- all_spread %>%
    mutate(commence_time = as_date(commence_time)) %>%
    filter(commence_time == Sys.Date()) %>%
    rename(line = spreads) %>%
    mutate(bet_type = "spread")

hist_total <- all_total %>%
    mutate(commence_time = as_date(commence_time)) %>%
    filter(commence_time == Sys.Date()) %>%
    rename(line = totals) %>%
    mutate(bet_type = "total")

hist_ml <- all_ml %>%
    mutate(commence_time = as_date(commence_time)) %>%
    filter(commence_time == Sys.Date()) %>%
    rename(line = h2h) %>%
    mutate(bet_type = "ml")

hist_odds <- bind_rows(hist_spread,hist_total,hist_ml)

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