### Results Book ----

library(tidyverse)
library(lubridate)
library(data.table)
library(nbastatR)

options(scipen = 999)

plays_db <- dplyr::tbl(DBI::dbConnect(RSQLite::SQLite(), "../NBAdb/nba_db.sqlite"), "Plays") %>%
    collect() %>%
    mutate(date = as_date(date, origin ="1970-01-01"))

yd <- max(plays_db$date)
# yd <- as_date(Sys.Date()-1)
# yd <- as_date("2022-12-17")

nba_plays <- plays_db %>% filter(date == yd)

dataGameLogsTeam <- game_logs(seasons = 2023, result_types = "team", season_types = "Regular Season")

results_scores <- dataGameLogsTeam %>%
    arrange(dateGame,idGame) %>%
    mutate(dateGame = as_date(dateGame)) %>%
    left_join(dataGameLogsTeam, 
              by = c("idGame" = "idGame", "slugTeam" = "slugOpponent")) %>%
    select(6,5,13,54,8,90,45) %>%
    filter(dateGame.x == yd)

colnames(results_scores) <- c("game_id", "date", "loc", "opp_team", "team", 
                              "opp_score", "team_score") 


yd_results <- nba_plays %>% 
    left_join(results_scores, by = c("game_id" = "game_id", "date" = "date",
                                     "loc" = "loc", "opp_team" = "opp_team",
                                     "team" = "team")
    ) %>% select(1:5, 76, 77, 6:75)

yd_results

results_book <- yd_results %>%
    mutate(margin = team_score - opp_score) %>%
    mutate(result = if_else(team_score > opp_score, "W", "L" )) %>%
    mutate(game_total = team_score + opp_score) %>%
    mutate(ats_margin = margin + spread) %>%
    mutate(ats_result = if_else((margin + spread) == 0, 0, if_else(ats_margin > 0, 1, -1.1))) %>%
    mutate(ml_result = case_when(ml > 0 & (team_score - opp_score) > 0 ~ ml/100, 
                                 ml > 0 & (team_score - opp_score) < 0 ~ -1,
                                 (team_score - opp_score) > 0 ~ 1,
                                 (team_score - opp_score) < 0 ~ ml/100)) %>%
    mutate(over_result = if_else((game_total - total) == 0, 0, if_else(game_total > total, 1, -1.1))) %>%
    mutate(under_result = if_else((game_total - total) == 0, 0, if_else(game_total < total, 1, -1.1))) %>%
    select(1:10, 78:85, 11:77)


results_book$kendall_spread_result <- with(results_book, ifelse(kendall_spread_edge>0,ats_result,0))
results_book$kendall_ml_result <- with(results_book, ifelse(kendall_ml_edge>0,ml_result,0))
results_book$kendall_over_result <- with(results_book, ifelse(kendall_over_edge>0,over_result,0))
results_book$kendall_under_result <- with(results_book, ifelse(kendall_under_edge>0,under_result,0))

results_book$tyra_spread_result <- with(results_book, ifelse(tyra_spread_edge>0,ats_result,0))
results_book$tyra_ml_result <- with(results_book, ifelse(tyra_ml_edge>0,ml_result,0))
results_book$tyra_over_result <- with(results_book, ifelse(tyra_over_edge>0,over_result,0))
results_book$tyra_under_result <- with(results_book, ifelse(tyra_under_edge>0,under_result,0))

results_book$gisele_spread_result <- with(results_book, ifelse(gisele_spread_edge>0,ats_result,0))
results_book$gisele_ml_result <- with(results_book, ifelse(gisele_ml_edge>0,ml_result,0))
results_book$gisele_over_result <- with(results_book, ifelse(gisele_over_edge>0,over_result,0))
results_book$gisele_under_result <- with(results_book, ifelse(gisele_under_edge>0,under_result,0))

results_book$chrissy_spread_result <- with(results_book, ifelse(chrissy_spread_edge>0,ats_result,0))
results_book$chrissy_ml_result <- with(results_book, ifelse(chrissy_ml_edge>0,ml_result,0))
results_book$chrissy_over_result <- with(results_book, ifelse(chrissy_over_edge>0,over_result,0))
results_book$chrissy_under_result <- with(results_book, ifelse(chrissy_under_edge>0,under_result,0))

results_book$kate_spread_result <- with(results_book, ifelse(kate_spread_edge>0,ats_result,0))
results_book$kate_ml_result <- with(results_book, ifelse(kate_ml_edge>0,ml_result,0))
results_book$kate_over_result <- with(results_book, ifelse(kate_over_edge>0,over_result,0))
results_book$kate_under_result <- with(results_book, ifelse(kate_under_edge>0,under_result,0))

results_book$cindy_spread_result <- with(results_book, ifelse(cindy_spread_edge>0,ats_result,0))
results_book$cindy_ml_result <- with(results_book, ifelse(cindy_ml_edge>0,ml_result,0))
results_book$cindy_over_result <- with(results_book, ifelse(cindy_over_edge>0,over_result,0))
results_book$cindy_under_result <- with(results_book, ifelse(cindy_under_edge>0,under_result,0))

results_book$naomi_spread_result <- with(results_book, ifelse(naomi_spread_edge>0,ats_result,0))
results_book$naomi_ml_result <- with(results_book, ifelse(naomi_ml_edge>0,ml_result,0))
results_book$naomi_over_result <- with(results_book, ifelse(naomi_over_edge>0,over_result,0))
results_book$naomi_under_result <- with(results_book, ifelse(naomi_under_edge>0,under_result,0))

results_book$heidi_spread_result <- with(results_book, ifelse(heidi_spread_edge>0,ats_result,0))
results_book$heidi_ml_result <- with(results_book, ifelse(heidi_ml_edge>0,ml_result,0))
results_book$heidi_over_result <- with(results_book, ifelse(heidi_over_edge>0,over_result,0))
results_book$heidi_under_result <- with(results_book, ifelse(heidi_under_edge>0,under_result,0))

results_book$adriana_spread_result <- with(results_book, ifelse(adriana_spread_edge>0,ats_result,0))
results_book$adriana_ml_result <- with(results_book, ifelse(adriana_ml_edge>0,ml_result,0))
results_book$adriana_over_result <- with(results_book, ifelse(adriana_over_edge>0,over_result,0))
results_book$adriana_under_result <- with(results_book, ifelse(adriana_under_edge>0,under_result,0))

results_book <- results_book %>% select(1:81, 86:121, 82:85)
results_book

### Results Book to DB ----
NBAdb <- DBI::dbConnect(RSQLite::SQLite(), "../NBAdb/nba_db.sqlite")

DBI::dbWriteTable(NBAdb, "ResultsBook", results_book, append = T)

DBI::dbDisconnect(NBAdb)


### Edge Analysis ----
results_book <- dplyr::tbl(DBI::dbConnect(RSQLite::SQLite(), "../NBAdb/nba_db.sqlite"), "ResultsBook") %>%
    collect() %>%
    mutate(date = as_date(date, origin ="1970-01-01"))

# results_book <- results_book %>%
#     filter(date >= max(date) - 20)

# add wager column for calculating ml roi
results_book <- results_book %>%
    mutate(ml_wager = ifelse(ml < 100, ml/-100, 1))


### Spread Key ----
names_spread <- names(results_book %>% select(ends_with("spread_edge")))

results_spread_list <- list()
for (i in seq_along(names_spread)) {
    nms <- names_spread[[i]]
    results_spread_list[[paste0(nms, collapse = "_")]] <- results_book %>% 
        filter(if_all(all_of(nms), ~ .> 0)) %>%
        select("date","loc","opp_team","team",names_spread[[i]],"ats_result") %>%
        arrange(desc(select(.,ends_with("edge")))) %>%
        mutate(cume = round(cumsum(ats_result),1)) %>%
        mutate(game_num = row_number()) %>%
        mutate(roi = round((cume /(game_num*1.1))*100,2)) %>%
        select(1:7,9,8)
    
}

peak_spread_list <- list()
for (j in seq_along(results_spread_list)) {
    x <- results_spread_list[[j]][order(-results_spread_list[[j]][,7]), ]
    peak_spread_list[[j]] <- as.data.table(head(x,1))
}

peak_spread <- rbindlist(peak_spread_list, fill = TRUE, idcol = F) %>%
    select(1,9,7:8,5,10:17)

peak_spread_filtered <- peak_spread %>%
    pivot_longer(cols = !date:roi, names_to = "model", values_to = "key") %>%
    arrange(desc(cume)) %>%
    mutate(model = gsub("([a-z]*_[a-z]*)(.*)", "\\1", model)) %>%
    drop_na()


### ML Key ----
names_ml <- names(results_book %>% select(ends_with("ml_edge")))

results_ml_list <- list()
for (i in seq_along(names_ml)) {
    nms <- names_ml[[i]]
    results_ml_list[[paste0(nms, collapse = "_")]] <- results_book %>% 
        filter(if_all(all_of(nms), ~ .> 0)) %>%
        select("date","loc","opp_team","team",names_ml[[i]],"ml_result","ml_wager") %>%
        arrange(desc(select(.,ends_with("edge")))) %>%
        mutate(cume = round(cumsum(ml_result),1)) %>%
        mutate(game_num = row_number()) %>%
        mutate(roi = round((cume / cumsum(ml_wager))*100,2)) %>%
        select(1:6,8,10,9)
    
}

peak_ml_list <- list()
for (j in seq_along(results_ml_list)) {
    x <- results_ml_list[[j]][order(-results_ml_list[[j]][,7]), ]
    peak_ml_list[[j]] <- as.data.table(head(x,1))
}

peak_ml <- rbindlist(peak_ml_list, fill = TRUE, idcol = F) %>%
    select(1,9,7:8,5,10:17)

peak_ml_filtered <- peak_ml %>%
    pivot_longer(cols = !date:roi, names_to = "model", values_to = "key") %>%
    arrange(desc(cume)) %>%
    mutate(model = gsub("([a-z]*_[a-z]*)(.*)", "\\1", model)) %>%
    drop_na()


### Over Key ----
names_over <- names(results_book %>% select(ends_with("over_edge")))

results_over_list <- list()
for (i in seq_along(names_over)) {
    nms <- names_over[[i]]
    results_over_list[[paste0(nms, collapse = "_")]] <- results_book %>% 
        filter(if_all(all_of(nms), ~ .> 0)) %>%
        select("date","loc","opp_team","team",names_over[[i]],"over_result") %>%
        arrange(desc(select(.,ends_with("edge")))) %>%
        mutate(cume = round(cumsum(over_result),1)) %>%
        mutate(game_num = row_number()) %>%
        mutate(roi = round((cume /(game_num*1.1))*100,2)) %>%
        select(1:7,9,8)
    
}

peak_over_list <- list()
for (j in seq_along(results_over_list)) {
    x <- results_over_list[[j]][order(-results_over_list[[j]][,7]), ]
    peak_over_list[[j]] <- as.data.table(head(x,1))
}

peak_over <- rbindlist(peak_over_list, fill = TRUE, idcol = F) %>%
    select(1,9,7:8,5,10:17)

peak_over_filtered <- peak_over %>%
    pivot_longer(cols = !date:roi, names_to = "model", values_to = "key") %>%
    arrange(desc(cume)) %>%
    mutate(model = gsub("([a-z]*_[a-z]*)(.*)", "\\1", model)) %>%
    drop_na()


### Under Key ----
names_under <- names(results_book %>% select(ends_with("under_edge")))

results_under_list <- list()
for (i in seq_along(names_under)) {
    nms <- names_under[[i]]
    results_under_list[[paste0(nms, collapse = "_")]] <- results_book %>% 
        filter(if_all(all_of(nms), ~ .> 0)) %>%
        select("date","loc","opp_team","team",names_under[[i]],"under_result") %>%
        arrange(desc(select(.,ends_with("edge")))) %>%
        mutate(cume = round(cumsum(under_result),1)) %>%
        mutate(game_num = row_number()) %>%
        mutate(roi = round((cume /(game_num*1.1))*100,2)) %>%
        select(1:7,9,8)
    
}

peak_under_list <- list()
for (j in seq_along(results_under_list)) {
    x <- results_under_list[[j]][order(-results_under_list[[j]][,7]), ]
    peak_under_list[[j]] <- as.data.table(head(x,1))
}

peak_under <- rbindlist(peak_under_list, fill = TRUE, idcol = F) %>%
    select(1,9,7:8,5,10:17)

peak_under_filtered <- peak_under %>%
    pivot_longer(cols = !date:roi, names_to = "model", values_to = "key") %>%
    arrange(desc(cume)) %>%
    mutate(model = gsub("([a-z]*_[a-z]*)(.*)", "\\1", model)) %>%
    drop_na()


### Clean Environment ----
rm(list=ls()[! ls() %in% c("peak_spread_filtered","peak_ml_filtered",
                           "peak_over_filtered","peak_under_filtered",
                           "results_spread_list","results_ml_list",
                           "results_over_list","results_under_list")])

print("Results Edges Complete")

# notes ----
# looking great through 11/13
# nn is unreal through 11/26
# heidi 12/16-12/31
# gisele 12/16-12/31
# heidi 12/18-01/02 - spread and ML

