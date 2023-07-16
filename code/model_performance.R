### Model Performance ----
library(tidyverse)
library(lubridate)
library(data.table)
library(gt)

results_book <- dplyr::tbl(DBI::dbConnect(RSQLite::SQLite(), "../NBAdb/nba_db.sqlite"), "ResultsBook") %>%
    collect() %>%
    mutate(date = as_date(date, origin ="1970-01-01"))

# add wager column for calculating ml roi
results_book <- results_book %>%
    mutate(ml_wager = ifelse(ml < 100, ml/-100, 1))

# fixed keys
results_book %>%
    filter(heidi_spread_edge >= 0) %>%
    group_by(date) %>%
    summarise(day_total_2022 = sum(heidi_spread_result)) %>%
    mutate(cume_2022 = cumsum(day_total_2022)) %>%
    add_row(date = min(results_book$date)-1, day_total_2022 = 0, cume_2022 = 0) %>%
    arrange(date) %>%
    ggplot(aes(date)) +
    geom_line(aes(y=cume_2022), size=1.24, group=1, color = "darkblue") +
    labs(x = "Date", y = "Units", title = "Neural Net Model Performance", subtitle = "Spread") +
    scale_x_date(date_breaks = "7 days") +
    theme_bw()



### models summary ----
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
        mutate(model = gsub("([a-z]*)(.*)", "\\1", as.character(nms))) %>%
        mutate(type = "spread") %>%
        mutate(wager = 1.1) %>%
        rename("edge" = as.character(nms),
               "result" = "ats_result") %>%
        select(1:7,9,10,11,12)
    
}

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
        mutate(model = gsub("([a-z]*)(.*)", "\\1", as.character(nms))) %>%
        mutate(type = "ml") %>%
        rename("edge" = as.character(nms),
               "result" = "ml_result",
               "wager" = "ml_wager") %>%
        select(1:6,8,10,11,12,7)
    
}

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
        mutate(model = gsub("([a-z]*)(.*)", "\\1", as.character(nms))) %>%
        mutate(type = "over") %>%
        mutate(wager = 1.1) %>%
        rename("edge" = as.character(nms),
               "result" = "over_result") %>%
        select(1:7,9,10,11,12)
    
}

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
        mutate(model = gsub("([a-z]*)(.*)", "\\1", as.character(nms))) %>%
        mutate(type = "under") %>%
        mutate(wager = 1.1) %>%
        rename("edge" = as.character(nms),
               "result" = "under_result") %>%
        select(1:7,9,10,11,12)
}

### no keys ----
results_df <- rbindlist(l = c(results_spread_list, results_ml_list, results_over_list, results_under_list))

results_df %>%
    group_by(model, type) %>%
    summarise(result = sum(result), game_num = n(), wager = sum(wager)) %>%
    mutate(roi = round((result/wager),4)) %>%
    arrange(desc(result)) %>%
    select(type, model, result, roi, game_num) %>%
    gt(groupname_col = "type") %>%
    row_group_order(groups = c("spread", "ml", "over", "under")) %>%
    fmt_percent(columns = roi, decimals = 1) %>%
    fmt_number(columns = result, decimals = 1) %>%
    tab_header(
        title = "Model Summary by Bet Type",
        subtitle = "All Plays"
    ) %>%
    cols_label(
        model = "Model",
        result = "Units",
        roi = "ROI",
        game_num = "Games",
    ) %>%
    cols_align(
        align = "center"
    ) %>%
    tab_source_note(
        source_note = "@MambaMetrics"
    ) %>%
    gtExtras::gt_theme_538()


### use results_edges for viz
viz_spread <- peak_spread_filtered %>%
    arrange(model)
viz_ml <- peak_ml_filtered %>%
    arrange(model)
viz_over <- peak_over_filtered %>%
    arrange(model)
viz_under <- peak_under_filtered %>%
    arrange(model)


### keys ----
results_gt <- rbind(viz_spread, viz_ml, viz_over, viz_under)

results_gt %>%
    mutate(roi = roi/100,
           type = word(model, 2, sep = "_"),
           model = gsub("([a-z]*)(.*)", "\\1", model)) %>%
    select(type, model, cume, roi, game_num, key) %>%
    arrange(desc(cume)) %>%
    gt(groupname_col = "type") %>%
    row_group_order(groups = c("spread", "ml", "over", "under")) %>%
    fmt_percent(columns = roi, decimals = 1) %>%
    fmt_percent(columns = key, rows = c(type == "ml"), decimals = 1) %>%
    fmt_number(columns = key, decimals = 2) %>%
    tab_header(
        title = "Model Summary by Bet Type",
        subtitle = "Filtered by Model Specific Keys"
    ) %>%
    cols_label(
        model = "Model",
        cume = "Units",
        roi = "ROI",
        game_num = "Games",
        key = "Key"
    ) %>%
    cols_align(
        align = "center"
    ) %>%
    tab_source_note(
        source_note = "@MambaMetrics"
    ) %>%
    gtExtras::gt_theme_538()

# Table: @MambaMetrics | Data: nba.com | Plot: @MambaMetrics

### spread line ----
adriana_spread_line <- results_book %>%
    filter(adriana_spread_edge >= as.numeric(viz_spread[1,6])) %>%
    group_by(date) %>%
    summarise(day_total_2022 = sum(adriana_spread_result)) %>%
    mutate(cume_2022 = cumsum(day_total_2022)) %>%
    add_row(date = min(results_book$date)-1, day_total_2022 = 0, cume_2022 = 0) %>%
    arrange(date) %>%
    mutate(model = "adriana - ensemble")

chrissy_spread_line <- results_book %>%
    filter(chrissy_spread_edge >= as.numeric(viz_spread[2,6])) %>%
    group_by(date) %>%
    summarise(day_total_2022 = sum(chrissy_spread_result)) %>%
    mutate(cume_2022 = cumsum(day_total_2022)) %>%
    add_row(date = min(results_book$date)-1, day_total_2022 = 0, cume_2022 = 0) %>%
    arrange(date) %>%
    mutate(model = "chrissy - knn")

cindy_spread_line <- results_book %>%
    filter(cindy_spread_edge >= as.numeric(viz_spread[3,6])) %>%
    group_by(date) %>%
    summarise(day_total_2022 = sum(cindy_spread_result)) %>%
    mutate(cume_2022 = cumsum(day_total_2022)) %>%
    add_row(date = min(results_book$date)-1, day_total_2022 = 0, cume_2022 = 0) %>%
    arrange(date) %>%
    mutate(model = "cindy - svm")

gisele_spread_line <- results_book %>%
    filter(gisele_spread_edge >= as.numeric(viz_spread[4,6])) %>%
    group_by(date) %>%
    summarise(day_total_2022 = sum(gisele_spread_result)) %>%
    mutate(cume_2022 = cumsum(day_total_2022)) %>%
    add_row(date = min(results_book$date)-1, day_total_2022 = 0, cume_2022 = 0) %>%
    arrange(date) %>%
    mutate(model = "gisele - ridge")

heidi_spread_line <- results_book %>%
    filter(heidi_spread_edge >= as.numeric(viz_spread[5,6])) %>%
    group_by(date) %>%
    summarise(day_total_2022 = sum(heidi_spread_result)) %>%
    mutate(cume_2022 = cumsum(day_total_2022)) %>%
    add_row(date = min(results_book$date)-1, day_total_2022 = 0, cume_2022 = 0) %>%
    arrange(date) %>%
    mutate(model = "heidi - xgb")

kate_spread_line <- results_book %>%
    filter(kate_spread_edge >= as.numeric(viz_spread[6,6])) %>%
    group_by(date) %>%
    summarise(day_total_2022 = sum(kate_spread_result)) %>%
    mutate(cume_2022 = cumsum(day_total_2022)) %>%
    add_row(date = min(results_book$date)-1, day_total_2022 = 0, cume_2022 = 0) %>%
    arrange(date) %>%
    mutate(model = "kate - rf")

kendall_spread_line <- results_book %>%
    filter(kendall_spread_edge >= as.numeric(viz_spread[7,6])) %>%
    group_by(date) %>%
    summarise(day_total_2022 = sum(kendall_spread_result)) %>%
    mutate(cume_2022 = cumsum(day_total_2022)) %>%
    add_row(date = min(results_book$date)-1, day_total_2022 = 0, cume_2022 = 0) %>%
    arrange(date) %>%
    mutate(model = "kendall - simple")

naomi_spread_line <- results_book %>%
    filter(naomi_spread_edge >= as.numeric(viz_spread[8,6])) %>%
    group_by(date) %>%
    summarise(day_total_2022 = sum(naomi_spread_result)) %>%
    mutate(cume_2022 = cumsum(day_total_2022)) %>%
    add_row(date = min(results_book$date)-1, day_total_2022 = 0, cume_2022 = 0) %>%
    arrange(date) %>%
    mutate(model = "naomi - nn")

tyra_spread_line <- results_book %>%
    filter(tyra_spread_edge >= as.numeric(viz_spread[9,6])) %>%
    group_by(date) %>%
    summarise(day_total_2022 = sum(tyra_spread_result)) %>%
    mutate(cume_2022 = cumsum(day_total_2022)) %>%
    add_row(date = min(results_book$date)-1, day_total_2022 = 0, cume_2022 = 0) %>%
    arrange(date) %>%
    mutate(model = "tyra - ls")


spread_line <- bind_rows(adriana_spread_line,chrissy_spread_line,cindy_spread_line,gisele_spread_line,
                               heidi_spread_line,kate_spread_line,kendall_spread_line,naomi_spread_line,
                               tyra_spread_line)

spread_line$model <- factor(spread_line$model, 
                                  levels=c("adriana - ensemble","chrissy - knn","cindy - svm",
                                           "gisele - ridge","heidi - xgb","kate - rf",
                                           "kendall - simple","naomi - nn","tyra - ls"))

spread_line %>%
    group_by(date, model) %>%
    ggplot(aes(x=date, y=cume_2022, color=model)) +
    geom_line(size=1.24) +
    scale_color_brewer(palette="Paired") + 
    labs(title = "Spread Bets", subtitle = "2022 Performance") +
    scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
    xlab("") + 
    ylab("Units") +
    theme_bw()


### ml line ----
adriana_ml_line <- results_book %>%
    filter(adriana_ml_edge >= as.numeric(viz_ml[1,5])) %>%
    group_by(date) %>%
    summarise(day_total_2022 = sum(adriana_ml_result)) %>%
    mutate(cume_2022 = cumsum(day_total_2022)) %>%
    add_row(date = min(results_book$date)-1, day_total_2022 = 0, cume_2022 = 0) %>%
    arrange(date) %>%
    mutate(model = "adriana - ensemble")

chrissy_ml_line <- results_book %>%
    filter(chrissy_ml_edge >= as.numeric(viz_ml[2,5])) %>%
    group_by(date) %>%
    summarise(day_total_2022 = sum(chrissy_ml_result)) %>%
    mutate(cume_2022 = cumsum(day_total_2022)) %>%
    add_row(date = min(results_book$date)-1, day_total_2022 = 0, cume_2022 = 0) %>%
    arrange(date) %>%
    mutate(model = "chrissy - knn")

cindy_ml_line <- results_book %>%
    filter(cindy_ml_edge >= as.numeric(viz_ml[3,5])) %>%
    group_by(date) %>%
    summarise(day_total_2022 = sum(cindy_ml_result)) %>%
    mutate(cume_2022 = cumsum(day_total_2022)) %>%
    add_row(date = min(results_book$date)-1, day_total_2022 = 0, cume_2022 = 0) %>%
    arrange(date) %>%
    mutate(model = "cindy - svm")

gisele_ml_line <- results_book %>%
    filter(gisele_ml_edge >= as.numeric(viz_ml[4,5])) %>%
    group_by(date) %>%
    summarise(day_total_2022 = sum(gisele_ml_result)) %>%
    mutate(cume_2022 = cumsum(day_total_2022)) %>%
    add_row(date = min(results_book$date)-1, day_total_2022 = 0, cume_2022 = 0) %>%
    arrange(date) %>%
    mutate(model = "gisele - ridge")

heidi_ml_line <- results_book %>%
    filter(heidi_ml_edge >= as.numeric(viz_ml[5,5])) %>%
    group_by(date) %>%
    summarise(day_total_2022 = sum(heidi_ml_result)) %>%
    mutate(cume_2022 = cumsum(day_total_2022)) %>%
    add_row(date = min(results_book$date)-1, day_total_2022 = 0, cume_2022 = 0) %>%
    arrange(date) %>%
    mutate(model = "heidi - xgb")

kate_ml_line <- results_book %>%
    filter(kate_ml_edge >= as.numeric(viz_ml[6,5])) %>%
    group_by(date) %>%
    summarise(day_total_2022 = sum(kate_ml_result)) %>%
    mutate(cume_2022 = cumsum(day_total_2022)) %>%
    add_row(date = min(results_book$date)-1, day_total_2022 = 0, cume_2022 = 0) %>%
    arrange(date) %>%
    mutate(model = "kate - rf")

kendall_ml_line <- results_book %>%
    filter(kendall_ml_edge >= as.numeric(viz_ml[7,5])) %>%
    group_by(date) %>%
    summarise(day_total_2022 = sum(kendall_ml_result)) %>%
    mutate(cume_2022 = cumsum(day_total_2022)) %>%
    add_row(date = min(results_book$date)-1, day_total_2022 = 0, cume_2022 = 0) %>%
    arrange(date) %>%
    mutate(model = "kendall - simple")

naomi_ml_line <- results_book %>%
    filter(naomi_ml_edge >= as.numeric(viz_ml[8,5])) %>%
    group_by(date) %>%
    summarise(day_total_2022 = sum(naomi_ml_result)) %>%
    mutate(cume_2022 = cumsum(day_total_2022)) %>%
    add_row(date = min(results_book$date)-1, day_total_2022 = 0, cume_2022 = 0) %>%
    arrange(date) %>%
    mutate(model = "naomi - nn")

tyra_ml_line <- results_book %>%
    filter(tyra_ml_edge >= as.numeric(viz_ml[9,5])) %>%
    group_by(date) %>%
    summarise(day_total_2022 = sum(tyra_ml_result)) %>%
    mutate(cume_2022 = cumsum(day_total_2022)) %>%
    add_row(date = min(results_book$date)-1, day_total_2022 = 0, cume_2022 = 0) %>%
    arrange(date) %>%
    mutate(model = "tyra - ls")


ml_line <- bind_rows(adriana_ml_line,chrissy_ml_line,cindy_ml_line,gisele_ml_line,
                         heidi_ml_line,kate_ml_line,kendall_ml_line,naomi_ml_line,
                         tyra_ml_line)

ml_line$model <- factor(ml_line$model, 
                            levels=c("adriana - ensemble","chrissy - knn","cindy - svm",
                                     "gisele - ridge","heidi - xgb","kate - rf",
                                     "kendall - simple","naomi - nn","tyra - ls"))

ml_line %>%
    group_by(date, model) %>%
    ggplot(aes(x=date, y=cume_2022, color=model)) +
    geom_line(size=1.24) +
    scale_color_brewer(palette="Paired") + 
    labs(title = "ML Bets", subtitle = "2022 Performance") +
    scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
    xlab("") + 
    ylab("Units") +
    theme_bw()


### over line ----
adriana_over_line <- results_book %>%
    filter(adriana_over_edge >= as.numeric(viz_over[1,5])) %>%
    group_by(date) %>%
    summarise(day_total_2022 = sum(adriana_over_result)) %>%
    mutate(cume_2022 = cumsum(day_total_2022)) %>%
    add_row(date = min(results_book$date)-1, day_total_2022 = 0, cume_2022 = 0) %>%
    arrange(date) %>%
    mutate(model = "adriana - ensemble")

chrissy_over_line <- results_book %>%
    filter(chrissy_over_edge >= as.numeric(viz_over[2,5])) %>%
    group_by(date) %>%
    summarise(day_total_2022 = sum(chrissy_over_result)) %>%
    mutate(cume_2022 = cumsum(day_total_2022)) %>%
    add_row(date = min(results_book$date)-1, day_total_2022 = 0, cume_2022 = 0) %>%
    arrange(date) %>%
    mutate(model = "chrissy - knn")

cindy_over_line <- results_book %>%
    filter(cindy_over_edge >= as.numeric(viz_over[3,5])) %>%
    group_by(date) %>%
    summarise(day_total_2022 = sum(cindy_over_result)) %>%
    mutate(cume_2022 = cumsum(day_total_2022)) %>%
    add_row(date = min(results_book$date)-1, day_total_2022 = 0, cume_2022 = 0) %>%
    arrange(date) %>%
    mutate(model = "cindy - svm")

gisele_over_line <- results_book %>%
    filter(gisele_over_edge >= as.numeric(viz_over[4,5])) %>%
    group_by(date) %>%
    summarise(day_total_2022 = sum(gisele_over_result)) %>%
    mutate(cume_2022 = cumsum(day_total_2022)) %>%
    add_row(date = min(results_book$date)-1, day_total_2022 = 0, cume_2022 = 0) %>%
    arrange(date) %>%
    mutate(model = "gisele - ridge")

heidi_over_line <- results_book %>%
    filter(heidi_over_edge >= as.numeric(viz_over[5,5])) %>%
    group_by(date) %>%
    summarise(day_total_2022 = sum(heidi_over_result)) %>%
    mutate(cume_2022 = cumsum(day_total_2022)) %>%
    add_row(date = min(results_book$date)-1, day_total_2022 = 0, cume_2022 = 0) %>%
    arrange(date) %>%
    mutate(model = "heidi - xgb")

kate_over_line <- results_book %>%
    filter(kate_over_edge >= as.numeric(viz_over[6,5])) %>%
    group_by(date) %>%
    summarise(day_total_2022 = sum(kate_over_result)) %>%
    mutate(cume_2022 = cumsum(day_total_2022)) %>%
    add_row(date = min(results_book$date)-1, day_total_2022 = 0, cume_2022 = 0) %>%
    arrange(date) %>%
    mutate(model = "kate - rf")

kendall_over_line <- results_book %>%
    filter(kendall_over_edge >= as.numeric(viz_over[7,5])) %>%
    group_by(date) %>%
    summarise(day_total_2022 = sum(kendall_over_result)) %>%
    mutate(cume_2022 = cumsum(day_total_2022)) %>%
    add_row(date = min(results_book$date)-1, day_total_2022 = 0, cume_2022 = 0) %>%
    arrange(date) %>%
    mutate(model = "kendall - simple")

naomi_over_line <- results_book %>%
    filter(naomi_over_edge >= as.numeric(viz_over[8,5])) %>%
    group_by(date) %>%
    summarise(day_total_2022 = sum(naomi_over_result)) %>%
    mutate(cume_2022 = cumsum(day_total_2022)) %>%
    add_row(date = min(results_book$date)-1, day_total_2022 = 0, cume_2022 = 0) %>%
    arrange(date) %>%
    mutate(model = "naomi - nn")

tyra_over_line <- results_book %>%
    filter(tyra_over_edge >= as.numeric(viz_over[9,5])) %>%
    group_by(date) %>%
    summarise(day_total_2022 = sum(tyra_over_result)) %>%
    mutate(cume_2022 = cumsum(day_total_2022)) %>%
    add_row(date = min(results_book$date)-1, day_total_2022 = 0, cume_2022 = 0) %>%
    arrange(date) %>%
    mutate(model = "tyra - ls")


over_line <- bind_rows(adriana_over_line,chrissy_over_line,cindy_over_line,gisele_over_line,
                     heidi_over_line,kate_over_line,kendall_over_line,naomi_over_line,
                     tyra_over_line)

over_line$model <- factor(over_line$model, 
                        levels=c("adriana - ensemble","chrissy - knn","cindy - svm",
                                 "gisele - ridge","heidi - xgb","kate - rf",
                                 "kendall - simple","naomi - nn","tyra - ls"))

over_line %>%
    group_by(date, model) %>%
    ggplot(aes(x=date, y=cume_2022, color=model)) +
    geom_line(size=1.24) +
    scale_color_brewer(palette="Paired") + 
    labs(title = "Over Bets", subtitle = "2022 Performance") +
    scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
    xlab("") + 
    ylab("Units") +
    theme_bw()


### under line ----
adriana_under_line <- results_book %>%
    filter(adriana_under_edge >= as.numeric(viz_under[1,5])) %>%
    group_by(date) %>%
    summarise(day_total_2022 = sum(adriana_under_result)) %>%
    mutate(cume_2022 = cumsum(day_total_2022)) %>%
    add_row(date = min(results_book$date)-1, day_total_2022 = 0, cume_2022 = 0) %>%
    arrange(date) %>%
    mutate(model = "adriana - ensemble")

chrissy_under_line <- results_book %>%
    filter(chrissy_under_edge >= as.numeric(viz_under[2,5])) %>%
    group_by(date) %>%
    summarise(day_total_2022 = sum(chrissy_under_result)) %>%
    mutate(cume_2022 = cumsum(day_total_2022)) %>%
    add_row(date = min(results_book$date)-1, day_total_2022 = 0, cume_2022 = 0) %>%
    arrange(date) %>%
    mutate(model = "chrissy - knn")

cindy_under_line <- results_book %>%
    filter(cindy_under_edge >= as.numeric(viz_under[3,5])) %>%
    group_by(date) %>%
    summarise(day_total_2022 = sum(cindy_under_result)) %>%
    mutate(cume_2022 = cumsum(day_total_2022)) %>%
    add_row(date = min(results_book$date)-1, day_total_2022 = 0, cume_2022 = 0) %>%
    arrange(date) %>%
    mutate(model = "cindy - svm")

gisele_under_line <- results_book %>%
    filter(gisele_under_edge >= as.numeric(viz_under[4,5])) %>%
    group_by(date) %>%
    summarise(day_total_2022 = sum(gisele_under_result)) %>%
    mutate(cume_2022 = cumsum(day_total_2022)) %>%
    add_row(date = min(results_book$date)-1, day_total_2022 = 0, cume_2022 = 0) %>%
    arrange(date) %>%
    mutate(model = "gisele - ridge")

heidi_under_line <- results_book %>%
    filter(heidi_under_edge >= as.numeric(viz_under[5,5])) %>%
    group_by(date) %>%
    summarise(day_total_2022 = sum(heidi_under_result)) %>%
    mutate(cume_2022 = cumsum(day_total_2022)) %>%
    add_row(date = min(results_book$date)-1, day_total_2022 = 0, cume_2022 = 0) %>%
    arrange(date) %>%
    mutate(model = "heidi - xgb")

kate_under_line <- results_book %>%
    filter(kate_under_edge >= as.numeric(viz_under[6,5])) %>%
    group_by(date) %>%
    summarise(day_total_2022 = sum(kate_under_result)) %>%
    mutate(cume_2022 = cumsum(day_total_2022)) %>%
    add_row(date = min(results_book$date)-1, day_total_2022 = 0, cume_2022 = 0) %>%
    arrange(date) %>%
    mutate(model = "kate - rf")

kendall_under_line <- results_book %>%
    filter(kendall_under_edge >= as.numeric(viz_under[7,5])) %>%
    group_by(date) %>%
    summarise(day_total_2022 = sum(kendall_under_result)) %>%
    mutate(cume_2022 = cumsum(day_total_2022)) %>%
    add_row(date = min(results_book$date)-1, day_total_2022 = 0, cume_2022 = 0) %>%
    arrange(date) %>%
    mutate(model = "kendall - simple")

naomi_under_line <- results_book %>%
    filter(naomi_under_edge >= as.numeric(viz_under[8,5])) %>%
    group_by(date) %>%
    summarise(day_total_2022 = sum(naomi_under_result)) %>%
    mutate(cume_2022 = cumsum(day_total_2022)) %>%
    add_row(date = min(results_book$date)-1, day_total_2022 = 0, cume_2022 = 0) %>%
    arrange(date) %>%
    mutate(model = "naomi - nn")

tyra_under_line <- results_book %>%
    filter(tyra_under_edge >= as.numeric(viz_under[9,5])) %>%
    group_by(date) %>%
    summarise(day_total_2022 = sum(tyra_under_result)) %>%
    mutate(cume_2022 = cumsum(day_total_2022)) %>%
    add_row(date = min(results_book$date)-1, day_total_2022 = 0, cume_2022 = 0) %>%
    arrange(date) %>%
    mutate(model = "tyra - ls")


under_line <- bind_rows(adriana_under_line,chrissy_under_line,cindy_under_line,gisele_under_line,
                     heidi_under_line,kate_under_line,kendall_under_line,naomi_under_line,
                     tyra_under_line)

under_line$model <- factor(under_line$model, 
                        levels=c("adriana - ensemble","chrissy - knn","cindy - svm",
                                 "gisele - ridge","heidi - xgb","kate - rf",
                                 "kendall - simple","naomi - nn","tyra - ls"))

under_line %>%
    group_by(date, model) %>%
    ggplot(aes(x=date, y=cume_2022, color=model)) +
    geom_line(size=1.24) +
    scale_color_brewer(palette="Paired") + 
    labs(title = "Under Bets", subtitle = "2022 Performance") +
    scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
    xlab("") + 
    ylab("Units") +
    theme_bw()





# dynamic keys

t <- seq(0.1,10,0.1)
# t <- seq(0.01,1,0.01)
b <- 1
h <- length(t)
bst_tot <- 0
# bst_flt <- 10

for (b in b:h) {
    
    flt <- as.numeric(t[b])
    
    temp <- as.numeric(results_book %>% filter(naomi_spread_edge >= flt) %>% 
                           summarise(day_total = sum(naomi_spread_result)))
    
    if (temp > bst_tot) {
        bst_tot <- temp
        bst_flt <- flt
    }
}

results_book %>%
    filter(naomi_spread_edge >= bst_flt) %>%
    group_by(date) %>%
    summarise(day_total = sum(naomi_spread_result)) %>%
    mutate(cume = cumsum(day_total)) %>%
    ggplot(aes(date)) +
    geom_line(aes(y=cume, color="All Plays"), size=1) +
    scale_color_manual(name = "", values = c("All Plays" = "darkblue")) +
    labs(x = "Date", y = "Units", title = "2022 Performance", subtitle = "Naomi") +
    theme_bw()

print(bst_tot)
print(bst_flt)
print(results_book %>% filter(naomi_spread_edge >= bst_flt) %>% nrow())
# use cat from model training for this output




