### Shiny App Website ----

library(shiny)
library(ggimage)
library(ggrepel)
library(data.table)
library(gt)
library(gtExtras)
library(janitor)
library(zoo)
# devtools::install_github("beanumber/teamcolors", force=TRUE)
# library(teamcolors)
# rm(list=ls())

rosters <- nbastatR::seasons_rosters(seasons = 2023)

sched <- nbastatR::current_schedule()

NBAdb <- DBI::dbConnect(RSQLite::SQLite(), 
                        "/Users/Jesse/Documents/MyStuff/NBA Betting/NBAdb/NBAdb.sqlite")

players_df_basic <- dplyr::tbl(DBI::dbConnect(RSQLite::SQLite(),
                                              "/Users/Jesse/Documents/MyStuff/NBA Betting/NBAdb/NBAdb.sqlite"),
                               "PlayerPerGame")

players_df_adv <- dplyr::tbl(DBI::dbConnect(RSQLite::SQLite(),
                                            "/Users/Jesse/Documents/MyStuff/NBA Betting/NBAdb/NBAdb.sqlite"),
                             "PlayerAdvanced")

team_dict <- dplyr::tbl(DBI::dbConnect(RSQLite::SQLite(),
                                       "/Users/Jesse/Documents/MyStuff/NBA Betting/NBAdb/NBAdb.sqlite"),
                        "TeamDictionary")

players_df_basic <- players_df_basic %>%
    collect() %>%
    filter(yearSeason == 2022)

players_df_adv <- players_df_adv %>%
    collect() %>%
    filter(yearSeason == 2022)

team_dict <- team_dict %>%
    collect()

DBI::dbDisconnect(NBAdb)

# td <- as_date("2022-10-18")
# 
# slate <- sched %>%
#     filter(dateGame == td) %>%
#     mutate(gameTime = hms::as_hms(datetimeGame - 18000)) %>%
#     select(4,29,24,2,34)
# 
# colnames(slate) <- c("idGame","Away","Home","Date","Game Time")

slate <- slate %>%
    mutate(across(where(is.character), str_replace_all, 
                  pattern = "Los Angeles Clippers", replacement = "LA Clippers"))

slate_df <- slate %>%
    mutate(gm = paste0(away, " @ ", home)) %>%
    select(6,2,3,5)

slate_df <- slate_df %>%
    left_join(team_dict[c(2,9,10)], by = c("home" = "nameTeam"))

colnames(slate_df) <- c("Game", "Away", "Home", "Game Time", "Arena", "Location")


### Shiny App Inputs ----

### Filter lists ----
gm_list <- unique(slate_df$Game)
metric_list <- c("All", "Margin", "Win %", "Game Points", "Team Points")
tm_list <- unique(season_final$teamName)
loc_list <- c("Season", "Away", "Home")

### Standings----
standings_df <- standings %>%
    select(1:4,6:8,10:12,14:16)

### Players ----
players_df <- rosters[c(3,4,5)] %>%
    left_join(team_dict[c(2:3)], by = "slugTeam") %>%
    left_join(., players_df_basic[c(7,10:16,19,25:42)], by = c("idPlayer"="idPlayerNBA")) %>%
    left_join(., players_df_adv[c(13,20:39)], by = c("idPlayer"="idPlayerNBA")) %>%
    select(4,3,5,6,7,13,30,
           14,15,8,16,17,9,20,21,12,22,23,24,25,28,26,27,29,
           11,32,42,31,47,48,49,50,43,44,45,46) %>%
    arrange(nameTeam,desc(ptsPerGame))

colnames(players_df) <- c('TEAM','PLAYER','POS','GP','GS','MIN','PTS','FGM','FGA','FG','3PM','3PA','3FG',
                          'FTM','FTA','FT','ORB','DRB','TRB','AST','TOV','STL','BLK','PF',
                          'EFG','TS','USG','PER','OBPM','DBPM','BPM','VORP','OWS','DWS','WS','WS48')

### Models ----
models_df_raw <- all_models %>%
    select(1:6,8,9,7,10) %>%
    mutate(gm = paste0(all_models$away, " @ ", all_models$home)) %>%
    rename_with(~ paste0('c', ceiling(seq_along(.x) / 2), '_', 1:2)) %>%
    pivot_longer(cols = c(1:9), names_to = c(".value", NA), names_sep = '_') %>%
    mutate(across(c5, ~ na.locf(.x))) %>%
    select(3:7,2,1)

colnames(models_df_raw) <- c("team", "margin", "win", "team_score", "total", "game", "model")

models_df <- models_df_raw %>%
    pivot_wider(names_from = model, values_from = c(margin, win, team_score, total))

models_df_marg <- models_df  %>% mutate(metric = "Margin") %>% select(2,39,1,3:11)
colnames(models_df_marg) <- c("Game","Metric","Team",
                              "Kendall", "Tyra", "Gisele", "Chrissy", 
                              "Kate", "Cindy","Naomi", "Heidi","Adriana")
models_df_win <- models_df  %>% mutate(metric = "Win %") %>% select(2,39,1,12:20)
colnames(models_df_win) <- c("Game","Metric","Team",
                             "Kendall", "Tyra", "Gisele", "Chrissy", 
                             "Kate", "Cindy","Naomi", "Heidi","Adriana")
models_df_gm_total <- models_df  %>% mutate(metric = "Game Points") %>% select(2,39,1,30:38)
colnames(models_df_gm_total) <- c("Game","Metric","Team",
                                  "Kendall", "Tyra", "Gisele", "Chrissy", 
                                  "Kate", "Cindy","Naomi", "Heidi","Adriana")
models_df_tm_total <- models_df  %>% mutate(metric = "Team Points") %>% select(2,39,1,21:29)
colnames(models_df_tm_total) <- c("Game","Metric","Team",
                                  "Kendall", "Tyra", "Gisele", "Chrissy", 
                                  "Kate", "Cindy","Naomi", "Heidi","Adriana")

models_df_fmt <- rbind(models_df_marg, models_df_win, models_df_gm_total, models_df_tm_total)

models_df_fmt <- models_df_fmt %>%
    mutate(Team = case_when(Metric == "Game Points" ~ Game,
                            TRUE ~ Team)) %>%
    distinct()

### Edges ----
# models_edges_df_raw <- models_df_raw %>% 
#     left_join(plays[,5:8], by = "team") %>%
#     mutate(margin = margin - spread) %>%
#     mutate(win = win - (if_else(ml<0,((ml*-1)/((ml*-1)+100)),(100/(ml+100))))) %>%
#     mutate(total = total.x - total.y) %>%
#     select(1:3,11,6,7)
# 
# models_edges_df_raw <- models_edges_df_raw %>%
#     mutate(margin = round(margin, 1)) %>%
#     mutate(win = round(win, 3)) %>%
#     mutate(total = round(total, 1))
# 
# colnames(models_edges_df_raw) <- c("team", "margin", "win", "total", "game", "model")
# 
# models_edges_df <- models_edges_df_raw %>%
#     pivot_wider(names_from = model, values_from = c(margin, win, total))
# 
# models_edges_df_marg <- models_edges_df  %>% mutate(metric = "Margin") %>% select(2,30,1,3:11)
# colnames(models_edges_df_marg) <- c("Game","Metric","Team",
#                               "Kendall", "Tyra", "Gisele", "Chrissy", 
#                               "Kate", "Cindy","Naomi", "Heidi","Adriana")
# models_edges_df_win <- models_edges_df  %>% mutate(metric = "Win %") %>% select(2,30,1,12:20)
# colnames(models_edges_df_win) <- c("Game","Metric","Team",
#                                    "Kendall", "Tyra", "Gisele", "Chrissy", 
#                                    "Kate", "Cindy","Naomi", "Heidi","Adriana")
# models_edges_df_gm_total <- models_edges_df  %>% mutate(metric = "Game Points") %>% select(2,30,1,21:29)
# colnames(models_edges_df_gm_total) <- c("Game","Metric","Team",
#                                         "Kendall", "Tyra", "Gisele", "Chrissy", 
#                                         "Kate", "Cindy","Naomi", "Heidi","Adriana")
# 
# models_edges_df_fmt <- rbind(models_edges_df_marg, models_edges_df_win, models_edges_df_gm_total)
# 
# models_edges_df_fmt <- models_edges_df_fmt %>%
#     mutate(Team = case_when(Metric == "Game Points" ~ Game,
#                             TRUE ~ Team)) %>%
#     distinct()
    
### Plays - All ----
bets_df <- bets %>%
    mutate(Game = if_else(loc == "H", paste0(opp_team, " @ ", team), paste0(team, " @ ", opp_team))) %>%
    left_join(slate[,c(1,5)], by = "game_id")  %>%
    mutate(team = if_else(bet_type %in% c("Over", "Under"), "Game Total", team)) %>%
    select(9,10,5,6,7,8)

bets_df_viz <- bets_df %>%
    mutate(line = case_when(bet_type %in% c("Spread","ML") & line > 0 ~ paste0("+", line),
                              TRUE ~ as.character(line)))

### Plays - Official ----
bets_official_df <- official_plays %>%
    mutate(Game = if_else(loc == "H", paste0(opp_team, " @ ", team), paste0(team, " @ ", opp_team))) %>%
    left_join(slate[,c(1,5)], by = "game_id")  %>%
    mutate(team = if_else(bet_type %in% c("Over", "Under"), "Game Total", team)) %>%
    select(9,10,5,6,7,8)

bets_official_df_viz <- bets_official_df %>%
    mutate(line = case_when(bet_type %in% c("Spread","ML") & line > 0 ~ paste0("+", line),
                              TRUE ~ as.character(line)))


### Var Imp ----
score_imp_bind <- score_imp %>%
    mutate(var = gsub('_.*','', var)) %>%
    group_by(var) %>%
    summarise(imp = round(max(imp),1)) %>%
    arrange(desc(imp)) %>%
    mutate(imp = as.character(imp)) %>%
    mutate(index = 1:nrow(.))


win_imp_bind <- win_imp %>%
    mutate(var = gsub('_.*','', var)) %>%
    group_by(var) %>%
    summarise(imp = round(max(imp),1)) %>%
    arrange(desc(imp)) %>%
    mutate(imp = as.character(imp)) %>%
    mutate(index = 1:nrow(.))


imp_viz <- full_join(score_imp_bind, win_imp_bind, by = "index") %>%
    select(1,2,4,5) %>%
    rename(score_var = var.x,
           imp_score = imp.x,
           win_var = var.y,
           imp_win = imp.y)

imp_viz[is.na(imp_viz)] <- ""


### Functions ----

# summary function
summary_tbl <- function(a, h, b, l) {
    
    standings_df_away <- standings_df %>%
        filter(Team == as.character(a)) %>%
        select(1:3,13,4:9) %>%
        mutate(index = nrow(.)) %>%
        mutate(across(everything(), as.character)) %>%
        pivot_longer(-index,
                     names_to = "stat",
                     values_to = "val") %>%
        select(2,3)
    
    standings_df_home <- standings_df %>%
        filter(Team == as.character(h)) %>%
        select(1:3,13,4:6,10:12) %>%
        mutate(index = nrow(.)) %>%
        mutate(across(everything(), as.character)) %>%
        pivot_longer(-index,
                     names_to = "stat",
                     values_to = "val") %>%
        select(2,3) %>%
        mutate(across(where(is.character), str_replace_all, c("Home Record" = "Away/Home Record",
                                                              "Home L10" = "Away/Home L10",
                                                              "Home Streak" = "Away/Home Streak",
                                                              "Team" = " ")))
    
    standings_df_viz <- cbind(standings_df_away$val,standings_df_home)
    colnames(standings_df_viz) <- c("Away"," ","Home")
    
    standings_df_viz %>%
        gt() %>%
        # opt_all_caps() %>%
        opt_table_font(
            font = list(
                google_font("Chivo"),
                default_fonts()
            )
        ) %>%
        tab_header(
            title = paste0(as.character(b)," - ",as.character(l))
        ) %>%
        tab_style(
            style = list(
                cell_fill(color = "#e3e2e1")
            ),
            locations = cells_body(
                columns = everything(),
                rows = 1
            )
        ) %>%
        tab_style(
            style = cell_borders(
                sides = "bottom", color = "transparent", weight = px(2)
            ),
            locations = cells_body(
                columns = everything(),
                rows = nrow(standings_df_viz)
            )
        ) %>%
        tab_style(
            style = cell_borders(
                sides = "bottom", color = "black", weight = px(3)
            ),
            locations = cells_body(
                columns = everything(),
                rows = 1
            )
        ) %>%
        tab_options(
            table.font.weight = "bold",
            table.font.size = 12,
            table.border.top.width = px(3),
            table.border.top.color = "transparent",
            table.border.bottom.color = "transparent",
            table.border.bottom.width = px(3),
            column_labels.border.top.width = px(3),
            column_labels.border.top.color = "transparent",
            column_labels.border.bottom.width = px(3),
            column_labels.border.bottom.color = "black",
            column_labels.font.weight = "bold",
            column_labels.background.color = "black",
            column_labels.font.size = 14,
            data_row.padding = px(3),
            row_group.padding = px(3),
            row_group.background.color = "#e3e2e1",
            row_group.font.size = 13,
            source_notes.font.size = 8
        ) %>%
        cols_align(
            align = "center",
            columns = everything()
        ) %>%
        tab_source_note(
            source_note = md("@MambaMetrics | Data: nbastatR")
        )
    
}

# player function
player_tbl <- function(a, h) {
    
    players_df_viz <- players_df %>% 
        filter(TEAM == as.character(a) | TEAM == as.character(h))

    players_df_viz %>%
        gt(groupname_col = "TEAM") %>%
        # text_transform(
        #     locations = cells_body(
        #         vars(logo)
        #     ),
        #     fn = function(x) {
        #         web_image(
        #             url = x,
        #             height = 25
        #         )
        #     }
        # ) %>%
        tab_spanner(
            label = "BASIC STATS",
            columns = 7:24
        ) %>%
        tab_spanner(
            label = "ADVANCED STATS",
            columns = 25:36
        ) %>%
        tab_spanner(
            label = "BLANK",
            columns = 1:6
        ) %>%
        tab_style(
            style = list(
                cell_fill(color = "#e4e8ed"),
                cell_text(color = "#878e94"),
                cell_borders(sides = "left", color = "white", weight = px(3))
            ),
            locations = list(
                cells_column_spanners(
                    spanners = c("BASIC STATS", "ADVANCED STATS")
                )
            )
        ) %>%
        tab_style(
            style = list(
                cell_fill(color = "transparent"),
                cell_text(color = "transparent")
            ),
            locations = list(
                cells_column_spanners(
                    spanners = c("BLANK")
                )
            )
        ) %>%
        tab_style(
            style = list(
                cell_text(color = "#3a3d42", weight = "bold")
            ),
            locations = cells_body(
                columns = 1:2
            )
        ) %>%
        opt_all_caps() %>%
        opt_row_striping() %>%
        tab_options(
            table.font.size = 12,
            row_group.font.size = 13,
            column_labels.background.color = "#585d63",
            table_body.hlines.color = "transparent",
            table.border.top.width = px(3),
            table.border.top.color = "transparent",
            table.border.bottom.color = "transparent",
            table.border.bottom.width = px(3),
            column_labels.border.top.width = px(3),
            column_labels.border.top.color = "transparent",
            column_labels.border.bottom.width = px(3),
            column_labels.border.bottom.color = "transparent",
            row.striping.background_color = "#f9f9fb",
            data_row.padding = px(3),
            source_notes.font.size = 8
        ) %>%
        fmt_percent(
            columns = c(10,13,16,25,26,27),
            decimals = 1
        ) %>%
        cols_width(
            2 ~ px(130),
            everything() ~ px(45)
        ) %>%
        cols_align(
            align = "center",
            columns = 3:36
        ) %>%
        tab_style(
            style = cell_borders(
                sides = "bottom", color = "#585d63", weight = px(2)
            ),
            locations = cells_body(
                columns = everything(),
                rows = nrow(players_df_viz)
            )
        ) %>%
        opt_table_font(
            font = c(
                google_font(name = "Lato"),
                default_fonts()
            )
        ) %>%
        tab_source_note(
            source_note = md("@MambaMetrics | Data: nbastatR")
        )
    
}

# team function
team_tbl <- function(l) {
    
    season_final <- season_final %>%
        left_join(standings[,c(1,4)], by = c("teamName" = "Team")) %>%
        mutate(loc = "Season")
    season_final <- as.data.frame(season_final)
    
    away_final <- away_final %>%
        left_join(standings[,c(1,8)], by = c("teamName" = "Team")) %>%
        mutate(loc = "Away")
    away_final <- as.data.frame(away_final)
    
    home_final <- home_final %>%
        left_join(standings[,c(1,12)], by = c("teamName" = "Team")) %>%
        mutate(loc = "Home")
    home_final <- as.data.frame(home_final)
    
    teams_df <- bind_rows(season_final,away_final,home_final)
    
    teams_df_viz <- teams_df %>% 
        filter(loc == as.character(l)) %>%
        select(-loc) %>%
        select(where(~!all(is.na(.)))) %>%
        select(1,37,34:36,2:33) %>%
        rename(Record = ends_with("Record"))

    teams_df_viz %>%
        gt() %>%
        tab_spanner(
            label = "OFFENSE",
            columns = 6:21
        ) %>%
        tab_spanner(
            label = "DEFENSE",
            columns = 22:37
        ) %>%
        tab_spanner(
            label = "BLANK",
            columns = 1:5
        ) %>%
        tab_style(
            style = list(
                cell_fill(color = "#e4e8ed"),
                cell_text(color = "#878e94"),
                cell_borders(sides = "left", color = "white", weight = px(3))
            ),
            locations = list(
                cells_column_spanners(
                    spanners = c("OFFENSE", "DEFENSE")
                )
            )
        ) %>%
        tab_style(
            style = list(
                cell_fill(color = "transparent"),
                cell_text(color = "transparent")
            ),
            locations = list(
                cells_column_spanners(
                    spanners = c("BLANK")
                )
            )
        ) %>%
        tab_style(
            style = list(
                cell_text(color = "#3a3d42", weight = "bold")
            ),
            locations = cells_body(
                columns = 1:2
            )
        ) %>%
        opt_row_striping() %>%
        tab_options(
            table.font.size = 12,
            row_group.font.size = 13,
            column_labels.background.color = "#585d63",
            table_body.hlines.color = "transparent",
            table.border.top.width = px(3),
            table.border.top.color = "transparent",
            table.border.bottom.color = "transparent",
            table.border.bottom.width = px(3),
            column_labels.border.top.width = px(3),
            column_labels.border.top.color = "transparent",
            column_labels.border.bottom.width = px(3),
            column_labels.border.bottom.color = "transparent",
            row.striping.background_color = "#f9f9fb",
            data_row.padding = px(3),
            source_notes.font.size = 8
        ) %>%
        fmt_percent(
            columns = c(6:37),
            decimals = 1
        ) %>%
        fmt_number(
            columns = c(3:5),
            decimals = 1
        ) %>%
        cols_width(
            teamName ~ px(150),
            everything() ~ px(50)
        ) %>%
        cols_align(
            align = "center",
            columns = 2:37
        ) %>%
        cols_label(
            teamName = "Team"
        ) %>%
        tab_style(
            style = cell_borders(
                sides = "bottom", color = "#585d63", weight = px(2)
            ),
            locations = cells_body(
                columns = everything(),
                rows = nrow(teams_df_viz)
            )
        ) %>%
        opt_table_font(
            font = c(
                google_font(name = "Lato"),
                default_fonts()
            )
        ) %>%
        tab_source_note(
            source_note = md("@MambaMetrics | Data: nbastatR")
        )
    
}

# rank function
rank_tbl <- function(a, h) {
    
    rank_df_away <- away_rank %>% 
        filter(team == as.character(a)) %>% 
        select(1,3,5,7,9,11,13,15,17,21,23,25,27,29,31,33,35,
               37,39,41,43,45,53,55,57,59,61,63,65,67,69,71) %>%
        transpose() %>%
        row_to_names(1) %>%
        mutate(sorter = c(1,2,3,4,5,6,7,23,8,9,10,11,12,13,14,17,
                          18,19,20,21,22,24,25,26,27,28,29,30,15,31,16)) %>%
        arrange(sorter) %>%
        select(1)
    
    rank_df_home <- home_rank %>% 
        filter(team == as.character(h)) %>% 
        select(1,3,5,7,9,11,13,15,17,21,23,25,27,29,31,33,35,
               37,39,41,43,45,53,55,57,59,61,63,65,67,69,71) %>%
        transpose() %>%
        row_to_names(1) %>%
        mutate(sorter = c(17,18,19,20,21,22,23,7,24,25,26,27,28,29,30,
                          1,2,3,4,5,6,8,9,10,11,12,13,14,31,15,16)) %>%
        arrange(sorter) %>%
        select(1)
    
    
    AT <- c('FG','SR2','FG3','SR3','FT','FTR','ORB/DRB','AST','TOV','STL','BLK','PF','eFG','TS','ORTG/DRTG',
            'Pace',
            'FG','SR2','FG3','SR3','FT','FTR','DRB/ORB','AST','TOV','STL','BLK','PF','eFG','TS','DRTG/ORTG')
    
    
    rank_df_viz <- cbind(rank_df_away,AT,rank_df_home)
    
    rank_df_viz[c(1:16),"off_def"] <- 'Offense'
    rank_df_viz[c(17:31),"off_def"] <- 'Defense'
    
    rank_df_viz[c(1:16),"def_off"] <- 'Defense'
    rank_df_viz[c(17:31),"def_off"] <- 'Offense'
    
    rank_df_viz[, c(1,3)] <- lapply(c(1,3), function(x) as.numeric(rank_df_viz[[x]]))
    
    rank_df_viz %>% 
        mutate(filler = "vs") %>%
        gt(groupname_col = c("off_def", "filler", "def_off")) %>%
        # opt_all_caps() %>%
        opt_table_font(
            font = list(
                google_font("Chivo"),
                default_fonts()
            )
        ) %>%
        tab_header(
            title = md("**Opponent Adjusted Rank**")
        ) %>%
        tab_spanner(
            label = "Away",
            columns = 1
        ) %>%
        tab_spanner(
            label = "Home",
            columns = 3
        ) %>%
        tab_spanner(
            label = "Blank",
            columns = 2
        ) %>%
        tab_style(
            style = list(
                cell_fill(color = "black"),
                cell_text(color = "white", size = px(x = 14))
            ),
            locations = list(
                cells_column_spanners(
                    spanners = c("Away", "Home")
                )
            )
        ) %>%
        tab_style(
            style = list(
                cell_fill(color = "black"),
                cell_text(color = "black")
            ),
            locations = list(
                cells_column_spanners(
                    spanners = c("Blank")
                )
            )
        ) %>%
        tab_style(
            style = list(
                cell_fill(color = "white")
            ),
            locations = cells_body(
                columns = 2
            )
        ) %>%
        tab_style(
            style = cell_borders(
                sides = "bottom", color = "transparent", weight = px(2)
            ),
            locations = cells_body(
                columns = everything(),
                rows = nrow(rank_df_viz)
            )
        ) %>%
        tab_options(
            table.font.weight = "bold",
            table.font.size = 12,
            table.border.top.width = px(3),
            table.border.top.color = "transparent",
            table.border.bottom.color = "transparent",
            table.border.bottom.width = px(3),
            column_labels.border.top.width = px(3),
            column_labels.border.top.color = "transparent",
            column_labels.border.bottom.width = px(3),
            column_labels.border.bottom.color = "black",
            column_labels.font.weight = "bold",
            column_labels.background.color = "#e3e2e1",
            column_labels.font.size = 12,
            data_row.padding = px(3),
            row_group.padding = px(3),
            row_group.background.color = "white",
            row_group.font.size = 13,
            source_notes.font.size = 8
        ) %>%
        cols_align(
            align = "center",
            columns = everything()
        ) %>%
        # cols_width(
        #     AT ~ px(85),
        #     !AT ~ px(150)
        # ) %>%
        cols_label(
            AT = " "
        ) %>%
        data_color(
            columns = c(1,3), 
            colors = scales::col_numeric(
                palette = c(
                    "#54d141","#f7e345","#f74545"),
                domain = c(1,15,30) 
            )
        ) %>%
        tab_source_note(
            source_note = md("@MambaMetrics | Data: nbastatR")
        )
    
}

# stats function
stats_tbl <- function(a, h) {
    
    stats_df_away <- away_final_wt %>% 
        filter(team == as.character(a)) %>% 
        select(-c(10,24,25,26)) %>%
        transpose() %>%
        row_to_names(1) %>%
        mutate(sorter = c(1,2,3,4,5,6,7,23,8,9,10,11,12,13,14,17,
                          18,19,20,21,22,24,25,26,27,28,29,30,15,31,16)) %>%
        arrange(sorter) %>%
        select(1)
    
    stats_df_home <- home_final_wt %>% 
        filter(team == as.character(h)) %>% 
        select(-c(10,24,25,26)) %>%
        transpose() %>%
        row_to_names(1) %>%
        mutate(sorter = c(17,18,19,20,21,22,23,7,24,25,26,27,28,29,30,
                          1,2,3,4,5,6,8,9,10,11,12,13,14,31,15,16)) %>%
        arrange(sorter) %>%
        select(1)
    
    
    AT <- c('FG','SR2','FG3','SR3','FT','FTR','ORB/DRB','AST','TOV','STL','BLK','PF','eFG','TS','ORTG/DRTG',
            'Pace',
            'FG','SR2','FG3','SR3','FT','FTR','DRB/ORB','AST','TOV','STL','BLK','PF','eFG','TS','DRTG/ORTG')
    
    
    stats_df_viz <- cbind(stats_df_away,AT,stats_df_home)
    
    stats_df_viz[c(1:16),"off_def"] <- 'Offense'
    stats_df_viz[c(17:31),"off_def"] <- 'Defense'
    
    stats_df_viz[c(1:16),"def_off"] <- 'Defense'
    stats_df_viz[c(17:31),"def_off"] <- 'Offense'
    
    stats_df_viz[, c(1,3)] <- lapply(c(1,3), function(x) as.numeric(stats_df_viz[[x]]))
    
    stats_df_viz %>% 
        mutate(filler = "vs") %>%
        gt(groupname_col = c("off_def", "filler", "def_off")) %>%
        # opt_all_caps() %>%
        opt_table_font(
            font = list(
                google_font("Chivo"),
                default_fonts()
            )
        ) %>%
        tab_header(
            title = md("**Opponent Adjusted Stats**")
        ) %>%
        tab_spanner(
            label = "Away",
            columns = 1
        ) %>%
        tab_spanner(
            label = "Home",
            columns = 3
        ) %>%
        tab_spanner(
            label = "Blank",
            columns = 2
        ) %>%
        tab_style(
            style = list(
                cell_fill(color = "black"),
                cell_text(color = "white", size = px(x = 14))
            ),
            locations = list(
                cells_column_spanners(
                    spanners = c("Away", "Home")
                )
            )
        ) %>%
        tab_style(
            style = list(
                cell_fill(color = "black"),
                cell_text(color = "black")
            ),
            locations = list(
                cells_column_spanners(
                    spanners = c("Blank")
                )
            )
        ) %>%
        tab_style(
            style = list(
                cell_fill(color = "white")
            ),
            locations = cells_body(
                columns = 2
            )
        ) %>%
        tab_style(
            style = list(
                cell_fill(color = "#e4c8f7") #"#d39dfa" #dfb9fa
            ),
            locations = cells_body(
                columns = 1
            )
        ) %>%
        tab_style(
            style = list(
                cell_fill(color = "#e4c8f7") #"#d39dfa" #dfb9fa
            ),
            locations = cells_body(
                columns = 3
            )
        ) %>%
        tab_style(
            style = cell_borders(
                sides = "bottom", color = "transparent", weight = px(2)
            ),
            locations = cells_body(
                columns = everything(),
                rows = nrow(stats_df_viz)
            )
        ) %>%
        tab_options(
            table.font.weight = "bold",
            table.font.size = 12,
            table.border.top.width = px(3),
            table.border.top.color = "transparent",
            table.border.bottom.color = "transparent",
            table.border.bottom.width = px(3),
            column_labels.border.top.width = px(3),
            column_labels.border.top.color = "transparent",
            column_labels.border.bottom.width = px(3),
            column_labels.border.bottom.color = "black",
            column_labels.font.weight = "bold",
            column_labels.background.color = "#e3e2e1",
            column_labels.font.size = 12,
            data_row.padding = px(3),
            row_group.padding = px(3),
            row_group.background.color = "white",
            row_group.font.size = 13,
            source_notes.font.size = 8
        ) %>%
        cols_align(
            align = "center",
            columns = everything()
        ) %>%
        # cols_width(
        #     AT ~ px(85),
        #     !AT ~ px(150)
        # ) %>%
        cols_label(
            AT = " "
        ) %>%
        fmt_percent(
            columns = c(1,3),
            rows = c(1:14,17:30),
            decimals = 1
        ) %>%
        fmt_number(
            columns = c(1,3),
            rows = c(15,16,31),
            decimals = 1
        ) %>%
        tab_source_note(
            source_note = md("@MambaMetrics | Data: nbastatR")
        )
    
}

# model output - game
models_tbl <- function(g) {
    
    model_df_viz <- models_df_fmt %>%
        filter(Game == as.character(g)) %>%
        select(-1)
    
    model_df_viz %>%
        gt(groupname_col = "Metric") %>%
        # opt_all_caps() %>%
        opt_table_font(
            font = list(
                google_font("Chivo"),
                default_fonts()
            )
        ) %>%
        tab_header(
            title = md("**Raw Model Output**")
        ) %>%
        tab_style(
            style = cell_borders(
                sides = "bottom", color = "transparent", weight = px(2)
            ),
            locations = cells_body(
                columns = everything(),
                rows = nrow(model_df_viz)
            )
        ) %>%
        tab_options(
            table.font.weight = "bold",
            table.font.size = 13,
            table.border.top.width = px(3),
            table.border.top.color = "transparent",
            table.border.bottom.color = "transparent",
            table.border.bottom.width = px(3),
            column_labels.border.top.width = px(3),
            column_labels.border.top.color = "transparent",
            column_labels.border.bottom.width = px(3),
            column_labels.border.bottom.color = "black",
            column_labels.font.weight = "bold",
            column_labels.background.color = "black",
            column_labels.font.size = 14,
            data_row.padding = px(3),
            row_group.padding = px(3),
            row_group.background.color = "#e3e2e1",
            row_group.font.size = 13,
            source_notes.font.size = 8
        ) %>%
        cols_label(
            Team = ""
        ) %>%
        cols_align(
            align = "center",
            columns = 3:11
        ) %>%
        fmt_percent(
            columns = 3:11,
            rows = 3:4,
            decimals = 1
        ) %>%
        fmt_number(
            columns = 3:11,
            rows = c(1,2,5:7),
            decimals = 1
        ) %>%
        tab_source_note(
            source_note = md("@MambaMetrics | MAMBA")
        )
}

# model output
models_all_tbl <- function(m) {
    
    model_df_all_viz <- models_df_fmt %>%
        select(1,3,2,4:12) %>%
        filter(if (m == "All") Metric != m else Metric == m )

    model_df_all_viz %>%
        gt(groupname_col = "Game") %>%
        # opt_all_caps() %>%
        opt_table_font(
            font = list(
                google_font("Chivo"),
                default_fonts()
            )
        ) %>%
        tab_header(
            title = md("**Raw Model Output by Bet Type**")
        ) %>%
        tab_style(
            style = cell_borders(
                sides = "bottom", color = "transparent", weight = px(2)
            ),
            locations = cells_body(
                columns = everything(),
                rows = nrow(model_df_all_viz)
            )
        ) %>%
        tab_options(
            table.font.weight = "bold",
            table.font.size = 12,
            table.border.top.width = px(3),
            table.border.top.color = "transparent",
            table.border.bottom.color = "transparent",
            table.border.bottom.width = px(3),
            column_labels.border.top.width = px(3),
            column_labels.border.top.color = "transparent",
            column_labels.border.bottom.width = px(3),
            column_labels.border.bottom.color = "black",
            column_labels.font.weight = "bold",
            column_labels.background.color = "black",
            column_labels.font.size = 14,
            data_row.padding = px(3),
            row_group.padding = px(3),
            row_group.background.color = "#e3e2e1",
            row_group.font.size = 13,
            source_notes.font.size = 8
        ) %>%
        cols_label(
            Team = "",
            Metric = ""
        ) %>%
        cols_align(
            align = "center",
            columns = 4:12
        ) %>%
        fmt_percent(
            columns = 4:12,
            rows = Metric == "Win %",
            decimals = 1
        ) %>%
        fmt_number(
            columns = 4:12,
            rows = Metric != "Win %",
            decimals = 1
        ) %>%
        opt_row_striping(
        ) %>%
        tab_source_note(
            source_note = md("@MambaMetrics | MAMBA")
        )
}


# model edges output - game
# models_edges_tbl <- function(g) {
#     
#     model_edges_df_viz <- models_edges_df_fmt %>%
#         filter(Game == as.character(g)) %>%
#         select(-1)
#     
#     model_edges_df_viz %>%
#         gt(groupname_col = "Metric") %>%
#         opt_all_caps()  %>%
#         opt_table_font(
#             font = list(
#                 google_font("Chivo"),
#                 default_fonts()
#             )
#         ) %>%
#         tab_style(
#             style = cell_borders(
#                 sides = "bottom", color = "transparent", weight = px(2)
#             ),
#             locations = cells_body(
#                 columns = everything(),
#                 rows = nrow(models_edges_tbl)
#             )
#         ) %>%
#         tab_options(
#             table.font.weight = "bold",
#             table.font.size = 12,
#             table.border.top.width = px(3),
#             table.border.top.color = "transparent",
#             table.border.bottom.color = "transparent",
#             table.border.bottom.width = px(3),
#             column_labels.border.top.width = px(3),
#             column_labels.border.top.color = "transparent",
#             column_labels.border.bottom.width = px(3),
#             column_labels.border.bottom.color = "black",
#             column_labels.font.weight = "bold",
#             column_labels.background.color = "black",
#             column_labels.font.size = 14,
#             data_row.padding = px(3),
#             row_group.padding = px(3),
#             row_group.background.color = "#e3e2e1",
#             row_group.font.size = 13,
#             source_notes.font.size = 10
#         ) %>%
#         cols_label(
#             Team = ""
#         ) %>%
#         cols_align(
#             align = "center",
#             columns = 3:11
#         ) %>%
#         fmt_percent(
#             columns = 3:11,
#             rows = 3:4,
#             decimals = 1
#         ) %>%
#         fmt_number(
#             columns = 3:11,
#             rows = c(1,2,5:7),
#             decimals = 1
#         )
# }

# model edges output
# models_edges_all_tbl <- function(m) {
#     
#     model_edges_df_all_viz <- models_edges_df_fmt %>%
#         select(1,3,2,4:12) %>%
#         filter(if (m == "All") Metric != m else Metric == m )
#     
#     model_edges_df_all_viz %>%
#         gt(groupname_col = "Game") %>%
#         opt_all_caps()  %>%
#         opt_table_font(
#             font = list(
#                 google_font("Chivo"),
#                 default_fonts()
#             )
#         ) %>%
#         tab_style(
#             style = cell_borders(
#                 sides = "bottom", color = "transparent", weight = px(2)
#             ),
#             locations = cells_body(
#                 columns = everything(),
#                 rows = nrow(model_edges_df_all_viz)
#             )
#         ) %>%
#         tab_options(
#             table.font.weight = "bold",
#             table.font.size = 12,
#             table.border.top.width = px(3),
#             table.border.top.color = "transparent",
#             table.border.bottom.color = "transparent",
#             table.border.bottom.width = px(3),
#             column_labels.border.top.width = px(3),
#             column_labels.border.top.color = "transparent",
#             column_labels.border.bottom.width = px(3),
#             column_labels.border.bottom.color = "black",
#             column_labels.font.weight = "bold",
#             column_labels.background.color = "black",
#             column_labels.font.size = 14,
#             data_row.padding = px(3),
#             row_group.padding = px(3),
#             row_group.background.color = "#e3e2e1",
#             row_group.font.size = 13,
#             source_notes.font.size = 10
#         ) %>%
#         cols_label(
#             Team = "",
#             Metric = ""
#         ) %>%
#         cols_align(
#             align = "center",
#             columns = 4:12
#         ) %>%
#         fmt_percent(
#             columns = 4:12,
#             rows = Metric == "Moneyline",
#             decimals = 1
#         ) %>%
#         fmt_number(
#             columns = 4:12,
#             rows = Metric != "Moneyline",
#             decimals = 1
#         ) %>%
#         opt_row_striping()
# }



# model plays
plays_tbl <- function() {
    
    bets_df_viz %>%
        gt() %>%
        # opt_all_caps()  %>%
        opt_table_font(
            font = list(
                google_font("Chivo"),
                default_fonts()
            )
        ) %>%
        tab_header(
            title = md("**All Model Plays**")
        ) %>%
        tab_style(
            style = cell_borders(
                sides = "bottom", color = "transparent", weight = px(2)
            ),
            locations = cells_body(
                columns = everything(),
                rows = nrow(bets_df_viz)
            )
        ) %>%
        tab_options(
            table.font.weight = "bold",
            table.font.size = 12,
            table.border.top.width = px(3),
            table.border.top.color = "transparent",
            table.border.bottom.color = "transparent",
            table.border.bottom.width = px(3),
            column_labels.border.top.width = px(3),
            column_labels.border.top.color = "transparent",
            column_labels.border.bottom.width = px(3),
            column_labels.border.bottom.color = "black",
            column_labels.font.weight = "bold",
            column_labels.background.color = "black",
            column_labels.font.size = 14,
            data_row.padding = px(3),
            row_group.padding = px(3),
            row_group.background.color = "#e3e2e1",
            row_group.font.size = 13,
            source_notes.font.size = 8
        ) %>%
        cols_label(
            Game = "Game",
            game_time = "Game Time",
            team = "Team",
            bet_type = "Bet Type",
            line = "Line",
            edge = "Edge"
        ) %>%
        cols_align(
            align = "center",
            columns = 2:ncol(bets_df_viz)
        ) %>%
        fmt_percent(
            columns = 6,
            rows = bet_type == "ML",
            decimals = 1
        ) %>%
        fmt_number(
            columns = 6,
            rows = bet_type != "ML",
            decimals = 1
        ) %>%
        opt_row_striping(
        ) %>%
        tab_source_note(
            source_note = md("@MambaMetrics | MAMBA")
        )
}


# official model plays
plays_official_tbl <- function() {
    
    bets_official_df_viz %>%
        gt() %>%
        # opt_all_caps()  %>%
        opt_table_font(
            font = list(
                google_font("Chivo"),
                default_fonts()
            )
        ) %>%
        tab_header(
            title = md("**All Official Plays**")
        ) %>%
        tab_style(
            style = cell_borders(
                sides = "bottom", color = "transparent", weight = px(2)
            ),
            locations = cells_body(
                columns = everything(),
                rows = nrow(bets_official_df_viz)
            )
        ) %>%
        tab_options(
            table.font.weight = "bold",
            table.font.size = 12,
            table.border.top.width = px(3),
            table.border.top.color = "transparent",
            table.border.bottom.color = "transparent",
            table.border.bottom.width = px(3),
            column_labels.border.top.width = px(3),
            column_labels.border.top.color = "transparent",
            column_labels.border.bottom.width = px(3),
            column_labels.border.bottom.color = "black",
            column_labels.font.weight = "bold",
            column_labels.background.color = "black",
            column_labels.font.size = 14,
            data_row.padding = px(3),
            row_group.padding = px(3),
            row_group.background.color = "#e3e2e1",
            row_group.font.size = 13,
            source_notes.font.size = 8
        ) %>%
        cols_label(
            Game = "Game",
            game_time = "Game Time",
            team = "Team",
            bet_type = "Bet type",
            line = "Line",
            edge = "Edge"
        ) %>%
        cols_align(
            align = "center",
            columns = 2:ncol(bets_official_df_viz)
        ) %>%
        fmt_percent(
            columns = 6,
            rows = bet_type == "ML",
            decimals = 1
        ) %>%
        fmt_number(
            columns = 6,
            rows = bet_type != "ML",
            decimals = 1
        ) %>%
        opt_row_striping(
        ) %>%
        tab_source_note(
            source_note = md("@MambaMetrics | MAMBA")
        )
}


# variable importance
var_imp_tbl <- function() {
    
    imp_viz %>%
        gt() %>%
        # opt_all_caps()  %>%
        opt_table_font(
            font = list(
                google_font("Chivo"),
                default_fonts()
            )
        ) %>%
        tab_header(
            title = md("**MAMBA Variable Importance**")
        ) %>%
        tab_spanner(
            label = "Scores",
            columns = 1:2
        ) %>%
        tab_spanner(
            label = "Win %",
            columns = 3:4
        ) %>%
        tab_style(
            style = list(
                cell_fill(color = "black"),
                cell_text(color = "white")
            ),
            locations = list(
                cells_column_spanners(
                    spanners = c("Scores", "Win %")
                )
            )
        ) %>%
        tab_style(
            style = cell_borders(
                sides = "bottom", color = "transparent", weight = px(2)
            ),
            locations = cells_body(
                columns = everything(),
                rows = nrow(imp_viz)
            )
        ) %>%
        tab_options(
            table.font.weight = "bold",
            table.font.size = 12,
            table.border.top.width = px(3),
            table.border.top.color = "transparent",
            table.border.bottom.color = "transparent",
            table.border.bottom.width = px(3),
            column_labels.border.top.width = px(3),
            column_labels.border.top.color = "transparent",
            column_labels.border.bottom.width = px(3),
            column_labels.border.bottom.color = "black",
            column_labels.font.weight = "bold",
            column_labels.background.color = "#e3e2e1",
            column_labels.font.size = 12,
            data_row.padding = px(3),
            row_group.padding = px(3),
            row_group.background.color = "white",
            row_group.font.size = 12,
            source_notes.font.size = 8
        ) %>%
        cols_align(
            align = "center",
            columns = everything()
        ) %>%
        cols_label(
            score_var = "Variable",
            imp_score = "Importance",
            win_var = "Variable",
            imp_win = "Importance"
        ) %>%
        gt_add_divider(columns = "imp_score", style = "solid"
        ) %>%
        opt_row_striping(
        ) %>%
        tab_source_note(
            source_note = md("@MambaMetrics | MAMBA")
        )

}





### Shiny App ----

# Define server logic to plot various variables against mpg
server <- function(input, output, session) {
    
    variablename <- reactiveVal(gm_list[1])
    
    metricname <- reactiveVal(metric_list[1])
    
    locationname <- reactiveVal(loc_list[1])
    
    formulaText <- reactive({
        paste0(variablename())
    })
    
    output$caption <- renderText({
        formulaText()
    })
    
    observeEvent(input$variable1, {
        updateSelectInput(session, "variable2", selected=input$variable1)
        variablename(input$variable1)
    })
    observeEvent(input$variable2, {
        updateSelectInput(session, "variable1", selected=input$variable2)
        variablename(input$variable2)
    })
    observeEvent(input$variable3, {
        updateSelectInput(session, "variable3", selected=input$variable3)
        metricname(input$variable3)
    })
    observeEvent(input$variable4, {
        updateSelectInput(session, "variable4", selected=input$variable4)
        locationname(input$variable4)
    })

    #Reactive
    gm_input <- reactive({
        
        xx <- slate_df %>% filter(Game == as.character(variablename()))
        return(xx)
        
    })
    
    metric_input <- reactive({

        metricname()
        
    })
    
    location_input <- reactive({
        
        locationname()
        
    })
    
    ### Summary Plot    
    output$summary_plot <- render_gt({
        
       summary_tbl(gm_input()$Away, gm_input()$Home, gm_input()$Arena, gm_input()$Location) 

    })
   
    ### Player Stats Plot
    output$player_plot <- render_gt({

        player_tbl(gm_input()$Away, gm_input()$Home)
        
    })
    
    ### Team Stats Plot
    output$team_plot <- render_gt({
        
        team_tbl(location_input())
        
    })
    
    ### Rank Plot
    output$rank_plot <- render_gt({
        
        rank_tbl(gm_input()$Away, gm_input()$Home)
        
    })
    
    ### Stats Plot
    output$stats_plot <- render_gt({
        
        stats_tbl(gm_input()$Away, gm_input()$Home)
        
    })
    
    ### Models Plot - Game
    output$models_plot <- render_gt({
        
        models_tbl(gm_input()$Game)
        
    })
    
    ### Models Plot - All
    output$models_all_plot <- render_gt({
        
        models_all_tbl(metric_input())
        
    })
    
    ### Plays Plot
    output$plays_plot <- render_gt({
        
        plays_tbl()
        
    })
    
    ### Official Plays Plot
    output$plays_official_plot <- render_gt({
        
        plays_official_tbl()
        
    })
    
    ### Var Imp Plot
    output$var_imp_plot <- render_gt({
        
        var_imp_tbl()
        
    })
    
}

### Define UI ----
ui <- fluidPage(
    
    theme = bslib::bs_theme(bootswatch = "darkly"),
    
    titlePanel("MAMBA Metrics - Today's Plays"),
    
    # fluidRow(column(selectInput("variable", "Game:", gm_list), width = 5)),
    
    tabsetPanel(type = "tabs",
                tabPanel("Plays", fluid = T,
                         sidebarLayout(
                             sidebarPanel(
                                 paste0("All Plays for ", format(Sys.Date(), "%B %d %Y")),
                                 p(),
                                 p("*Official Plays contain a human element and are filtered by me")
                             ),
                             mainPanel(
                                 gt_output("plays_plot"),
                                 h3(textOutput(""), align = "center"),
                                 gt_output("plays_official_plot")
                             )
                         )
                ),
                tabPanel("Game Summary", fluid = T,
                         sidebarLayout(
                             sidebarPanel(
                                 selectInput("variable1", "Game:", gm_list),
                                 gt_output("summary_plot"),
                                 h1(textOutput(""), align = "center"),
                                 radioButtons("visuBtn", "Show Team:", choices = c(Stats = "stats", Rank = "rank"),
                                              inline = T),
                                 conditionalPanel(
                                     condition = "input.visuBtn == 'stats'",
                                     gt_output("stats_plot")
                                 ),
                                 conditionalPanel(
                                     condition = "input.visuBtn == 'rank'",
                                     gt_output("rank_plot")
                                 )
                             ),
                             mainPanel(fluidRow(
                                 h3(textOutput("caption"), align = "center"),
                                 gt_output("models_plot")
                             )
                             )
                         )
                ),
                tabPanel("All Games", fluid = T,
                         sidebarLayout(
                             sidebarPanel(
                                 selectInput("variable3", "Bet Type:", metric_list),
                                 gt_output("var_imp_plot")
                             ),
                             mainPanel(
                                 gt_output("models_all_plot")
                             )
                         )
                ),
                tabPanel("Player Stats", fluid = T,
                         selectInput("variable2", "Game:", gm_list, width = '375px'),
                         mainPanel(
                             gt_output("player_plot"),
                             width = 12
                         )
                ),
                tabPanel("Team Stats", fluid = T,
                         selectInput("variable4", "Location:", loc_list, width = '375px'),
                         mainPanel(
                             gt_output("team_plot"),
                             width = 12
                         )
                )
                
    )
)



shinyApp(ui, server)
## runApp("~/shinyapp")











