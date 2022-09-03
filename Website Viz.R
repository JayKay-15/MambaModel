if (!require("pacman")) install.packages("pacman"); library(pacman)
pacman::p_load(tidyverse, readxl, lubridate, openxlsx, shiny,
               nbastatR, ggimage, ggrepel, data.table, gt, janitor, zoo, gt)
# devtools::install_github("beanumber/teamcolors", force=TRUE)
# library(teamcolors)
# rm(list=ls())

rosters <- nbastatR::seasons_rosters(seasons = 2022)

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
    filter(isSeasonCurrent == 1)

players_df_adv <- players_df_adv %>%
    collect() %>%
    filter(isSeasonCurrent == 1)

team_dict <- team_dict %>%
    collect()

DBI::dbDisconnect(NBAdb)

td <- as_date("2022-05-15")

slate <- sched %>%
    filter(dateGame == td) %>%
    mutate(gameTime = hms::as_hms(datetimeGame - 18000)) %>%
    select(4,29,24,2,34)

colnames(slate) <- c("idGame","Away","Home","Date","Game Time")

slate <- slate %>%
    mutate(across(where(is.character), str_replace_all, 
                  pattern = "Los Angeles Clippers", replacement = "LA Clippers"))

slate_df <- slate %>%
    mutate(gm = paste0(Away," @ ",Home)) %>%
    select(6,2,3,5)

colnames(slate_df) <- c("GM","Away","Home","GM_Time")

slate_df <- slate_df %>%
    left_join(team_dict[c(2,9,10)], by = c("Home" = "nameTeam"))


### Model Performance ------------------------------------------
# fixed keys
df <- read_xlsx(paste0("/Users/Jesse/Documents/MyStuff/NBA Betting/Backtest 2022/Backtest Output/",
                       "2021 Tyra/Backtest_Results_2021_Tyra_eFG.xlsx"))
df <- df %>% 
    filter(Tyra_ML_Edge >= 0.1) %>%
    # mutate(DayMonth = format(as.Date(Date), "%d-%m")) %>%
    mutate(DayMonth = lubridate::month(Date, label = T)) %>%
    group_by(DayMonth) %>%
    summarise(day_total_2021 = sum(Tyra_ML_Result))
    # mutate(cume_2021 = cumsum(day_total_2021))
    


df2 <- read_xlsx(paste0("/Users/Jesse/Documents/MyStuff/NBA Betting/Backtest 2022/Backtest Output/",
                       "Tyra/Backtest_Results_2022_Tyra_eFG.xlsx"))
df2 <- df2 %>% 
    filter(Tyra_ML_Edge >= 0.2) %>%
    # mutate(DayMonth = format(as.Date(Date), "%d-%m")) %>%
    mutate(DayMonth = lubridate::month(Date, label = T)) %>%
    group_by(DayMonth) %>%
    summarise(day_total_2022 = sum(Tyra_ML_Result))
    # mutate(cume_2022 = cumsum(day_total_2022))

# df <- rbind(df, df2)
df <- df %>% full_join(df2)
df$DayMonth <- factor(df$DayMonth, levels = c('Nov', 'Dec', 'Jan', 'Feb', 'Mar', 'Apr', 'May'))

df <- df %>%
    mutate(cume_2021 = cumsum(day_total_2021)) %>%
    arrange(DayMonth) %>%
    mutate(cume_2022 = cumsum(day_total_2022))


df %>%
    ggplot(aes(DayMonth)) +
    geom_line(aes(y=cume_2021, color="2021"), size=1.24, group=1) +
    geom_line(aes(y=cume_2022, color="2022"), size=1.24, group=1) +
    scale_color_manual(name = "", values = c("2021" = "#9590FF", "2022" = "darkblue")) +
    labs(x = "Month", y = "Units", title = "Least Squares Model Performance", subtitle = "Moneyline") +
    scale_x_discrete() + 
    theme_bw()



# dynamic keys
df <- read_xlsx(paste0("/Users/Jesse/Documents/MyStuff/NBA Betting/Backtest 2022/Backtest Output/",
                        "Backtest_Results_2022_Tyra_All_Adj.xlsx"))
df$Date <- as_date(df$Date)

t <- seq(0.1,25,0.1)
# t <- seq(0.01,1,0.01)
b <- 1
h <- length(t)
bst_tot <- 0
# bst_flt <- 10

for (b in b:h) {
    
    flt <- as.numeric(t[b])
    
    temp <- as.numeric(df %>% filter(Tyra_Over_Edge >= flt) %>% 
                           summarise(day_total = sum(Tyra_Over_Result)))
    
    if (temp > bst_tot) {
        bst_tot <- temp
        bst_flt <- flt
    }
}

df %>%
    filter(Tyra_Over_Edge >= bst_flt) %>%
    group_by(Date) %>%
    summarise(day_total = sum(Tyra_Over_Result)) %>%
    mutate(cume = cumsum(day_total)) %>%
    ggplot(aes(Date)) +
    geom_line(aes(y=cume, color="All Plays"), size=1) +
    scale_color_manual(name = "", values = c("All Plays" = "darkblue")) +
    labs(x = "Date", y = "Units", title = "2022 Performance", subtitle = "Model") +
    theme_bw()

print(bst_tot)
print(bst_flt)
print(df %>% filter(Tyra_Over_Edge >= bst_flt) %>% nrow())






### players - old verison
# players_df <- dataGameLogsPlayer %>%
#     select(16,42,17:36) %>%
#     group_by(nameTeam,namePlayer) %>%
#     summarise(across(numberGamePlayerSeason, max), 
#               across(c(minutes:fga, fg3m:fg3a, ftm:fta, oreb:pts), ~round(mean(.),1)),
#               across(plusminus, sum)) %>%
#     mutate(pctFG = round(fgm/fga,3),
#            pctFG3 = round(fg3m/fg3a,3),
#            pctFT = round(ftm/fta,3)) %>%
#     mutate(across(everything(), ~replace(., is.nan(.), 0))) %>%
#     select(1:6,20,7:8,21,9:10,22,11:19)








#########################################################################
####################### Working Shiny App ###############################
#########################################################################

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
        opt_all_caps()  %>%
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
            source_notes.font.size = 10
        ) %>%
        cols_align(
            align = "center",
            columns = everything()
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
            data_row.padding = px(3)
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
            source_note = md("@MambaMetrics")
        )
    
}

# rank function
rank_tbl <- function(a, h) {
    
    rank_df_away <- away_rank %>% 
        filter(Team == as.character(a)) %>% 
        select(1,3,5,7,9,11,13,15,17,21,23,25,27,29,31,33,35,
               37,39,41,43,45,53,55,57,59,61,63,65,67,69,71) %>%
        transpose() %>%
        row_to_names(1) %>%
        mutate(sorter = c(1,2,3,4,5,6,7,23,8,9,10,11,12,13,14,17,
                          18,19,20,21,22,24,25,26,27,28,29,30,15,31,16)) %>%
        arrange(sorter) %>%
        select(1)
    
    rank_df_home <- home_rank %>% 
        filter(Team == as.character(h)) %>% 
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
        mutate(filler = "----------------------") %>%
        gt(groupname_col = c("off_def", "filler", "def_off")) %>%
        opt_all_caps()  %>%
        opt_table_font(
            font = list(
                google_font("Chivo"),
                default_fonts()
            )
        ) %>%
        tab_spanner(
            label = "AWAY",
            columns = 1
        ) %>%
        tab_spanner(
            label = "HOME",
            columns = 3
        ) %>%
        tab_spanner(
            label = "BLANK",
            columns = 2
        ) %>%
        tab_style(
            style = list(
                cell_fill(color = "black"),
                cell_text(color = "white")
            ),
            locations = list(
                cells_column_spanners(
                    spanners = c("AWAY", "HOME")
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
                    spanners = c("BLANK")
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
            row_group.font.size = 12,
            source_notes.font.size = 10
        ) %>%
        cols_align(
            align = "center",
            columns = everything()
        ) %>%
        cols_width(
            AT ~ px(85),
            !AT ~ px(90)
        ) %>%
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
        )
}

# stats function
stats_tbl <- function(a, h) {
    
    stats_df_away <- away_final_wt %>% 
        filter(Team == as.character(a)) %>% 
        select(-c(10,24,25,26)) %>%
        transpose() %>%
        row_to_names(1) %>%
        mutate(sorter = c(1,2,3,4,5,6,7,23,8,9,10,11,12,13,14,17,
                          18,19,20,21,22,24,25,26,27,28,29,30,15,31,16)) %>%
        arrange(sorter) %>%
        select(1)
    
    stats_df_home <- home_final_wt %>% 
        filter(Team == as.character(h)) %>% 
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
        mutate(filler = "----------------------") %>%
        gt(groupname_col = c("off_def", "filler", "def_off")) %>%
        opt_all_caps()  %>%
        opt_table_font(
            font = list(
                google_font("Chivo"),
                default_fonts()
            )
        ) %>%
        tab_spanner(
            label = "AWAY",
            columns = 1
        ) %>%
        tab_spanner(
            label = "HOME",
            columns = 3
        ) %>%
        tab_spanner(
            label = "BLANK",
            columns = 2
        ) %>%
        tab_style(
            style = list(
                cell_fill(color = "black"),
                cell_text(color = "white")
            ),
            locations = list(
                cells_column_spanners(
                    spanners = c("AWAY", "HOME")
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
                    spanners = c("BLANK")
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
                cell_fill(color = "#d39dfa")
            ),
            locations = cells_body(
                columns = 1
            )
        ) %>%
        tab_style(
            style = list(
                cell_fill(color = "#d39dfa")
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
            row_group.font.size = 12,
            source_notes.font.size = 10
        ) %>%
        cols_align(
            align = "center",
            columns = everything()
        ) %>%
        cols_width(
            AT ~ px(85),
            !AT ~ px(90)
        ) %>%
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
        )
    
}

# model output - game
models_tbl <- function(g) {
    
    model_df_viz <- model_df_fmt %>%
        filter(Game == as.character(g)) %>%
        select(-1)
    
    model_df_viz %>%
        gt(groupname_col = "Metric") %>%
        opt_all_caps()  %>%
        opt_table_font(
            font = list(
                google_font("Chivo"),
                default_fonts()
            )
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
            source_notes.font.size = 10
        ) %>%
        cols_align(
            align = "center",
            columns = 3:9
        ) %>%
        fmt_percent(
            columns = 3:9,
            rows = 3:4,
            decimals = 1
        ) %>%
        fmt_number(
            columns = 3:9,
            rows = c(1,2,5:8),
            decimals = 1
        )
}

# model output
models_all_tbl <- function(m) {
    
    model_df_all_viz <- model_df_fmt %>%
        select(1,3,2,4:10) %>%
        filter(if (m == "All") Metric != m else Metric == m )

    model_df_all_viz %>%
        gt(groupname_col = "Game") %>%
        opt_all_caps()  %>%
        opt_table_font(
            font = list(
                google_font("Chivo"),
                default_fonts()
            )
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
            source_notes.font.size = 10
        ) %>%
        cols_align(
            align = "center",
            columns = 4:10
        ) %>%
        fmt_percent(
            columns = 4:10,
            rows = Metric == "Moneyline",
            decimals = 1
        ) %>%
        fmt_number(
            columns = 4:10,
            rows = Metric != "Moneyline",
            decimals = 1
        ) %>%
        opt_row_striping()
}


### Shiny App ---------------------------------------------
# Game list
gm_list <- unique(slate_df$GM)
metric_list <- c("All","Spread","Moneyline","Team Total","Total")

# Standings
standings_df <- standings %>%
    select(1:4,6:8,10:12,14:16)

# Players
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
# Models
models_df <- all_models %>%
    select(1:6,8,9,7,10) %>%
    mutate(gm = paste0(all_models$Away, " @ ", all_models$Home)) %>%
    rename_with(~ paste0('c', ceiling(seq_along(.x) / 2), '_', 1:2)) %>%
    pivot_longer(cols = c(1:9), names_to = c(".value", NA), names_sep = '_') %>%
    mutate(across(c5, ~ na.locf(.x))) %>%
    select(3:7,2,1)

colnames(models_df) <- c("Team", "Margin", "Win", "TeamScore", "GameTotal", "Game", "Model")

models_df <- models_df %>%
    pivot_wider(names_from = Model, values_from = c(Margin, Win, TeamScore, GameTotal))

models_df_marg <- models_df  %>% mutate(Metric = "Spread") %>% select(2,31,1,3:9)
colnames(models_df_marg) <- c("Game","Metric","Team",
                              "Kendall - Simple","Tyra - Least Squares","Gisele - KNN","Kate - Random Forest",
                              "Cindy - SVM","Naomi - Neural Net","Adriana - Ensemble")
models_df_win <- models_df  %>% mutate(Metric = "Moneyline") %>% select(2,31,1,10:16)
colnames(models_df_win) <- c("Game","Metric","Team",
                             "Kendall - Simple","Tyra - Least Squares","Gisele - KNN","Kate - Random Forest",
                             "Cindy - SVM","Naomi - Neural Net","Adriana - Ensemble")
models_df_tm_total <- models_df  %>% mutate(Metric = "Total") %>% select(2,31,1,24:30)
colnames(models_df_tm_total) <- c("Game","Metric","Team",
                                  "Kendall - Simple","Tyra - Least Squares","Gisele - KNN","Kate - Random Forest",
                                  "Cindy - SVM","Naomi - Neural Net","Adriana - Ensemble")
models_df_gm_total <- models_df  %>% mutate(Metric = "Team Total") %>% select(2,31,1,17:23)
colnames(models_df_gm_total) <- c("Game","Metric","Team",
                                  "Kendall - Simple","Tyra - Least Squares","Gisele - KNN","Kate - Random Forest",
                                  "Cindy - SVM","Naomi - Neural Net","Adriana - Ensemble")

model_df_fmt <- rbind(models_df_marg, models_df_win, models_df_gm_total, models_df_tm_total)

# Define server logic to plot various variables against mpg ----
server <- function(input, output, session) {
    
    variablename <- reactiveVal(gm_list[1])
    
    metricname <- reactiveVal(metric_list[1])
    
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

    #Reactive
    gm_input <- reactive({
        
        xx <- slate_df %>% filter(GM == as.character(variablename()))
        return(xx)
        
    })
    
    metric_input <- reactive({

        metricname()
        
    })
    
    ### Summary Plot    
    output$summary_plot <- render_gt({
        
       summary_tbl(gm_input()$Away, gm_input()$Home, gm_input()$Arena, gm_input()$Location) 

    })
   
    ### Player Stats Plot
    output$player_plot <- render_gt({

        player_tbl(gm_input()$Away, gm_input()$Home)
        
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
        
        models_tbl(gm_input()$GM)
        
    })
    
    ### Models Plot - All
    output$models_all_plot <- render_gt({
        
        models_all_tbl(metric_input())
        
    })
    
}

# Define UI
ui <- fluidPage(
    
    theme = bslib::bs_theme(bootswatch = "darkly"),
    
    titlePanel("Mamba Metrics - Today's Plays"),
    
    # fluidRow(column(selectInput("variable", "Game:", gm_list), width = 5)),
    
    tabsetPanel(type = "tabs",
                tabPanel("Models & Summary", fluid = T,
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
                tabPanel("Player Stats", fluid = T,
                         selectInput("variable2", "Game:", gm_list),
                         mainPanel(
                             gt_output("player_plot"),
                             width = 12
                         )
                ),
                tabPanel("All Models", fluid = T,
                         sidebarLayout(
                             sidebarPanel(
                                 selectInput("variable3", "Metric:", metric_list),
                                 "Model Stats"
                             ),
                             mainPanel(
                                 gt_output("models_all_plot")
                             )
                         )
                )
    )
)


shinyApp(ui, server) 
## runApp("~/shinyapp")



