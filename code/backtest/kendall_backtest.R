# if (!require("pacman")) install.packages("pacman"); library(pacman)
# pacman::p_load(tidyverse, readxl, lubridate, openxlsx, nbastatR, rvest, progress, XLConnect)

library(tidyverse)
library(lubridate)
library(nbastatR)
library(readxl)
# library(openxlsx)
# library(XLConnect)
# library(progress)
# library(rvest)

Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)

rm(list=ls())
setwd("/Users/Jesse/Documents/MyStuff/NBA Betting/Backtest 2022/")

dataGameLogsTeam <- game_logs(seasons = c(2019:2023), result_types = "team")

# dataGameLogsTeam <- read_xlsx("/Users/Jesse/Documents/MyStuff/NBA Betting/Backtest/DataGameLogsTeam.xlsx")

dataGameLogsTeam <- dataGameLogsTeam %>% arrange(dateGame,idGame)
dataGameLogsTeam$dateGame <- as_date(dataGameLogsTeam$dateGame)

league_avg <- read_xlsx("/Users/Jesse/Documents/MyStuff/NBA Betting/NBAdb/NBAdb1722_Lg_Avg.xlsx")

# master_db_all <- read_xlsx("/Users/Jesse/Documents/MyStuff/NBA Betting/NBAdb/NBAdb1722_noadj.xlsx")
master_db_all <- read_xlsx("/Users/Jesse/Documents/MyStuff/NBA Betting/NBAdb/NBAdb1722.xlsx")

odds <- read_xlsx("/Users/Jesse/Documents/MyStuff/NBA Betting/Backtest 2022/nba odds 2021-22.xlsx") 
odds <- odds %>%
    filter(Test == 1)

dates_distinct <- odds %>%
    distinct(Date) %>%
    mutate(Stat_Date = as_date(Date - 1))

odds$Date <- as.character(odds$Date)

dates_distinct$Date <- as.character(dates_distinct$Date)
dates_distinct$Stat_Date <- as.character(dates_distinct$Stat_Date)

final_backtest <- data.frame()


### Run Models ###

b <- 1
h <- nrow(dates_distinct)

for (b in b:h) {
    
    stats_end_gxg <- as.character(dates_distinct[b,2])
    gm_day_gxg <- as.character(dates_distinct[b,1])
    
    td <- as_date(gm_day_gxg)
    
    slate <- master_db_all %>%
        filter(Date == td & Loc == 'H') %>%
        select(1,3,4)
    
    colnames(slate) <- c("Date","Away","Home")
    
    slate <- slate %>%
        mutate(across(where(is.character), str_replace_all, pattern = "Los Angeles Clippers", 
                      replacement = "LA Clippers"))
    
    ### Read in Database ###
    
    master_db <- master_db_all %>%
        filter(Date < as_date(gm_day_gxg))
    
    away_final_wt <- master_db_all %>%
        filter(Date == td) %>%
        select(3,9:43) %>%
        rename(Team = Away)
    
    home_final_wt <- master_db_all %>%
        filter(Date == td) %>%
        select(4,44:78) %>%
        rename(Team = Home)
    
    
    #### Kendall #### - Rating and Pythag formulas
    
    pb <- progress_bar$new(
        format = "  running model... [:bar] :percent eta: :eta",
        total = nrow(slate), clear = FALSE, width = 60, show_after = 0)
    invisible(pb$tick(0))
    
    kendall_predict <- data.frame() # Predictions frame
    
    a <- 1
    g <- nrow(slate)
    
    for (a in a:g) {
        
        slate_away <- as.character(slate[a,2])
        slate_home <- as.character(slate[a,3])
        
        kendall_away <- away_final_wt %>%
            filter(Team == slate_away)
        
        kendall_home <- home_final_wt %>%
            filter(Team == slate_home)
        
        kendall_away_oe <- as.numeric(kendall_away[,34])
        kendall_away_de <- as.numeric(kendall_away[,35])
        kendall_away_pa <- as.numeric(kendall_away[,36])
        
        kendall_home_oe <- as.numeric(kendall_home[,34])
        kendall_home_de <- as.numeric(kendall_home[,35])
        kendall_home_pa <- as.numeric(kendall_home[,36])
        
        league_avg_td <- league_avg %>%
            filter(Date == td)
        
        lg_pace <- as.numeric(league_avg_td[1,34])
        lg_oe <- as.numeric(league_avg_td[1,36])
        
        away_pace_vslg <- kendall_away_pa - lg_pace
        home_pace_vslg <- kendall_home_pa - lg_pace
        vslg_sums <- away_pace_vslg + home_pace_vslg
        
        expected_pace <- (lg_pace + vslg_sums)
        
        away_oe_vslg <- kendall_away_oe - lg_oe
        away_de_vslg <- kendall_away_de - lg_oe
        home_oe_vslg <- kendall_home_oe - lg_oe
        home_de_vslg <- kendall_home_de - lg_oe
        
        away_oe_vslgsums <- away_oe_vslg + home_de_vslg
        home_oe_vslgsums <- home_oe_vslg + away_de_vslg
        
        away_proj_oe <- lg_oe + away_oe_vslgsums
        home_proj_oe <- lg_oe + home_oe_vslgsums
        
        away_proj_oe <- away_proj_oe / 100
        home_proj_oe <- home_proj_oe / 100
        
        away_kendall_score <- (away_proj_oe * expected_pace)
        home_kendall_score <- (home_proj_oe * expected_pace) 
        
        away_kendall_win <- (away_proj_oe ^ 14.23) / ((away_proj_oe ^ 14.23) + (home_proj_oe ^ 14.23))
        home_kendall_win <- 1 - away_kendall_win
        
        holder <- slate[a,2:3]
        holder$Away_Margin <- away_kendall_score - home_kendall_score
        holder$Home_Margin <- home_kendall_score - away_kendall_score
        holder$Away_Margin2 <- away_kendall_score - home_kendall_score
        holder$Home_Margin2 <- home_kendall_score - away_kendall_score
        holder$Away_Win <- away_kendall_win
        holder$Home_Win <- home_kendall_win
        holder$Total <- away_kendall_score + home_kendall_score
        
        kendall_predict <- bind_rows(kendall_predict,holder)
        
        pb$tick()
        Sys.sleep(1 / nrow(slate))
        
    }
    
    kendall_predict <- kendall_predict %>%
        mutate(across(where(is.numeric), round, 3))
    
    
    
    kendall_predict$Model <- "Kendall Jenner - Simple Model"

    all_models <- kendall_predict
    
    slate <- slate %>%
        left_join(., dataGameLogsTeam, by = c("Date" = "dateGame", "Home" = "nameTeam")) %>%
        select(1:3,9)
    
    plays_a <- slate %>%
        mutate(Loc = "A") %>%
        select(4,1,5,3,2)
    colnames(plays_a) <- c("idGame", "Date", "Loc", "oppTeam", "Team")
    
    plays_a <- plays_a %>%
        left_join(kendall_predict, by = c("Team" = "Away"))
    
    plays_a <- plays_a %>%
        select(1:5, 
               7, 9, 11, 13)
    
    colnames(plays_a) <- c("idGame", "Date", "Loc", "oppTeam", "Team", 
                           "Kendall_Margin", "Kendall_Margin2", "Kendall_Win", "Kendall_Total")
    
    plays_h <- slate %>%
        mutate(Loc = "H") %>%
        select(4,1,5,2,3)
    colnames(plays_h) <- c("idGame", "Date", "Loc", "oppTeam", "Team")
    
    plays_h <- plays_h %>%
        left_join(kendall_predict, by = c("oppTeam" = "Away"))
    
    plays_h <- plays_h %>%
        select(1:5, 
               8, 10, 12, 13)
    
    colnames(plays_h) <- c("idGame", "Date", "Loc", "oppTeam", "Team", 
                           "Kendall_Margin", "Kendall_Margin2", "Kendall_Win", "Kendall_Total")
    
    plays <- bind_rows(plays_a, plays_h)
    
    plays <- plays %>%
        arrange(idGame)
    
    
    # bring in odds
    odds <- read_xlsx("/Users/Jesse/Documents/MyStuff/NBA Betting/Backtest 2022/nba odds 2021-22.xlsx")
    
    odds <- odds %>%
        mutate(across(where(is.character), str_replace_all, pattern = "L.A. Clippers", replacement = "LA Clippers"))
    
    plays <- plays %>%
        left_join(odds, by = c("Team", "Date")) %>%
        select(1:5, 12, 13, 14, 6:9)
    
    plays$Kendall_Spread_Edge <- with(plays, Kendall_Margin + Spread)
    plays$Kendall_Spread2_Edge <- with(plays, Kendall_Margin2 + Spread)
    plays$Kendall_ML_Edge <- with(plays, Kendall_Win - round((if_else(ML<0,((ML*-1)/((ML*-1)+100)),(100/(ML+100)))), 3))
    plays$Kendall_Over_Edge <- with(plays, Kendall_Total - Total)
    plays$Kendall_Under_Edge <- with(plays, Total - Kendall_Total)
    
    final_backtest <- bind_rows(final_backtest,plays)
    
    print(paste0(round((b/h)*100,1),"%"))
    
    
}    


detach("package:XLConnect", unload = TRUE)

fn <- "Backtest_2022_Kendall_Adj"
u <- paste0("/Users/Jesse/Documents/MyStuff/NBA Betting/Backtest 2022/Backtest Output/",fn,".xlsx")

wb <- createWorkbook()
addWorksheet(wb, sheetName = "Backtest")
writeData(wb, sheet = "Backtest", x = final_backtest)
saveWorkbook(wb, file = u)