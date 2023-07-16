if (!require("pacman")) install.packages("pacman"); library(pacman)
pacman::p_load(tidyverse, readxl, lubridate, openxlsx, nbastatR, rvest, progress, XLConnect, caret, e1071)

Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)

rm(list=ls())
setwd("/Users/Jesse/Documents/MyStuff/NBA Betting/Backtest 2022/")

# game_logs(seasons = 2022, result_types = c("team","players"))

dataGameLogsTeam <- read_xlsx("/Users/Jesse/Documents/MyStuff/NBA Betting/Backtest 2022/DataGameLogsTeam.xlsx")

dataGameLogsTeam <- dataGameLogsTeam %>% arrange(dateGame,idGame)
dataGameLogsTeam$dateGame <- as_date(dataGameLogsTeam$dateGame)

master_db_all <- read_xlsx("/Users/Jesse/Documents/MyStuff/NBA Betting/NBAdb/NBAdb1722_noadj.xlsx")
# master_db_all <- read_xlsx("/Users/Jesse/Documents/MyStuff/NBA Betting/NBAdb/NBAdb1722.xlsx")

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
        filter(Date < td)
    
    master_db_today <- master_db_all %>%
        filter(Date == td)
    
    nba_scaled <- master_db
    nba_scaled_today <- master_db_today
    
    pre_proc_scaled <- preProcess(nba_scaled[,-c(1:8)], method = c("center", "scale"))
    
    nba_scaled[,-c(1:8)] = predict(pre_proc_scaled, nba_scaled[,-c(1:8)])
    nba_scaled_today[,-c(1:8)] = predict(pre_proc_scaled, nba_scaled_today[,-c(1:8)])

    # All
    nba_marg <- nba_scaled  %>% select(7, 9:78)
    nba_win <- nba_scaled %>% select(8, 9:78)
    nba_as <- nba_scaled %>% select(5, 9:78)
    nba_hs <- nba_scaled %>% select(6, 9:78)
    
    # # TS
    # nba_marg <- nba_scaled %>%
    #     select(7,14,15,16,18,19,21,24,30,34,35,37,40,43,
    #            49,50,51,53,54,56,59,65,69,70,72,75,78)
    # 
    # nba_win <- nba_scaled %>%
    #     select(8,14,15,16,18,19,21,24,30,34,35,37,40,43,
    #            49,50,51,53,54,56,59,65,69,70,72,75,78)
    # 
    # nba_as <- nba_scaled %>%
    #     select(5,14,15,16,18,19,21,24,30,34,35,37,40,43,
    #            49,50,51,53,54,56,59,65,69,70,72,75,78)
    # 
    # nba_hs <- nba_scaled %>%
    #     select(6,14,15,16,18,19,21,24,30,34,35,37,40,43,
    #            49,50,51,53,54,56,59,65,69,70,72,75,78)
    
    # # eFG
    # nba_marg <- nba_scaled %>%
    #     select(7,14,15,16,18,19,21,23,30,34,35,37,39,43,
    #            49,50,51,53,54,56,58,65,69,70,72,74,78)
    # 
    # nba_win <- nba_scaled %>%
    #     select(8,14,15,16,18,19,21,23,30,34,35,37,39,43,
    #            49,50,51,53,54,56,58,65,69,70,72,74,78)
    # 
    # nba_as <- nba_scaled %>%
    #     select(5,14,15,16,18,19,21,23,30,34,35,37,39,43,
    #            49,50,51,53,54,56,58,65,69,70,72,74,78)
    # 
    # nba_hs <- nba_scaled %>%
    #     select(6,14,15,16,18,19,21,23,30,34,35,37,39,43,
    #            49,50,51,53,54,56,58,65,69,70,72,74,78)
    
    #### Cindy #### - SVM
    
    pb <- progress_bar$new(
        format = "  running model... [:bar] :percent eta: :eta",
        total = nrow(slate), clear = FALSE, width = 60, show_after = 0)
    invisible(pb$tick(0))
    
    svm_marg <- svm(Margin ~., nba_marg, cost=10, kernel = "linear")
    svm_as <- svm(AS ~., nba_as, cost=10, kernel = "linear")
    svm_hs <- svm(HS ~., nba_hs, cost=10, kernel = "linear")
    svm_win <- svm(as.factor(Win) ~ ., data = nba_win, cost = .005, kernal = 'linear', probability=TRUE)
    
    cindy_predict <- data.frame() # Predictions frame
    
    a <- 1
    g <- nrow(slate)
    
    for (a in a:g) {
        
        slate_away <- as.character(slate[a,2])
        slate_home <- as.character(slate[a,3])
        
        # All
        svm_input <- nba_scaled_today %>%
            filter(Away == slate_away & Home == slate_home) %>%
            select(9:78)
        
        # # TS
        # svm_input <- nba_scaled_today %>%
        #     filter(Away == slate_away & Home == slate_home) %>%
        #     select(14,15,16,18,19,21,24,30,34,35,37,40,43,
        #            49,50,51,53,54,56,59,65,69,70,72,75,78)
        
        # # eFG
        # svm_input <- nba_scaled_today %>%
        #     filter(Away == slate_away & Home == slate_home) %>%
        #     select(14,15,16,18,19,21,23,30,34,35,37,39,43,
        #            49,50,51,53,54,56,58,65,69,70,72,74,78)
        
        cindy_margin <- predict(svm_marg, svm_input)
        cindy_as <- predict(svm_as, svm_input)
        cindy_hs <- predict(svm_hs, svm_input)
        cindy_win <- predict(svm_win, svm_input, probability=TRUE)
        
        holder <- slate[a,2:3]
        holder$Away_Margin <- cindy_margin
        holder$Home_Margin <- cindy_margin*-1
        holder$Away_Margin2 <- cindy_as - cindy_hs
        holder$Home_Margin2 <- cindy_hs - cindy_as
        holder$Away_Win <- attr(cindy_win, "probabilities")[,2]
        holder$Home_Win <- attr(cindy_win, "probabilities")[,1]
        holder$Total <- cindy_as + cindy_hs
        
        cindy_predict <- bind_rows(cindy_predict,holder)
        
        pb$tick()
        Sys.sleep(1 / nrow(slate))
        
    }
    
    cindy_predict <- cindy_predict %>%
        mutate(across(where(is.numeric), round, 3))
    
    cindy_predict$Model <- "Cindy Crawford - SVM"
    
    all_models <- cindy_predict
    
    slate <- slate %>%
        left_join(., dataGameLogsTeam, by = c("Date" = "dateGame", "Home" = "nameTeam")) %>%
        select(1:3,9)
    
    plays_a <- slate %>%
        mutate(Loc = "A") %>%
        select(4,1,5,3,2)
    colnames(plays_a) <- c("idGame", "Date", "Loc", "oppTeam", "Team")
    
    plays_a <- plays_a %>%
        left_join(cindy_predict, by = c("Team" = "Away"))
    
    plays_a <- plays_a %>%
        select(1:5, 
               7, 9, 11, 13)
    
    colnames(plays_a) <- c("idGame", "Date", "Loc", "oppTeam", "Team", 
                           "Cindy_Margin", "Cindy_Margin2", "Cindy_Win", "Cindy_Total")
    
    plays_h <- slate %>%
        mutate(Loc = "H") %>%
        select(4,1,5,2,3)
    colnames(plays_h) <- c("idGame", "Date", "Loc", "oppTeam", "Team")
    
    plays_h <- plays_h %>%
        left_join(cindy_predict, by = c("oppTeam" = "Away"))
    
    plays_h <- plays_h %>%
        select(1:5, 
               8, 10, 12, 13)
    
    colnames(plays_h) <- c("idGame", "Date", "Loc", "oppTeam", "Team", 
                           "Cindy_Margin", "Cindy_Margin2", "Cindy_Win", "Cindy_Total")
    
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
    
    plays$Cindy_Spread_Edge <- with(plays, Cindy_Margin + Spread)
    plays$Cindy_Spread2_Edge <- with(plays, Cindy_Margin2 + Spread)
    plays$Cindy_ML_Edge <- with(plays, Cindy_Win - round((if_else(ML<0,((ML*-1)/((ML*-1)+100)),(100/(ML+100)))), 3))
    plays$Cindy_Over_Edge <- with(plays, Cindy_Total - Total)
    plays$Cindy_Under_Edge <- with(plays, Total - Cindy_Total)
    
    final_backtest <- bind_rows(final_backtest,plays)
    
    print(paste0(round((b/h)*100,1),"%"))
    
}    


detach("package:XLConnect", unload = TRUE)

fn <- "Backtest_2022_Cindy_All"
u <- paste0("/Users/Jesse/Documents/MyStuff/NBA Betting/Backtest 2022/",fn,".xlsx")

wb <- createWorkbook()
addWorksheet(wb, sheetName = "Backtest")
writeData(wb, sheet = "Backtest", x = final_backtest)
saveWorkbook(wb, file = u)