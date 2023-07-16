if (!require("pacman")) install.packages("pacman"); library(pacman)
pacman::p_load(tidyverse, readxl, lubridate, openxlsx, nbastatR, rvest, progress, XLConnect, caret, glmnet)

Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)

rm(list=ls())
setwd("/Users/Jesse/Documents/MyStuff/NBA Betting/Backtest 2022/")

# game_logs(seasons = 2021, result_types = c("team","players"))

dataGameLogsTeam <- read_xlsx("/Users/Jesse/Documents/MyStuff/NBA Betting/Backtest 2022/DataGameLogsTeam.xlsx")

dataGameLogsTeam <- dataGameLogsTeam %>% arrange(dateGame,idGame)
dataGameLogsTeam$dateGame <- as_date(dataGameLogsTeam$dateGame)

master_db_all <- read_xlsx("/Users/Jesse/Documents/MyStuff/NBA Betting/NBAdb/NBAdb1422.xlsx")
# master_db_all <- read_xlsx("/Users/Jesse/Documents/MyStuff/NBA Betting/NBAdb/NBAdb1422_adj.xlsx")
master_db_all <- master_db_all %>% filter(Season >= 2019) %>% select(-1)
# master_db_all <- master_db_all %>% select(-1)

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
    # nba_marg <- nba_scaled  %>% select(7, 9:78)
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
     
    #### Gisele #### - Regularization
    
    pb <- progress_bar$new(
        format = "  running model... [:bar] :percent eta: :eta",
        total = nrow(slate), clear = FALSE, width = 60, show_after = 0)
    invisible(pb$tick(0))
    
    # # Elastic Net
    # lambdas <- seq(0.0001,3,length=50)
    # alpha_grid <- seq(0, 1, 0.1)
    # grid <- expand.grid(.alpha = alpha_grid, .lambda = lambdas)
    # 
    # ctrl <- trainControl(method = "repeatedcv",
    #                      number = 10,
    #                      repeats = 3,
    #                      verboseIter = F)
    # 
    # # Margin
    # reg_model_marg <- train(Margin ~ .,
    #                      data = nba_marg,
    #                      method = "glmnet",
    #                      tuneGrid = grid,
    #                      trControl = ctrl)
    # # reg_model_marg$bestTune
    # 
    # # Away score
    # reg_model_as <- train(AS ~ .,
    #                       data = nba_as,
    #                       method = "glmnet",
    #                       tuneGrid = grid,
    #                       trControl = ctrl)
    # # reg_model_as$bestTune
    # 
    # # Home score
    # reg_model_hs <- train(HS ~ .,
    #                       data = nba_hs,
    #                       method = "glmnet",
    #                       tuneGrid = grid,
    #                       trControl = ctrl)
    # # reg_model_hs$bestTune
    # 
    # # Win
    # reg_model_win <- train(as.factor(Win) ~ .,
    #                        data = nba_win,
    #                        method = "glmnet",
    #                        tuneGrid = grid,
    #                        trControl = ctrl)
    # # reg_model_win$bestTune
    
    # Ridge/Lasso - alpha = 1 for lasso; alpha = 0 for ridge
    lambdas <- 10^seq(2, -3, by = -.1)
    
    ## Margin
    # reg_marg <- cv.glmnet(as.matrix(nba_marg[-1]), nba_marg$Margin, alpha = 1, lambda = lambdas,
    #                         family='gaussian', type.measure='mse')
    # lambda_marg <- reg_marg$lambda.min
    # reg_model_marg <- glmnet(as.matrix(nba_marg[-1]), nba_marg$Margin, alpha = 1,
    #                            lambda = lambda_marg, family='gaussian')

    # Away score
    reg_as <- cv.glmnet(as.matrix(nba_as[-1]), nba_as$AS, alpha = 0, lambda = lambdas,
                          family='gaussian', type.measure='mse')
    lambda_as <- reg_as$lambda.min
    reg_model_as <- glmnet(as.matrix(nba_as[-1]), nba_as$AS, alpha = 0,
                             lambda = lambda_as, family='gaussian')

    # Home score
    reg_hs <- cv.glmnet(as.matrix(nba_hs[-1]), nba_hs$HS, alpha = 0, lambda = lambdas,
                          family='gaussian', type.measure='mse')
    lambda_hs <- reg_hs$lambda.min
    reg_model_hs <- glmnet(as.matrix(nba_hs[-1]), nba_hs$HS, alpha = 0,
                             lambda = lambda_hs, family='gaussian')

    # Win
    reg_win <- cv.glmnet(as.matrix(nba_win[-1]), nba_win$Win, alpha = 0, lambda = lambdas,
                           family='binomial', type.measure='auc')
    lambda_win <- reg_win$lambda.min
    reg_model_win <- glmnet(as.matrix(nba_win[-1]), nba_win$Win, alpha = 0,
                              lambda = lambda_win, family='binomial')

    gisele_predict <- data.frame() # Predictions frame
    
    a <- 1
    g <- nrow(slate)
    
    for (a in a:g) {
        
        slate_away <- as.character(slate[a,2])
        slate_home <- as.character(slate[a,3])
        
        # All
        reg_input <- nba_scaled_today %>%
            filter(Away == slate_away & Home == slate_home) %>%
            select(9:78)
        
        # # TS
        # reg_input <- nba_scaled_today %>%
        #     filter(Away == slate_away & Home == slate_home) %>%
        #     select(14,15,16,18,19,21,24,30,34,35,37,40,43,
        #            49,50,51,53,54,56,59,65,69,70,72,75,78)
        
        # # eFG
        # reg_input <- nba_scaled_today %>%
        #     filter(Away == slate_away & Home == slate_home) %>%
        #     select(14,15,16,18,19,21,23,30,34,35,37,39,43,
        #            49,50,51,53,54,56,58,65,69,70,72,74,78)
        
        # gisele_margin <- as.numeric(predict(reg_model_marg, s = lambda_marg, newx = as.matrix(reg_input)))
        gisele_as <- as.numeric(predict(reg_model_as, s = lambda_as, newx = as.matrix(reg_input)))
        gisele_hs <- as.numeric(predict(reg_model_hs, s = lambda_hs, newx = as.matrix(reg_input)))
        gisele_win <- as.numeric(predict(reg_model_win, s = lambda_win, newx = as.matrix(reg_input), type = "response"))
        
        # gisele_margin <- as.numeric(predict(reg_model_marg, as.matrix(reg_input))) # Elastic Net
        # gisele_as <- as.numeric(predict(reg_model_as,as.matrix(reg_input))) # Elastic Net
        # gisele_hs <- as.numeric(predict(reg_model_hs, as.matrix(reg_input))) # Elastic Net
        # gisele_win <- as.numeric(predict(reg_model_win, as.matrix(reg_input), type = "prob")) # Elastic Net
        
        holder <- slate[a,2:3]
        # holder$Away_Margin <- gisele_margin
        # holder$Home_Margin <- gisele_margin*-1
        holder$Away_Margin <- gisele_as - gisele_hs
        holder$Home_Margin <- gisele_hs - gisele_as
        holder$Away_Win <- gisele_win[1]
        holder$Home_Win <- 1 - gisele_win[1]
        holder$Total <- gisele_as + gisele_hs
        
        gisele_predict <- bind_rows(gisele_predict,holder)
        
        pb$tick()
        Sys.sleep(1 / nrow(slate))
        
    }
    
    gisele_predict <- gisele_predict %>%
        mutate(across(where(is.numeric), round, 3))
    
    
    gisele_predict$Model <- "Gisele Bundchen - Regularization"
    
    all_models <- gisele_predict
    
    slate <- slate %>%
        left_join(., dataGameLogsTeam, by = c("Date" = "dateGame", "Home" = "nameTeam")) %>%
        select(1:3,9)
    
    plays_a <- slate %>%
        mutate(Loc = "A") %>%
        select(4,1,5,3,2)
    colnames(plays_a) <- c("idGame", "Date", "Loc", "oppTeam", "Team")
    
    plays_a <- plays_a %>%
        left_join(gisele_predict, by = c("Team" = "Away"))
    
    plays_a <- plays_a %>%
        select(1:5,7,9,11)
    
    colnames(plays_a) <- c("idGame", "Date", "Loc", "oppTeam", "Team", 
                           "Gisele_Margin", "Gisele_Win", "Gisele_Total")
    
    plays_h <- slate %>%
        mutate(Loc = "H") %>%
        select(4,1,5,2,3)
    colnames(plays_h) <- c("idGame", "Date", "Loc", "oppTeam", "Team")
    
    plays_h <- plays_h %>%
        left_join(gisele_predict, by = c("oppTeam" = "Away"))
    
    plays_h <- plays_h %>%
        select(1:5,8,10,11)
    
    colnames(plays_h) <- c("idGame", "Date", "Loc", "oppTeam", "Team", 
                           "Gisele_Margin", "Gisele_Win", "Gisele_Total")
    
    plays <- bind_rows(plays_a, plays_h)
    
    plays <- plays %>%
        arrange(idGame)
    
    
    # bring in odds
    odds <- read_xlsx("/Users/Jesse/Documents/MyStuff/NBA Betting/Backtest 2022/nba odds 2021-22.xlsx")
    
    odds <- odds %>%
        mutate(across(where(is.character), str_replace_all, pattern = "L.A. Clippers", replacement = "LA Clippers"))
    
    plays <- plays %>%
        left_join(odds, by = c("Team", "Date")) %>%
        select(1:5,11,12,13,6:8)
    
    plays$Gisele_Spread_Edge <- with(plays, Gisele_Margin + Spread)
    plays$Gisele_ML_Edge <- with(plays, Gisele_Win - round((if_else(ML<0,((ML*-1)/((ML*-1)+100)),(100/(ML+100)))), 3))
    plays$Gisele_Over_Edge <- with(plays, Gisele_Total - Total)
    plays$Gisele_Under_Edge <- with(plays, Total - Gisele_Total)
    
    final_backtest <- bind_rows(final_backtest,plays)
    
    print(paste0(round((b/h)*100,1),"%"))
    
    
}    


detach("package:XLConnect", unload = TRUE)

fn <- "Backtest_2022_Gisele_All_Ridge"
u <- paste0("/Users/Jesse/Documents/MyStuff/NBA Betting/Backtest 2022/Backtest Output/",fn,".xlsx")

wb <- createWorkbook()
addWorksheet(wb, sheetName = "Backtest")
writeData(wb, sheet = "Backtest", x = final_backtest)
saveWorkbook(wb, file = u)