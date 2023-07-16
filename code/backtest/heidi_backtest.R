if (!require("pacman")) install.packages("pacman"); library(pacman)
pacman::p_load(tidyverse, readxl, lubridate, openxlsx, nbastatR, rvest, progress, XLConnect, caret, xgboost)

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
    nba_marg_x <- as.matrix(nba_marg[-1])
    nba_marg_y <- as.matrix(nba_marg[1])
    nba_marg <- xgb.DMatrix(nba_marg_x, label = nba_marg_y)

    train_id <- sample(1:nrow(nba_scaled), size = floor(0.8 * nrow(nba_scaled)), replace=FALSE)
    nba_win <- nba_scaled[train_id,]
    validation <- nba_scaled[-train_id,]
    nba_win <- nba_win %>% select(8, 9:78)
    nba_val <- validation %>% select(8, 9:78)
    training_matrix <- model.matrix(Win ~.-1, data = nba_win)
    validation_matrix <- model.matrix(Win ~.-1, data = nba_val)
    nba_win <- xgb.DMatrix(data = training_matrix, label = nba_win$Win)
    nba_val <- xgb.DMatrix(data = validation_matrix, label = nba_val$Win)

    nba_as <- nba_scaled %>% select(5, 9:78)
    nba_as_x <- as.matrix(nba_as[-1])
    nba_as_y <- as.matrix(nba_as[1])
    nba_as <- xgb.DMatrix(nba_as_x, label = nba_as_y)

    nba_hs <- nba_scaled %>% select(6, 9:78)
    nba_hs_x <- as.matrix(nba_hs[-1])
    nba_hs_y <- as.matrix(nba_hs[1])
    nba_hs <- xgb.DMatrix(nba_hs_x, label = nba_hs_y)
    
    # # TS
    # nba_marg <- nba_scaled %>%
    #     select(7,14,15,16,18,19,21,24,30,34,35,37,40,43,
    #            49,50,51,53,54,56,59,65,69,70,72,75,78)
    # nba_marg_x <- as.matrix(nba_marg[-1])
    # nba_marg_y <- as.matrix(nba_marg[1])
    # nba_marg <- xgb.DMatrix(nba_marg_x, label = nba_marg_y)
    # 
    # nba_win <- nba_scaled %>%
    #     select(8,14,15,16,18,19,21,24,30,34,35,37,40,43,
    #            49,50,51,53,54,56,59,65,69,70,72,75,78)
    # nba_win_x <- as.matrix(nba_win[-1])
    # nba_win_y <- as.matrix(nba_win[1])
    # nba_win <- xgb.DMatrix(nba_win_x, label = nba_win_y)
    # 
    # nba_as <- nba_scaled %>%
    #     select(5,14,15,16,18,19,21,24,30,34,35,37,40,43,
    #            49,50,51,53,54,56,59,65,69,70,72,75,78)
    # nba_as_x <- as.matrix(nba_as[-1])
    # nba_as_y <- as.matrix(nba_as[1])
    # nba_as <- xgb.DMatrix(nba_as_x, label = nba_as_y)
    # 
    # nba_hs <- nba_scaled %>%
    #     select(6,14,15,16,18,19,21,24,30,34,35,37,40,43,
    #            49,50,51,53,54,56,59,65,69,70,72,75,78)
    # nba_hs_x <- as.matrix(nba_hs[-1])
    # nba_hs_y <- as.matrix(nba_hs[1])
    # nba_hs <- xgb.DMatrix(nba_hs_x, label = nba_hs_y)

    # # eFG
    # nba_marg <- nba_scaled %>%
    #     select(7,14,15,16,18,19,21,23,30,34,35,37,39,43,
    #            49,50,51,53,54,56,58,65,69,70,72,74,78)
    # nba_marg_x <- as.matrix(nba_marg[-1])
    # nba_marg_y <- as.matrix(nba_marg[1])
    # nba_marg <- xgb.DMatrix(nba_marg_x, label = nba_marg_y)
    # 
    # nba_win <- nba_scaled %>%
    #     select(8,14,15,16,18,19,21,23,30,34,35,37,39,43,
    #            49,50,51,53,54,56,58,65,69,70,72,74,78)
    # nba_win_x <- as.matrix(nba_win[-1])
    # nba_win_y <- as.matrix(nba_win[1])
    # nba_win <- xgb.DMatrix(nba_win_x, label = nba_win_y)
    # 
    # nba_as <- nba_scaled %>%
    #     select(5,14,15,16,18,19,21,23,30,34,35,37,39,43,
    #            49,50,51,53,54,56,58,65,69,70,72,74,78)
    # nba_as_x <- as.matrix(nba_as[-1])
    # nba_as_y <- as.matrix(nba_as[1])
    # nba_as <- xgb.DMatrix(nba_as_x, label = nba_as_y)
    # 
    # nba_hs <- nba_scaled %>%
    #     select(6,14,15,16,18,19,21,23,30,34,35,37,39,43,
    #            49,50,51,53,54,56,58,65,69,70,72,74,78)
    # nba_hs_x <- as.matrix(nba_hs[-1])
    # nba_hs_y <- as.matrix(nba_hs[1])
    # nba_hs <- xgb.DMatrix(nba_hs_x, label = nba_hs_y)
    
    #### Heidi #### - XGB
    
    pb <- progress_bar$new(
        format = "  running model... [:bar] :percent eta: :eta",
        total = nrow(slate), clear = FALSE, width = 60, show_after = 0)
    invisible(pb$tick(0))
    
    # Margin
    best_param_marg <- list()
    best_rmse_marg <- Inf
    best_rmse_index_marg <- 0

    for (iter in 1:100) {
        param_marg <- list(objective = "reg:squarederror",
                      eval_metric = "rmse",
                      max_depth = sample(6:10, 1),
                      eta = runif(1, .01, .3), # Learning rate, default: 0.3
                      subsample = runif(1, .6, .9),
                      colsample_bytree = runif(1, .5, .8),
                      min_child_weight = sample(1:40, 1),
                      max_delta_step = sample(1:10, 1)
        )
        cv.nround <-  1000
        cv.nfold <-  5 # 5-fold cross-validation
        mdcv <- xgb.cv(data = nba_marg, params = param_marg,
                       nfold = cv.nfold, nrounds = cv.nround,
                       verbose = F, early_stopping_rounds = 8, maximize = F)

        min_rmse_index_marg  <-  mdcv$best_iteration
        min_rmse_marg <-  mdcv$evaluation_log[min_rmse_index_marg]$test_rmse_mean

        if (min_rmse_marg < best_rmse_marg) {
            best_rmse_marg <- min_rmse_marg
            best_rmse_index_marg <- min_rmse_index_marg
            best_param_marg <- param_marg
        }
    }

    # AS
    best_param_as <- list()
    best_rmse_as <- Inf
    best_rmse_index_as <- 0

    for (iter in 1:100) {
        param_as <- list(objective = "reg:squarederror",
                      eval_metric = "rmse",
                      max_depth = sample(6:10, 1),
                      eta = runif(1, .01, .3), # Learning rate, default: 0.3
                      subsample = runif(1, .6, .9),
                      colsample_bytree = runif(1, .5, .8),
                      min_child_weight = sample(1:40, 1),
                      max_delta_step = sample(1:10, 1)
        )
        cv.nround <-  1000
        cv.nfold <-  5 # 5-fold cross-validation
        mdcv <- xgb.cv(data = nba_as, params = param_as,
                       nfold = cv.nfold, nrounds = cv.nround,
                       verbose = F, early_stopping_rounds = 8, maximize = F)

        min_rmse_index_as  <-  mdcv$best_iteration
        min_rmse_as <-  mdcv$evaluation_log[min_rmse_index_as]$test_rmse_mean

        if (min_rmse_as < best_rmse_as) {
            best_rmse_as <- min_rmse_as
            best_rmse_index_as <- min_rmse_index_as
            best_param_as <- param_as
        }
    }

    # HS
    best_param_hs <- list()
    best_rmse_hs <- Inf
    best_rmse_index_hs <- 0

    for (iter in 1:100) {
        param_hs <- list(objective = "reg:squarederror",
                      eval_metric = "rmse",
                      max_depth = sample(6:10, 1),
                      eta = runif(1, .01, .3), # Learning rate, default: 0.3
                      subsample = runif(1, .6, .9),
                      colsample_bytree = runif(1, .5, .8),
                      min_child_weight = sample(1:40, 1),
                      max_delta_step = sample(1:10, 1)
        )
        cv.nround <-  1000
        cv.nfold <-  5 # 5-fold cross-validation
        mdcv <- xgb.cv(data = nba_hs, params = param_hs,
                       nfold = cv.nfold, nrounds = cv.nround,
                       verbose = F, early_stopping_rounds = 8, maximize = F)

        min_rmse_index_hs  <-  mdcv$best_iteration
        min_rmse_hs <-  mdcv$evaluation_log[min_rmse_index_hs]$test_rmse_mean

        if (min_rmse_hs < best_rmse_hs) {
            best_rmse_hs <- min_rmse_hs
            best_rmse_index_hs <- min_rmse_index_hs
            best_param_hs <- param_hs
        }
    }

    # Win
    # Create empty lists
    lowest_error_list = list()
    parameters_list = list()

    # Create 10,000 rows with random hyperparameters
    for (iter in 1:500){
        param <- list(booster = "gbtree",
                      objective = "binary:logistic",
                      max_depth = sample(3:10, 1),
                      eta = runif(1, .01, .3),
                      subsample = runif(1, .7, 1),
                      colsample_bytree = runif(1, .6, 1),
                      min_child_weight = sample(0:10, 1)
        )
        parameters <- as.data.frame(param)
        parameters_list[[iter]] <- parameters
    }

    # Create object that contains all randomly created hyperparameters
    parameters_df = do.call(rbind, parameters_list)

    # Use randomly created parameters to create 10,000 XGBoost-models
    for (row in 1:nrow(parameters_df)){
        mdcv <- xgb.train(data=nba_win,
                          booster = "gbtree",
                          objective = "binary:logistic",
                          max_depth = parameters_df$max_depth[row],
                          eta = parameters_df$eta[row],
                          subsample = parameters_df$subsample[row],
                          colsample_bytree = parameters_df$colsample_bytree[row],
                          min_child_weight = parameters_df$min_child_weight[row],
                          nrounds= 300,
                          eval_metric = "error",
                          early_stopping_rounds= 30,
                          print_every_n = 100,
                          watchlist = list(train= nba_win, val= nba_val)
        )
        lowest_error <- as.data.frame(1 - min(mdcv$evaluation_log$val_error))
        lowest_error_list[[row]] <- lowest_error
    }

    # Create object that contains all accuracy's
    lowest_error_df = do.call(rbind, lowest_error_list)

    # Bind columns of accuracy values and random hyperparameter values
    randomsearch = cbind(lowest_error_df, parameters_df)

    # Quickly display highest accuracy
    max(randomsearch$`1 - min(mdcv$evaluation_log$val_error)`)

    # Prepare table
    randomsearch <- as.data.frame(randomsearch) %>%
        rename(val_acc = `1 - min(mdcv$evaluation_log$val_error)`) %>%
        arrange(-val_acc)


    # Tuned-XGBoost model
    params <- list(booster = "gbtree",
                   objective = "binary:logistic",
                   max_depth = randomsearch[1,]$max_depth,
                   eta = randomsearch[1,]$eta,
                   subsample = randomsearch[1,]$subsample,
                   colsample_bytree = randomsearch[1,]$colsample_bytree,
                   min_child_weight = randomsearch[1,]$min_child_weight)
    xgb_tuned <- xgb.train(params = params,
                           data = nba_win,
                           nrounds = 1000,
                           print_every_n = 10,
                           eval_metric = "auc",
                           eval_metric = "error",
                           early_stopping_rounds = 30,
                           watchlist = list(train= nba_win, val= nba_val))

    # Make prediction on dvalid
    validation$pred_win_tuned <- predict(xgb_tuned, nba_val)
    validation$pred_win_factor_tuned <- factor(ifelse(validation$pred_win_tuned > 0.5, 1, 0),
                                               labels=c("Loss","Win"))

    # Check accuracy with the confusion matrix
    caret::confusionMatrix(validation$pred_win_factor_tuned,
                           factor(validation$Win, labels=c("Loss", "Win")),
                    positive = "Win",
                    dnn = c("Prediction", "Actual Data"))

    xgb_marg <- xgboost(data = nba_marg, params = best_param_marg, nround = best_rmse_index_marg, verbose = F)
    xgb_as <- xgboost(data = nba_as, params = best_param_as, nround = best_rmse_index_as, verbose = F)
    xgb_hs <- xgboost(data = nba_hs, params = best_param_hs, nround = best_rmse_index_hs, verbose = F)

    heidi_predict <- data.frame() # Predictions frame
    
    a <- 1
    g <- nrow(slate)
    
    for (a in a:g) {
        
        slate_away <- as.character(slate[a,2])
        slate_home <- as.character(slate[a,3])
        
        # All
        xgb_input <- nba_scaled_today %>%
            filter(Away == slate_away & Home == slate_home) %>%
            select(9:78)
        xgb_input <- model.matrix(~.-1, data = xgb_input)
        xgb_input <- xgb.DMatrix(xgb_input)
        # colnames(xgb_input) <- NULL
        
        # # TS
        # xgb_input <- nba_scaled_today %>%
        #     filter(Away == slate_away & Home == slate_home) %>%
        #     select(14,15,16,18,19,21,24,30,34,35,37,40,43,
        #            49,50,51,53,54,56,59,65,69,70,72,75,78)
        # xgb_input <- xgb.DMatrix(xgb_input)
        
        # # eFG
        # xgb_input <- nba_scaled_today %>%
        #     filter(Away == slate_away & Home == slate_home) %>%
        #     select(14,15,16,18,19,21,23,30,34,35,37,39,43,
        #            49,50,51,53,54,56,58,65,69,70,72,74,78)
        # xgb_input <- xgb.DMatrix(xgb_input)
        
        
        heidi_margin <- predict(xgb_marg, xgb_input)
        heidi_as <- predict(xgb_as, xgb_input)
        heidi_hs <- predict(xgb_hs, xgb_input)
        heidi_win <- predict(xgb_tuned, xgb_input)
        
        
        holder <- slate[a,2:3]
        holder$Away_Margin <- heidi_margin
        holder$Home_Margin <- heidi_margin*-1
        holder$Away_Margin2 <- heidi_as - heidi_hs
        holder$Home_Margin2 <- heidi_hs - heidi_as
        holder$Away_Win <- heidi_win
        holder$Home_Win <- 1 - heidi_win
        holder$Total <- heidi_as + heidi_hs
        
        heidi_predict <- bind_rows(heidi_predict,holder)
        
        pb$tick()
        Sys.sleep(1 / nrow(slate))
        
    }
    
    heidi_predict <- heidi_predict %>%
        mutate(across(where(is.numeric), round, 3))
    
    
    heidi_predict$Model <- "Heidi Klum - Gradient Boosting"
    
    all_models <- heidi_predict
    
    slate <- slate %>%
        left_join(., dataGameLogsTeam, by = c("Date" = "dateGame", "Home" = "nameTeam")) %>%
        select(1:3,9)
    
    plays_a <- slate %>%
        mutate(Loc = "A") %>%
        select(4,1,5,3,2)
    colnames(plays_a) <- c("idGame", "Date", "Loc", "oppTeam", "Team")
    
    plays_a <- plays_a %>%
        left_join(heidi_predict, by = c("Team" = "Away"))
    
    plays_a <- plays_a %>%
        select(1:5, 
               7, 9, 11, 13)
    
    colnames(plays_a) <- c("idGame", "Date", "Loc", "oppTeam", "Team", 
                           "Heidi_Margin", "Heidi_Margin2", "Heidi_Win", "Heidi_Total")
    
    plays_h <- slate %>%
        mutate(Loc = "H") %>%
        select(4,1,5,2,3)
    colnames(plays_h) <- c("idGame", "Date", "Loc", "oppTeam", "Team")
    
    plays_h <- plays_h %>%
        left_join(heidi_predict, by = c("oppTeam" = "Away"))
    
    plays_h <- plays_h %>%
        select(1:5, 
               8, 10, 12, 13)
    
    colnames(plays_h) <- c("idGame", "Date", "Loc", "oppTeam", "Team", 
                           "Heidi_Margin", "Heidi_Margin2", "Heidi_Win", "Heidi_Total")
    
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
    
    plays$Heidi_Spread_Edge <- with(plays, Heidi_Margin + Spread)
    plays$Heidi_Spread2_Edge <- with(plays, Heidi_Margin2 + Spread)
    plays$Heidi_ML_Edge <- with(plays, Heidi_Win - round((if_else(ML<0,((ML*-1)/((ML*-1)+100)),(100/(ML+100)))), 3))
    plays$Heidi_Over_Edge <- with(plays, Heidi_Total - Total)
    plays$Heidi_Under_Edge <- with(plays, Total - Heidi_Total)
    
    # plays[, c(29:32,62:66)] <- sapply(plays[, c(29:32,62:66)], as.numeric)
    
    final_backtest <- bind_rows(final_backtest,plays)
    
    print(paste0(round((b/h)*100,1),"%"))
    
    
}    


detach("package:XLConnect", unload = TRUE)

fn <- "Backtest_2022_Heidi_All"
u <- paste0("/Users/Jesse/Documents/MyStuff/NBA Betting/Backtest 2022/Backtest Output/",fn,".xlsx")

wb <- createWorkbook()
addWorksheet(wb, sheetName = "Backtest")
writeData(wb, sheet = "Backtest", x = final_backtest)
saveWorkbook(wb, file = u)