if (!require("pacman")) install.packages("pacman"); library(pacman)
pacman::p_load(tidyverse, readxl, lubridate, openxlsx, nbastatR, rvest, progress, XLConnect, 
               caret, glmnet, randomForest, gbm, e1071)
devtools::install_github("bips-hb/neuralnet", force = T)
library(neuralnet)

Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)

rm(list=ls())
setwd("/Users/Jesse/Documents/MyStuff/NBA Betting/Backtest 2022/")

game_logs(seasons = 2022, result_types = c("team","players"))

dataGameLogsTeam <- dataGameLogsTeam %>% arrange(dateGame,idGame)
dataGameLogsTeam$dateGame <- as_date(dataGameLogsTeam$dateGame)
dataGameLogsPlayer <- dataGameLogsPlayer %>% arrange(dateGame,idGame)


league_avg <- read_xlsx("/Users/Jesse/Documents/MyStuff/NBA Betting/NBAdb/NBAdb1722_lg_avg.xlsx")

master_db_all <- read_xlsx("/Users/Jesse/Documents/MyStuff/NBA Betting/NBAdb/NBAdb1722_oneadj.xlsx")

odds <- read_xlsx("/Users/Jesse/Documents/MyStuff/NBA Betting/Backtest 2022/nba odds 2021-22.xlsx") 
odds <- odds %>%
    filter(Test == 1)

dates_distinct <- odds %>%
    distinct(Date) %>%
    mutate(Stat_Date = as_date(Date - 1))

odds$Date <- as.character(odds$Date)
odds$`Stat Date` <- as.character(odds$`Stat Date`)

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
    
    nba_marg <- master_db %>%
        select(7,
               14,15,16,18,19,21,23,30,34,35,37,39,43,
               49,50,51,53,54,56,58,65,69,70,72,74,78)
    pre_proc_marg <- preProcess(nba_marg[,-1], method = c("center", "scale"))
    nba_marg[,-1] <- predict(pre_proc_marg, nba_marg[,-1])
    
    nba_win <- master_db %>%
        select(8,
               14,15,16,18,19,21,23,30,34,35,37,39,43,
               49,50,51,53,54,56,58,65,69,70,72,74,78)
    pre_proc_win <- preProcess(nba_win[,-1], method = c("center", "scale"))
    nba_win[,-1] = predict(pre_proc_win, nba_win[,-1])
    
    nba_as <- master_db %>%
        select(5,
               14,15,16,18,19,21,23,30,34,35,37,39,43,
               49,50,51,53,54,56,58,65,69,70,72,74,78)
    pre_proc_as <- preProcess(nba_as[,-1], method = c("center", "scale"))
    nba_as[,-1] <- predict(pre_proc_as, nba_as[,-1])
    
    nba_hs <- master_db %>%
        select(6,
               14,15,16,18,19,21,23,30,34,35,37,39,43,
               49,50,51,53,54,56,58,65,69,70,72,74,78)
    pre_proc_hs <- preProcess(nba_hs[,-1], method = c("center", "scale"))
    nba_hs[,-1] <- predict(pre_proc_hs, nba_hs[,-1])
    
    away_final_wt <- master_db_all %>%
        filter(Date == td) %>%
        select(3,9:43) %>%
        rename(Team = Away)
    
    home_final_wt <- master_db_all %>%
        filter(Date == td) %>%
        select(4,44:78) %>%
        rename(Team = Home)
    
    league_avg <- league_avg %>%
        filter(Date == td)
        
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
        
        lg_pace <- as.numeric(league_avg[1,34])
        lg_oe <- as.numeric(league_avg[1,36])
        
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
        
        # away_proj_oe <- away_proj_oe * as.numeric(advantage_percent[1,2])
        # home_proj_oe <- home_proj_oe * as.numeric(advantage_percent[1,1])
        
        away_proj_oe <- away_proj_oe / 100
        home_proj_oe <- home_proj_oe / 100
        
        away_kendall_score <- (away_proj_oe * expected_pace) #+ as.numeric(advantage_mx[1,2])
        home_kendall_score <- (home_proj_oe * expected_pace) #+ as.numeric(advantage_mx[1,2])
        
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
    
    
    #### Tyra #### - Least Squares
    
    pb <- progress_bar$new(
        format = "  running model... [:bar] :percent eta: :eta",
        total = nrow(slate), clear = FALSE, width = 60, show_after = 0)
    invisible(pb$tick(0))
    
    ls_marg <- lm(Margin ~ ., data = nba_marg)
    ls_as <- lm(AS ~ ., data = nba_as)
    ls_hs <- lm(HS ~ ., data = nba_hs)
    ls_win <- glm(Win ~ ., data = nba_win, family = "binomial")
    
    tyra_predict <- data.frame() # Predictions frame
    
    a <- 1
    g <- nrow(slate)
    
    for (a in a:g) {
        
        slate_away <- as.character(slate[a,2])
        slate_home <- as.character(slate[a,3])
        
        tyra_away <- away_final_wt %>%
            filter(Team == slate_away) %>%
            select(7,8,9,11,12,14,16,23,27,28,30,32,36)   
        
        tyra_home <- home_final_wt %>%
            filter(Team == slate_home) %>%
            select(7,8,9,11,12,14,16,23,27,28,30,32,36)
        
        ls_input <- cbind(tyra_away, tyra_home)
        
        ls_scale <- master_db %>%
            select(14,15,16,18,19,21,23,30,34,35,37,39,43,
                   49,50,51,53,54,56,58,65,69,70,72,74,78) %>%
            bind_rows(ls_input)
        
        pre_proc_ls_scale <- preProcess(ls_scale, method = c("center", "scale"))
        ls_scale <- predict(pre_proc_ls_scale, ls_scale)
        
        ls_input <- ls_scale %>% tail(1)
        
        tyra_margin <- as.numeric(predict(ls_marg, newdata = ls_input))
        tyra_as <- as.numeric(predict(ls_as, newdata = ls_input))
        tyra_hs <- as.numeric(predict(ls_hs, newdata = ls_input))
        tyra_win <- as.numeric(predict(ls_win, newdata = ls_input, type = "response"))
        
        holder <- slate[a,2:3]
        holder$Away_Margin <- tyra_margin
        holder$Home_Margin <- tyra_margin*-1
        holder$Away_Margin2 <- tyra_as - tyra_hs
        holder$Home_Margin2 <- tyra_hs - tyra_as
        holder$Away_Win <- tyra_win
        holder$Home_Win <- 1 - tyra_win
        holder$Total <- tyra_as + tyra_hs
        
        tyra_predict <- bind_rows(tyra_predict,holder)
        
        pb$tick()
        Sys.sleep(1 / nrow(slate))
        
    }
    
    tyra_predict <- tyra_predict %>%
        mutate(across(where(is.numeric), round, 3))
    
    
    #### Gisele #### - Lasso
    
    pb <- progress_bar$new(
        format = "  running model... [:bar] :percent eta: :eta",
        total = nrow(slate), clear = FALSE, width = 60, show_after = 0)
    invisible(pb$tick(0))
    
    
    # Lambdas
    lambdas <- 10^seq(2, -3, by = -.1)
    
    # Margin
    lasso_marg <- cv.glmnet(as.matrix(nba_marg[-1]), nba_marg$Margin, alpha = 1, lambda = lambdas, 
                           standardize = TRUE, nfolds = 5,
                           family='gaussian', type.measure='mse')
    lambda_marg <- lasso_marg$lambda.min 
    lasso_model_marg <- glmnet(as.matrix(nba_marg[-1]), nba_marg$Margin, alpha = 1, 
                          lambda = lambda_marg, standardize = TRUE, family='gaussian')
    
    # Away score
    lasso_as <- cv.glmnet(as.matrix(nba_as[-1]), nba_as$AS, alpha = 1, lambda = lambdas, 
                            standardize = TRUE, nfolds = 5,
                            family='gaussian', type.measure='mse')
    lambda_as <- lasso_as$lambda.min 
    lasso_model_as <- glmnet(as.matrix(nba_as[-1]), nba_as$AS, alpha = 1, 
                               lambda = lambda_as, standardize = TRUE, family='gaussian')
    
    # Home score
    lasso_hs <- cv.glmnet(as.matrix(nba_hs[-1]), nba_hs$HS, alpha = 1, lambda = lambdas, 
                          standardize = TRUE, nfolds = 5,
                          family='gaussian', type.measure='mse')
    lambda_hs <- lasso_hs$lambda.min 
    lasso_model_hs <- glmnet(as.matrix(nba_hs[-1]), nba_hs$HS, alpha = 1, 
                             lambda = lambda_hs, standardize = TRUE, family='gaussian')
    
    # Win
    lasso_win <- cv.glmnet(as.matrix(nba_win[-1]), nba_win$Win, alpha = 1, lambda = lambdas, 
                          standardize = TRUE, nfolds = 5,
                          family='binomial', type.measure='auc')
    lambda_win <- lasso_win$lambda.min 
    lasso_model_win <- glmnet(as.matrix(nba_win[-1]), nba_win$Win, alpha = 1,
                              lambda = lambda_win, standardize = TRUE, family='binomial')
    

    gisele_predict <- data.frame() # Predictions frame
    
    a <- 1
    g <- nrow(slate)
    
    for (a in a:g) {
        
        slate_away <- as.character(slate[a,2])
        slate_home <- as.character(slate[a,3])
        
        gisele_away <- away_final_wt %>%
            filter(Team == slate_away) %>%
            select(7,8,9,11,12,14,16,23,27,28,30,32,36)   
        
        gisele_home <- home_final_wt %>%
            filter(Team == slate_home) %>%
            select(7,8,9,11,12,14,16,23,27,28,30,32,36)
        
        lasso_input <- cbind(gisele_away, gisele_home)

        lasso_scale <- master_db %>%
            select(14,15,16,18,19,21,23,30,34,35,37,39,43,
                   49,50,51,53,54,56,58,65,69,70,72,74,78) %>%
            bind_rows(lasso_input)
        
        pre_proc_lasso_scale <- preProcess(lasso_scale, method = c("center", "scale"))
        lasso_scale <- predict(pre_proc_lasso_scale, lasso_scale)
        
        lasso_input <- lasso_scale %>% tail(1)
                            
        gisele_margin <- as.numeric(predict(lasso_model_marg, s = lambda_marg, newx = as.matrix(lasso_input)))
        gisele_as <- as.numeric(predict(lasso_model_as, s = lambda_as, newx = as.matrix(lasso_input)))
        gisele_hs <- as.numeric(predict(lasso_model_hs, s = lambda_hs, newx = as.matrix(lasso_input)))
        gisele_win <- as.numeric(predict(lasso_model_win, s = lambda_win, newx = as.matrix(lasso_input), type = "response"))

        holder <- slate[a,2:3]
        holder$Away_Margin <- gisele_margin
        holder$Home_Margin <- gisele_margin*-1
        holder$Away_Margin2 <- gisele_as - gisele_hs
        holder$Home_Margin2 <- gisele_hs - gisele_as
        holder$Away_Win <- gisele_win
        holder$Home_Win <- 1 - gisele_win
        holder$Total <- gisele_as + gisele_hs
        
        gisele_predict <- bind_rows(gisele_predict,holder)
        
        pb$tick()
        Sys.sleep(1 / nrow(slate))
        
    }
    
    gisele_predict <- gisele_predict %>%
        mutate(across(where(is.numeric), round, 3))
    
    
    #### Kate #### - Random Forest
    
    pb <- progress_bar$new(
        format = "  running model... [:bar] :percent eta: :eta",
        total = nrow(slate), clear = FALSE, width = 60, show_after = 0)
    invisible(pb$tick(0))
    
    rf_marg <- randomForest(Margin ~ ., data = nba_marg, mtry = 9, ntrees = 500)
    rf_as <- randomForest(AS ~ ., data = nba_as, mtry = 6, ntrees = 500)
    rf_hs <- randomForest(HS ~ ., data = nba_hs, mtry = 6, ntrees = 500)
    rf_win <- randomForest(as.factor(Win) ~ ., data = nba_win, mtry = 4, ntrees = 500)
    
    kate_predict <- data.frame() # Predictions frame
    
    a <- 1
    g <- nrow(slate)
    
    for (a in a:g) {
        
        slate_away <- as.character(slate[a,2])
        slate_home <- as.character(slate[a,3])
        
        kate_away <- away_final_wt %>%
            filter(Team == slate_away) %>%
            select(7,8,9,11,12,14,16,23,27,28,30,32,36)   
        
        kate_home <- home_final_wt %>%
            filter(Team == slate_home) %>%
            select(7,8,9,11,12,14,16,23,27,28,30,32,36)
        
        rf_input <- cbind(kate_away, kate_home)
        
        rf_scale <- master_db %>%
            select(14,15,16,18,19,21,23,30,34,35,37,39,43,
                   49,50,51,53,54,56,58,65,69,70,72,74,78) %>%
            bind_rows(rf_input)
        
        pre_proc_rf_scale <- preProcess(rf_scale, method = c("center", "scale"))
        rf_scale <- predict(pre_proc_rf_scale, rf_scale)
        
        rf_input <- rf_scale %>% tail(1)
        
        kate_margin <- predict(rf_marg, rf_input)
        kate_as <- predict(rf_as, rf_input)
        kate_hs <- predict(rf_hs, rf_input)
        kate_win <- predict(rf_win, rf_input, type = "prob")
        
        holder <- slate[a,2:3]
        holder$Away_Margin <- kate_margin
        holder$Home_Margin <- kate_margin*-1
        holder$Away_Margin2 <- kate_as - kate_hs
        holder$Home_Margin2 <- kate_hs - kate_as
        holder$Away_Win <- kate_win[,2]
        holder$Home_Win <- kate_win[,1]
        holder$Total <- kate_as + kate_hs
        
        kate_predict <- bind_rows(kate_predict,holder)
        
        pb$tick()
        Sys.sleep(1 / nrow(slate))
        
    }
    
    kate_predict <- kate_predict %>%
        mutate(across(where(is.numeric), round, 3))
    
    
    #### Cindy #### - Support Vector Machine
    
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
        
        cindy_away <- away_final_wt %>%
            filter(Team == slate_away) %>%
            select(7,8,9,11,12,14,16,23,27,28,30,32,36)
        
        cindy_home <- home_final_wt %>%
            filter(Team == slate_home) %>%
            select(7,8,9,11,12,14,16,23,27,28,30,32,36)
        
        svm_input <- cbind(cindy_away, cindy_home)
        
        svm_scale <- master_db %>%
            select(14,15,16,18,19,21,23,30,34,35,37,39,43,
                   49,50,51,53,54,56,58,65,69,70,72,74,78) %>%
            bind_rows(svm_input)
        
        pre_proc_svm_scale <- preProcess(svm_scale, method = c("center", "scale"))
        svm_scale <- predict(pre_proc_svm_scale, svm_scale)
        
        svm_input <- svm_scale %>% tail(1)
        
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
    
    
    #### Naomi #### - Artificial Neural Network
    
    pb <- progress_bar$new(
        format = "  running model... [:bar] :percent eta: :eta",
        total = nrow(slate), clear = FALSE, width = 60, show_after = 0)
    invisible(pb$tick(0))
    
    nn_marg <- neuralnet(Margin ~., data = nba_marg, linear.output = T, rep = 1, threshold = .8, stepmax = 1e7)
    nn_as <- neuralnet(AS ~., data = nba_as, linear.output = T, rep = 1, threshold = .8, stepmax = 1e7)
    nn_hs <- neuralnet(HS ~., data = nba_hs, linear.output = T, rep = 1, threshold = .8, stepmax = 1e7)
    nn_win <- neuralnet(Win ~., data = nba_win, linear.output = F, rep = 1, threshold = .01, stepmax = 1e7)
    
    naomi_predict <- data.frame() # Predictions frame
    
    a <- 1
    g <- nrow(slate)
    
    for (a in a:g) {
        
        slate_away <- as.character(slate[a,2])
        slate_home <- as.character(slate[a,3])
        
        naomi_away <- away_final_wt %>%
            filter(Team == slate_away) %>%
            select(7,8,9,11,12,14,16,23,27,28,30,32,36)
        
        naomi_home <- home_final_wt %>%
            filter(Team == slate_home) %>%
            select(7,8,9,11,12,14,16,23,27,28,30,32,36)
        
        nn_input <- cbind(naomi_away, naomi_home)

        nn_scale <- master_db %>%
            select(14,15,16,18,19,21,23,30,34,35,37,39,43,
                   49,50,51,53,54,56,58,65,69,70,72,74,78) %>%
            bind_rows(nn_input)
        
        pre_proc_nn_scale <- preProcess(nn_scale, method = c("center", "scale"))
        nn_scale <- predict(pre_proc_nn_scale, nn_scale)
        
        nn_input <- nn_scale %>% tail(1)
        
        naomi_margin <- compute(nn_marg, nn_input)
        naomi_as <- compute(nn_as, nn_input)
        naomi_hs <- compute(nn_hs, nn_input)
        naomi_win <- compute(nn_win, nn_input)
        

        holder <- slate[a,2:3]
        holder$Away_Margin <- naomi_margin$net.result
        holder$Home_Margin <- naomi_margin$net.result*-1
        holder$Away_Margin2 <- naomi_as$net.result - naomi_hs$net.result
        holder$Home_Margin2 <- naomi_hs$net.result - naomi_as$net.result
        holder$Away_Win <- naomi_win$net.result
        holder$Home_Win <- 1 - naomi_win$net.result
        holder$Total <- naomi_as$net.result + naomi_hs$net.result
        
        naomi_predict <- bind_rows(naomi_predict,holder)
        
        pb$tick()
        Sys.sleep(1 / nrow(slate))
        
    }
    
    naomi_predict <- naomi_predict %>%
        mutate(across(where(is.numeric), round, 3))
    
    #### Heidi #### - GB
    
    pb <- progress_bar$new(
        format = "  running model... [:bar] :percent eta: :eta",
        total = nrow(slate), clear = FALSE, width = 60, show_after = 0)
    invisible(pb$tick(0))
    
    gb_marg <- gbm(Margin ~ ., data = nba_marg, distribution = "gaussian", 
                    n.trees = 1500, interaction.depth = 2, shrinkage = 0.01)
    gb_as <- gbm(AS ~ ., data = nba_as, distribution = "gaussian", 
                   n.trees = 1500, interaction.depth = 3, shrinkage = 0.01)
    gb_hs <- gbm(HS ~ ., data = nba_hs, distribution = "gaussian", 
                   n.trees = 1500, interaction.depth = 3, shrinkage = 0.01)
    gb_win <- gbm(Win ~ ., data = nba_win, distribution = "bernoulli", 
                  n.trees = 2500, interaction.depth = 1, shrinkage = 0.01)
    
    heidi_predict <- data.frame() # Predictions frame
    
    a <- 1
    g <- nrow(slate)
    
    for (a in a:g) {
        
        slate_away <- as.character(slate[a,2])
        slate_home <- as.character(slate[a,3])
        
        heidi_away <- away_final_wt %>%
            filter(Team == slate_away) %>%
            select(7,8,9,11,12,14,16,23,27,28,30,32,36)
        
        heidi_home <- home_final_wt %>%
            filter(Team == slate_home) %>%
            select(7,8,9,11,12,14,16,23,27,28,30,32,36)
        
        gb_input <- cbind(heidi_away, heidi_home)
        
        gb_scale <- master_db %>%
            select(14,15,16,18,19,21,23,30,34,35,37,39,43,
                   49,50,51,53,54,56,58,65,69,70,72,74,78) %>%
            bind_rows(gb_input)
        
        pre_proc_gb_scale <- preProcess(gb_scale, method = c("center", "scale"))
        gb_scale <- predict(pre_proc_gb_scale, gb_scale)
        
        gb_input <- gb_scale %>% tail(1)
        
        
        heidi_margin <- predict(gb_marg, newdata = gb_input, n.trees = 1500)
        heidi_as <- predict(gb_as, newdata = gb_input, n.trees = 1500)
        heidi_hs <- predict(gb_hs, newdata = gb_input, n.trees = 1500)
        heidi_win <- predict(gb_win, newdata = gb_input, n.trees = 2500, type = "response")
        
        
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
    
    
    
    
    kendall_predict$Model <- "Kendall Jenner - Simple Model"
    tyra_predict$Model <- "Tyra Banks - Least Squares"
    gisele_predict$Model <- "Gisele Bundchen - Regularization"
    kate_predict$Model <- "Kate Moss - Random Forest"
    cindy_predict$Model <- "Cindy Crawford - SVM"
    naomi_predict$Model <- "Naomi Campbell - Neural Network"
    heidi_predict$Model <- "Heidi Klum - Gradient Boost"
    
    all_models <- rbind(kendall_predict, tyra_predict, gisele_predict, 
                        kate_predict, cindy_predict, naomi_predict, heidi_predict)
    
    slate <- slate %>%
        left_join(., dataGameLogsTeam, by = c("Date" = "dateGame", "Home" = "nameTeam")) %>%
        select(1:3,9)
    
    plays_a <- slate %>%
        mutate(Loc = "A") %>%
        select(4,1,5,3,2)
    colnames(plays_a) <- c("idGame", "Date", "Loc", "oppTeam", "Team")
    
    plays_a <- plays_a %>%
        left_join(kendall_predict, by = c("Team" = "Away")) %>%
        left_join(., tyra_predict, by = c("Team" = "Away")) %>%
        left_join(., gisele_predict, by = c("Team" = "Away")) %>%
        left_join(., kate_predict, by = c("Team" = "Away")) %>%
        left_join(., cindy_predict, by = c("Team" = "Away")) %>%
        left_join(., naomi_predict, by = c("Team" = "Away")) %>%
        left_join(., heidi_predict, by = c("Team" = "Away"))
    
    plays_a <- plays_a %>%
        select(1:5, 
               7, 9, 11, 13,
               16, 18, 20, 22, 
               25, 27, 29, 31, 
               34, 36, 38, 40, 
               43, 45, 47, 49, 
               52, 54, 56, 58,
               61, 63, 65, 67)
    
    colnames(plays_a) <- c("idGame", "Date", "Loc", "oppTeam", "Team", 
                           "Kendall_Margin", "Kendall_Margin2", "Kendall_Win", "Kendall_Total",
                           "Tyra_Margin", "Tyra_Margin2", "Tyra_Win", "Tyra_Total",
                           "Gisele_Margin", "Gisele_Margin2", "Gisele_Win", "Gisele_Total",
                           "Kate_Margin", "Kate_Margin2", "Kate_Win", "Kate_Total",
                           "Cindy_Margin", "Cindy_Margin2", "Cindy_Win", "Cindy_Total",
                           "Naomi_Margin", "Naomi_Margin2", "Naomi_Win", "Naomi_Total",
                           "Heidi_Margin", "Heidi_Margin2", "Heidi_Win", "Heidi_Total")
    
    plays_h <- slate %>%
        mutate(Loc = "H") %>%
        select(4,1,5,2,3)
    colnames(plays_h) <- c("idGame", "Date", "Loc", "oppTeam", "Team")
    
    plays_h <- plays_h %>%
        left_join(kendall_predict, by = c("oppTeam" = "Away")) %>%
        left_join(., tyra_predict, by = c("oppTeam" = "Away")) %>%
        left_join(., gisele_predict, by = c("oppTeam" = "Away")) %>%
        left_join(., kate_predict, by = c("oppTeam" = "Away")) %>%
        left_join(., cindy_predict, by = c("oppTeam" = "Away")) %>%
        left_join(., naomi_predict, by = c("oppTeam" = "Away")) %>%
        left_join(., heidi_predict, by = c("oppTeam" = "Away"))
    
    plays_h <- plays_h %>%
        select(1:5, 
               8, 10, 12, 13,
               17, 19, 21, 22, 
               26, 28, 30, 31, 
               35, 37, 39, 40, 
               44, 46, 48, 49, 
               53, 55, 57, 58,
               62, 64, 66, 67)
    
    colnames(plays_h) <- c("idGame", "Date", "Loc", "oppTeam", "Team", 
                           "Kendall_Margin", "Kendall_Margin2", "Kendall_Win", "Kendall_Total",
                           "Tyra_Margin", "Tyra_Margin2", "Tyra_Win", "Tyra_Total",
                           "Gisele_Margin", "Gisele_Margin2", "Gisele_Win", "Gisele_Total",
                           "Kate_Margin", "Kate_Margin2", "Kate_Win", "Kate_Total",
                           "Cindy_Margin", "Cindy_Margin2", "Cindy_Win", "Cindy_Total",
                           "Naomi_Margin", "Naomi_Margin2", "Naomi_Win", "Naomi_Total",
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
        select(1:5, 36, 37, 38, 6:33)
    
    plays$Kendall_Spread_Edge <- with(plays, Kendall_Margin + Spread)
    plays$Kendall_Spread2_Edge <- with(plays, Kendall_Margin2 + Spread)
    plays$Kendall_ML_Edge <- with(plays, Kendall_Win - round((if_else(ML<0,((ML*-1)/((ML*-1)+100)),(100/(ML+100)))), 3))
    plays$Kendall_Over_Edge <- with(plays, Kendall_Total - Total)
    plays$Kendall_Under_Edge <- with(plays, Total - Kendall_Total)
    
    plays$Tyra_Spread_Edge <- with(plays, Tyra_Margin + Spread)
    plays$Tyra_Spread2_Edge <- with(plays, Tyra_Margin2 + Spread)
    plays$Tyra_ML_Edge <- with(plays, Tyra_Win - round((if_else(ML<0,((ML*-1)/((ML*-1)+100)),(100/(ML+100)))), 3))
    plays$Tyra_Over_Edge <- with(plays, Tyra_Total - Total)
    plays$Tyra_Under_Edge <- with(plays, Total - Tyra_Total)
    
    plays$Gisele_Spread_Edge <- with(plays, Gisele_Margin + Spread)
    plays$Gisele_Spread2_Edge <- with(plays, Gisele_Margin2 + Spread)
    plays$Gisele_ML_Edge <- with(plays, Gisele_Win - round((if_else(ML<0,((ML*-1)/((ML*-1)+100)),(100/(ML+100)))), 3))
    plays$Gisele_Over_Edge <- with(plays, Gisele_Total - Total)
    plays$Gisele_Under_Edge <- with(plays, Total - Gisele_Total)
    
    plays$Kate_Spread_Edge <- with(plays, Kate_Margin + Spread)
    plays$Kate_Spread2_Edge <- with(plays, Kate_Margin2 + Spread)
    plays$Kate_ML_Edge <- with(plays, Kate_Win - round((if_else(ML<0,((ML*-1)/((ML*-1)+100)),(100/(ML+100)))), 3))
    plays$Kate_Over_Edge <- with(plays, Kate_Total - Total)
    plays$Kate_Under_Edge <- with(plays, Total - Kate_Total)
    
    plays$Cindy_Spread_Edge <- with(plays, Cindy_Margin + Spread)
    plays$Cindy_Spread2_Edge <- with(plays, Cindy_Margin2 + Spread)
    plays$Cindy_ML_Edge <- with(plays, Cindy_Win - round((if_else(ML<0,((ML*-1)/((ML*-1)+100)),(100/(ML+100)))), 3))
    plays$Cindy_Over_Edge <- with(plays, Cindy_Total - Total)
    plays$Cindy_Under_Edge <- with(plays, Total - Cindy_Total)
    
    plays$Naomi_Spread_Edge <- with(plays, Naomi_Margin + Spread)
    plays$Naomi_Spread2_Edge <- with(plays, Naomi_Margin2 + Spread)
    plays$Naomi_ML_Edge <- with(plays, Naomi_Win - round((if_else(ML<0,((ML*-1)/((ML*-1)+100)),(100/(ML+100)))), 3))
    plays$Naomi_Over_Edge <- with(plays, Naomi_Total - Total)
    plays$Naomi_Under_Edge <- with(plays, Total - Naomi_Total)
    
    plays$Heidi_Spread_Edge <- with(plays, Heidi_Margin + Spread)
    plays$Heidi_Spread2_Edge <- with(plays, Heidi_Margin2 + Spread)
    plays$Heidi_ML_Edge <- with(plays, Heidi_Win - round((if_else(ML<0,((ML*-1)/((ML*-1)+100)),(100/(ML+100)))), 3))
    plays$Heidi_Over_Edge <- with(plays, Heidi_Total - Total)
    plays$Heidi_Under_Edge <- with(plays, Total - Heidi_Total)
    
    
    plays[, c(29:32,62:66)] <- sapply(plays[, c(29:32,62:66)], as.numeric)
    
    final_backtest <- bind_rows(final_backtest,plays)
    
    print(paste0(round((b/h)*100,1),"%"))



}

detach("package:XLConnect", unload = TRUE)

fn <- "Backtest_2022"
u <- paste0("/Users/Jesse/Documents/MyStuff/NBA Betting/Backtest 2022/",fn,".xlsx")

wb <- createWorkbook()
addWorksheet(wb, sheetName = "Backtest")
writeData(wb, sheet = "Backtest", x = final_backtest)
saveWorkbook(wb, file = u)



