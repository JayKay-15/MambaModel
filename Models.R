### Models 2022 ----

td <- as_date(Sys.Date())
# td <- as_date("2022-10-18")

### Slate ----

sched <- nbastatR::current_schedule()

# slate <- sched %>%
#     filter(dateGame == td) %>%
#     mutate(gameTime = hms::as_hms(datetimeGame - 18000)) %>%
#     select(4,29,24,2,34)

slate <- sched %>%
    filter(dateGame == td) %>%
    select(4,29,24,2,16)

colnames(slate) <- c("game_id","away","home","date","game_time")

slate <- slate %>%
    mutate(across(where(is.character), str_replace_all, 
                  pattern = "Los Angeles Clippers", replacement = "LA Clippers"))

game_times <- slate %>% select(2,3,5)

### Use Season Stats ----
# away_final_wt <- season_final %>% rename(team = teamName)
# home_final_wt <- season_final %>% rename(team = teamName)

### Preprocessing Stats ----

pre_proc_val_all <- readRDS("/Users/Jesse/Documents/MyStuff/NBA Betting/NBAdb/Models/pre_proc_val_all.rds")
pre_proc_val_ts <- readRDS("/Users/Jesse/Documents/MyStuff/NBA Betting/NBAdb/Models/pre_proc_val_ts.rds")

### Kendall - Rating and Pythag formulas ----

pb <- progress_bar$new(
    format = "  running model kendall... [:bar] :percent eta: :eta",
    total = nrow(slate), clear = FALSE, width = 60, show_after = 0)
invisible(pb$tick(0))

kendall_predict <- data.frame()

a <- 1
g <- nrow(slate)

for (a in a:g) {
    
    slate_away <- as.character(slate[a,2])
    slate_home <- as.character(slate[a,3])
    
    kendall_away <- away_final_wt %>%
        filter(team == slate_away)
    
    kendall_home <- home_final_wt %>%
        filter(team == slate_home)
    
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

    away_proj_oe <- away_proj_oe / 100
    home_proj_oe <- home_proj_oe / 100
    
    away_kendall_score <- (away_proj_oe * expected_pace)
    home_kendall_score <- (home_proj_oe * expected_pace)
    
    away_kendall_win <- (away_proj_oe ^ 14.23) / ((away_proj_oe ^ 14.23) + (home_proj_oe ^ 14.23))
    home_kendall_win <- 1 - away_kendall_win
    
    holder <- slate[a,2:3]
    holder$away_margin <- away_kendall_score - home_kendall_score
    holder$home_margin <- home_kendall_score - away_kendall_score
    holder$away_win <- away_kendall_win
    holder$home_win <- home_kendall_win
    holder$total <- away_kendall_score + home_kendall_score
    
    holder$away_score <- away_kendall_score
    holder$home_score <- home_kendall_score
    
    kendall_predict <- bind_rows(kendall_predict, holder)
    
    pb$tick()
    Sys.sleep(1 / nrow(slate))
    
}

kendall_predict <- kendall_predict %>%
    mutate(across(where(is.numeric), round, 3))


### Tyra - Least Squares ----

pb <- progress_bar$new(
    format = "  running model tyra... [:bar] :percent eta: :eta",
    total = nrow(slate), clear = FALSE, width = 60, show_after = 0)
invisible(pb$tick(0))

log_win <- readRDS("/Users/Jesse/Documents/MyStuff/NBA Betting/NBAdb/Models/log_win_model.rds")
lin_as <- readRDS("/Users/Jesse/Documents/MyStuff/NBA Betting/NBAdb/Models/lin_as_model.rds")
lin_hs <- readRDS("/Users/Jesse/Documents/MyStuff/NBA Betting/NBAdb/Models/lin_hs_model.rds")

tyra_predict <- data.frame()

a <- 1
g <- nrow(slate)

for (a in a:g) {
    
    slate_away <- as.character(slate[a,2])
    slate_home <- as.character(slate[a,3])
    
    tyra_away <- away_final_wt %>%
        filter(team == slate_away) %>%
        rename_with(~ paste0(names(away_final_wt),"_away")) %>%
        select(7,8,9,11,12,14,17,23,27,28,30,33,36)
    
    tyra_home <- home_final_wt %>%
        filter(team == slate_home) %>%
        rename_with(~ paste0(names(home_final_wt),"_home")) %>%
        select(7,8,9,11,12,14,17,23,27,28,30,33,36)
    
    tyra_input <- cbind(tyra_away, tyra_home)
    tyra_input <- predict(pre_proc_val_ts, tyra_input)
    
    tyra_ascore <- as.numeric(predict(lin_as, tyra_input))
    tyra_hscore <- as.numeric(predict(lin_hs, tyra_input))
    
    tyra_awin <- as.numeric(predict(log_win, tyra_input, type = "prob")[2])
    tyra_hwin <- 1 - tyra_awin
    
    holder <- slate[a,2:3]
    holder$away_margin <- tyra_ascore - tyra_hscore
    holder$home_margin <- tyra_hscore - tyra_ascore
    holder$away_win <- tyra_awin
    holder$home_win <- tyra_hwin
    holder$total <- tyra_ascore + tyra_hscore
    
    holder$away_score <- tyra_ascore
    holder$home_score <- tyra_hscore
    
    tyra_predict <- bind_rows(tyra_predict, holder)
    
    pb$tick()
    Sys.sleep(1 / nrow(slate))
    
}

tyra_predict <- tyra_predict %>%
    mutate(across(where(is.numeric), round, 3))


### Gisele - Regularization ----

pb <- progress_bar$new(
    format = "  running model gisele... [:bar] :percent eta: :eta",
    total = nrow(slate), clear = FALSE, width = 60, show_after = 0)
invisible(pb$tick(0))

reg_win <- readRDS("/Users/Jesse/Documents/MyStuff/NBA Betting/NBAdb/Models/reg_win_model.rds")
reg_as <- readRDS("/Users/Jesse/Documents/MyStuff/NBA Betting/NBAdb/Models/reg_as_model.rds")
reg_hs <- readRDS("/Users/Jesse/Documents/MyStuff/NBA Betting/NBAdb/Models/reg_hs_model.rds")

gisele_predict <- data.frame()

a <- 1
g <- nrow(slate)

for (a in a:g) {
    
    slate_away <- as.character(slate[a,2])
    slate_home <- as.character(slate[a,3])
    
    gisele_away <- away_final_wt %>%
        filter(team == slate_away) %>%
        rename_with(~ paste0(names(away_final_wt),"_away")) %>%
        select(-1)
    
    gisele_home <- home_final_wt %>%
        filter(team == slate_home) %>%
        rename_with(~ paste0(names(home_final_wt),"_home")) %>%
        select(-1)
    
    gisele_input <- cbind(gisele_away, gisele_home)
    gisele_input <- predict(pre_proc_val_all, gisele_input)
    
    gisele_ascore <- as.numeric(predict(reg_as, gisele_input))
    gisele_hscore <- as.numeric(predict(reg_hs, gisele_input))
    
    gisele_awin <- as.numeric(predict(reg_win, gisele_input, type = "prob")[2])
    gisele_hwin <- 1 - gisele_awin
    
    holder <- slate[a,2:3]
    holder$away_margin <- gisele_ascore - gisele_hscore
    holder$home_margin <- gisele_hscore - gisele_ascore
    holder$away_win <- gisele_awin
    holder$home_win <- gisele_hwin
    holder$total <- gisele_ascore + gisele_hscore
    
    holder$away_score <- gisele_ascore
    holder$home_score <- gisele_hscore
    
    gisele_predict <- bind_rows(gisele_predict, holder)
    
    pb$tick()
    Sys.sleep(1 / nrow(slate))    
}

gisele_predict <- gisele_predict %>%
    mutate(across(where(is.numeric), round, 3))


### Chrissy - K Nearest Neighbors ----

pb <- progress_bar$new(
    format = "  running model chrissy... [:bar] :percent eta: :eta",
    total = nrow(slate), clear = FALSE, width = 60, show_after = 0)
invisible(pb$tick(0))

knn_win <- readRDS("/Users/Jesse/Documents/MyStuff/NBA Betting/NBAdb/Models/knn_win_model.rds")
knn_as <- readRDS("/Users/Jesse/Documents/MyStuff/NBA Betting/NBAdb/Models/knn_as_model.rds")
knn_hs <- readRDS("/Users/Jesse/Documents/MyStuff/NBA Betting/NBAdb/Models/knn_hs_model.rds")

chrissy_predict <- data.frame()

a <- 1
g <- nrow(slate)

for (a in a:g) {
    
    slate_away <- as.character(slate[a,2])
    slate_home <- as.character(slate[a,3])
    
    chrissy_away <- away_final_wt %>%
        filter(team == slate_away) %>%
        rename_with(~ paste0(names(away_final_wt),"_away")) %>%
        select(7,8,9,11,12,14,17,23,27,28,30,33,36)
    
    chrissy_home <- home_final_wt %>%
        filter(team == slate_home) %>%
        rename_with(~ paste0(names(home_final_wt),"_home")) %>%
        select(7,8,9,11,12,14,17,23,27,28,30,33,36)
    
    chrissy_input <- cbind(chrissy_away, chrissy_home)
    chrissy_input <- predict(pre_proc_val_ts, chrissy_input)
    
    chrissy_ascore <- as.numeric(predict(knn_as, chrissy_input))
    chrissy_hscore <- as.numeric(predict(knn_hs, chrissy_input))
    
    chrissy_awin <- as.numeric(predict(knn_win, chrissy_input, type = "prob")[2])
    chrissy_hwin <- 1 - chrissy_awin
    
    holder <- slate[a,2:3]
    holder$away_margin <- chrissy_ascore - chrissy_hscore
    holder$home_margin <- chrissy_hscore - chrissy_ascore
    holder$away_win <- chrissy_awin
    holder$home_win <- chrissy_hwin
    holder$total <- chrissy_ascore + chrissy_hscore
    
    holder$away_score <- chrissy_ascore
    holder$home_score <- chrissy_hscore
    
    chrissy_predict <- bind_rows(chrissy_predict, holder)
    
    pb$tick()
    Sys.sleep(1 / nrow(slate))
    
}

chrissy_predict <- chrissy_predict %>%
    mutate(across(where(is.numeric), round, 3))


### Kate - Random Forest ----

pb <- progress_bar$new(
    format = "  running model kate... [:bar] :percent eta: :eta",
    total = nrow(slate), clear = FALSE, width = 60, show_after = 0)
invisible(pb$tick(0))

rf_win <- readRDS("/Users/Jesse/Documents/MyStuff/NBA Betting/NBAdb/Models/rf_win_model.rds")
rf_as <- readRDS("/Users/Jesse/Documents/MyStuff/NBA Betting/NBAdb/Models/rf_as_model.rds")
rf_hs <- readRDS("/Users/Jesse/Documents/MyStuff/NBA Betting/NBAdb/Models/rf_hs_model.rds")

kate_predict <- data.frame()

a <- 1
g <- nrow(slate)

for (a in a:g) {
    
    slate_away <- as.character(slate[a,2])
    slate_home <- as.character(slate[a,3])
    
    kate_away <- away_final_wt %>%
        filter(team == slate_away) %>%
        rename_with(~ paste0(names(away_final_wt),"_away")) %>%
        select(-1)
    
    kate_home <- home_final_wt %>%
        filter(team == slate_home) %>%
        rename_with(~ paste0(names(home_final_wt),"_home")) %>%
        select(-1)
    
    kate_input <- cbind(kate_away, kate_home)
    kate_input <- predict(pre_proc_val_all, kate_input)
    
    kate_ascore <- as.numeric(predict(rf_as, kate_input))
    kate_hscore <- as.numeric(predict(rf_hs, kate_input))
    
    kate_awin <- as.numeric(predict(rf_win, kate_input, type = "prob")[2])
    kate_hwin <- 1 - kate_awin
    
    holder <- slate[a,2:3]
    holder$away_margin <- kate_ascore - kate_hscore
    holder$home_margin <- kate_hscore - kate_ascore
    holder$away_win <- kate_awin
    holder$home_win <- kate_hwin
    holder$total <- kate_ascore + kate_hscore
    
    holder$away_score <- kate_ascore
    holder$home_score <- kate_hscore
    
    kate_predict <- bind_rows(kate_predict, holder)
    
    pb$tick()
    Sys.sleep(1 / nrow(slate))    
}

kate_predict <- kate_predict %>%
    mutate(across(where(is.numeric), round, 3))


### Cindy - Support Vector Machine ----

pb <- progress_bar$new(
    format = "  running model cindy... [:bar] :percent eta: :eta",
    total = nrow(slate), clear = FALSE, width = 60, show_after = 0)
invisible(pb$tick(0))

svm_win <- readRDS("/Users/Jesse/Documents/MyStuff/NBA Betting/NBAdb/Models/svm_win_model.rds")
svm_as <- readRDS("/Users/Jesse/Documents/MyStuff/NBA Betting/NBAdb/Models/svm_as_model.rds")
svm_hs <- readRDS("/Users/Jesse/Documents/MyStuff/NBA Betting/NBAdb/Models/svm_hs_model.rds")

cindy_predict <- data.frame()

a <- 1
g <- nrow(slate)

for (a in a:g) {
    
    slate_away <- as.character(slate[a,2])
    slate_home <- as.character(slate[a,3])
    
    cindy_away <- away_final_wt %>%
        filter(team == slate_away) %>%
        rename_with(~ paste0(names(away_final_wt),"_away")) %>%
        select(7,8,9,11,12,14,17,23,27,28,30,33,36)
    
    cindy_home <- home_final_wt %>%
        filter(team == slate_home) %>%
        rename_with(~ paste0(names(home_final_wt),"_home")) %>%
        select(7,8,9,11,12,14,17,23,27,28,30,33,36)
    
    cindy_input <- cbind(cindy_away, cindy_home)
    cindy_input <- predict(pre_proc_val_ts, cindy_input)
    
    cindy_ascore <- as.numeric(predict(svm_as, cindy_input))
    cindy_hscore <- as.numeric(predict(svm_hs, cindy_input))
    
    cindy_awin <- as.numeric(predict(svm_win, cindy_input, type = "prob")[2])
    cindy_hwin <- 1 - cindy_awin
    
    holder <- slate[a,2:3]
    holder$away_margin <- cindy_ascore - cindy_hscore
    holder$home_margin <- cindy_hscore - cindy_ascore
    holder$away_win <- cindy_awin
    holder$home_win <- cindy_hwin
    holder$total <- cindy_ascore + cindy_hscore
    
    holder$away_score <- cindy_ascore
    holder$home_score <- cindy_hscore
    
    cindy_predict <- bind_rows(cindy_predict, holder)
    
    pb$tick()
    Sys.sleep(1 / nrow(slate))
    
}

cindy_predict <- cindy_predict %>%
    mutate(across(where(is.numeric), round, 3))


### Naomi - Neural Network ----

pb <- progress_bar$new(
    format = "  running model naomi... [:bar] :percent eta: :eta",
    total = nrow(slate), clear = FALSE, width = 60, show_after = 0)
invisible(pb$tick(0))

nn_win <- readRDS("/Users/Jesse/Documents/MyStuff/NBA Betting/NBAdb/Models/nn_win_model.rds")
nn_as <- readRDS("/Users/Jesse/Documents/MyStuff/NBA Betting/NBAdb/Models/nn_as_model.rds")
nn_hs <- readRDS("/Users/Jesse/Documents/MyStuff/NBA Betting/NBAdb/Models/nn_hs_model.rds")

naomi_predict <- data.frame()

a <- 1
g <- nrow(slate)

for (a in a:g) {
    
    slate_away <- as.character(slate[a,2])
    slate_home <- as.character(slate[a,3])
    
    naomi_away <- away_final_wt %>%
        filter(team == slate_away) %>%
        rename_with(~ paste0(names(away_final_wt),"_away")) %>%
        select(7,8,9,11,12,14,17,23,27,28,30,33,36)
    
    naomi_home <- home_final_wt %>%
        filter(team == slate_home) %>%
        rename_with(~ paste0(names(home_final_wt),"_home")) %>%
        select(7,8,9,11,12,14,17,23,27,28,30,33,36)
    
    naomi_input <- cbind(naomi_away, naomi_home)
    naomi_input <- predict(pre_proc_val_ts, naomi_input)
    
    naomi_ascore <- as.numeric(predict(nn_as, naomi_input))
    naomi_hscore <- as.numeric(predict(nn_hs, naomi_input))
    
    naomi_awin <- as.numeric(predict(nn_win, naomi_input, type = "prob")[2])
    naomi_hwin <- 1 - naomi_awin
    
    holder <- slate[a,2:3]
    holder$away_margin <- naomi_ascore - naomi_hscore
    holder$home_margin <- naomi_hscore - naomi_ascore
    holder$away_win <- naomi_awin
    holder$home_win <- naomi_hwin
    holder$total <- naomi_ascore + naomi_hscore
    
    holder$away_score <- naomi_ascore
    holder$home_score <- naomi_hscore
    
    naomi_predict <- bind_rows(naomi_predict, holder)
    
    pb$tick()
    Sys.sleep(1 / nrow(slate))
    
}

naomi_predict <- naomi_predict %>%
    mutate(across(where(is.numeric), round, 3))

### Heidi - Extreme Gradient Boosting ----

pb <- progress_bar$new(
    format = "  running model heidi... [:bar] :percent eta: :eta",
    total = nrow(slate), clear = FALSE, width = 60, show_after = 0)
invisible(pb$tick(0))

xgb_win <- readRDS("/Users/Jesse/Documents/MyStuff/NBA Betting/NBAdb/Models/xgb_win_model.rds")
xgb_as <- readRDS("/Users/Jesse/Documents/MyStuff/NBA Betting/NBAdb/Models/xgb_as_model.rds")
xgb_hs <- readRDS("/Users/Jesse/Documents/MyStuff/NBA Betting/NBAdb/Models/xgb_hs_model.rds")

heidi_predict <- data.frame()

a <- 1
g <- nrow(slate)

for (a in a:g) {
    
    slate_away <- as.character(slate[a,2])
    slate_home <- as.character(slate[a,3])
    
    heidi_away <- away_final_wt %>%
        filter(team == slate_away) %>%
        rename_with(~ paste0(names(away_final_wt),"_away")) %>%
        select(7,8,9,11,12,14,17,23,27,28,30,33,36)
    
    heidi_home <- home_final_wt %>%
        filter(team == slate_home) %>%
        rename_with(~ paste0(names(home_final_wt),"_home")) %>%
        select(7,8,9,11,12,14,17,23,27,28,30,33,36)
    
    heidi_input <- cbind(heidi_away, heidi_home)
    heidi_input <- predict(pre_proc_val_ts, heidi_input)
    
    heidi_ascore <- as.numeric(predict(xgb_as, heidi_input))
    heidi_hscore <- as.numeric(predict(xgb_hs, heidi_input))
    
    heidi_awin <- as.numeric(predict(xgb_win, heidi_input, type = "prob")[2])
    heidi_hwin <- 1 - heidi_awin
    
    holder <- slate[a,2:3]
    holder$away_margin <- heidi_ascore - heidi_hscore
    holder$home_margin <- heidi_hscore - heidi_ascore
    holder$away_win <- heidi_awin
    holder$home_win <- heidi_hwin
    holder$total <- heidi_ascore + heidi_hscore
    
    holder$away_score <- heidi_ascore
    holder$home_score <- heidi_hscore
    
    heidi_predict <- bind_rows(heidi_predict, holder)
    
    pb$tick()
    Sys.sleep(1 / nrow(slate))
    
}

heidi_predict <- heidi_predict %>%
    mutate(across(where(is.numeric), round, 3))

### Adriana - Combo ----

adriana_predict <- slate[c(2,3)]

adriana_predict$away_margin <- round(rowMeans(cbind(kendall_predict[,3], tyra_predict[,3], gisele_predict[,3],chrissy_predict[,3],
                                                    kate_predict[,3], cindy_predict[,3], naomi_predict[,3],heidi_predict[,3])),3)

adriana_predict$home_margin <- round(rowMeans(cbind(kendall_predict[,4], tyra_predict[,4], gisele_predict[,4],chrissy_predict[,4],
                                                    kate_predict[,4], cindy_predict[,4], naomi_predict[,4],heidi_predict[,4])),3)

adriana_predict$away_win <- round(rowMeans(cbind(kendall_predict[,5], tyra_predict[,5], gisele_predict[,5],chrissy_predict[,5],
                                                 kate_predict[,5], cindy_predict[,5], naomi_predict[,5],heidi_predict[,5])),3)

adriana_predict$home_win <- round(rowMeans(cbind(kendall_predict[,6], tyra_predict[,6], gisele_predict[,6],chrissy_predict[,6],
                                                 kate_predict[,6], cindy_predict[,6], naomi_predict[,6],heidi_predict[,6])),3)

adriana_predict$total <- round(rowMeans(cbind(kendall_predict[,7], tyra_predict[,7], gisele_predict[,7],chrissy_predict[,7],
                                              kate_predict[,7], cindy_predict[,7], naomi_predict[,7],heidi_predict[,7])),3)

adriana_predict$away_score <- round(rowMeans(cbind(kendall_predict[,8], tyra_predict[,8], gisele_predict[,8],chrissy_predict[,8],
                                                   kate_predict[,8], cindy_predict[,8], naomi_predict[,8],heidi_predict[,8])),3)

adriana_predict$home_score <- round(rowMeans(cbind(kendall_predict[,9], tyra_predict[,9], gisele_predict[,9],chrissy_predict[,9],
                                                   kate_predict[,9], cindy_predict[,9], naomi_predict[,9],heidi_predict[,9])),3)

### All Models

kendall_predict$model <- "Kendall Jenner - Simple Model"
tyra_predict$model <- "Tyra Banks - Least Squares"
gisele_predict$model <- "Gisele Bundchen - Regularization"
chrissy_predict$model <- "Chrissy Teigen - K Nearest Neighbors"
kate_predict$model <- "Kate Moss - Random Forest"
cindy_predict$model <- "Cindy Crawford - Support Vector Machines"
naomi_predict$model <- "Naomi Campbell - Neural Network"
heidi_predict$model <- "Heidi Klum -  Extreme Gradient Boosting"
adriana_predict$model <- "Adriana Lima - Combination"

all_models <- rbind(kendall_predict, tyra_predict, gisele_predict, chrissy_predict,
                    kate_predict, cindy_predict, naomi_predict, heidi_predict,
                    adriana_predict)

## Variable Importance ----
# log_win_imp <- rownames_to_column(varImp(log_win)[["importance"]], "var") %>%
#     arrange(desc(Overall)) %>% head()
# 
# lin_as_imp <- rownames_to_column(varImp(lin_as)[["importance"]], "var") %>%
#     arrange(desc(Overall)) %>% head()
# 
# lin_hs_imp <- rownames_to_column(varImp(lin_hs)[["importance"]], "var") %>%
#     arrange(desc(Overall)) %>% head()
# 
# 
# reg_win_imp <- rownames_to_column(varImp(reg_win)[["importance"]], "var") %>%
#     arrange(desc(Overall)) %>% head()
# 
# reg_as_imp <- rownames_to_column(varImp(reg_as)[["importance"]], "var") %>%
#     arrange(desc(Overall)) %>% head()
# 
# reg_hs_imp <- rownames_to_column(varImp(reg_hs)[["importance"]], "var") %>%
#     arrange(desc(Overall)) %>% head()
# 
# 
# nn_win_imp <- rownames_to_column(varImp(nn_win)[["importance"]], "var") %>%
#     arrange(desc(Overall)) %>% head()
# 
# nn_as_imp <- rownames_to_column(varImp(nn_as)[["importance"]], "var") %>%
#     arrange(desc(Overall)) %>% head()
# 
# nn_hs_imp <- rownames_to_column(varImp(nn_hs)[["importance"]], "var") %>%
#     arrange(desc(Overall)) %>% head()
# 
# 
# xgb_win_imp <- rownames_to_column(varImp(xgb_win)[["importance"]], "var") %>%
#     arrange(desc(Overall)) %>% head()
# 
# xgb_as_imp <- rownames_to_column(varImp(xgb_as)[["importance"]], "var") %>%
#     arrange(desc(Overall)) %>% head()
# 
# xgb_hs_imp <- rownames_to_column(varImp(xgb_hs)[["importance"]], "var") %>%
#     arrange(desc(Overall)) %>% head()
# 
# 
# score_imp <- bind_rows(lin_as_imp,lin_hs_imp,nn_as_imp,nn_hs_imp,xgb_as_imp,xgb_hs_imp)
# score_imp <- score_imp %>%
#     group_by(var = score_imp$var) %>%
#     summarise(imp = max(Overall)) %>%
#     arrange(desc(imp))
# 
# win_imp <- bind_rows(log_win_imp,nn_win_imp,xgb_win_imp)
# win_imp <- win_imp %>%
#     group_by(var = win_imp$var) %>%
#     summarise(imp = max(Overall)) %>%
#     arrange(desc(imp))

# saveRDS(score_imp, "score_imp.rds")
# saveRDS(win_imp, "win_imp.rds")

score_imp <- readRDS("/Users/Jesse/Documents/MyStuff/NBA Betting/NBAdb/Models/score_imp.rds")
win_imp <- readRDS("/Users/Jesse/Documents/MyStuff/NBA Betting/NBAdb/Models/win_imp.rds")


### Clean Environment ----
rm(list=ls()[! ls() %in% c("away_final_wt","home_final_wt","league_avg","standings",
                           "away_rank","home_rank","season_final","away_final","home_final",
                           "slate", "all_models","score_imp", "win_imp",
                           "kendall_predict", "tyra_predict", "gisele_predict",
                           "chrissy_predict","kate_predict", "cindy_predict", 
                           "naomi_predict", "heidi_predict", "adriana_predict")])

print("Models Complete")