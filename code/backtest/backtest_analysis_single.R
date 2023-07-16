if (!require("pacman")) install.packages("pacman"); library(pacman)
pacman::p_load(tidyverse, readxl, lubridate, openxlsx, nbastatR, rvest, progress, XLConnect, data.table)

Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)

rm(list=ls())
setwd("/Users/Jesse/Documents/MyStuff/NBA Betting/Backtest 2022/")

##### CREATE BACKTEST RESULTS BOOK #####

# game_logs(seasons = c(2021), result_types = c("team","players"))

dataGameLogsTeam <- read_xlsx("/Users/Jesse/Documents/MyStuff/NBA Betting/Backtest 2022/DataGameLogsTeam.xlsx")

dataGameLogsTeam <- dataGameLogsTeam %>% arrange(dateGame,idGame)
dataGameLogsTeam$dateGame <- as_date(dataGameLogsTeam$dateGame)

backtest <- read_xlsx(paste0("/Users/Jesse/Documents/MyStuff/NBA Betting/Backtest 2022/Backtest Output/",
                             "Backtest_2022_Tyra_All.xlsx"))
backtest$Date <- as_date(backtest$Date)

### Attach game logs to itself to get all stats for each game in one row

gl <- left_join(dataGameLogsTeam, dataGameLogsTeam, by = c("idGame" = "idGame", "slugTeam" = "slugOpponent"))

gl <- gl %>%
    select(6,13,8,62,17,90,45)
colnames(gl) <- c("idGame", "Date", "Loc", "oppTeam", "Team", "oppScore", "Score")

results_book <- gl %>%
    arrange(idGame, Loc)

backtest <- backtest[c(1,4:15)]

results_book <- results_book %>%
    left_join(backtest, by = c("idGame","Team"))

results_book <- results_book %>%
    drop_na()

results_book <- results_book %>%
    mutate(Margin = Score - oppScore) %>%
    mutate(Game_Total = Score + oppScore) %>%
    mutate(ATS_Margin = Margin + Spread) %>%
    mutate(ATS_Result = if_else((Margin + Spread) == 0, 0, if_else(ATS_Margin > 0, 1, -1.1))) %>%
    mutate(ML_Result = case_when(ML > 0 & (Score - oppScore) > 0 ~ ML/100, 
                                 ML > 0 & (Score - oppScore) < 0 ~ -1,
                                 (Score - oppScore) > 0 ~ 1,
                                 (Score - oppScore) < 0 ~ ML/100)) %>%
    mutate(Over_Result = if_else(Game_Total > Total, 1, -1.1)) %>%
    mutate(Under_Result = if_else(Game_Total < Total, 1, -1.1)) %>%
    select(1:7, 9:11, 19:25, 12:18) %>%
    rename(oppTeam = oppTeam.x)

# results_book$Kendall_Spread_Result <- with(results_book, ifelse(Kendall_Spread_Edge>0,ATS_Result,0))
# # results_book$Kendall_Spread2_Result <- with(results_book, ifelse(Kendall_Spread2_Edge>0,ATS_Result,0))
# results_book$Kendall_ML_Result <- with(results_book, ifelse(Kendall_ML_Edge>0,ML_Result,0))
# results_book$Kendall_Over_Result <- with(results_book, ifelse(Kendall_Over_Edge>0,Over_Result,0))
# results_book$Kendall_Under_Result <- with(results_book, ifelse(Kendall_Under_Edge>0,Under_Result,0))

results_book$Tyra_Spread_Result <- with(results_book, ifelse(Tyra_Spread_Edge>0,ATS_Result,0))
# results_book$Tyra_Spread2_Result <- with(results_book, ifelse(Tyra_Spread2_Edge>0,ATS_Result,0))
results_book$Tyra_ML_Result <- with(results_book, ifelse(Tyra_ML_Edge>0,ML_Result,0))
results_book$Tyra_Over_Result <- with(results_book, ifelse(Tyra_Over_Edge>0,Over_Result,0))
results_book$Tyra_Under_Result <- with(results_book, ifelse(Tyra_Under_Edge>0,Under_Result,0))

# results_book$Gisele_Spread_Result <- with(results_book, ifelse(Gisele_Spread_Edge>0,ATS_Result,0))
# # results_book$Gisele_Spread2_Result <- with(results_book, ifelse(Gisele_Spread2_Edge>0,ATS_Result,0))
# results_book$Gisele_ML_Result <- with(results_book, ifelse(Gisele_ML_Edge>0,ML_Result,0))
# results_book$Gisele_Over_Result <- with(results_book, ifelse(Gisele_Over_Edge>0,Over_Result,0))
# results_book$Gisele_Under_Result <- with(results_book, ifelse(Gisele_Under_Edge>0,Under_Result,0))

# results_book$Kate_Spread_Result <- with(results_book, ifelse(Kate_Spread_Edge>0,ATS_Result,0))
# # results_book$Kate_Spread2_Result <- with(results_book, ifelse(Kate_Spread2_Edge>0,ATS_Result,0))
# results_book$Kate_ML_Result <- with(results_book, ifelse(Kate_ML_Edge>0,ML_Result,0))
# results_book$Kate_Over_Result <- with(results_book, ifelse(Kate_Over_Edge>0,Over_Result,0))
# results_book$Kate_Under_Result <- with(results_book, ifelse(Kate_Under_Edge>0,Under_Result,0))

# results_book$Cindy_Spread_Result <- with(results_book, ifelse(Cindy_Spread_Edge>0,ATS_Result,0))
# # results_book$Cindy_Spread2_Result <- with(results_book, ifelse(Cindy_Spread2_Edge>0,ATS_Result,0))
# results_book$Cindy_ML_Result <- with(results_book, ifelse(Cindy_ML_Edge>0,ML_Result,0))
# results_book$Cindy_Over_Result <- with(results_book, ifelse(Cindy_Over_Edge>0,Over_Result,0))
# results_book$Cindy_Under_Result <- with(results_book, ifelse(Cindy_Under_Edge>0,Under_Result,0))

# results_book$Naomi_Spread_Result <- with(results_book, ifelse(Naomi_Spread_Edge>0,ATS_Result,0))
# # results_book$Naomi_Spread2_Result <- with(results_book, ifelse(Naomi_Spread2_Edge>0,ATS_Result,0))
# results_book$Naomi_ML_Result <- with(results_book, ifelse(Naomi_ML_Edge>0,ML_Result,0))
# results_book$Naomi_Over_Result <- with(results_book, ifelse(Naomi_Over_Edge>0,Over_Result,0))
# results_book$Naomi_Under_Result <- with(results_book, ifelse(Naomi_Under_Edge>0,Under_Result,0))

# results_book$Heidi_Spread_Result <- with(results_book, ifelse(Heidi_Spread_Edge>0,ATS_Result,0))
# # results_book$Heidi_Spread2_Result <- with(results_book, ifelse(Heidi_Spread2_Edge>0,ATS_Result,0))
# results_book$Heidi_ML_Result <- with(results_book, ifelse(Heidi_ML_Edge>0,ML_Result,0))
# results_book$Heidi_Over_Result <- with(results_book, ifelse(Heidi_Over_Edge>0,Over_Result,0))
# results_book$Heidi_Under_Result <- with(results_book, ifelse(Heidi_Under_Edge>0,Under_Result,0))

results_book2 <- results_book

##### BACKTEST ANALYSIS #####

results_book <- results_book %>%
    mutate(ML_Wager = ifelse(ML < 100, ML/-100, 1))

combos_s <- lapply(1:1, function(x) combn(c("Tyra_Spread_Edge"), x, simplify = FALSE))
combos_s <- unlist(combos_s, recursive = F)

combos_result_s <- lapply(1:1, function(x) combn(c("Tyra_Spread_Result"), x, simplify = FALSE))
combos_result_s <- unlist(combos_result_s, recursive = F)

# combos_s2 <- lapply(1:1, function(x) combn(c("Tyra_Spread2_Edge"), x, simplify = FALSE))
# combos_s2 <- unlist(combos_s2, recursive = F)
# 
# combos_result_s2 <- lapply(1:1, function(x) combn(c("Tyra_Spread2_Result"), x, simplify = FALSE))
# combos_result_s2 <- unlist(combos_result_s2, recursive = F)

combos_m <- lapply(1:1, function(x) combn(c("Tyra_ML_Edge"), x, simplify = FALSE))
combos_m <- unlist(combos_m, recursive = F)

combos_result_m <- lapply(1:1, function(x) combn(c("Tyra_ML_Result"), x, simplify = FALSE))
combos_result_m <- unlist(combos_result_m, recursive = F)

combos_o <- lapply(1:1, function(x) combn(c("Tyra_Over_Edge"), x, simplify = FALSE))
combos_o <- unlist(combos_o, recursive = F)

combos_result_o <- lapply(1:1, function(x) combn(c("Tyra_Over_Result"), x, simplify = FALSE))
combos_result_o <- unlist(combos_result_o, recursive = F)

combos_u <- lapply(1:1, function(x) combn(c("Tyra_Under_Edge"), x, simplify = FALSE))
combos_u <- unlist(combos_u, recursive = F)

combos_result_u <- lapply(1:1, function(x) combn(c("Tyra_Under_Result"), x, simplify = FALSE))
combos_result_u <- unlist(combos_result_u, recursive = F)



### Spread
results_s <- list()
for (i in seq_along(combos_s)) {
    nms <- combos_s[[i]]
    rslt <- combos_result_s[[i]]
    results_s[[paste0(nms, collapse = "_")]] <- results_book %>% 
        filter(if_all(all_of(nms), ~ .> 0)) %>%
        select(combos_s[[i]], combos_result_s[[i]], "Loc") %>%
        mutate(Result = rowMeans(select(.,ends_with("Result")), na.rm = TRUE)) %>%
        select("Loc","Result", ends_with("Edge")) %>%
        arrange(desc(select(.,ends_with("Edge")))) %>%
        mutate(Cume = round(cumsum(Result),1)) %>%
        mutate(gameNum = row_number()) %>%
        mutate(ROI = round((Cume /(gameNum*1.1))*100,2)) %>%
        select("gameNum","Loc","Result","Cume","ROI",ends_with("Edge"))
    
}

peaker_s <- list()
for (j in seq_along(results_s)) {
    for (i in 6:ncol(results_s[[j]])) {
        x <- results_s[[j]][order(-results_s[[j]][,4]), ]
        peaker_s[[j]] <- head(x,1)
    }
}

peak_list_s <- map(peaker_s, as.data.table)
spread_peak <- rbindlist(peak_list_s, fill = TRUE, idcol = F)
spread_peak <- spread_peak %>% select(1,4:6) %>% arrange(desc(Cume))

spread_peak_filtered <- spread_peak




# ### Spread2
# results_s2 <- list()
# for (i in seq_along(combos_s2)) {
#     nms <- combos_s2[[i]]
#     rslt <- combos_result_s2[[i]]
#     results_s2[[paste0(nms, collapse = "_")]] <- results_book %>% 
#         filter(if_all(all_of(nms), ~ .> 0)) %>%
#         select(combos_s2[[i]], combos_result_s2[[i]], "Loc") %>%
#         mutate(Result = rowMeans(select(.,ends_with("Result")), na.rm = TRUE)) %>%
#         select("Loc","Result", ends_with("Edge")) %>%
#         arrange(desc(select(.,ends_with("Edge")))) %>%
#         mutate(Cume = round(cumsum(Result),1)) %>%
#         mutate(gameNum = row_number()) %>%
#         mutate(ROI = round((Cume /(gameNum*1.1))*100,2)) %>%
#         select("gameNum","Loc","Result","Cume","ROI",ends_with("Edge"))
#     
# }
# 
# peaker_s2 <- list()
# for (j in seq_along(results_s2)) {
#     for (i in 6:ncol(results_s2[[j]])) {
#         x <- results_s2[[j]][order(-results_s2[[j]][,4]), ]
#         peaker_s2[[j]] <- head(x,1)
#     }
# }
# 
# peak_list_s2 <- map(peaker_s2, as.data.table)
# spread2_peak <- rbindlist(peak_list_s2, fill = TRUE, idcol = F)
# spread2_peak <- spread2_peak %>% select(1,4:6) %>% arrange(desc(Cume))
# 
# spread2_peak_filtered <- spread2_peak




### ML
results_m <- list()
for (i in seq_along(combos_m)) {
    nms <- combos_m[[i]]
    rslt <- combos_result_m[[i]]
    results_m[[paste0(nms, collapse = "_")]] <- results_book %>% 
        filter(if_all(all_of(nms), ~ .> 0)) %>%
        select(combos_m[[i]], combos_result_m[[i]], "Loc", "ML_Wager") %>%
        mutate(Result = rowMeans(select(.,ends_with("Result")), na.rm = TRUE)) %>%
        select("Loc","Result", ends_with("Edge"), "ML_Wager") %>%
        arrange(desc(select(.,ends_with("Edge"), "ML_Wager"))) %>%
        mutate(Cume = round(cumsum(Result),1)) %>%
        mutate(gameNum = row_number()) %>%
        mutate(ROI = round((Cume / cumsum(ML_Wager))*100,2)) %>%
        select("gameNum","Loc","Result","Cume", "ROI", ends_with("Edge"))
    
}

peaker_m <- list()
for (j in seq_along(results_m)) {
    for (i in 6:ncol(results_m[[j]])) {
        x <- results_m[[j]][order(-results_m[[j]][,4]), ]
        peaker_m[[j]] <- head(x,1)
    }
}

peak_list_m <- map(peaker_m, as.data.table)
ml_peak <- rbindlist(peak_list_m, fill = TRUE, idcol = F)
ml_peak <- ml_peak %>% select(1,4:6) %>% arrange(desc(Cume))

ml_peak_filtered <- ml_peak




### Over
results_o <- list()
for (i in seq_along(combos_o)) {
    nms <- combos_o[[i]]
    rslt <- combos_result_o[[i]]
    results_o[[paste0(nms, collapse = "_")]] <- results_book %>% 
        filter(if_all(all_of(nms), ~ .> 0)) %>%
        select(combos_o[[i]], combos_result_o[[i]], "Loc") %>%
        mutate(Result = rowMeans(select(.,ends_with("Result")), na.rm = TRUE)) %>%
        select("Loc","Result", ends_with("Edge")) %>%
        arrange(desc(select(.,ends_with("Edge")))) %>%
        mutate(Cume = round(cumsum(Result),1)) %>%
        mutate(gameNum = row_number()) %>%
        mutate(ROI = round((Cume /(gameNum*1.1))*100,2)) %>%
        select("gameNum","Loc","Result","Cume","ROI",ends_with("Edge"))
    
}

peaker_o <- list()
for (j in seq_along(results_o)) {
    for (i in 6:ncol(results_o[[j]])) {
        x <- results_o[[j]][order(-results_o[[j]][,4]), ]
        peaker_o[[j]] <- head(x,1)
    }
}

peak_list_o <- map(peaker_o, as.data.table)
over_peak <- rbindlist(peak_list_o, fill = TRUE, idcol = F)
over_peak <- over_peak %>% select(1,4:6) %>% arrange(desc(Cume))

over_peak_filtered <- over_peak




### Under
results_u <- list()
for (i in seq_along(combos_u)) {
    nms <- combos_u[[i]]
    rslt <- combos_result_u[[i]]
    results_u[[paste0(nms, collapse = "_")]] <- results_book %>% 
        filter(if_all(all_of(nms), ~ .> 0)) %>%
        select(combos_u[[i]], combos_result_u[[i]], "Loc") %>%
        mutate(Result = rowMeans(select(.,ends_with("Result")), na.rm = TRUE)) %>%
        select("Loc","Result", ends_with("Edge")) %>%
        arrange(desc(select(.,ends_with("Edge")))) %>%
        mutate(Cume = round(cumsum(Result),1)) %>%
        mutate(gameNum = row_number()) %>%
        mutate(ROI = round((Cume /(gameNum*1.1))*100,2)) %>%
        select("gameNum","Loc","Result","Cume","ROI",ends_with("Edge"))
    
}

peaker_u <- list()
for (j in seq_along(results_u)) {
    for (i in 6:ncol(results_u[[j]])) {
        x <- results_u[[j]][order(-results_u[[j]][,4]), ]
        peaker_u[[j]] <- head(x,1)
    }
}

peak_list_u <- map(peaker_u, as.data.table)
under_peak <- rbindlist(peak_list_u, fill = TRUE, idcol = F)
under_peak <- under_peak %>% select(1,4:6) %>% arrange(desc(Cume))

under_peak_filtered <- under_peak


#### Print to Excel ####

#### Creating workbook - results
detach("package:XLConnect", unload = TRUE)

fn <- "Backtest_Results_2022_Tyra_All"
u <- paste0("/Users/Jesse/Documents/MyStuff/NBA Betting/Backtest 2022/Backtest Output/",fn,".xlsx")

wb <- createWorkbook()
addWorksheet(wb, sheetName = "Results")
writeData(wb, sheet = "Results", x = results_book)
saveWorkbook(wb, u)


#### Creating workbook - keys
# detach("package:XLConnect", unload = TRUE)

fn <- "Backtest_Keys_2022_Tyra_All"
u <- paste0("/Users/Jesse/Documents/MyStuff/NBA Betting/Backtest 2022/Backtest Output/",fn,".xlsx")

wb <- createWorkbook()

addWorksheet(wb, sheetName = "Spread Peak Filtered")
# addWorksheet(wb, sheetName = "Spread2 Peak Filtered")
addWorksheet(wb, sheetName = "ML Peak Filtered")
addWorksheet(wb, sheetName = "Over Peak Filtered")
addWorksheet(wb, sheetName = "Under Peak Filtered")
writeData(wb, sheet = "Spread Peak Filtered", x = spread_peak_filtered)
# writeData(wb, sheet = "Spread2 Peak Filtered", x = spread2_peak_filtered)
writeData(wb, sheet = "ML Peak Filtered", x = ml_peak_filtered)
writeData(wb, sheet = "Over Peak Filtered", x = over_peak_filtered)
writeData(wb, sheet = "Under Peak Filtered", x = under_peak_filtered)

saveWorkbook(wb, file = u)