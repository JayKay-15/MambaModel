if (!require("pacman")) install.packages("pacman"); library(pacman)
pacman::p_load(tidyverse, readxl, lubridate, openxlsx, nbastatR, rvest, progress, XLConnect, data.table)

Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)

rm(list=ls())
setwd("/Users/Jesse/Documents/MyStuff/NBA Betting/Backtest 2022/")

##### CREATE BACKTEST RESULTS BOOK #####

game_logs(seasons = c(2019:2022), result_types = c("team","players"))

dataGameLogsTeam <- dataGameLogsTeam %>% arrange(dateGame,idGame)
dataGameLogsTeam$dateGame <- as_date(dataGameLogsTeam$dateGame)
dataGameLogsPlayer <- dataGameLogsPlayer %>% arrange(dateGame,idGame)


# backtest19 <- read_xlsx("/Users/Jesse/Documents/MyStuff/NBA Betting/Backtest 2022/Backtest_2018_2019.xlsx")
# backtest19$Date <- as_date(backtest19$Date)
# 
# backtest20 <- read_xlsx("/Users/Jesse/Documents/MyStuff/NBA Betting/Backtest 2022/Backtest_2019_2020.xlsx")
# backtest20$Date <- as_date(backtest20$Date)
# 
# backtest21 <- read_xlsx("/Users/Jesse/Documents/MyStuff/NBA Betting/Backtest 2022/Backtest_2020_2021.xlsx")
# backtest21$Date <- as_date(backtest21$Date)

backtest22 <- read_xlsx("/Users/Jesse/Documents/MyStuff/NBA Betting/Backtest 2022/Backtest_2022_TS.xlsx")
backtest22$Date <- as_date(backtest22$Date)

backtest <- bind_rows(backtest22)

# backtest <- backtest %>%
#     filter(Date >= max(Date)-30)

### Attach game logs to itself to get all stats for each game in one row

gl <- left_join(dataGameLogsTeam, dataGameLogsTeam, by = c("idGame" = "idGame", "slugTeam" = "slugOpponent"))

gl <- gl %>%
    select(6,13,8,62,17,90,45)
colnames(gl) <- c("idGame", "Date", "Loc", "oppTeam", "Team", "oppScore", "Score")

results_book <- gl %>%
    arrange(idGame, Loc)

backtest <- backtest[c(1,4:71)]

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
    select(1:7, 9:11, 75:81, 12:74) %>%
    rename(oppTeam = oppTeam.x)

results_book$Kendall_Spread_Result <- with(results_book, ifelse(Kendall_Spread_Edge>0,ATS_Result,0))
results_book$Kendall_Spread2_Result <- with(results_book, ifelse(Kendall_Spread2_Edge>0,ATS_Result,0))
results_book$Kendall_ML_Result <- with(results_book, ifelse(Kendall_ML_Edge>0,ML_Result,0))
results_book$Kendall_Over_Result <- with(results_book, ifelse(Kendall_Over_Edge>0,Over_Result,0))
results_book$Kendall_Under_Result <- with(results_book, ifelse(Kendall_Under_Edge>0,Under_Result,0))

results_book$Tyra_Spread_Result <- with(results_book, ifelse(Tyra_Spread_Edge>0,ATS_Result,0))
results_book$Tyra_Spread2_Result <- with(results_book, ifelse(Tyra_Spread2_Edge>0,ATS_Result,0))
results_book$Tyra_ML_Result <- with(results_book, ifelse(Tyra_ML_Edge>0,ML_Result,0))
results_book$Tyra_Over_Result <- with(results_book, ifelse(Tyra_Over_Edge>0,Over_Result,0))
results_book$Tyra_Under_Result <- with(results_book, ifelse(Tyra_Under_Edge>0,Under_Result,0))

results_book$Gisele_Spread_Result <- with(results_book, ifelse(Gisele_Spread_Edge>0,ATS_Result,0))
results_book$Gisele_Spread2_Result <- with(results_book, ifelse(Gisele_Spread2_Edge>0,ATS_Result,0))
results_book$Gisele_ML_Result <- with(results_book, ifelse(Gisele_ML_Edge>0,ML_Result,0))
results_book$Gisele_Over_Result <- with(results_book, ifelse(Gisele_Over_Edge>0,Over_Result,0))
results_book$Gisele_Under_Result <- with(results_book, ifelse(Gisele_Under_Edge>0,Under_Result,0))

results_book$Kate_Spread_Result <- with(results_book, ifelse(Kate_Spread_Edge>0,ATS_Result,0))
results_book$Kate_Spread2_Result <- with(results_book, ifelse(Kate_Spread2_Edge>0,ATS_Result,0))
results_book$Kate_ML_Result <- with(results_book, ifelse(Kate_ML_Edge>0,ML_Result,0))
results_book$Kate_Over_Result <- with(results_book, ifelse(Kate_Over_Edge>0,Over_Result,0))
results_book$Kate_Under_Result <- with(results_book, ifelse(Kate_Under_Edge>0,Under_Result,0))

results_book$Cindy_Spread_Result <- with(results_book, ifelse(Cindy_Spread_Edge>0,ATS_Result,0))
results_book$Cindy_Spread2_Result <- with(results_book, ifelse(Cindy_Spread2_Edge>0,ATS_Result,0))
results_book$Cindy_ML_Result <- with(results_book, ifelse(Cindy_ML_Edge>0,ML_Result,0))
results_book$Cindy_Over_Result <- with(results_book, ifelse(Cindy_Over_Edge>0,Over_Result,0))
results_book$Cindy_Under_Result <- with(results_book, ifelse(Cindy_Under_Edge>0,Under_Result,0))

results_book$Naomi_Spread_Result <- with(results_book, ifelse(Naomi_Spread_Edge>0,ATS_Result,0))
results_book$Naomi_Spread2_Result <- with(results_book, ifelse(Naomi_Spread2_Edge>0,ATS_Result,0))
results_book$Naomi_ML_Result <- with(results_book, ifelse(Naomi_ML_Edge>0,ML_Result,0))
results_book$Naomi_Over_Result <- with(results_book, ifelse(Naomi_Over_Edge>0,Over_Result,0))
results_book$Naomi_Under_Result <- with(results_book, ifelse(Naomi_Under_Edge>0,Under_Result,0))

results_book$Heidi_Spread_Result <- with(results_book, ifelse(Heidi_Spread_Edge>0,ATS_Result,0))
results_book$Heidi_Spread2_Result <- with(results_book, ifelse(Heidi_Spread2_Edge>0,ATS_Result,0))
results_book$Heidi_ML_Result <- with(results_book, ifelse(Heidi_ML_Edge>0,ML_Result,0))
results_book$Heidi_Over_Result <- with(results_book, ifelse(Heidi_Over_Edge>0,Over_Result,0))
results_book$Heidi_Under_Result <- with(results_book, ifelse(Heidi_Under_Edge>0,Under_Result,0))

results_book2 <- results_book

##### BACKTEST ANALYSIS #####

# if (!require("pacman")) install.packages("pacman"); library(pacman)
# pacman::p_load(tidyverse, readxl, lubridate, openxlsx, nbastatR, rvest, data.table)
# 
# backtest22 <- read_xlsx("/Users/Jesse/Documents/MyStuff/NBA Betting/NBA-Betting-21-22/Results copy.xlsx")
# backtest22$Date <- as_date(backtest22$Date)
# 
# results_book <- bind_rows(backtest22)
# 
# fil <- 7
# results_book <- results_book %>%
#     filter(Date >= max(Date) - fil)

# results_book <- results_book %>%
#     filter(if_all(ends_with("_Edge"), ~ . <10))

# results_book <- results_book %>%
#     filter(between(Spread, -11, 11))


### Add Wager columns for calculating ML ROI
results_book <- results_book %>%
    mutate(ML_Wager = ifelse(ML < 100, ML/-100, 1))

combos_s <- lapply(1:7, function(x) combn(c("Kendall_Spread_Edge","Tyra_Spread_Edge","Gisele_Spread_Edge",
                                            "Kate_Spread_Edge","Cindy_Spread_Edge","Naomi_Spread_Edge",
                                            "Heidi_Spread_Edge"), x, simplify = FALSE))
combos_s <- unlist(combos_s, recursive = F)

combos_result_s <- lapply(1:7, function(x) combn(c("Kendall_Spread_Result","Tyra_Spread_Result","Gisele_Spread_Result",
                                                   "Kate_Spread_Result","Cindy_Spread_Result","Naomi_Spread_Result",
                                                   "Heidi_Spread_Result"), x, simplify = FALSE))
combos_result_s <- unlist(combos_result_s, recursive = F)

combos_s2 <- lapply(1:7, function(x) combn(c("Kendall_Spread2_Edge","Tyra_Spread2_Edge","Gisele_Spread2_Edge",
                                             "Kate_Spread2_Edge","Cindy_Spread2_Edge","Naomi_Spread2_Edge",
                                             "Heidi_Spread2_Edge"), x, simplify = FALSE))
combos_s2 <- unlist(combos_s2, recursive = F)

combos_result_s2 <- lapply(1:7, function(x) combn(c("Kendall_Spread2_Result","Tyra_Spread2_Result","Gisele_Spread2_Result",
                                                    "Kate_Spread2_Result","Cindy_Spread2_Result","Naomi_Spread2_Result",
                                                    "Heidi_Spread2_Result"), x, simplify = FALSE))
combos_result_s2 <- unlist(combos_result_s2, recursive = F)

combos_m <- lapply(1:7, function(x) combn(c("Kendall_ML_Edge","Tyra_ML_Edge","Gisele_ML_Edge",
                                            "Kate_ML_Edge","Cindy_ML_Edge","Naomi_ML_Edge",
                                            "Heidi_ML_Edge"), x, simplify = FALSE))
combos_m <- unlist(combos_m, recursive = F)

combos_result_m <- lapply(1:7, function(x) combn(c("Kendall_ML_Result","Tyra_ML_Result","Gisele_ML_Result",
                                                   "Kate_ML_Result","Cindy_ML_Result","Naomi_ML_Result",
                                                   "Heidi_ML_Result"), x, simplify = FALSE))
combos_result_m <- unlist(combos_result_m, recursive = F)

combos_o <- lapply(1:7, function(x) combn(c("Kendall_Over_Edge","Tyra_Over_Edge","Gisele_Over_Edge",
                                            "Kate_Over_Edge","Cindy_Over_Edge","Naomi_Over_Edge",
                                            "Heidi_Over_Edge"), x, simplify = FALSE))
combos_o <- unlist(combos_o, recursive = F)


combos_result_o <- lapply(1:7, function(x) combn(c("Kendall_Over_Result","Tyra_Over_Result","Gisele_Over_Result",
                                                   "Kate_Over_Result","Cindy_Over_Result","Naomi_Over_Result",
                                                   "Heidi_Over_Result"), x, simplify = FALSE))
combos_result_o <- unlist(combos_result_o, recursive = F)

combos_u <- lapply(1:7, function(x) combn(c("Kendall_Under_Edge","Tyra_Under_Edge","Gisele_Under_Edge",
                                            "Kate_Under_Edge","Cindy_Under_Edge","Naomi_Under_Edge",
                                            "Heidi_Under_Edge"), x, simplify = FALSE))
combos_u <- unlist(combos_u, recursive = F)


combos_result_u <- lapply(1:7, function(x) combn(c("Kendall_Under_Result","Tyra_Under_Result","Gisele_Under_Result",
                                                   "Kate_Under_Result","Cindy_Under_Result","Naomi_Under_Result",
                                                   "Heidi_Under_Result"), x, simplify = FALSE))
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
spread_peak <- spread_peak %>% select(1,4:12) %>% arrange(desc(Cume))

spread_peak_filtered <- spread_peak %>%
    mutate(num_models = rowSums(is.na(spread_peak))) %>%
    filter(num_models == 6) %>%
    select(1:10)

spread_peak_filtered <- spread_peak_filtered %>%
    pivot_longer(cols = !gameNum:ROI, names_to = "Model", values_to = "Key") %>%
    drop_na()


over_zero_s <- list()
for (j in seq_along(results_s)) {
    for (i in 6:ncol(results_s[[j]])) {
        x <- results_s[[j]]
        over_zero_s[[j]] <- tail(x,1)
    }
}

zero_list_s <- map(over_zero_s, as.data.table)
spread_over_zero <- rbindlist(zero_list_s, fill = TRUE, idcol = F)
spread_over_zero <- spread_over_zero %>% select(1,4:12) %>% arrange(desc(Cume))


### Spread2
results_s2 <- list()
for (i in seq_along(combos_s2)) {
    nms <- combos_s2[[i]]
    rslt <- combos_result_s2[[i]]
    results_s2[[paste0(nms, collapse = "_")]] <- results_book %>% 
        filter(if_all(all_of(nms), ~ .> 0)) %>%
        select(combos_s2[[i]], combos_result_s2[[i]], "Loc") %>%
        mutate(Result = rowMeans(select(.,ends_with("Result")), na.rm = TRUE)) %>%
        select("Loc","Result", ends_with("Edge")) %>%
        arrange(desc(select(.,ends_with("Edge")))) %>%
        mutate(Cume = round(cumsum(Result),1)) %>%
        mutate(gameNum = row_number()) %>%
        mutate(ROI = round((Cume /(gameNum*1.1))*100,2)) %>%
        select("gameNum","Loc","Result","Cume","ROI",ends_with("Edge"))
    
}

peaker_s2 <- list()
for (j in seq_along(results_s2)) {
    for (i in 6:ncol(results_s2[[j]])) {
        x <- results_s2[[j]][order(-results_s2[[j]][,4]), ]
        peaker_s2[[j]] <- head(x,1)
    }
}

peak_list_s2 <- map(peaker_s2, as.data.table)
spread2_peak <- rbindlist(peak_list_s2, fill = TRUE, idcol = F)
spread2_peak <- spread2_peak %>% select(1,4:12) %>% arrange(desc(Cume))

spread2_peak_filtered <- spread2_peak %>%
    mutate(num_models = rowSums(is.na(spread2_peak))) %>%
    filter(num_models == 6) %>%
    select(1:10)

spread2_peak_filtered <- spread2_peak_filtered %>%
    pivot_longer(cols = !gameNum:ROI, names_to = "Model", values_to = "Key") %>%
    drop_na()

over_zero_s2 <- list()
for (j in seq_along(results_s2)) {
    for (i in 6:ncol(results_s2[[j]])) {
        x <- results_s2[[j]]
        over_zero_s2[[j]] <- tail(x,1)
    }
}

zero_list_s2 <- map(over_zero_s2, as.data.table)
spread2_over_zero <- rbindlist(zero_list_s2, fill = TRUE, idcol = F)
spread2_over_zero <- spread2_over_zero %>% select(1,4:12) %>% arrange(desc(Cume))

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
ml_peak <- ml_peak %>% select(1,4:12) %>% arrange(desc(Cume))

ml_peak_filtered <- ml_peak %>%
    mutate(num_models = rowSums(is.na(ml_peak))) %>%
    filter(num_models == 6) %>%
    select(1:10)

ml_peak_filtered <- ml_peak_filtered %>%
    pivot_longer(cols = !gameNum:ROI, names_to = "Model", values_to = "Key") %>%
    drop_na()

over_zero_m <- list()
for (j in seq_along(results_m)) {
    for (i in 6:ncol(results_m[[j]])) {
        x <- results_m[[j]]
        over_zero_m[[j]] <- tail(x,1)
    }
}

zero_list_m <- map(over_zero_m, as.data.table)
ml_over_zero <- rbindlist(zero_list_m, fill = TRUE, idcol = F)
ml_over_zero <- ml_over_zero %>% select(1,4:12) %>% arrange(desc(Cume))

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
over_peak <- over_peak %>% select(1,4:12) %>% arrange(desc(Cume))

over_peak_filtered <- over_peak %>%
    mutate(num_models = rowSums(is.na(over_peak))) %>%
    filter(num_models == 6) %>%
    select(1:10)

over_peak_filtered <- over_peak_filtered %>%
    pivot_longer(cols = !gameNum:ROI, names_to = "Model", values_to = "Key") %>%
    drop_na()

over_zero_o <- list()
for (j in seq_along(results_o)) {
    for (i in 6:ncol(results_o[[j]])) {
        x <- results_o[[j]]
        over_zero_o[[j]] <- tail(x,1)
    }
}

zero_list_o <- map(over_zero_o, as.data.table)
over_over_zero <- rbindlist(zero_list_o, fill = TRUE, idcol = F)
over_over_zero <- over_over_zero %>% select(1,4:12) %>% arrange(desc(Cume))

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
under_peak <- under_peak %>% select(1,4:12) %>% arrange(desc(Cume))

under_peak_filtered <- under_peak %>%
    mutate(num_models = rowSums(is.na(under_peak))) %>%
    filter(num_models == 6) %>%
    select(1:10)

under_peak_filtered <- under_peak_filtered %>%
    pivot_longer(cols = !gameNum:ROI, names_to = "Model", values_to = "Key") %>%
    drop_na()

over_zero_u <- list()
for (j in seq_along(results_u)) {
    for (i in 6:ncol(results_u[[j]])) {
        x <- results_u[[j]]
        over_zero_u[[j]] <- tail(x,1)
    }
}

zero_list_u <- map(over_zero_u, as.data.table)
under_over_zero <- rbindlist(zero_list_u, fill = TRUE, idcol = F)
under_over_zero <- under_over_zero %>% select(1,4:12) %>% arrange(desc(Cume))

#### Print to Excel ####

#### Creating workbook - results
detach("package:XLConnect", unload = TRUE)

fn <- "Backtest_2022_TS_Results"
u <- paste0("/Users/Jesse/Documents/MyStuff/NBA Betting/Backtest 2022/",fn,".xlsx")

wb <- createWorkbook()
addWorksheet(wb, sheetName = "Results")
writeData(wb, sheet = "Results", x = results_book)
saveWorkbook(wb, u)



#### Creating workbook - keys
# detach("package:XLConnect", unload = TRUE)

fn <- "Backtest_Keys_2022_TS"
u <- paste0("/Users/Jesse/Documents/MyStuff/NBA Betting/Backtest 2022/",fn,".xlsx")

wb <- createWorkbook()
addWorksheet(wb, sheetName = "Spread")
addWorksheet(wb, sheetName = "Spread2")
addWorksheet(wb, sheetName = "ML")
addWorksheet(wb, sheetName = "Over")
addWorksheet(wb, sheetName = "Under")
addWorksheet(wb, sheetName = "Spread Peak Filtered")
addWorksheet(wb, sheetName = "Spread2 Peak Filtered")
addWorksheet(wb, sheetName = "ML Peak Filtered")
addWorksheet(wb, sheetName = "Over Peak Filtered")
addWorksheet(wb, sheetName = "Under Peak Filtered")
writeData(wb, sheet = "Spread", x = spread_over_zero)
writeData(wb, sheet = "Spread2", x = spread2_over_zero)
writeData(wb, sheet = "ML", x = ml_over_zero)
writeData(wb, sheet = "Over", x = over_over_zero)
writeData(wb, sheet = "Under", x = under_over_zero)
writeData(wb, sheet = "Spread Peak Filtered", x = spread_peak_filtered)
writeData(wb, sheet = "Spread2 Peak Filtered", x = spread2_peak_filtered)
writeData(wb, sheet = "ML Peak Filtered", x = ml_peak_filtered)
writeData(wb, sheet = "Over Peak Filtered", x = over_peak_filtered)
writeData(wb, sheet = "Under Peak Filtered", x = under_peak_filtered)

saveWorkbook(wb, file = u)



#### Multiple Key Logic #####
# nms <- combos_s[[8]]
# i <- 8
# 
# holder <- as.numeric(results_book %>% 
#                          filter(if_all(all_of(nms), ~ .> 0)) %>%
#                          select(combos_s[[i]], combos_result_s[[i]], "Loc") %>%
#                          mutate(Result = rowMeans(select(.,ends_with("Result")), na.rm = TRUE)) %>%
#                          select("Loc","Result", ends_with("Edge")) %>%
#                          arrange(desc(across(3, ~.))) %>%
#                          mutate(Cume = round(cumsum(Result),1)) %>%
#                          arrange(desc(Cume)) %>%
#                          head(1) %>%
#                          select(3))
# 
# holder2 <- as.numeric(results_book %>% 
#                           filter(if_all(all_of(nms), ~ .> 0)) %>%
#                           filter(if_all(all_of(pluck(nms,1)), ~. >= holder)) %>%
#                           select(combos_s[[i]], combos_result_s[[i]], "Loc") %>%
#                           mutate(Result = rowMeans(select(.,ends_with("Result")), na.rm = TRUE)) %>%
#                           select("Loc","Result", ends_with("Edge")) %>%
#                           arrange(desc(across(4, ~.))) %>%
#                           mutate(Cume = round(cumsum(Result),1)) %>%
#                           arrange(desc(Cume)) %>%
#                           head(1) %>%
#                           select(4))
# 
# test <- results_book %>% 
#     filter(if_all(all_of(nms), ~ .> 0)) %>%
#     filter(if_all(all_of(pluck(nms,1)),~. >= holder) & if_all(all_of(pluck(nms,2)),~. >= holder2)) %>%
#     select(combos_s[[i]], combos_result_s[[i]], "Loc") %>%
#     mutate(Result = rowMeans(select(.,ends_with("Result")), na.rm = TRUE)) %>%
#     select("Loc","Result", ends_with("Edge")) %>%
#     arrange(desc(select(.,ends_with("Edge")))) %>%
#     mutate(Cume = round(cumsum(Result),1)) %>%
#     mutate(gameNum = row_number()) %>%
#     mutate(ROI = round((Cume /(gameNum*1.1))*100,2)) %>%
#     select("gameNum","Loc","Result","Cume","ROI",ends_with("Edge")) %>%
#     arrange(desc(Cume)) %>%
#     head(1)



