######## Database ########

if (!require("pacman")) install.packages("pacman"); library(pacman)
pacman::p_load(tidyverse, readxl, lubridate, openxlsx, nbastatR)

options(dplyr.summarise.inform = FALSE)
Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)

rm(list=ls())

fn <- "NBAdb1722_Lg_Avg"
u <- paste0("/Users/Jesse/Documents/MyStuff/NBA Betting/NBAdb/",fn,".xlsx")

final_db <- data.frame()

game_logs(seasons = c(2017:2022), result_types = c("team","players"))

dataGameLogsTeam <- dataGameLogsTeam %>% arrange(dateGame,idGame)
dataGameLogsTeam$dateGame <- as_date(dataGameLogsTeam$dateGame)

dates17 <- dataGameLogsTeam %>%
    filter(yearSeason == 2017) %>%
    distinct(dateGame) %>%
    mutate(stat_start = min(dateGame)) %>%
    mutate(stat_end = dateGame - 1) %>%
    mutate(adj = if_else(dateGame <= min(as.Date(dateGame)) + 20, 0, 1))

dates17[c(1:11),3] <- dates17[c(1:11),1]

dates18 <- dataGameLogsTeam %>%
    filter(yearSeason == 2018) %>%
    distinct(dateGame) %>%
    mutate(stat_start = min(dateGame)) %>%
    mutate(stat_end = dateGame - 1) %>%
    mutate(adj = if_else(dateGame <= min(as.Date(dateGame)) + 20, 0, 1))

dates18[c(1:11),3] <- dates18[c(1:11),1]

dates19 <- dataGameLogsTeam %>%
    filter(yearSeason == 2019) %>%
    distinct(dateGame) %>%
    mutate(stat_start = min(dateGame)) %>%
    mutate(stat_end = dateGame - 1) %>%
    mutate(adj = if_else(dateGame <= min(as.Date(dateGame)) + 20, 0, 1))

dates19[c(1:11),3] <- dates19[c(1:11),1]

dates20 <- dataGameLogsTeam %>%
    filter(yearSeason == 2020) %>%
    distinct(dateGame) %>%
    mutate(stat_start = min(dateGame)) %>%
    mutate(stat_end = dateGame - 1) %>%
    mutate(adj = if_else(dateGame <= min(as.Date(dateGame)) + 20, 0, 1))

dates20[c(1:11),3] <- dates20[c(1:11),1]

dates21 <- dataGameLogsTeam %>%
    filter(yearSeason == 2021) %>%
    distinct(dateGame) %>%
    mutate(stat_start = min(dateGame)) %>%
    mutate(stat_end = dateGame - 1) %>%
    mutate(adj = if_else(dateGame <= min(as.Date(dateGame)) + 13, 0, 1))

dates21[c(1:11),3] <- dates21[c(1:11),1]

dates22 <- dataGameLogsTeam %>%
    filter(yearSeason == 2022) %>%
    distinct(dateGame) %>%
    mutate(stat_start = min(dateGame)) %>%
    mutate(stat_end = dateGame - 1) %>%
    mutate(adj = if_else(dateGame <= min(as.Date(dateGame)) + 20, 0, 1))

dates22[c(1:11),3] <- dates22[c(1:11),1]

dates <- as.data.frame(bind_rows(dates22, dates21, dates20, dates19, dates18, dates17))


#### lg avg ####

b <- 1
h <- nrow(dates)

for (b in b:h) {
    
    stats_start_gxg <- dates[b,2]
    stats_end_gxg <- dates[b,3]
    
    gm_day_gxg <- dates[b,1]
    
    stat_range <- dataGameLogsTeam %>% filter(dateGame >= stats_start_gxg & dateGame <= stats_end_gxg)
    
    game_range <- left_join(dataGameLogsTeam, dataGameLogsTeam, 
                            by = c("idGame" = "idGame", "slugTeam" = "slugOpponent"))
    
    ### Attach game logs to itself to get all stats for each game in one row
    
    gl <- left_join(stat_range, stat_range, by = c("idGame" = "idGame", "slugTeam" = "slugOpponent"))
    
    gl <- gl %>%
        select(13,8,17,62,7,45,90,34,79,
               24,25,27,28,35,36,37,38,39,40,43,41,42,44,
               69,70,72,73,80,81,82,83,84,85,88,86,87,89)
    
    colnames(gl) <- c("Date", "teamLoc", "teamName", "opptName", "teamRslt", 
                      "teamPTS", "opptPTS", "teamMin", "opptMin", 
                      "teamFGM", "teamFGA", "team3PM", "team3PA", "teamFTM",
                      "teamFTA", "teamORB", "teamDRB", "teamTRB", "teamAST",
                      "teamTOV", "teamSTL", "teamBLK", "teamPF", 
                      "opptFGM", "opptFGA", "oppt3PM", "oppt3PA", "opptFTM", 
                      "opptFTA", "opptORB", "opptDRB", "opptTRB", "opptAST", 
                      "opptTOV", "opptSTL", "opptBLK", "opptPF")
    
    # Filter for home/away 
    
    home <- gl %>%
        filter(teamLoc == "H")
    
    away <- gl %>%
        filter(teamLoc == "A")
    
    ##### Games count - Season #####
    
    gl <- gl %>%
        add_count(teamName, name = "teamGameCount") %>%
        add_count(opptName, name = "opptGameCount")
    
    ##### Games count - Away #####
    
    away <- away %>%
        add_count(teamName, name = "teamGameCount") %>%
        add_count(opptName, name = "opptGameCount")
    
    ##### Games count - Home #####
    
    home <- home %>%
        add_count(teamName, name = "teamGameCount") %>%
        add_count(opptName, name = "opptGameCount")



##### SEASON TOTALS #####

season_grouped <- gl %>%
    select(3,6:38) %>%
    group_by(teamName,teamGameCount) %>%
    summarise(across(c(teamPTS:opptPF), sum))

##### SEASON ADVANCED STATS #####

season_adv <- season_grouped

season_adv$Poss <- with(season_adv, teamFGA - teamORB + teamTOV + (.44 * teamFTA))
season_adv$oPoss <- with(season_adv, opptFGA - opptORB + opptTOV + (.44 * opptFTA))
season_adv$Pace <- with(season_adv, 48 * (Poss + oPoss) / (2 * (teamMin/5)))
season_adv$oPace <- with(season_adv, 48 * (Poss + oPoss) / (2 * (opptMin/5)))
season_adv$ORtg <- with(season_adv, (teamPTS / Poss) * 100)
season_adv$DRtg <- with(season_adv, (opptPTS / oPoss) * 100)

season_adv$FG <- with(season_adv, teamFGM / teamFGA)
season_adv$SR2 <- with(season_adv, (teamFGA - team3PA) / teamFGA)
season_adv$FG3 <- with(season_adv, team3PM / team3PA)
season_adv$SR3 <- with(season_adv, team3PA / teamFGA)
season_adv$FT <- with(season_adv, teamFTM / teamFTA)
season_adv$FTR <- with(season_adv, teamFTM / teamFGA)
season_adv$ORB <- with(season_adv, teamORB / (teamORB + opptDRB))
season_adv$DRB <- with(season_adv, teamDRB / (teamDRB + opptORB))
season_adv$TRB <- with(season_adv, teamTRB / (teamTRB + opptTRB))
season_adv$AST <- with(season_adv, teamAST / teamFGM)
season_adv$TOV <- with(season_adv, teamTOV / Poss)
season_adv$STL <- with(season_adv, teamSTL / oPoss)
season_adv$BLK <- with(season_adv, teamBLK / (opptFGA - oppt3PA))
season_adv$PF <- with(season_adv, teamPF / oPoss)
season_adv$eFG <- with(season_adv, (teamFGM + .5 * team3PM) / teamFGA)
season_adv$TS <- with(season_adv, teamPTS / (2 * teamFGA + .44 * teamFTA))

season_adv$oFG <- with(season_adv, opptFGM / opptFGA)
season_adv$oSR2 <- with(season_adv, (opptFGA - oppt3PA) / opptFGA)
season_adv$oFG3 <- with(season_adv, oppt3PM / oppt3PA)
season_adv$oSR3 <- with(season_adv, oppt3PA / opptFGA)
season_adv$oFT <- with(season_adv, opptFTM / opptFTA)
season_adv$oFTR <- with(season_adv, opptFTM / opptFGA)
season_adv$oORB <- with(season_adv, opptORB / (opptORB + teamDRB))
season_adv$oDRB <- with(season_adv, opptDRB / (opptDRB + teamORB))
season_adv$oTRB <- with(season_adv, opptTRB / (teamTRB + opptTRB))
season_adv$oAST <- with(season_adv, opptAST / opptFGM)
season_adv$oTOV <- with(season_adv, opptTOV / oPoss)
season_adv$oSTL <- with(season_adv, opptSTL / Poss)
season_adv$oBLK <- with(season_adv, opptBLK / (teamFGA - team3PA))
season_adv$oPF <- with(season_adv, opptPF / Poss)
season_adv$oeFG <- with(season_adv, (opptFGM + .5 * oppt3PM) / opptFGA)
season_adv$oTS <- with(season_adv, opptPTS / (2 * opptFGA + .44 * opptFTA))

season_final <- season_adv %>%
    select(1,41:72,39,40,37)

### GROUPING HOME GAMES 

home_grouped <- home %>%
    select(3,6:38) %>%
    group_by(teamName,teamGameCount) %>%
    summarise(across(c(teamPTS:opptPF), sum))

###### HOME ADVANCED STATS ######

home_adv <- home_grouped

home_adv$Poss <- with(home_adv, teamFGA - teamORB + teamTOV + (.44 * teamFTA))
home_adv$oPoss <- with(home_adv, opptFGA - opptORB + opptTOV + (.44 * opptFTA))
home_adv$Pace <- with(home_adv, 48 * (Poss + oPoss) / (2 * (teamMin/5)))
home_adv$oPace <- with(home_adv, 48 * (Poss + oPoss) / (2 * (opptMin/5)))
home_adv$ORtg <- with(home_adv, (teamPTS / Poss) * 100)
home_adv$DRtg <- with(home_adv, (opptPTS / oPoss) * 100)

home_adv$FG <- with(home_adv, teamFGM / teamFGA)
home_adv$SR2 <- with(home_adv, (teamFGA - team3PA) / teamFGA)
home_adv$FG3 <- with(home_adv, team3PM / team3PA)
home_adv$SR3 <- with(home_adv, team3PA / teamFGA)
home_adv$FT <- with(home_adv, teamFTM / teamFTA)
home_adv$FTR <- with(home_adv, teamFTM / teamFGA)
home_adv$ORB <- with(home_adv, teamORB / (teamORB + opptDRB))
home_adv$DRB <- with(home_adv, teamDRB / (teamDRB + opptORB))
home_adv$TRB <- with(home_adv, teamTRB / (teamTRB + opptTRB))
home_adv$AST <- with(home_adv, teamAST / teamFGM)
home_adv$TOV <- with(home_adv, teamTOV / Poss)
home_adv$STL <- with(home_adv, teamSTL / oPoss)
home_adv$BLK <- with(home_adv, teamBLK / (opptFGA - oppt3PA))
home_adv$PF <- with(home_adv, teamPF / oPoss)
home_adv$eFG <- with(home_adv, (teamFGM + .5 * team3PM) / teamFGA)
home_adv$TS <- with(home_adv, teamPTS / (2 * teamFGA + .44 * teamFTA))

home_adv$oFG <- with(home_adv, opptFGM / opptFGA)
home_adv$oSR2 <- with(home_adv, (opptFGA - oppt3PA) / opptFGA)
home_adv$oFG3 <- with(home_adv, oppt3PM / oppt3PA)
home_adv$oSR3 <- with(home_adv, oppt3PA / opptFGA)
home_adv$oFT <- with(home_adv, opptFTM / opptFTA)
home_adv$oFTR <- with(home_adv, opptFTM / opptFGA)
home_adv$oORB <- with(home_adv, opptORB / (opptORB + teamDRB))
home_adv$oDRB <- with(home_adv, opptDRB / (opptDRB + teamORB))
home_adv$oTRB <- with(home_adv, opptTRB / (teamTRB + opptTRB))
home_adv$oAST <- with(home_adv, opptAST / opptFGM)
home_adv$oTOV <- with(home_adv, opptTOV / oPoss)
home_adv$oSTL <- with(home_adv, opptSTL / Poss)
home_adv$oBLK <- with(home_adv, opptBLK / (teamFGA - team3PA))
home_adv$oPF <- with(home_adv, opptPF / Poss)
home_adv$oeFG <- with(home_adv, (opptFGM + .5 * oppt3PM) / opptFGA)
home_adv$oTS <- with(home_adv, opptPTS / (2 * opptFGA + .44 * opptFTA))

home_final <- home_adv %>%
    select(1,41:72,39,40,37)

### GROUPING AWAY GAMES

away_grouped <- away %>%
    select(3,6:38) %>%
    group_by(teamName,teamGameCount) %>%
    summarise(across(c(teamPTS:opptPF), sum))

###### AWAY ADVANCED STATS #####

away_adv <- away_grouped

away_adv$Poss <- with(away_adv, teamFGA - teamORB + teamTOV + (.44 * teamFTA))
away_adv$oPoss <- with(away_adv, opptFGA - opptORB + opptTOV + (.44 * opptFTA))
away_adv$Pace <- with(away_adv, 48 * (Poss + oPoss) / (2 * (teamMin/5)))
away_adv$oPace <- with(away_adv, 48 * (Poss + oPoss) / (2 * (opptMin/5)))
away_adv$ORtg <- with(away_adv, (teamPTS / Poss) * 100)
away_adv$DRtg <- with(away_adv, (opptPTS / oPoss) * 100)

away_adv$FG <- with(away_adv, teamFGM / teamFGA)
away_adv$SR2 <- with(away_adv, (teamFGA - team3PA) / teamFGA)
away_adv$FG3 <- with(away_adv, team3PM / team3PA)
away_adv$SR3 <- with(away_adv, team3PA / teamFGA)
away_adv$FT <- with(away_adv, teamFTM / teamFTA)
away_adv$FTR <- with(away_adv, teamFTM / teamFGA)
away_adv$ORB <- with(away_adv, teamORB / (teamORB + opptDRB))
away_adv$DRB <- with(away_adv, teamDRB / (teamDRB + opptORB))
away_adv$TRB <- with(away_adv, teamTRB / (teamTRB + opptTRB))
away_adv$AST <- with(away_adv, teamAST / teamFGM)
away_adv$TOV <- with(away_adv, teamTOV / Poss)
away_adv$STL <- with(away_adv, teamSTL / oPoss)
away_adv$BLK <- with(away_adv, teamBLK / (opptFGA - oppt3PA))
away_adv$PF <- with(away_adv, teamPF / oPoss)
away_adv$eFG <- with(away_adv, (teamFGM + .5 * team3PM) / teamFGA)
away_adv$TS <- with(away_adv, teamPTS / (2 * teamFGA + .44 * teamFTA))

away_adv$oFG <- with(away_adv, opptFGM / opptFGA)
away_adv$oSR2 <- with(away_adv, (opptFGA - oppt3PA) / opptFGA)
away_adv$oFG3 <- with(away_adv, oppt3PM / oppt3PA)
away_adv$oSR3 <- with(away_adv, oppt3PA / opptFGA)
away_adv$oFT <- with(away_adv, opptFTM / opptFTA)
away_adv$oFTR <- with(away_adv, opptFTM / opptFGA)
away_adv$oORB <- with(away_adv, opptORB / (opptORB + teamDRB))
away_adv$oDRB <- with(away_adv, opptDRB / (opptDRB + teamORB))
away_adv$oTRB <- with(away_adv, opptTRB / (teamTRB + opptTRB))
away_adv$oAST <- with(away_adv, opptAST / opptFGM)
away_adv$oTOV <- with(away_adv, opptTOV / oPoss)
away_adv$oSTL <- with(away_adv, opptSTL / Poss)
away_adv$oBLK <- with(away_adv, opptBLK / (teamFGA - team3PA))
away_adv$oPF <- with(away_adv, opptPF / Poss)
away_adv$oeFG <- with(away_adv, (opptFGM + .5 * oppt3PM) / opptFGA)
away_adv$oTS <- with(away_adv, opptPTS / (2 * opptFGA + .44 * opptFTA))

away_final <- away_adv %>%
    select(1,41:72,39,40,37)

### HOME LEAGUE AVG STATS

home_lg_avg <- home_final %>%
    group_by() %>%
    summarise(across(where(is.numeric), mean))

home_lg_avg$Lg_Avg <- "Home"
home_lg_avg <- home_lg_avg %>%
    select(36,1:35)

### AWAY LEAGUE AVG STATS

away_lg_avg <- away_final %>%
    group_by() %>%
    summarise(across(where(is.numeric), mean))

away_lg_avg$Lg_Avg <- "Away"
away_lg_avg <- away_lg_avg %>%
    select(36,1:35)

### SEASON LEAGUE AVG STATS

season_lg_avg <- season_final %>%
    group_by() %>%
    summarise(across(where(is.numeric), mean))

season_lg_avg$Lg_Avg <- "Season"
season_lg_avg <- season_lg_avg %>%
    select(36,1:35)

# COMBINE LEAGUE AVERAGE TABLES

league_avg <- bind_rows(season_lg_avg, home_lg_avg, away_lg_avg)
league_avg$Date <- as_date(gm_day_gxg)


final_db <- bind_rows(final_db, league_avg)

print(paste(tail(final_db$Date, n = 1), "Complete"))

}

wb <- createWorkbook()
addWorksheet(wb, sheetName = "final_db")
writeData(wb, sheet = "final_db", x = final_db)

openxlsx::saveWorkbook(wb, file = u)