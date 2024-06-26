######## Stat CleanR ########

### Creates adjusted year to date stats for score and win predictions

library(tidyverse)
library(lubridate)
library(data.table)
library(readxl)
library(openxlsx)
library(nbastatR)
library(rvest)
library(caret)
library(progress)

Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)

rm(list=ls())

## Filter date if needed

td <- as_date(Sys.Date())
# td <- as_date("2022-03-04")

### Pull game logs & arrange by date

dataGameLogsTeam <- game_logs(seasons = 2023, result_types = "team", season_types = "Regular Season")

### Attach game logs to itself to get all stats for each game in one row

gl <- dataGameLogsTeam %>%
    arrange(dateGame,idGame) %>%
    mutate(dateGame = as_date(dateGame)) %>%
    left_join(dataGameLogsTeam, 
              by = c("idGame" = "idGame", "slugTeam" = "slugOpponent")) %>%
    select(5,13,8,54,21,
           45,90,34,79,
           23,24,26,27,35,36,
           37,38,39,40,43,41,42,44,
           68,69,71,72,80,81,
           82,83,84,85,88,86,87,89) %>%
    filter(dateGame.x < td)

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

##### Games count - Season

gl <- gl %>%
    add_count(teamName, name = "teamGameCount") %>%
    add_count(opptName, name = "opptGameCount")

##### Games count - Away

away <- away %>%
    add_count(teamName, name = "teamGameCount") %>%
    add_count(opptName, name = "opptGameCount")

##### Games count - Home

home <- home %>%
    add_count(teamName, name = "teamGameCount") %>%
    add_count(opptName, name = "opptGameCount")

### GROUPING SEASON GAMES 

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

##### HOME ADVANCED STATS #####

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

##### AWAY ADVANCED STATS #####

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

##### LEAGUE AVERAGES #####

### HOME LEAGUE AVG STATS

home_lg_avg <- home_final %>%
    group_by() %>%
    summarise(across(where(is.numeric), mean))

home_lg_avg$PPG <- mean(home$teamPTS)

home_lg_avg$Lg_Avg <- "Home"
home_lg_avg <- home_lg_avg %>%
    select(37,1:36)

### AWAY LEAGUE AVG STATS

away_lg_avg <- away_final %>%
    group_by() %>%
    summarise(across(where(is.numeric), mean))

away_lg_avg$PPG <- mean(away$teamPTS)

away_lg_avg$Lg_Avg <- "Away"
away_lg_avg <- away_lg_avg %>%
    select(37,1:36)

### SEASON LEAGUE AVG STATS

season_lg_avg <- season_final %>%
    group_by() %>%
    summarise(across(where(is.numeric), mean))

season_lg_avg$PPG <- (away_lg_avg$PPG + home_lg_avg$PPG)/2

season_lg_avg$Lg_Avg <- "Season"
season_lg_avg <- season_lg_avg %>%
    select(37,1:36)

# COMBINE LEAGUE AVERAGE TABLES

league_avg <- bind_rows(season_lg_avg, home_lg_avg, away_lg_avg)


##### RAW SCHEDULE AND RESULTS ######

raw_adv <- gl

raw_adv$Poss <- with(raw_adv, teamFGA - teamORB + teamTOV + (.44 * teamFTA))
raw_adv$oPoss <- with(raw_adv, opptFGA - opptORB + opptTOV + (.44 * opptFTA))
raw_adv$Pace <- with(raw_adv, 48 * (Poss + oPoss) / (2 * (teamMin/5)))
raw_adv$oPace <- with(raw_adv, 48 * (Poss + oPoss) / (2 * (opptMin/5)))
raw_adv$ORtg <- with(raw_adv, (teamPTS / Poss) * 100)
raw_adv$DRtg <- with(raw_adv, (opptPTS / oPoss) * 100)

raw_adv$FG <- with(raw_adv, teamFGM / teamFGA)
raw_adv$SR2 <- with(raw_adv, (teamFGA - team3PA) / teamFGA)
raw_adv$FG3 <- with(raw_adv, team3PM / raw_adv$team3PA)
raw_adv$SR3 <- with(raw_adv, team3PA / teamFGA)
raw_adv$FT <- with(raw_adv, teamFTM / teamFTA)
raw_adv$FTR <- with(raw_adv, teamFTM / teamFGA)
raw_adv$ORB <- with(raw_adv, teamORB / (teamORB + opptDRB))
raw_adv$DRB <- with(raw_adv, teamDRB / (teamDRB + opptORB))
raw_adv$TRB <- with(raw_adv, teamTRB / (teamTRB + opptTRB))
raw_adv$AST <- with(raw_adv, teamAST / teamFGM)
raw_adv$TOV <- with(raw_adv, teamTOV / Poss)
raw_adv$STL <- with(raw_adv, teamSTL / oPoss)
raw_adv$BLK <- with(raw_adv, teamBLK / (opptFGA - oppt3PA))
raw_adv$PF <- with(raw_adv, teamPF / oPoss)
raw_adv$eFG <- with(raw_adv, (teamFGM + .5 * team3PM) / teamFGA)
raw_adv$TS <- with(raw_adv, teamPTS / (2 * teamFGA + .44 * teamFTA))

raw_adv$oFG <- with(raw_adv, opptFGM / opptFGA)
raw_adv$oSR2 <- with(raw_adv, (opptFGA - oppt3PA) / opptFGA)
raw_adv$oFG3 <- with(raw_adv, oppt3PM / oppt3PA)
raw_adv$oSR3 <- with(raw_adv, oppt3PA / opptFGA)
raw_adv$oFT <- with(raw_adv, opptFTM / opptFTA)
raw_adv$oFTR <- with(raw_adv, opptFTM / opptFGA)
raw_adv$oORB <- with(raw_adv, opptORB / (opptORB + teamDRB))
raw_adv$oDRB <- with(raw_adv, opptDRB / (opptDRB + teamORB))
raw_adv$oTRB <- with(raw_adv, opptTRB / (teamTRB + opptTRB))
raw_adv$oAST <- with(raw_adv, opptAST / opptFGM)
raw_adv$oTOV <- with(raw_adv, opptTOV / oPoss)
raw_adv$oSTL <- with(raw_adv, opptSTL / Poss)
raw_adv$oBLK <- with(raw_adv, opptBLK / (teamFGA - team3PA))
raw_adv$oPF <- with(raw_adv, opptPF / Poss)
raw_adv$oeFG <- with(raw_adv, (opptFGM + .5 * oppt3PM) / opptFGA)
raw_adv$oTS <- with(raw_adv, opptPTS / (2 * opptFGA + .44 * opptFTA))

raw_final <- raw_adv %>%
    select(2:4,46:77,44,45,42)

######### ROUND 1 ADJUSTMENTS ########

## join each team's average stats on to raw_adj
## split by home/away then add averages
## bring file back together

raw_adj_home <- raw_final %>%
    left_join(away_final, by = c("opptName" = "teamName")) %>%
    left_join(., home_final, by = c("teamName" = "teamName")) %>%
    filter(teamLoc == "H")

raw_adj_away <- raw_final %>%
    left_join(home_final, by = c("opptName" = "teamName")) %>%
    left_join(., away_final, by = c("teamName" = "teamName")) %>%
    filter(teamLoc == "A")

raw_adj <- bind_rows(raw_adj_home, raw_adj_away)

# opptavg = .y, team avg. = no tail, team actual for that game = .x

raw_adj$FG_adj <- (raw_adj$FG.x - (raw_adj$oFG.y - season_lg_avg$FG))
raw_adj$SR2_adj <- (raw_adj$SR2.x - (raw_adj$oSR2.y - season_lg_avg$SR2))
raw_adj$FG3_adj <- (raw_adj$FG3.x - (raw_adj$oFG3.y - season_lg_avg$FG3 ))
raw_adj$SR3_adj <- (raw_adj$SR3.x - (raw_adj$oSR3.y - season_lg_avg$SR3))
raw_adj$FT_adj <- (raw_adj$FT.x - (raw_adj$oFT.y - season_lg_avg$FT))
raw_adj$FTR_adj <- (raw_adj$FTR.x - (raw_adj$oFTR.y - season_lg_avg$FTR))
raw_adj$ORB_adj <- (raw_adj$ORB.x + (raw_adj$oDRB.y - season_lg_avg$DRB))
raw_adj$DRB_adj <- (raw_adj$DRB.x - (raw_adj$oORB.y - season_lg_avg$ORB))
raw_adj$TRB_adj <- (raw_adj$TRB.x + (raw_adj$oTRB.y - season_lg_avg$TRB))
raw_adj$AST_adj <- (raw_adj$AST.x - (raw_adj$oAST.y - season_lg_avg$AST))
raw_adj$TOV_adj <- (raw_adj$TOV.x - (raw_adj$oTOV.y - season_lg_avg$TOV))
raw_adj$STL_adj <- (raw_adj$STL.x - (raw_adj$oSTL.y - season_lg_avg$STL))
raw_adj$BLK_adj <- (raw_adj$BLK.x - (raw_adj$oBLK.y - season_lg_avg$BLK))
raw_adj$PF_adj <- (raw_adj$PF.x - (raw_adj$oPF.y - season_lg_avg$PF)) 
raw_adj$eFG_adj <- (raw_adj$eFG.x - (raw_adj$oeFG.y - season_lg_avg$eFG))
raw_adj$TS_adj <- (raw_adj$TS.x - (raw_adj$oTS.y - season_lg_avg$TS))
raw_adj$ExpPace <- (season_lg_avg$Pace + (raw_adj$Pace - season_lg_avg$Pace) + 
                        (raw_adj$Pace.y - season_lg_avg$Pace))
raw_adj$PaceDiff <- (raw_adj$Pace.x - raw_adj$ExpPace)
raw_adj$PaceR <- (raw_adj$Pace / (raw_adj$Pace + raw_adj$Pace.y))
raw_adj$oPaceR <- (raw_adj$Pace.y / (raw_adj$Pace + raw_adj$Pace.y))
raw_adj$Pace_adj <- (raw_adj$Pace + (raw_adj$PaceDiff * raw_adj$PaceR))
raw_adj$ORtg_adj <- (raw_adj$ORtg.x - (raw_adj$DRtg.y - season_lg_avg$DRtg))
raw_adj$DRtg_adj <- (raw_adj$DRtg.x - (raw_adj$ORtg.y - season_lg_avg$ORtg))

raw_adj$oFG_adj <- (raw_adj$oFG.x - (raw_adj$FG - season_lg_avg$FG))
raw_adj$oSR2_adj <- (raw_adj$oSR2.x - (raw_adj$SR2 - season_lg_avg$SR2))
raw_adj$oFG3_adj <- (raw_adj$oFG3.x - (raw_adj$FG3 - season_lg_avg$FG3 ))
raw_adj$oSR3_adj <- (raw_adj$oSR3.x - (raw_adj$SR3 - season_lg_avg$SR3))
raw_adj$oFT_adj <- (raw_adj$oFT.x - (raw_adj$FT - season_lg_avg$FT))
raw_adj$oFTR_adj <- (raw_adj$oFTR.x - (raw_adj$FTR - season_lg_avg$FTR))
raw_adj$oORB_adj <- (raw_adj$oORB.x + (raw_adj$DRB - season_lg_avg$DRB))
raw_adj$oDRB_adj <- (raw_adj$oDRB.x - (raw_adj$ORB - season_lg_avg$ORB))
raw_adj$oTRB_adj <- (raw_adj$oTRB.x + (raw_adj$TRB - season_lg_avg$TRB))
raw_adj$oAST_adj <- (raw_adj$oAST.x - (raw_adj$AST - season_lg_avg$AST))
raw_adj$oTOV_adj <- (raw_adj$oTOV.x - (raw_adj$TOV - season_lg_avg$TOV))
raw_adj$oSTL_adj <- (raw_adj$oSTL.x - (raw_adj$STL - season_lg_avg$STL))
raw_adj$oBLK_adj <- (raw_adj$oBLK.x - (raw_adj$BLK - season_lg_avg$BLK))
raw_adj$oPF_adj <- (raw_adj$oPF.x - (raw_adj$PF - season_lg_avg$PF)) 
raw_adj$oeFG_adj <- (raw_adj$oeFG.x - (raw_adj$eFG - season_lg_avg$eFG))
raw_adj$oTS_adj <- (raw_adj$oTS.x - (raw_adj$TS - season_lg_avg$TS))

### GROUP ROUND 1 ADJUSTMENTS

season_adj_round_1 <- raw_adj %>%
    select(2,109:124,132:147,130,131,129) %>%
    group_by(teamName) %>%
    summarise(across(where(is.numeric), mean))

home_adj_round_1 <- raw_adj %>%
    select(1,2,109:124,132:147,130,131,129) %>%
    filter(teamLoc == "H") %>%
    group_by(teamName) %>%
    summarise(across(where(is.numeric), mean))

away_adj_round_1 <- raw_adj %>%
    select(1,2,109:124,132:147,130,131,129) %>%
    filter(teamLoc == "A") %>%
    group_by(teamName) %>%
    summarise(across(where(is.numeric), mean))

### Weighting Data frames ###

season_uw <- raw_adj %>%
    select(2,109:124,132:147,130,131,129)

home_uw <- raw_adj %>%
    select(1,2,109:124,132:147,130,131,129) %>%
    filter(teamLoc == "H")

away_uw <- raw_adj %>%
    select(1,2,109:124,132:147,130,131,129) %>%
    filter(teamLoc == "A")

##### AWAY WEIGHTING ####

wt_df_away <- data.frame()

a <- 1
g <- nrow(away_final)

for (a in a:g) {
    
    act_id <- as.character(away_final[a,1])
    
    adj_gxg <- away_uw %>%
        filter(teamName == act_id)
    
    ngames <- nrow(adj_gxg)
    
    if (ngames > 20) { weightmax <- 4 } else { weightmax <- 1 + ((ngames - 1) * .157894737)  }
    weightmin <- 1
    weightdist <- (weightmax - weightmin) / (ngames - 1)
    if (ngames < 2) { weightdist <- 0 }
    
    weightcurve <- matrix(0, nrow = ngames, ncol = 1)
    c <- 1
    i <- nrow(weightcurve)
    
    for (c in c:i) {
        
        weightcurve[c] <- weightmin + ((c - 1) * weightdist)
        
    }
    
    weight_sums <- sum(weightcurve)
    weight_avg <- mean(weightcurve)
    
    FG_wt <- (adj_gxg$FG_adj * weightcurve) / weight_sums
    SR2_wt <- (adj_gxg$SR2_adj * weightcurve) / weight_sums
    FG3_wt <- (adj_gxg$FG3_adj * weightcurve) / weight_sums
    SR3_wt <- (adj_gxg$SR3_adj * weightcurve) / weight_sums
    FT_wt <- (adj_gxg$FT_adj * weightcurve) / weight_sums
    FTR_wt <- (adj_gxg$FTR_adj * weightcurve) / weight_sums
    ORB_wt <- (adj_gxg$ORB_adj * weightcurve) / weight_sums
    DRB_wt <- (adj_gxg$DRB_adj * weightcurve) / weight_sums
    TRB_wt <- (adj_gxg$TRB_adj * weightcurve) / weight_sums
    AST_wt <- (adj_gxg$AST_adj * weightcurve) / weight_sums
    TOV_wt <- (adj_gxg$TOV_adj * weightcurve) / weight_sums
    STL_wt <- (adj_gxg$STL_adj * weightcurve) / weight_sums
    BLK_wt <- (adj_gxg$BLK_adj * weightcurve) / weight_sums
    PF_wt <- (adj_gxg$PF_adj * weightcurve) / weight_sums 
    eFG_wt <- (adj_gxg$eFG_adj * weightcurve) / weight_sums
    TS_wt <- (adj_gxg$TS_adj * weightcurve) / weight_sums
    Pace_wt <- (adj_gxg$Pace_adj * weightcurve) / weight_sums
    ORtg_wt <- (adj_gxg$ORtg_adj * weightcurve) / weight_sums
    DRtg_wt <- (adj_gxg$DRtg_adj * weightcurve) / weight_sums
    
    oFG_wt <- (adj_gxg$oFG_adj * weightcurve) / weight_sums
    oSR2_wt <- (adj_gxg$oSR2_adj * weightcurve) / weight_sums
    oFG3_wt <- (adj_gxg$oFG3_adj * weightcurve) / weight_sums
    oSR3_wt <- (adj_gxg$oSR3_adj * weightcurve) / weight_sums
    oFT_wt <- (adj_gxg$oFT_adj * weightcurve) / weight_sums
    oFTR_wt <- (adj_gxg$oFTR_adj * weightcurve) / weight_sums
    oORB_wt <- (adj_gxg$oORB_adj * weightcurve) / weight_sums
    oDRB_wt <- (adj_gxg$oDRB_adj * weightcurve) / weight_sums
    oTRB_wt <- (adj_gxg$oTRB_adj * weightcurve) / weight_sums
    oAST_wt <- (adj_gxg$oAST_adj * weightcurve) / weight_sums
    oTOV_wt <- (adj_gxg$oTOV_adj * weightcurve) / weight_sums
    oSTL_wt <- (adj_gxg$oSTL_adj * weightcurve) / weight_sums
    oBLK_wt <- (adj_gxg$oBLK_adj * weightcurve) / weight_sums
    oPF_wt <- (adj_gxg$oPF_adj * weightcurve) / weight_sums
    oeFG_wt <- (adj_gxg$oeFG_adj * weightcurve) / weight_sums
    oTS_wt <- (adj_gxg$oTS_adj * weightcurve) / weight_sums
    
    FG_wt <- sum(FG_wt)
    SR2_wt <- sum(SR2_wt)
    FG3_wt <- sum(FG3_wt)
    SR3_wt <- sum(SR3_wt)
    FT_wt <- sum(FT_wt)
    FTR_wt <- sum(FTR_wt)
    ORB_wt <- sum(ORB_wt)
    DRB_wt <- sum(DRB_wt)
    TRB_wt <- sum(TRB_wt)
    AST_wt <- sum(AST_wt)
    TOV_wt <- sum(TOV_wt)
    STL_wt <- sum(STL_wt)
    BLK_wt <- sum(BLK_wt)
    PF_wt <- sum(PF_wt)
    eFG_wt <- sum(eFG_wt)
    TS_wt <- sum(TS_wt)
    Pace_wt <- sum(Pace_wt)
    ORtg_wt <- sum(ORtg_wt)
    DRtg_wt <- sum(DRtg_wt)
    
    oFG_wt <- sum(oFG_wt)
    oSR2_wt <- sum(oSR2_wt)
    oFG3_wt <- sum(oFG3_wt)
    oSR3_wt <- sum(oSR3_wt)
    oFT_wt <- sum(oFT_wt)
    oFTR_wt <- sum(oFTR_wt)
    oORB_wt <- sum(oORB_wt)
    oDRB_wt <- sum(oDRB_wt)
    oTRB_wt <- sum(oTRB_wt)
    oAST_wt <- sum(oAST_wt)
    oTOV_wt <- sum(oTOV_wt)
    oSTL_wt <- sum(oSTL_wt)
    oBLK_wt <- sum(oBLK_wt)
    oPF_wt <- sum(oPF_wt)
    oeFG_wt <- sum(oeFG_wt)
    oTS_wt <- sum(oTS_wt)
    
    wt_holder_away <- data.frame(act_id,FG_wt,SR2_wt,FG3_wt,SR3_wt,FT_wt,
                        FTR_wt,ORB_wt,DRB_wt,TRB_wt,AST_wt,TOV_wt,
                        STL_wt,BLK_wt,PF_wt,eFG_wt,TS_wt,oFG_wt,oSR2_wt,oFG3_wt,oSR3_wt,
                        oFT_wt,oFTR_wt,oORB_wt,oDRB_wt,oTRB_wt,oAST_wt,
                        oTOV_wt,oSTL_wt,oBLK_wt,oPF_wt,oeFG_wt,oTS_wt,
                        ORtg_wt,DRtg_wt,Pace_wt)
    
    wt_df_away <- bind_rows(wt_df_away, wt_holder_away)
    
}

away_final_wt <- wt_df_away

colnames(away_final_wt) <- c("team","FG","SR2","FG3","SR3","FT","FTR","ORB","DRB","TRB",
                             "AST","TOV","STL","BLK","PF","eFG","TS",
                             "oFG","oSR2","oFG3","oSR3","oFT","oFTR","oORB","oDRB","oTRB",
                             "oAST","oTOV","oSTL","oBLK","oPF","oeFG","oTS",
                             "ORtg","DRtg","Pace")

##### HOME WEIGHTING ####

wt_df_home <- data.frame()

a <- 1
g <- nrow(home_final)

for (a in a:g) {
    
    act_id <- as.character(home_final[a,1])
    
    adj_gxg <- home_uw %>%
        filter(teamName == act_id)
    
    ngames <- nrow(adj_gxg)
    
    if (ngames > 20) { weightmax <- 4 } else { weightmax <- 1 + ((ngames - 1) * .157894737)  }
    weightmin <- 1
    weightdist <- (weightmax - weightmin) / (ngames - 1)
    if (ngames < 2) { weightdist <- 0 }
    
    weightcurve <- matrix(0, nrow = ngames, ncol = 1)
    c <- 1
    i <- nrow(weightcurve)
    
    for (c in c:i) {
        
        weightcurve[c] <- weightmin + ((c - 1) * weightdist)
        
    }
    
    weight_sums <- sum(weightcurve)
    weight_avg <- mean(weightcurve)
    
    FG_wt <- (adj_gxg$FG_adj * weightcurve) / weight_sums
    SR2_wt <- (adj_gxg$SR2_adj * weightcurve) / weight_sums
    FG3_wt <- (adj_gxg$FG3_adj * weightcurve) / weight_sums
    SR3_wt <- (adj_gxg$SR3_adj * weightcurve) / weight_sums
    FT_wt <- (adj_gxg$FT_adj * weightcurve) / weight_sums
    FTR_wt <- (adj_gxg$FTR_adj * weightcurve) / weight_sums
    ORB_wt <- (adj_gxg$ORB_adj * weightcurve) / weight_sums
    DRB_wt <- (adj_gxg$DRB_adj * weightcurve) / weight_sums
    TRB_wt <- (adj_gxg$TRB_adj * weightcurve) / weight_sums
    AST_wt <- (adj_gxg$AST_adj * weightcurve) / weight_sums
    TOV_wt <- (adj_gxg$TOV_adj * weightcurve) / weight_sums
    STL_wt <- (adj_gxg$STL_adj * weightcurve) / weight_sums
    BLK_wt <- (adj_gxg$BLK_adj * weightcurve) / weight_sums
    PF_wt <- (adj_gxg$PF_adj * weightcurve) / weight_sums 
    eFG_wt <- (adj_gxg$eFG_adj * weightcurve) / weight_sums
    TS_wt <- (adj_gxg$TS_adj * weightcurve) / weight_sums
    Pace_wt <- (adj_gxg$Pace_adj * weightcurve) / weight_sums
    ORtg_wt <- (adj_gxg$ORtg_adj * weightcurve) / weight_sums
    DRtg_wt <- (adj_gxg$DRtg_adj * weightcurve) / weight_sums
    
    oFG_wt <- (adj_gxg$oFG_adj * weightcurve) / weight_sums
    oSR2_wt <- (adj_gxg$oSR2_adj * weightcurve) / weight_sums
    oFG3_wt <- (adj_gxg$oFG3_adj * weightcurve) / weight_sums
    oSR3_wt <- (adj_gxg$oSR3_adj * weightcurve) / weight_sums
    oFT_wt <- (adj_gxg$oFT_adj * weightcurve) / weight_sums
    oFTR_wt <- (adj_gxg$oFTR_adj * weightcurve) / weight_sums
    oORB_wt <- (adj_gxg$oORB_adj * weightcurve) / weight_sums
    oDRB_wt <- (adj_gxg$oDRB_adj * weightcurve) / weight_sums
    oTRB_wt <- (adj_gxg$oTRB_adj * weightcurve) / weight_sums
    oAST_wt <- (adj_gxg$oAST_adj * weightcurve) / weight_sums
    oTOV_wt <- (adj_gxg$oTOV_adj * weightcurve) / weight_sums
    oSTL_wt <- (adj_gxg$oSTL_adj * weightcurve) / weight_sums
    oBLK_wt <- (adj_gxg$oBLK_adj * weightcurve) / weight_sums
    oPF_wt <- (adj_gxg$oPF_adj * weightcurve) / weight_sums
    oeFG_wt <- (adj_gxg$oeFG_adj * weightcurve) / weight_sums
    oTS_wt <- (adj_gxg$oTS_adj * weightcurve) / weight_sums
    
    FG_wt <- sum(FG_wt)
    SR2_wt <- sum(SR2_wt)
    FG3_wt <- sum(FG3_wt)
    SR3_wt <- sum(SR3_wt)
    FT_wt <- sum(FT_wt)
    FTR_wt <- sum(FTR_wt)
    ORB_wt <- sum(ORB_wt)
    DRB_wt <- sum(DRB_wt)
    TRB_wt <- sum(TRB_wt)
    AST_wt <- sum(AST_wt)
    TOV_wt <- sum(TOV_wt)
    STL_wt <- sum(STL_wt)
    BLK_wt <- sum(BLK_wt)
    PF_wt <- sum(PF_wt)
    eFG_wt <- sum(eFG_wt)
    TS_wt <- sum(TS_wt)
    Pace_wt <- sum(Pace_wt)
    ORtg_wt <- sum(ORtg_wt)
    DRtg_wt <- sum(DRtg_wt)
    
    oFG_wt <- sum(oFG_wt)
    oSR2_wt <- sum(oSR2_wt)
    oFG3_wt <- sum(oFG3_wt)
    oSR3_wt <- sum(oSR3_wt)
    oFT_wt <- sum(oFT_wt)
    oFTR_wt <- sum(oFTR_wt)
    oORB_wt <- sum(oORB_wt)
    oDRB_wt <- sum(oDRB_wt)
    oTRB_wt <- sum(oTRB_wt)
    oAST_wt <- sum(oAST_wt)
    oTOV_wt <- sum(oTOV_wt)
    oSTL_wt <- sum(oSTL_wt)
    oBLK_wt <- sum(oBLK_wt)
    oPF_wt <- sum(oPF_wt)
    oeFG_wt <- sum(oeFG_wt)
    oTS_wt <- sum(oTS_wt)
    
    wt_holder_home <- data.frame(act_id,FG_wt,SR2_wt,FG3_wt,SR3_wt,FT_wt,
                        FTR_wt,ORB_wt,DRB_wt,TRB_wt,AST_wt,TOV_wt,
                        STL_wt,BLK_wt,PF_wt,eFG_wt,TS_wt,oFG_wt,oSR2_wt,oFG3_wt,oSR3_wt,
                        oFT_wt,oFTR_wt,oORB_wt,oDRB_wt,oTRB_wt,oAST_wt,
                        oTOV_wt,oSTL_wt,oBLK_wt,oPF_wt,oeFG_wt,oTS_wt,
                        ORtg_wt,DRtg_wt,Pace_wt)
    
    wt_df_home <- bind_rows(wt_df_home, wt_holder_home)
    
}

home_final_wt <- wt_df_home

colnames(home_final_wt) <- c("team","FG","SR2","FG3","SR3","FT","FTR","ORB","DRB","TRB",
                             "AST","TOV","STL","BLK","PF","eFG","TS",
                             "oFG","oSR2","oFG3","oSR3","oFT","oFTR","oORB","oDRB","oTRB",
                             "oAST","oTOV","oSTL","oBLK","oPF","oeFG","oTS",
                             "ORtg","DRtg","Pace")

##### SEASON WEIGHTING ####

wt_df_season <- data.frame()

a <- 1
g <- nrow(season_final)

for (a in a:g) {
    
    act_id <- as.character(season_final[a,1])
    
    adj_gxg <- season_uw %>%
        filter(teamName == act_id)
    
    ngames <- nrow(adj_gxg)
    
    if (ngames > 20) { weightmax <- 4 } else { weightmax <- 1 + ((ngames - 1) * .157894737)  }
    weightmin <- 1
    weightdist <- (weightmax - weightmin) / (ngames - 1)
    if (ngames < 2) { weightdist <- 0 }
    
    weightcurve <- matrix(0, nrow = ngames, ncol = 1)
    c <- 1
    i <- nrow(weightcurve)
    
    for (c in c:i) {
        
        weightcurve[c] <- weightmin + ((c - 1) * weightdist)
        
    }
    
    weight_sums <- sum(weightcurve)
    weight_avg <- mean(weightcurve)
    
    FG_wt <- (adj_gxg$FG_adj * weightcurve) / weight_sums
    SR2_wt <- (adj_gxg$SR2_adj * weightcurve) / weight_sums
    FG3_wt <- (adj_gxg$FG3_adj * weightcurve) / weight_sums
    SR3_wt <- (adj_gxg$SR3_adj * weightcurve) / weight_sums
    FT_wt <- (adj_gxg$FT_adj * weightcurve) / weight_sums
    FTR_wt <- (adj_gxg$FTR_adj * weightcurve) / weight_sums
    ORB_wt <- (adj_gxg$ORB_adj * weightcurve) / weight_sums
    DRB_wt <- (adj_gxg$DRB_adj * weightcurve) / weight_sums
    TRB_wt <- (adj_gxg$TRB_adj * weightcurve) / weight_sums
    AST_wt <- (adj_gxg$AST_adj * weightcurve) / weight_sums
    TOV_wt <- (adj_gxg$TOV_adj * weightcurve) / weight_sums
    STL_wt <- (adj_gxg$STL_adj * weightcurve) / weight_sums
    BLK_wt <- (adj_gxg$BLK_adj * weightcurve) / weight_sums
    PF_wt <- (adj_gxg$PF_adj * weightcurve) / weight_sums 
    eFG_wt <- (adj_gxg$eFG_adj * weightcurve) / weight_sums
    TS_wt <- (adj_gxg$TS_adj * weightcurve) / weight_sums
    Pace_wt <- (adj_gxg$Pace_adj * weightcurve) / weight_sums
    ORtg_wt <- (adj_gxg$ORtg_adj * weightcurve) / weight_sums
    DRtg_wt <- (adj_gxg$DRtg_adj * weightcurve) / weight_sums
    
    oFG_wt <- (adj_gxg$oFG_adj * weightcurve) / weight_sums
    oSR2_wt <- (adj_gxg$oSR2_adj * weightcurve) / weight_sums
    oFG3_wt <- (adj_gxg$oFG3_adj * weightcurve) / weight_sums
    oSR3_wt <- (adj_gxg$oSR3_adj * weightcurve) / weight_sums
    oFT_wt <- (adj_gxg$oFT_adj * weightcurve) / weight_sums
    oFTR_wt <- (adj_gxg$oFTR_adj * weightcurve) / weight_sums
    oORB_wt <- (adj_gxg$oORB_adj * weightcurve) / weight_sums
    oDRB_wt <- (adj_gxg$oDRB_adj * weightcurve) / weight_sums
    oTRB_wt <- (adj_gxg$oTRB_adj * weightcurve) / weight_sums
    oAST_wt <- (adj_gxg$oAST_adj * weightcurve) / weight_sums
    oTOV_wt <- (adj_gxg$oTOV_adj * weightcurve) / weight_sums
    oSTL_wt <- (adj_gxg$oSTL_adj * weightcurve) / weight_sums
    oBLK_wt <- (adj_gxg$oBLK_adj * weightcurve) / weight_sums
    oPF_wt <- (adj_gxg$oPF_adj * weightcurve) / weight_sums
    oeFG_wt <- (adj_gxg$oeFG_adj * weightcurve) / weight_sums
    oTS_wt <- (adj_gxg$oTS_adj * weightcurve) / weight_sums
    
    FG_wt <- sum(FG_wt)
    SR2_wt <- sum(SR2_wt)
    FG3_wt <- sum(FG3_wt)
    SR3_wt <- sum(SR3_wt)
    FT_wt <- sum(FT_wt)
    FTR_wt <- sum(FTR_wt)
    ORB_wt <- sum(ORB_wt)
    DRB_wt <- sum(DRB_wt)
    TRB_wt <- sum(TRB_wt)
    AST_wt <- sum(AST_wt)
    TOV_wt <- sum(TOV_wt)
    STL_wt <- sum(STL_wt)
    BLK_wt <- sum(BLK_wt)
    PF_wt <- sum(PF_wt)
    eFG_wt <- sum(eFG_wt)
    TS_wt <- sum(TS_wt)
    Pace_wt <- sum(Pace_wt)
    ORtg_wt <- sum(ORtg_wt)
    DRtg_wt <- sum(DRtg_wt)
    
    oFG_wt <- sum(oFG_wt)
    oSR2_wt <- sum(oSR2_wt)
    oFG3_wt <- sum(oFG3_wt)
    oSR3_wt <- sum(oSR3_wt)
    oFT_wt <- sum(oFT_wt)
    oFTR_wt <- sum(oFTR_wt)
    oORB_wt <- sum(oORB_wt)
    oDRB_wt <- sum(oDRB_wt)
    oTRB_wt <- sum(oTRB_wt)
    oAST_wt <- sum(oAST_wt)
    oTOV_wt <- sum(oTOV_wt)
    oSTL_wt <- sum(oSTL_wt)
    oBLK_wt <- sum(oBLK_wt)
    oPF_wt <- sum(oPF_wt)
    oeFG_wt <- sum(oeFG_wt)
    oTS_wt <- sum(oTS_wt)
    
    wt_holder_season <- data.frame(act_id,FG_wt,SR2_wt,FG3_wt,SR3_wt,FT_wt,
                        FTR_wt,ORB_wt,DRB_wt,TRB_wt,AST_wt,TOV_wt,
                        STL_wt,BLK_wt,PF_wt,eFG_wt,TS_wt,oFG_wt,oSR2_wt,oFG3_wt,oSR3_wt,
                        oFT_wt,oFTR_wt,oORB_wt,oDRB_wt,oTRB_wt,oAST_wt,
                        oTOV_wt,oSTL_wt,oBLK_wt,oPF_wt,oeFG_wt,oTS_wt,
                        ORtg_wt,DRtg_wt,Pace_wt)
    
    wt_df_season <- bind_rows(wt_df_season, wt_holder_season)
    
}

season_final_wt <- wt_df_season

colnames(season_final_wt) <- c("team","FG","SR2","FG3","SR3","FT","FTR","ORB","DRB","TRB",
                             "AST","TOV","STL","BLK","PF","eFG","TS",
                             "oFG","oSR2","oFG3","oSR3","oFT","oFTR","oORB","oDRB","oTRB",
                             "oAST","oTOV","oSTL","oBLK","oPF","oeFG","oTS",
                             "ORtg","DRtg","Pace")

#### GAME DETAILS ####

standings_raw <- standings(seasons = 2023, season_types = c("Regular Season"))

standings <- standings_raw %>%
    select("nameTeam","nameConference","nameDivison","recordOverall","pctWinTeam","recordLast10","slugStreakCurrent",
           "recordAway","recordAwayWinPct","recordLast10Away","slugStreakAwayCurrent",
           "recordHome","recordHomeWinPct","recordLast10Home","slugStreakHomeCurrent",
           "rankPlayoffs")

colnames(standings) <- c("Team","Conference","Division","Record","Win%","L10","Streak",
                         "Away Record","Away Win%","Away L10","Away Streak",
                         "Home Record", "Home Win%","Home L10","Home Streak",
                         "Conference Rank")

#### RANKINGS ####

a_list <- list("TOV","STL","BLK","PF","oFG","oSR2","oFG3","oSR3","oFT","oFTR","oORB",
               "oDRB","oTRB","oAST","oeFG","oTS","DRtg")
d_list <- list("FG","SR2","FG3","SR3","FT","FTR","ORB","DRB","TRB","AST",
               "eFG","TS","ORtg","Pace","oTOV","oSTL","oBLK","oPF")

away_rank_a <- away_final_wt %>%
    mutate_if(grepl(paste(a_list, collapse = "|"), names(.)), list(rank=~rank( .)))

away_rank_d <- away_final_wt %>%
    mutate_if(grepl(paste(d_list, collapse = "|"), names(.)), list(rank=~rank(-.)))

away_rank <- away_final_wt %>%
    left_join(away_rank_d[,c(1,37:48,59:62,65,66)]) %>%
    left_join(.,away_rank_a[,c(1,37:50,55:57)], by = "team") %>%
    select(1,2,37,3,38,4,39,5,40,6,41,7,42,8,43,9,44,10,45,11,46,12,55,13,56,14,57,15,58,16,47,17,48,
           18,59,19,60,20,61,21,62,22,63,23,64,24,65,25,66,26,67,27,68,28,49,29,50,30,51,31,52,32,69,33,70,
           34,53,35,71,36,54)

home_rank_a <- home_final_wt %>%
    mutate_if(grepl(paste(a_list, collapse = "|"), names(.)), list(rank=~rank( .)))

home_rank_d <- home_final_wt %>%
    mutate_if(grepl(paste(d_list, collapse = "|"), names(.)), list(rank=~rank(-.)))

home_rank <- home_final_wt %>%
    left_join(home_rank_d[,c(1,37:48,59:62,65,66)]) %>%
    left_join(.,home_rank_a[,c(1,37:50,55:57)], by = "team") %>%
    select(1,2,37,3,38,4,39,5,40,6,41,7,42,8,43,9,44,10,45,11,46,12,55,13,56,14,57,15,58,16,47,17,48,
           18,59,19,60,20,61,21,62,22,63,23,64,24,65,25,66,26,67,27,68,28,49,29,50,30,51,31,52,32,69,33,70,
           34,53,35,71,36,54)

#### LEAGUE AVERAGE FOR ANALYSIS ####
lg_avg <- gl

lg_avg$Poss <- with(lg_avg, teamFGA - teamORB + teamTOV + (.44 * teamFTA))
lg_avg$oPoss <- with(lg_avg, opptFGA - opptORB + opptTOV + (.44 * opptFTA))
lg_avg$Pace <- with(lg_avg, 48 * (Poss + oPoss) / (2 * (teamMin/5)))
lg_avg$ORtg <- with(lg_avg, (teamPTS / Poss) * 100)
lg_avg$DRtg <- with(lg_avg, (opptPTS / oPoss) * 100)
lg_avg$NRtg <- with(lg_avg, ORtg - DRtg)

lg_avg$FG <- with(lg_avg, teamFGM / teamFGA)
lg_avg$SR2 <- with(lg_avg, (teamFGA - team3PA) / teamFGA)
lg_avg$FG3 <- with(lg_avg, team3PM / team3PA)
lg_avg$SR3 <- with(lg_avg, team3PA / teamFGA)
lg_avg$FT <- with(lg_avg, teamFTM / teamFTA)
lg_avg$FTR <- with(lg_avg, teamFTM / teamFGA)
lg_avg$ORB <- with(lg_avg, teamORB / (teamORB + opptDRB))
lg_avg$DRB <- with(lg_avg, teamDRB / (teamDRB + opptORB))
lg_avg$TRB <- with(lg_avg, teamTRB / (teamTRB + opptTRB))
lg_avg$AST_TOV <- with(lg_avg, teamAST / teamTOV)
lg_avg$AST <- with(lg_avg, teamAST / teamFGM)
lg_avg$AST_RATIO <- with(lg_avg, (teamAST*100) / (teamFGA + (0.44 * teamFTA) + teamAST + teamTOV))
lg_avg$TOV <- with(lg_avg, teamTOV / Poss)
lg_avg$STL <- with(lg_avg, teamSTL / oPoss)
lg_avg$BLK <- with(lg_avg, teamBLK / (opptFGA - oppt3PA))
lg_avg$PF <- with(lg_avg, teamPF / oPoss)
lg_avg$eFG <- with(lg_avg, (teamFGM + .5 * team3PM) / teamFGA)
lg_avg$TS <- with(lg_avg, teamPTS / (2 * teamFGA + .44 * teamFTA))

NBAdb <- DBI::dbConnect(RSQLite::SQLite(), "../nba_sql_db/nba_db")
DBI::dbWriteTable(NBAdb, "league_avg_current", lg_avg, overwrite = T)
DBI::dbDisconnect(NBAdb)


### Clean Environment ----
rm(list=ls()[! ls() %in% c("away_final_wt","home_final_wt","league_avg","standings",
                           "away_rank","home_rank","season_final","away_final","home_final")])

print("Stat CleanR Complete")