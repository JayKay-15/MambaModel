### stat cleanR ----
library(tidyverse)
library(nbastatR)
library(data.table)
library(janitor)
library(RSQLite)
library(DBI)


Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)

# pull game logs
# game_logs(seasons = c(2014:2023), result_types = c("team","players"))

dataGameLogsTeam <- tbl(dbConnect(SQLite(), "../nba_sql_db/nba_db"),
                        "GameLogsTeam") %>%
    collect() %>%
    filter(yearSeason %in% c(2014:2023)) %>%
    mutate(dateGame = as_date(dateGame, origin ="1970-01-01"))
    
# pull historical odds
odds_df <- dplyr::tbl(DBI::dbConnect(RSQLite::SQLite(), "../nba_sql_db/nba_db"),
                      "odds_table") %>% 
    collect()

# create box scores and additional stats
box_scores <- dataGameLogsTeam %>%
    mutate(
        outcomeGame = if_else(outcomeGame == "W", "win", "loss"),
        team_winner = factor(outcomeGame, levels = c("win","loss")),
        team_margin = plusminusTeam,
        team_loc = if_else(locationGame == "A", "away", "home"),
    ) %>%
    rename(
        game_id = idGame,
        season = yearSeason,
        season_type = typeSeason,
        game_date = dateGame,
        game_count = numberGameTeamSeason,
        team_id = idTeam,
        team_name = nameTeam,
        fg2m = fg2mTeam,
        fg2a = fg2aTeam,
        fg3m = fg3mTeam,
        fg3a = fg3aTeam,
        fgm = fgmTeam,
        fga = fgaTeam,
        ftm = ftmTeam,
        fta = ftaTeam,
        oreb = orebTeam,
        dreb = drebTeam,
        treb = trebTeam,
        ast = astTeam,
        tov = tovTeam,
        stl = stlTeam,
        blk = blkTeam,
        pf = pfTeam,
        team_score = ptsTeam,
        mins = minutesTeam,
        b2b_first = isB2BFirst,
        b2b_second = isB2BSecond
    ) %>%
    select(
        game_id, season, season_type, game_date, team_loc, team_id, team_name,
        team_winner, team_margin, b2b_first, b2b_second, game_count,
        mins, team_score,
        fg2m, fg2a, fg3m, fg3a, fgm, fga, ftm, fta,
        oreb, dreb, treb, ast, tov, stl, blk, pf, slugTeam, slugOpponent
    )

# create opponents box scores
box_scores_opp <- box_scores %>%
    select(-c(season:team_loc, mins)) %>%
    rename(opp_name = team_name,
           opp_id = team_id,
           opp_score = team_score) %>%
    rename_with(~paste0("opp_", .),
                -c(game_id, opp_name, opp_id, opp_score, slugTeam, slugOpponent))

# game by game box scores
box_scores_gbg <- box_scores %>%
    left_join(box_scores_opp, by = c("game_id" = "game_id",
                                     "slugTeam" = "slugOpponent")) %>%
    select(game_id:team_loc, team_id, opp_id, team_name, opp_name,
           team_winner, team_margin,
           b2b_first, opp_b2b_first, b2b_second, opp_b2b_second,
           game_count, opp_game_count, mins, team_score, opp_score,
           fg2m:pf, opp_fg2m:opp_pf)

# league averages
nba_league_avg <- box_scores_gbg %>%
    arrange(game_date, game_id) %>%
    group_by(season, team_loc) %>%
    summarize(across(mins:opp_pf, \(x) mean(x))) %>%
    ungroup() %>%
    mutate(
        poss = round(fga - oreb + tov + (0.44*fta), 0),
        opp_poss = round(opp_fga - opp_oreb + opp_tov + (0.44*opp_fta), 0),
        pace = round((poss + opp_poss)*48 / ((mins/5)*2), 0),
        off_rtg = round((team_score/poss)*100, 1),
        def_rtg = round((opp_score/opp_poss)*100, 1),
        net_rtg = off_rtg - def_rtg,
        fg2_pct = fg2m/fg2a,
        fg2_sr = (fga-fg2a)/fga,
        fg3_pct = fg3m/fg3a,
        fg3_sr = (fga-fg3a)/fga,
        fg_pct = fgm/fga,
        efg_pct = (fg3m*0.5 + fgm)/fga,
        ts_pct = team_score/(fga*2 + fta*0.44),
        ft_pct = ftm/fta,
        ftr = ftm/fga,
        oreb_pct = oreb/(oreb+opp_dreb),
        dreb_pct = dreb/(dreb+opp_oreb),
        treb_pct = treb/(treb+opp_treb),
        ast_pct = ast/fgm,
        tov_pct = tov/poss,
        ast_tov_pct = ast/tov,
        stl_pct = stl/opp_poss,
        blk_pct = blk/(opp_fga-opp_fg3a),
        pf_pct = pf/(poss+opp_poss),
        opp_fg2_pct = opp_fg2m/opp_fg2a,
        opp_fg2_sr = (opp_fga-opp_fg2a)/opp_fga,
        opp_fg3_pct = opp_fg3m/opp_fg3a,
        opp_fg3_sr = (opp_fga-opp_fg3a)/opp_fga,
        opp_fg_pct = opp_fgm/opp_fga,
        opp_efg_pct = (opp_fg3m*0.5 + opp_fgm)/opp_fga,
        opp_ts_pct = opp_score/(opp_fga*2 + opp_fta*0.44),
        opp_ft_pct = opp_ftm/opp_fta,
        opp_ftr = opp_ftm/opp_fga,
        opp_oreb_pct = opp_oreb/(opp_oreb+dreb),
        opp_dreb_pct = opp_dreb/(opp_dreb+oreb),
        opp_treb_pct = opp_treb/(treb+opp_treb),
        opp_ast_pct = opp_ast/opp_fgm,
        opp_tov_pct = opp_tov/opp_poss,
        opp_ast_tov_pct = opp_ast/opp_tov,
        opp_stl_pct = opp_stl/poss,
        opp_blk_pct = opp_blk/(fga-fg3a),
        opp_pf_pct = opp_pf/(poss+opp_poss)
    ) %>%
    select(
        season,team_loc,team_score,opp_score,
        fg2m,fg2a,fg2_pct,fg2_sr,fg3m,fg3a,fg3_pct,fg3_sr,
        fgm,fga,fg_pct,efg_pct,ts_pct,ftm,fta,ft_pct,ftr,oreb,oreb_pct,
        dreb,dreb_pct,treb,treb_pct,ast,ast_pct,tov,tov_pct,ast_tov_pct,
        stl,stl_pct,blk,blk_pct,pf,pf_pct,
        opp_fg2m,opp_fg2a,opp_fg2_pct,opp_fg2_sr,opp_fg3m,opp_fg3a,
        opp_fg3_pct,opp_fg3_sr,opp_fgm,opp_fga,opp_fg_pct,opp_efg_pct,
        opp_ts_pct,opp_ftm,opp_fta,opp_ft_pct,opp_ftr,opp_oreb,opp_oreb_pct,
        opp_dreb,opp_dreb_pct,opp_treb,opp_treb_pct,opp_ast,opp_ast_pct,
        opp_tov,opp_tov_pct,opp_ast_tov_pct,opp_stl,opp_stl_pct,
        opp_blk,opp_blk_pct,opp_pf,opp_pf_pct,
        poss,opp_poss,pace,off_rtg,def_rtg,net_rtg
    )

# straight average
nba_team_avg <- box_scores_gbg %>%
    arrange(game_date, game_id) %>%
    group_by(season, team_id, team_name, team_loc) %>%
    summarize(across(c(mins:opp_score, fg2m:opp_pf),
                     \(x) mean(x))) %>%
    ungroup() %>%
    mutate(
        poss = round(fga - oreb + tov + (0.44*fta), 0),
        opp_poss = round(opp_fga - opp_oreb + opp_tov + (0.44*opp_fta), 0),
        pace = round((poss + opp_poss)*48 / ((mins/5)*2), 0),
        off_rtg = round((team_score/poss)*100, 1),
        def_rtg = round((opp_score/opp_poss)*100, 1),
        net_rtg = off_rtg - def_rtg,
        fg2_pct = fg2m/fg2a,
        fg2_sr = (fga-fg2a)/fga,
        fg3_pct = fg3m/fg3a,
        fg3_sr = (fga-fg3a)/fga,
        fg_pct = fgm/fga,
        efg_pct = (fg3m*0.5 + fgm)/fga,
        ts_pct = team_score/(fga*2 + fta*0.44),
        ft_pct = ftm/fta,
        ftr = ftm/fga,
        oreb_pct = oreb/(oreb+opp_dreb),
        dreb_pct = dreb/(dreb+opp_oreb),
        treb_pct = treb/(treb+opp_treb),
        ast_pct = ast/fgm,
        tov_pct = tov/poss,
        ast_tov_pct = ast/tov,
        stl_pct = stl/opp_poss,
        blk_pct = blk/(opp_fga-opp_fg3a),
        pf_pct = pf/(poss+opp_poss),
        opp_fg2_pct = opp_fg2m/opp_fg2a,
        opp_fg2_sr = (opp_fga-opp_fg2a)/opp_fga,
        opp_fg3_pct = opp_fg3m/opp_fg3a,
        opp_fg3_sr = (opp_fga-opp_fg3a)/opp_fga,
        opp_fg_pct = opp_fgm/opp_fga,
        opp_efg_pct = (opp_fg3m*0.5 + opp_fgm)/opp_fga,
        opp_ts_pct = opp_score/(opp_fga*2 + opp_fta*0.44),
        opp_ft_pct = opp_ftm/opp_fta,
        opp_ftr = opp_ftm/opp_fga,
        opp_oreb_pct = opp_oreb/(opp_oreb+dreb),
        opp_dreb_pct = opp_dreb/(opp_dreb+oreb),
        opp_treb_pct = opp_treb/(treb+opp_treb),
        opp_ast_pct = opp_ast/opp_fgm,
        opp_tov_pct = opp_tov/opp_poss,
        opp_ast_tov_pct = opp_ast/opp_tov,
        opp_stl_pct = opp_stl/poss,
        opp_blk_pct = opp_blk/(fga-fg3a),
        opp_pf_pct = opp_pf/(poss+opp_poss),
    ) %>%
    select(
        season,team_id,team_name,team_loc,team_score,opp_score,
        fg2m,fg2a,fg2_pct,fg2_sr,fg3m,fg3a,fg3_pct,fg3_sr,
        fgm,fga,fg_pct,efg_pct,ts_pct,ftm,fta,ft_pct,ftr,oreb,oreb_pct,
        dreb,dreb_pct,treb,treb_pct,ast,ast_pct,tov,tov_pct,ast_tov_pct,
        stl,stl_pct,blk,blk_pct,pf,pf_pct,
        opp_fg2m,opp_fg2a,opp_fg2_pct,opp_fg2_sr,opp_fg3m,opp_fg3a,
        opp_fg3_pct,opp_fg3_sr,opp_fgm,opp_fga,opp_fg_pct,opp_efg_pct,
        opp_ts_pct,opp_ftm,opp_fta,opp_ft_pct,opp_ftr,opp_oreb,opp_oreb_pct,
        opp_dreb,opp_dreb_pct,opp_treb,opp_treb_pct,opp_ast,opp_ast_pct,
        opp_tov,opp_tov_pct,opp_ast_tov_pct,opp_stl,opp_stl_pct,
        opp_blk,opp_blk_pct,opp_pf,opp_pf_pct,
        poss,opp_poss,pace,off_rtg,def_rtg,net_rtg
    )

# weighted moving average
nba_wt_avg <- box_scores_gbg %>%
    arrange(game_date, game_id) %>%
    group_by(season, team_id, team_loc) %>%
    mutate(across(c(mins:opp_score, fg2m:opp_pf),
                  \(x) pracma::movavg(x, n = 10, type = 'e'))) %>%
    ungroup()

# base columns for final data frame 
nba_base <- box_scores_gbg %>%
    filter(team_loc == "away") %>%
    left_join(odds_df %>% select(-hoopr_id),
              by = c("game_id" = "statr_id")) %>%
    select(game_id:opp_score, away_spread:home_implied_prob)

# lagged away stats
nba_lag_away <- nba_wt_avg %>%
    filter(team_loc == "away") %>%
    select(game_id, team_id, mins:opp_score, fg2m:opp_pf) %>%
    mutate(
        poss = round(fga - oreb + tov + (0.44*fta), 0),
        opp_poss = round(opp_fga - opp_oreb + opp_tov + (0.44*opp_fta), 0),
        pace = round((poss + opp_poss)*48 / ((mins/5)*2), 0),
        off_rtg = round((team_score/poss)*100, 1),
        def_rtg = round((opp_score/opp_poss)*100, 1),
        net_rtg = off_rtg - def_rtg,
        fg2_pct = fg2m/fg2a,
        fg2_sr = (fga-fg2a)/fga,
        fg3_pct = fg3m/fg3a,
        fg3_sr = (fga-fg3a)/fga,
        fg_pct = fgm/fga,
        efg_pct = (fg3m*0.5 + fgm)/fga,
        ts_pct = team_score/(fga*2 + fta*0.44),
        ft_pct = ftm/fta,
        ftr = ftm/fga,
        oreb_pct = oreb/(oreb+opp_dreb),
        dreb_pct = dreb/(dreb+opp_oreb),
        treb_pct = treb/(treb+opp_treb),
        ast_pct = ast/fgm,
        tov_pct = tov/poss,
        ast_tov_pct = ast/tov,
        stl_pct = stl/opp_poss,
        blk_pct = blk/(opp_fga-opp_fg3a),
        pf_pct = pf/(poss+opp_poss),
        opp_fg2_pct = opp_fg2m/opp_fg2a,
        opp_fg2_sr = (opp_fga-opp_fg2a)/opp_fga,
        opp_fg3_pct = opp_fg3m/opp_fg3a,
        opp_fg3_sr = (opp_fga-opp_fg3a)/opp_fga,
        opp_fg_pct = opp_fgm/opp_fga,
        opp_efg_pct = (opp_fg3m*0.5 + opp_fgm)/opp_fga,
        opp_ts_pct = opp_score/(opp_fga*2 + opp_fta*0.44),
        opp_ft_pct = opp_ftm/opp_fta,
        opp_ftr = opp_ftm/opp_fga,
        opp_oreb_pct = opp_oreb/(opp_oreb+dreb),
        opp_dreb_pct = opp_dreb/(opp_dreb+oreb),
        opp_treb_pct = opp_treb/(treb+opp_treb),
        opp_ast_pct = opp_ast/opp_fgm,
        opp_tov_pct = opp_tov/opp_poss,
        opp_ast_tov_pct = opp_ast/opp_tov,
        opp_stl_pct = opp_stl/poss,
        opp_blk_pct = opp_blk/(fga-fg3a),
        opp_pf_pct = opp_pf/(poss+opp_poss),
    ) %>%
    select(
        game_id,team_id,fg2m,fg2a,fg2_pct,fg2_sr,fg3m,fg3a,fg3_pct,fg3_sr,
        fgm,fga,fg_pct,efg_pct,ts_pct,ftm,fta,ft_pct,ftr,oreb,oreb_pct,
        dreb,dreb_pct,treb,treb_pct,ast,ast_pct,tov,tov_pct,ast_tov_pct,
        stl,stl_pct,blk,blk_pct,pf,pf_pct,
        opp_fg2m,opp_fg2a,opp_fg2_pct,opp_fg2_sr,opp_fg3m,opp_fg3a,
        opp_fg3_pct,opp_fg3_sr,opp_fgm,opp_fga,opp_fg_pct,opp_efg_pct,
        opp_ts_pct,opp_ftm,opp_fta,opp_ft_pct,opp_ftr,opp_oreb,opp_oreb_pct,
        opp_dreb,opp_dreb_pct,opp_treb,opp_treb_pct,opp_ast,opp_ast_pct,
        opp_tov,opp_tov_pct,opp_ast_tov_pct,opp_stl,opp_stl_pct,
        opp_blk,opp_blk_pct,opp_pf,opp_pf_pct,
        poss,opp_poss,pace,off_rtg,def_rtg,net_rtg
    ) %>%
    rename_with(~paste0("away_", .), -c(game_id, team_id)) %>%
    group_by(team_id) %>%
    mutate(across(away_fg2m:away_net_rtg, \(x) lag(x, n = 1))) %>%
    ungroup()

# lagged home stats
nba_lag_home <- nba_wt_avg %>%
    filter(team_loc == "home") %>%
    select(game_id, team_id, mins:opp_score, fg2m:opp_pf) %>%
    mutate(
        poss = round(fga - oreb + tov + (0.44*fta), 0),
        opp_poss = round(opp_fga - opp_oreb + opp_tov + (0.44*opp_fta), 0),
        pace = round((poss + opp_poss)*48 / ((mins/5)*2), 0),
        off_rtg = round((team_score/poss)*100, 1),
        def_rtg = round((opp_score/opp_poss)*100, 1),
        net_rtg = off_rtg - def_rtg,
        fg2_pct = fg2m/fg2a,
        fg2_sr = (fga-fg2a)/fga,
        fg3_pct = fg3m/fg3a,
        fg3_sr = (fga-fg3a)/fga,
        fg_pct = fgm/fga,
        efg_pct = (fg3m*0.5 + fgm)/fga,
        ts_pct = team_score/(fga*2 + fta*0.44),
        ft_pct = ftm/fta,
        ftr = ftm/fga,
        oreb_pct = oreb/(oreb+opp_dreb),
        dreb_pct = dreb/(dreb+opp_oreb),
        treb_pct = treb/(treb+opp_treb),
        ast_pct = ast/fgm,
        tov_pct = tov/poss,
        ast_tov_pct = ast/tov,
        stl_pct = stl/opp_poss,
        blk_pct = blk/(opp_fga-opp_fg3a),
        pf_pct = pf/(poss+opp_poss),
        opp_fg2_pct = opp_fg2m/opp_fg2a,
        opp_fg2_sr = (opp_fga-opp_fg2a)/opp_fga,
        opp_fg3_pct = opp_fg3m/opp_fg3a,
        opp_fg3_sr = (opp_fga-opp_fg3a)/opp_fga,
        opp_fg_pct = opp_fgm/opp_fga,
        opp_efg_pct = (opp_fg3m*0.5 + opp_fgm)/opp_fga,
        opp_ts_pct = opp_score/(opp_fga*2 + opp_fta*0.44),
        opp_ft_pct = opp_ftm/opp_fta,
        opp_ftr = opp_ftm/opp_fga,
        opp_oreb_pct = opp_oreb/(opp_oreb+dreb),
        opp_dreb_pct = opp_dreb/(opp_dreb+oreb),
        opp_treb_pct = opp_treb/(treb+opp_treb),
        opp_ast_pct = opp_ast/opp_fgm,
        opp_tov_pct = opp_tov/opp_poss,
        opp_ast_tov_pct = opp_ast/opp_tov,
        opp_stl_pct = opp_stl/poss,
        opp_blk_pct = opp_blk/(fga-fg3a),
        opp_pf_pct = opp_pf/(poss+opp_poss),
    ) %>%
    select(
        game_id,team_id,fg2m,fg2a,fg2_pct,fg2_sr,fg3m,fg3a,fg3_pct,fg3_sr,
        fgm,fga,fg_pct,efg_pct,ts_pct,ftm,fta,ft_pct,ftr,oreb,oreb_pct,
        dreb,dreb_pct,treb,treb_pct,ast,ast_pct,tov,tov_pct,ast_tov_pct,
        stl,stl_pct,blk,blk_pct,pf,pf_pct,
        opp_fg2m,opp_fg2a,opp_fg2_pct,opp_fg2_sr,opp_fg3m,opp_fg3a,
        opp_fg3_pct,opp_fg3_sr,opp_fgm,opp_fga,opp_fg_pct,opp_efg_pct,
        opp_ts_pct,opp_ftm,opp_fta,opp_ft_pct,opp_ftr,opp_oreb,opp_oreb_pct,
        opp_dreb,opp_dreb_pct,opp_treb,opp_treb_pct,opp_ast,opp_ast_pct,
        opp_tov,opp_tov_pct,opp_ast_tov_pct,opp_stl,opp_stl_pct,
        opp_blk,opp_blk_pct,opp_pf,opp_pf_pct,
        poss,opp_poss,pace,off_rtg,def_rtg,net_rtg
    ) %>%
    rename_with(~paste0("home_", .), -c(game_id, team_id)) %>%
    group_by(team_id) %>%
    mutate(across(home_fg2m:home_net_rtg, \(x) lag(x, n = 1))) %>%
    ungroup()

# final data frame
nba_final <- nba_base %>%
    left_join(nba_lag_away, by = c("game_id" = "game_id",
                                   "team_id" = "team_id")) %>%
    left_join(nba_lag_home, by = c("game_id" = "game_id",
                                   "opp_id" = "team_id")) %>%
    na.exclude() %>%
    select(-c(game_count:mins)) %>%
    arrange(game_date, game_id)


#### game details ####

standings_raw <- standings(seasons = 2023, season_types = c("Regular Season"))

standings <- standings_raw %>%
    select("nameTeam","nameConference","nameDivison","recordOverall",
           "pctWinTeam","recordLast10","slugStreakCurrent",
           "recordAway","recordAwayWinPct",
           "recordLast10Away","slugStreakAwayCurrent",
           "recordHome","recordHomeWinPct",
           "recordLast10Home","slugStreakHomeCurrent",
           "rankPlayoffs")

colnames(standings) <- c("team","conference","division","record",
                         "win_pct","last_10","streak",
                         "away_record","away_win_pct",
                         "away_last_10","away_steak",
                         "home_record", "home_win_pct",
                         "home_last_10","home_streak",
                         "conference_rank")


#### rankings ####

asc_cols <- names(nba_team_avg %>%
                      select(tov,tov_pct,
                             opp_fg2m:opp_ast_pct,opp_ast_tov_pct:opp_pf_pct))

desc_cols <- names(nba_team_avg %>%
                       select(fg2m:ast_pct,ast_tov_pct:pf_pct,
                              opp_tov:opp_tov_pct))

rankings <- nba_team_avg %>%
    group_by(season, team_loc) %>%
    mutate(across(all_of(asc_cols),
                  list(rank = ~rank(., ties.method = "min"))),
           across(all_of(desc_cols),
                  list(rank = ~rank(-., ties.method = "min"))))


#### clean environment ####

rm(list=ls()[! ls() %in% c("box_scores_gbg", "nba_final",
                           "nba_league_avg", "nba_team_avg",
                           "standings", "rankings")])









nba_schedule_current <- tbl(dbConnect(SQLite(), "../nba_sql_db/nba_db"),
                            "nba_schedule_current") %>%
    collect() %>%
    mutate(game_date = as_date(game_date, origin ="1970-01-01"))



generate_headers <- function() {
    headers <- c(
        `Sec-Fetch-Site` = "same-site",
        `Accept` = "*/*",
        `Origin` = "https://www.nba.com",
        `Sec-Fetch-Dest` = "empty",
        `Accept-Language` = "en-US,en;q=0.9",
        `Sec-Fetch-Mode` = "cors",
        `Host` = "stats.nba.com",
        `User-Agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.0 Safari/605.1.15",
        `Referer` = "https://www.nba.com/",
        `Accept-Encoding` = "gzip, deflate, br",
        `Connection` = "keep-alive"
    )
    return(headers)
}
generate_parameters <- function(year, measure_type) {
    year <- (year - 1)
    season <- sprintf("%d-%02d", year, (year + 1) %% 100)
    params <- list(
        `DateFrom` = "",
        `DateTo` = "",
        `GameSegment` = "",
        `ISTRound` = "",
        `LastNGames` = "0",
        `LeagueID` = "00",
        `Location` = "",
        `MeasureType` = measure_type,
        `Month` = "0",
        `OpponentTeamID` = "0",
        `Outcome` = "",
        `PORound` = "0",
        `PaceAdjust` = "N",
        `PerMode` = "Totals",
        `Period` = "0",
        `PlusMinus` = "N",
        `Rank` = "N",
        `Season` = season,
        `SeasonSegment` = "",
        `SeasonType` = "Regular Season",
        `ShotClockRange` = "",
        `VsConference` = "",
        `VsDivision` = ""
    )
    return(params)
}

mamba_nba_cleanR <- function(seasons) {
    headers <- generate_headers()
    all_data_list <- list()
    
    # Define available measure types
    available_measure_types <- c("Base", "Advanced", "Four Factors", "Misc", "Scoring")
    
    for (measure_type in available_measure_types) {
        all_data <- data.frame()
        
        for (year in seasons) {
            params <- generate_parameters(year, measure_type)
            
            res <- httr::GET(url = "https://stats.nba.com/stats/teamgamelogs",
                             httr::add_headers(.headers = headers), query = params)
            data <- httr::content(res) %>% .[['resultSets']] %>% .[[1]]
            column_names <- data$headers %>% as.character()
            dt <- rbindlist(data$rowSet) %>% setnames(column_names)
            
            all_data <- bind_rows(all_data, dt)
            
            print(paste0(params$Season, " ", params$MeasureType))
        }
        
        all_data_list[[measure_type]] <- all_data
    }
    
    # Data transformation code
    cleaned_team_list <- lapply(all_data_list, function(df) {
        df <- clean_names(df)
        return(df)
    })
    
    combined_df <- cleaned_team_list[[1]]
    for (i in 2:length(cleaned_team_list)) {
        df_to_join <- cleaned_team_list[[i]]
        existing_cols <- intersect(names(combined_df), names(df_to_join))
        existing_cols <- setdiff(existing_cols, c("game_id", "team_name"))
        df_to_join <- df_to_join %>% select(-any_of(existing_cols))
        combined_df <- left_join(combined_df, df_to_join, by = c("game_id", "team_name"))
    }
    
    team_all_stats <- combined_df %>%
        select(-starts_with(c("e_","opp_")), -ends_with(c("_rank","_flag"))) %>%
        arrange(game_date, game_id) %>%
        mutate(location = if_else(grepl("@", matchup) == T, "away", "home"),
               game_date = as_date(game_date),
               season_year = as.numeric(substr(season_year, 1, 4)) + 1) %>%
        select(season_year:matchup, location, wl:pct_uast_fgm)
    
    team_games <- team_all_stats %>%
        distinct(season_year, game_id, game_date, team_id, team_name) %>%
        group_by(season_year, team_id) %>%
        mutate(
            game_count_season = 1:n(),
            days_rest_team = ifelse(game_count_season > 1,
                                    (game_date - lag(game_date) - 1),
                                    120),
            days_next_game_team =
                ifelse(game_count_season < 82,
                       ((
                           lead(game_date) - game_date
                       ) - 1),
                       120),
            days_next_game_team = days_next_game_team %>% as.numeric(),
            days_rest_team = days_rest_team %>% as.numeric(),
            is_b2b = if_else(days_next_game_team == 0 |
                                 days_rest_team == 0, TRUE, FALSE),
            is_b2b_first = if_else(lead(days_next_game_team) == 0, TRUE, FALSE),
            is_b2b_second = if_else(lag(days_next_game_team) == 0, TRUE, FALSE)
        ) %>%
        ungroup() %>%
        mutate_if(is.logical, ~ ifelse(is.na(.), FALSE, .)) %>%
        select(game_id, team_name,
               is_b2b_first, is_b2b_second, game_count_season)

    opp_all_stats <- team_all_stats %>%
        select(game_id, team_name, fgm:pct_uast_fgm) %>%
        rename_with(~paste0("opp_", .), -c(game_id)) %>%
        select(-opp_plus_minus)
    
    min_games <- min(team_games$game_count_season)
    
    nba_final <- team_all_stats %>%
        inner_join(opp_all_stats, by = c("game_id"), relationship = "many-to-many") %>%
        filter(team_name != opp_team_name) %>%
        select(season_year, team_name, location, fgm:opp_pct_uast_fgm) %>%
        group_by(season_year, team_name, location) %>%
        mutate(across(c(fgm:opp_pct_uast_fgm),
                      ~ if (min_games >= 10) {
                          pracma::movavg(.x, n = 10, type = 'e')
                      } else if (min_games > 2) {
                          pracma::movavg(.x, n = min_games, type = 'e')
                      } else {
                          mean(.x)
                      })
        ) %>%
        slice(n()) %>%
        ungroup() %>%
        select(season_year, team_name, location,
               fgm:pct_uast_fgm, opp_fgm:opp_pct_uast_fgm)
    
    return(nba_final)
}

nba_final <- mamba_nba_cleanR(2024)

log_win <- read_rds("../NBAdb/models/log_win_2023.rds")

slate <- nba_schedule_current %>%
    filter(game_date == Sys.Date()) %>%
    left_join(final_odds)

df_away <- nba_final %>%
    filter(location == "away" & team_name %in% slate$away_team_name) %>%
    select(-c(season_year, location)) %>%
    rename_with(~paste0("away_", .), -team_name)

df_home <- nba_final %>%
    filter(location == "home" & team_name %in% slate$home_team_name) %>%
    select(-c(season_year, location)) %>%
    rename_with(~paste0("home_", .), -team_name)

df_slate <- slate %>%
    left_join(df_away, by = c("away_team_name" = "team_name")) %>%
    left_join(df_home, by = c("home_team_name" = "team_name")) %>%
    select(away_team_name, all_of(log_win$coefnames))





games_today <- df_slate[-1]

win_pred <- predict(log_win, games_today, type = "prob")

bets <- slate %>%
    select(game_id, game_date, away_team_name, home_team_name,
           away_spread:over_under) %>%
    mutate(away_pred = win_pred$win,
           home_pred = win_pred$loss,
           away_edge = away_pred - round((if_else(away_moneyline<0,
                                      ((away_moneyline*-1)/((away_moneyline*-1)+100)),
                                      (100/(away_moneyline+100)))),3),
           home_edge = home_pred - round((if_else(home_moneyline<0,
                                      ((home_moneyline*-1)/((home_moneyline*-1)+100)),
                                      (100/(home_moneyline+100)))),3))

bets_screen <- bets %>% select(game_date, away_team_name, home_team_name,
                               away_edge, home_edge)

    



