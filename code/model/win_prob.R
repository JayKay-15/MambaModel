### Win Probability Model ----

library(tidyverse)
library(lubridate)
library(caTools)
library(caret)
library(tidymodels)
# pbp <- pbp22 %>% select(-c(locX:locY)) %>% filter(game_id == 22100001)
# write.csv(pbp, "/Users/Jesse/Desktop/pbp.csv")
# pbp <- read.csv(file = "/Users/Jesse/Desktop/pbp.csv")

gl_adj <- dplyr::tbl(DBI::dbConnect(RSQLite::SQLite(),
                                "/Users/Jesse/Documents/MyStuff/NBA Betting/NBAdb/NBAdb.sqlite"),
                 "GameLogsAdj") %>% collect() %>% mutate(date = as_date(date, origin ="1970-01-01"))

gm_ids <- dataGameLogsTeam %>%
    select(dateGame,idGame,nameTeam)

gl_adj <- gl_adj %>%
    left_join(gm_ids, by = c("date" = "dateGame", "away" = "nameTeam"))


pbp22 <- readRDS("/Users/Jesse/Documents/MyStuff/NBA Analysis/NBA in R/NBA-in-R-master20221202/2022_23/regseason/pbp/pbp-poss-rs23/data.rds")
pbp21 <- readRDS("/Users/Jesse/Documents/MyStuff/NBA Analysis/NBA in R/NBA-in-R-master20221202/2021_22/regseason/pbp/pbp-poss-rs22/data.rds")
pbp20 <- readRDS("/Users/Jesse/Documents/MyStuff/NBA Analysis/NBA in R/NBA-in-R-master20221202/pbp-final-gt21/data.rds")
pbp19 <- readRDS("/Users/Jesse/Documents/MyStuff/NBA Analysis/NBA in R/NBA-in-R-master20221202/pbp-final-gt20/data.rds")


pbp22 <- pbp22 %>%
    select(game_date, game_id, msg_type, act_type, slug_team, player1, player2, player3,
           desc_value, hs, vs, team_home, team_away, secs_passed_game, shot_pts, poss_home, poss_away) %>%
    mutate(margin_away = vs - hs)
pbp21 <- pbp21 %>%
    select(game_date, game_id, msg_type, act_type, slug_team, player1, player2, player3,
           desc_value, hs, vs, team_home, team_away, secs_passed_game, shot_pts, poss_home, poss_away) %>%
    mutate(margin_away = vs - hs)
pbp20 <- pbp20 %>%
    select(game_date, game_id, msg_type, act_type, slug_team, player1, player2, player3,
           desc_value, hs, vs, team_home, team_away, secs_passed_game, shot_pts, poss_home, poss_away) %>%
    mutate(margin_away = vs - hs)
pbp19 <- pbp19 %>%
    select(game_date, game_id, msg_type, act_type, slug_team, player1, player2, player3,
           desc_value, hs, vs, team_home, team_away, secs_passed_game, shot_pts, poss_home, poss_away) %>%
    mutate(margin_away = vs - hs)

pbp <- rbind(pbp22, pbp21, pbp20, pbp19)

wl <- pbp %>%
    group_by(across(game_id)) %>%
    summarise(vs = max(vs),
              hs = max(hs)) %>%
    mutate(result = ifelse(vs > hs, "W", "L"))

pbp_df <- pbp %>%
    group_by(across(game_id)) %>%
    mutate(away_fga_dummy = ifelse((msg_type==1 | msg_type==2) & slug_team==team_away, 1, 0),
           home_fga_dummy = ifelse((msg_type==1 | msg_type==2) & slug_team==team_home, 1, 0),
           away_fgm_dummy = ifelse(msg_type==1 & slug_team==team_away, 1, 0),
           home_fgm_dummy = ifelse(msg_type==1 & slug_team==team_home, 1, 0),
           away_fga2_dummy = ifelse((msg_type==1 | msg_type==2) & desc_value==2 & slug_team==team_away, 1, 0),
           home_fga2_dummy = ifelse((msg_type==1 | msg_type==2) & desc_value==2 & slug_team==team_home, 1, 0),
           away_fgm2_dummy = ifelse(msg_type==1 & shot_pts==2 & slug_team==team_away, 1, 0),
           home_fgm2_dummy = ifelse(msg_type==1 & shot_pts==2 & slug_team==team_home, 1, 0),
           away_fga3_dummy = ifelse((msg_type==1 | msg_type==2) & desc_value==3 & slug_team==team_away, 1, 0),
           home_fga3_dummy = ifelse((msg_type==1 | msg_type==2) & desc_value==3 & slug_team==team_home, 1, 0),
           away_fgm3_dummy = ifelse(msg_type==1 & shot_pts==3 & slug_team==team_away, 1, 0),
           home_fgm3_dummy = ifelse(msg_type==1 & shot_pts==3 & slug_team==team_home, 1, 0),
           away_fta_dummy = ifelse(msg_type==3 & slug_team==team_away, 1, 0),
           home_fta_dummy = ifelse(msg_type==3 & slug_team==team_home, 1, 0),
           away_ftm_dummy = ifelse(msg_type==3 & shot_pts==1 & slug_team==team_away, 1, 0),
           home_ftm_dummy = ifelse(msg_type==3 & shot_pts==1 & slug_team==team_home, 1, 0),
           away_orb_dummy = ifelse(msg_type==4 & desc_value==1 & slug_team==team_away, 1, 0),
           home_orb_dummy = ifelse(msg_type==4 & desc_value==1 & slug_team==team_home, 1, 0),
           away_drb_dummy = ifelse(msg_type==4 & desc_value==0 & slug_team==team_away, 1, 0),
           home_drb_dummy = ifelse(msg_type==4 & desc_value==0 & slug_team==team_home, 1, 0),
           away_ast_dummy = ifelse(msg_type==1 & !is.na(player2) & slug_team==team_away, 1, 0),
           home_ast_dummy = ifelse(msg_type==1 & !is.na(player2) & slug_team==team_home, 1, 0),
           away_tov_dummy = ifelse(msg_type==5 & slug_team==team_away, 1, 0),
           home_tov_dummy = ifelse(msg_type==5 & slug_team==team_home, 1, 0),
           away_stl_dummy = ifelse(msg_type==5 & !is.na(player3) & slug_team==team_home, 1, 0),
           home_stl_dummy = ifelse(msg_type==5 & !is.na(player3) & slug_team==team_away, 1, 0),
           away_blk_dummy = ifelse(msg_type==2 & !is.na(player3) & slug_team==team_home, 1, 0),
           home_blk_dummy = ifelse(msg_type==2 & !is.na(player3) & slug_team==team_away, 1, 0),
           away_pf_dummy = ifelse(msg_type==6 & slug_team==team_away, 1, 0),
           home_pf_dummy = ifelse(msg_type==6 & slug_team==team_home, 1, 0),
           away_poss_cume = cumsum(poss_away),
           home_poss_cume = cumsum(poss_home),
           away_fga_cume = cumsum(away_fga_dummy),
           home_fga_cume = cumsum(home_fga_dummy),
           away_fgm_cume = cumsum(away_fgm_dummy),
           home_fgm_cume = cumsum(home_fgm_dummy),
           away_fga2_cume = cumsum(away_fga2_dummy),
           home_fga2_cume = cumsum(home_fga2_dummy),
           away_fgm2_cume = cumsum(away_fgm2_dummy),
           home_fgm2_cume = cumsum(home_fgm2_dummy),
           away_fga3_cume = cumsum(away_fga3_dummy),
           home_fga3_cume = cumsum(home_fga3_dummy),
           away_fgm3_cume = cumsum(away_fgm3_dummy),
           home_fgm3_cume = cumsum(home_fgm3_dummy),
           away_fta_cume = cumsum(away_fta_dummy),
           home_fta_cume = cumsum(home_fta_dummy),
           away_ftm_cume = cumsum(away_ftm_dummy),
           home_ftm_cume = cumsum(home_ftm_dummy),
           away_orb_cume = cumsum(away_orb_dummy),
           home_orb_cume = cumsum(home_orb_dummy),
           away_drb_cume = cumsum(away_drb_dummy),
           home_drb_cume = cumsum(home_drb_dummy),
           away_ast_cume = cumsum(away_ast_dummy),
           home_ast_cume = cumsum(home_ast_dummy),
           away_tov_cume = cumsum(away_tov_dummy),
           home_tov_cume = cumsum(home_tov_dummy),
           away_stl_cume = cumsum(away_stl_dummy),
           home_stl_cume = cumsum(home_stl_dummy),
           away_blk_cume = cumsum(away_blk_dummy),
           home_blk_cume = cumsum(home_blk_dummy),
           away_pf_cume = cumsum(away_pf_dummy),
           home_pf_cume = cumsum(home_pf_dummy),
           away_ortg = (vs/away_poss_cume)*100,
           home_ortg = (hs/home_poss_cume)*100,
           away_sr2 = away_fga2_cume/away_fga_cume,
           home_sr2 = home_fga2_cume/home_fga_cume,
           away_sr3 = away_fga3_cume/away_fga_cume,
           home_sr3 = home_fga3_cume/home_fga_cume,
           away_ftr = away_fta_cume/away_fga_cume,
           home_ftr = home_fta_cume/home_fga_cume,
           away_orb = away_orb_cume/(away_orb_cume+home_drb_cume),
           home_orb = home_orb_cume/(home_orb_cume+away_drb_cume),
           away_drb = away_drb_cume/(away_drb_cume+home_orb_cume),
           home_drb = home_drb_cume/(home_drb_cume+away_orb_cume),
           away_ast = away_ast_cume/away_fgm_cume,
           home_ast = home_ast_cume/home_fgm_cume,
           away_ast_tov = away_ast_cume/away_tov_cume,
           home_ast_tov = home_ast_cume/home_tov_cume,
           away_tov = away_tov_cume/away_poss_cume,
           home_tov = home_tov_cume/home_poss_cume,
           away_stl = away_stl_cume/home_poss_cume,
           home_stl = home_stl_cume/away_poss_cume,
           away_blk = away_blk_cume/(away_fga_cume-away_fga3_cume),
           home_blk = home_blk_cume/(home_fga_cume-home_fga3_cume),
           away_pf = away_pf_cume/home_poss_cume,
           home_pf = home_pf_cume/away_poss_cume,
           away_efg = (away_fgm_cume + .5 * away_fgm3_cume) / away_fga_cume,
           home_efg = (home_fgm_cume + .5 * home_fgm3_cume) / home_fga_cume,
           away_ts = vs / (2 * away_fga_cume + .44 * away_fta_cume),
           home_ts = hs / (2 * home_fga_cume + .44 * home_fta_cume)
           ) %>%
    filter(msg_type %in% c(1,2,3,4,5,6)) %>%
    select(1:18, 49:80) %>%
    # replace(is.na(.), 0) %>%
    left_join(wl[,c(1,4)], by = "game_id")
    # filter(game_id == 22200001)

rm(pbp19, pbp20, pbp21, pbp22, pbp)

gl_adj2 <- gl_adj %>%
    select(80,10:79)


pbp_df2 <- pbp_df %>%
    left_join(gl_adj2, by = c("game_id" = "idGame"))

# set.seed(214)
# games <- unique(pbp_df$game_id) 
# train_games <- sample(games, 0.7 * length(games))
# 
# train <- pbp_df %>% filter(game_id %in% train_games) %>% select(2,4,3,40,5:39) %>% as.data.frame()
# test <- pbp_df %>% filter(!game_id %in% train_games) %>% select(2,4,3,40,5:39) %>% as.data.frame()
# 
# train <- pbp_df %>% filter(game_id %in% train_games) %>% select(2,4,3,40,5:7,38,39) %>% as.data.frame()
# test <- pbp_df %>% filter(!game_id %in% train_games) %>% select(2,4,3,40,5:7,38,39) %>% as.data.frame()

train <- pbp_df %>% filter(game_date < '2022-10-01') %>% select(2,13,12,11,10,18,14,19:51) %>% as.data.frame()
test <- pbp_df %>% filter(game_date > '2022-10-01') %>% select(2,13,12,11,10,18,14,19:51) %>% as.data.frame()

train <- pbp_df2 %>% filter(game_date < '2022-10-01') %>% select(2,13,12,11,10,18,14,19:121) %>% as.data.frame()
test <- pbp_df2 %>% filter(game_date > '2022-10-01') %>% select(2,13,12,11,10,18,14,19:121) %>% as.data.frame()

train <- train[!is.na(train$FG_away), ]
test <- test[!is.na(test$FG_away), ]

# pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))
# train[,-1] = predict(pre_proc_val, train[,-1])
# test[,-1] = predict(pre_proc_val, test[,-1])

# Model
ctrl <- trainControl(method = "cv", number = 5, verboseIter = T, 
                     classProbs = T, summaryFunction = twoClassSummary)

log_win <- train(as.factor(result) ~. -game_id -team_away -team_home, data = train,
                 trControl = ctrl,
                 method = "glm",
                 metric = "ROC",
                 family = "binomial")

# log_win <- train(as.factor(result) ~ vs + hs + away_pf_cume + home_pf_cume, data = train,
#                  trControl = ctrl,
#                  method = "glm",
#                  metric = "ROC",
#                  family = "binomial")

log_win
log_win$resample
log_win$results
summary(log_win) # Model Components
confusionMatrix(log_win) # Confusion Matrix
glance(log_win$finalModel) # Entire Model
tidy(log_win$finalModel) # Model Components
augment(log_win$finalModel) # Observations

win_pred <- predict(log_win, test, type = "prob")
confusionMatrix(as.factor(test$result), as.factor(ifelse(win_pred[,2] > 0.5, "W", "L")),
                positive = "W")

L <- as.numeric(win_pred[,1])
W <- as.numeric(win_pred[,2])
obs <- factor(test$result)
pred <- factor(ifelse(W > 0.5, "W", "L"))
obs_pred <- data.frame(obs = obs,
                       pred = pred,
                       L = L, 
                       W = W,
                       act = ifelse(obs == "W", 1, 0))

twoClassSummary(obs_pred, lev = levels(obs)) # ROC
prSummary(obs_pred, lev = levels(obs)) # AUC

InformationValue::plotROC(obs_pred$act, obs_pred$W, returnSensitivityMat = T)

obs_pred %>%
    metrics(obs, pred)
obs_pred %>%
    roc_auc(obs, L)

log_win_imp <- rownames_to_column(varImp(log_win)[["importance"]], "Var") %>%
    arrange(desc(Overall)) %>%
    head()


pbp %>%
    filter(game_id == 22200334) %>%
    select(team_away, team_home, vs, hs) %>%
    group_by(team_away, team_home) %>%
    summarise(vs = max(vs),
              hs = max(hs))

plot_data <- test %>% 
    bind_cols(win_pred) %>%
    filter(game_id == 22200334) %>% 
    mutate(secs_passed_game = secs_passed_game/60,
           away_margin = vs-hs,
           away_win_prob = ifelse(secs_passed_game == max(secs_passed_game) & away_margin > 0, 1,
                              ifelse(secs_passed_game == max(secs_passed_game) & away_margin < 0, 0, W)))


score_labels <- plot_data[seq(1, nrow(plot_data), 20), ] %>%
    select(secs_passed_game, away_margin)


ggplot(data = plot_data, aes(x = secs_passed_game, y = away_win_prob)) +
    geom_line() +
    scale_x_continuous(limits = c(0,48), breaks = seq(0, 48, 6)) +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1)) +
    geom_text(data = score_labels, aes(x = secs_passed_game, y = 0.1, label = away_margin), color = "darkblue") +
    labs(title = "Ugly Win Probability Chart",
         x = "Minutes",
         y = "Away Win Probability") +
    theme_bw()
