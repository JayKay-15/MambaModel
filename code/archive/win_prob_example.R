# 1. https://github.com/anpatton/basic-nba-tutorials/blob/main/win_probability/make_win_probability_model.md
library(tidyverse)
library(arrow)
library(knitr)
set.seed(42)

#### Data & Formatting ----
pbp <- arrow::read_parquet("/Users/Jesse/Desktop/pbp.parquet") %>% 
    filter(is.na(nameTeam) == FALSE) %>% 
    filter(is.na(eventGeneral) == FALSE) %>% 
    replace_na(list(shotResultPoints = 0)) %>% 
    mutate(home_points_scored = ifelse(nameTeam == homeTeam, shotResultPoints, 0)) %>% 
    
    mutate(away_points_scored = ifelse(nameTeam == awayTeam, shotResultPoints, 0)) %>% 
    mutate(home_foul = ifelse(nameTeam == homeTeam & grepl("foul", .$eventType), 1, 0)) %>%
    mutate(away_foul = ifelse(nameTeam == awayTeam & grepl("foul", .$eventType), 1, 0)) %>%
    mutate(time_remaining = 2880 - gametime) %>% 
    group_by(idGame) %>% 
    mutate(home_score = cumsum(home_points_scored)) %>% 
    mutate(away_score = cumsum(away_points_scored)) %>% 
    mutate(home_margin = home_score - away_score) %>% 
    mutate(home_foul_totals = cumsum(home_foul)) %>%
    mutate(away_foul_totals = cumsum(away_foul)) %>%
    mutate(home_win = ifelse(max(home_score) > max(away_score), 1, 0)) %>% 
    ungroup() %>% 
    select(idGame, time_remaining, home_score, away_score, home_margin, 
           home_foul_totals, away_foul_totals, home_win) 

kable(pbp[3:12, -1], align = "c")

### Logistic Model ----
games <- unique(pbp$idGame) ## get all the unique games (there are 500 of them)

train_games <- sample(games, 0.8 * length(games)) ## select 400 games for our training set

train <- pbp %>% 
    filter(idGame %in% train_games) 

test <- pbp %>% 
    filter(!idGame %in% train_games) 

mod <- glm(home_win ~ ., data = select(train, -idGame, -home_margin), family = "binomial")

summary(mod)

### Evaluate Model ----
train_predictions <- as.numeric(predict(mod, train, type = "response"))
train_predictions_01 <- round(train_predictions, 0)
train_accuracy <- Metrics::accuracy(train$home_win, train_predictions_01) 

test_predictions <- as.numeric(predict(mod, test, type = "response"))
test_predictions_01 <- round(test_predictions, 0)
test_accuracy <- Metrics::accuracy(test$home_win, test_predictions_01) 

print(paste0("Training Accuracy %: ", round(100 * train_accuracy, 1)))

print(paste0("Testing Accuracy %: ", round(100 * test_accuracy, 1)))


### XGB Model ----
library(splitTools)

games <- unique(pbp$idGame) 

train_games <- sample(games, 0.8 * length(games))

train <- pbp %>% 
    filter(idGame %in% train_games)

test <- pbp %>% 
    filter(!idGame %in% train_games)

x_train <- train %>% 
    select(-idGame, -home_win) %>% 
    as.matrix() ## xgboost necessary step, although you can use other ways

y_train <- as.numeric(train$home_win)

x_test <- test %>% 
    select(-idGame, -home_win) %>% 
    as.matrix() ## xgboost necessary step, although you can use other ways

y_test <- test$home_win

#grouped_folds <-  map(0:4, function(x) { ## this was 100% written by Ben Baldwin and is a manual splitter
#  fold_indices <- which(train$idGame %in% games[(1 + 80 * x) : (80 + (x * 80))])
#  return(fold_indices)
#  })

grouped_folds <- splitTools::create_folds(train$idGame, k = 5, type = "grouped", invert = TRUE)

# tuning
param_grid = list()

for(i in 1:5){
    
    param_grid[[i]] <- list(booster = "gbtree",
                            objective = "binary:logistic",
                            max_depth = sample(c(3:10), 1),
                            eta = runif(1, .01, .3),
                            subsample = runif(1, .7, 1),
                            colsample_bytree = runif(1, .6, 1),
                            min_child_weight = sample(0:10, 1))
    
}

# xgb model
par_xgboost <- function(params, data, labels, folds) {
    
    xgbcv <- xgboost::xgb.cv(params = params, 
                             data = x_train, 
                             label = y_train,
                             nrounds = 1000, ## somewhat arbitrary, can let early stopping solve this
                             folds = folds,
                             early_stop_round = 5,
                             eval_metric = "error")
    
    test_error <- xgbcv$evaluation_log %>% 
        filter(test_error_mean == min(test_error_mean)) %>% 
        pull(test_error_mean)
    
    return(list(test_error, params, xgbcv$niter))
    
}

cores_minus_one <- parallel::detectCores() - 1 

cl <- parallel::makeCluster(spec = cores_minus_one) 

parallel::clusterExport(cl, c("par_xgboost", "x_train", "y_train", "grouped_folds"))

parallel::clusterEvalQ(cl, {
    library(dplyr)
    library(xgboost)
})

res <- parallel::parLapply(cl = cl, param_grid, 
                           function(x) par_xgboost(params = x, data = x_train, labels = y_train, folds = grouped_folds))

parallel::stopCluster(cl) 

# refit and evaluate
best_error_index <- which.min(unlist(sapply(res, function(x) unique(x[[1]]))))
best_error <- res[[best_error_index]][[1]] 
best_param_set <- res[[best_error_index]][[2]] 
best_num_iterations <- res[[best_error_index]][[3]]

xgb_tuned <- xgboost::xgboost(params = best_param_set,
                              data = x_train,
                              label = y_train,
                              nrounds = best_num_iterations,
                              print_every_n = 10,
                              eval_metric = "error",
                              early_stopping_rounds = 5)

preds <- predict(xgb_tuned, x_test)
preds_01 <- round(preds, 0)

test <- test %>% 
    mutate(home_win_prob = preds) %>% 
    mutate(home_win_pred = preds_01)

print(paste0("Training Error %: ", round(100 * best_error, 1)))

print(paste0("Testing Error %: ", round(100 * Metrics::accuracy(y_test, !preds_01), 1)))

# win prob chart
random_game <- test$idGame[[15]]

plot_data <- test %>% 
    filter(idGame == random_game) %>% 
    mutate(time_remaining = time_remaining/60) %>% 
    mutate(home_win_prob = ifelse(time_remaining == 0 & home_margin > 0, 1,
                                  ifelse(time_remaining == 0 & home_margin < 0, 0, home_win_prob)))

score_labels <- plot_data[seq(1, nrow(plot_data), 20), ] %>% 
    select(time_remaining, home_margin)

ggplot(data = plot_data, aes(x = time_remaining, y = home_win_prob)) +
    geom_line() +
    scale_x_reverse(limits = c(48, 0), breaks = seq(0, 48, 6)) +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1)) +
    geom_text(data = score_labels, aes(x = time_remaining, y = 0.1, label = home_margin), color = "darkblue") +
    labs(title = "Ugly Win Probability Chart",
         x = "Minutes Remaining",
         y = "Home Win Probability") +
    theme_bw()