### model tuning ----  

library(RSQLite) # db
library(DBI) # db

library(tidyverse)
library(caret) # model training
library(tidymodels) # model eval
library(finetune)   # package for more advanced hyperparameter tuning 
library(ggfortify) # autoplot
library(doParallel) # parallel

library(embed)      # use the create embeddings for categorical features
library(stacks)     # used for stacking models 
library(rules)      # contains the cubist modelling algorithm
library(vip)        # variable importance plots
library(brulee)     # torch model
library(bonsai)
library(tictoc)     # measure how long processes take

# Check how many cores you have to work with
detectCores()

# Set the number of clusters tidymodels has to work with
cl <- makePSOCKcluster(8)  # Create 8 clusters
registerDoParallel(cl)
getDoParWorkers()

# Turn off the parallel processing once you're finished with it
stopCluster(cl)
registerDoSEQ()

options(scipen = 999999)
set.seed(214)

# https://www.stepbystepdatascience.com/ml-with-tidymodels


# pull all historical data
nba_final_cor <- tbl(dbConnect(SQLite(), "../nba_sql_db/nba_db"), "mamba_long_odds") %>%
    collect() %>%
    filter(season_year >= 2020) %>%
    rename(team_winner = wl,
           team_score = pts,
           opp_score = opp_pts) %>%
    mutate(
        game_date = as_date(game_date, origin ="1970-01-01"),
        team_winner = if_else(team_winner == "W", "win", "loss"),
        team_winner = factor(team_winner, levels = c("win", "loss")),
        location = if_else(location == "away", 1, 0)
    )


# nba_final <- read_rds("/Users/kartesj/OneDrive - Trinity Industries Inc/Personal/nba_final_w10.rds")
# model_outputs <- read_rds("/Users/kartesj/OneDrive - Trinity Industries Inc/Personal/model_outputs.rds")

nba_final <- nba_final_cor %>%
    filter(season_year >= 2020) %>%
    mutate(
        game_date = as_date(game_date, origin ="1970-01-01"),
        team_winner = factor(team_winner, levels = c("win", "loss")),
        across(c(location, is_b2b_first:opp_is_b2b_second), factor)
        # location = if_else(location == "away", 1, 0)
    )


# # correlations ----
# feature correlations
cor_df <- nba_final_cor %>%
    select(is_b2b_first:opp_is_b2b_second, over_under, team_implied_prob,
           team_fgm:opp_opp_pct_uast_fgm) %>%
    select(-contains("_rating"))

# check for extreme correlation
cor_mx <- cor(cor_df)

# find highly correlated features
cor_cols <- findCorrelation(cor_mx, cutoff = .5, exact = F, names = T)
cor_cols


# clear environment ----
rm(list=ls()[! ls() %in% c("nba_final", "cor_cols", "cl")])


# team score models ----
# all features
train <- nba_final %>%
    filter(season_year <= 2022) %>%
    select(team_score, location, is_b2b_first:opp_is_b2b_second, over_under,
           team_implied_prob, team_fgm:opp_opp_pct_uast_fgm) %>%
    select(-contains("_rating"))

test <- nba_final %>%
    filter(season_year > 2022) %>%
    select(team_score, location, is_b2b_first:opp_is_b2b_second, over_under,
           team_implied_prob, team_fgm:opp_opp_pct_uast_fgm) %>%
    select(-contains("_rating"))

# highly correlated features removed
train <- train %>%
    select(-all_of(cor_cols)) %>%
    mutate(across(location:opp_is_b2b_second, factor))

test <- test %>%
    select(-all_of(cor_cols)) %>%
    mutate(across(location:opp_is_b2b_second, factor))


# Create train-test set
train_test_split <- make_splits(x = train, assessment = test)
# train_test_split <- initial_split(nba_final,
#                                   prop = 0.8,      # take a 10% sample for Train
#                                   strata = team_winner) # tell tidymodels to keep the distribution of team_winner the same in each sample
train_test_split

test  <- testing(train_test_split)  # create the test set
train <- training(train_test_split) # create the train set

# Examine data
skimr::skim(train)

# team_score histogram
ggplot(train, aes(x = team_score)) +
    geom_histogram()  +
    ggtitle(paste0("Histogram of team_score"))

#### tidymodels tuning ----
# k-fold cross-validation - vfold_cv()
train_cv <- vfold_cv(train, v = 5, repeats = 1) # create resampling scheme on train
train_cv

# Model specification
# lm_spec <- linear_reg() %>% 
#     set_engine("lm") %>%
#     set_mode("regression")

glmnet_spec <- linear_reg(penalty = tune(), mixture = tune()) %>%
    set_engine("glmnet") %>%
    set_mode("regression")

# Create recipe
ts_recipe <- recipe(team_score ~ ., data = train) %>%
    step_dummy(location, is_b2b_first:opp_is_b2b_second) %>%
    step_corr(all_numeric_predictors()) %>%
    step_normalize(all_numeric_predictors()) %>%
    step_nzv(all_predictors())

# Create workflow
workflow <- workflow() %>%
    add_recipe(ts_recipe) %>% # add in our newly created recipe
    add_model(glmnet_spec) # add the model spec we made earlier

# Grid options
manual_grid <- crossing(penalty = 10^seq(2, -3, by = -.1),
                        mixture = c(0, 1))

latin_grid <- glmnet_spec %>%
    parameters() %>%
    grid_latin_hypercube(size = 50)

entropy_grid <- glmnet_spec %>%
    parameters() %>%
    grid_max_entropy(size = 50)

# Grid function
diff_params <- function(name, x){
    tune_grid(workflow,
              resamples = train_cv,
              grid = x,
              metrics = metric_set(mae)) %>% 
        show_best(metric = 'mae', n = 1) %>% 
        mutate(param_set = name)
}
# grid <- diff_params("grid", grid_regular(penalty(), mixture(), levels = 5))
# random <- diff_params("random", grid_random(penalty(), mixture(), size = 25))
latin <- diff_params("latin", grid_latin_hypercube(penalty(), mixture(), size = 50))
entropy <- diff_params("entropy", grid_max_entropy(penalty(), mixture(), size = 50))
manual <- diff_params("manual", manual_grid) # manually created grid from earlier

tibble(bind_rows(latin, entropy, manual))[-8]
diff_grids <- tibble(bind_rows(latin, entropy, manual))

# create_latin_grid <- function(mod, x){
#     mod %>%
#         parameters() %>%
#         grid_latin_hypercube(size = x)
# }
# 
# latin_grid <- create_latin_grid(glmnet_spec, 25)
# 
# create_entropy_grid <- function(mod, x){
#     mod %>%
#         parameters() %>%
#         grid_max_entropy(size = x)
# }
# 
# entropy_grid <- create_entropy_grid(glmnet_spec, 25)

# Initial tune
glmnet_initial <- tune_grid(workflow,
                            resamples = train_cv,
                            grid = latin_grid,
                            metrics = metric_set(mae))
glmnet_initial %>% show_best(metric = 'mae', n = 5)

# Anneal tune
glmnet_anneal <- tune_sim_anneal(workflow,
                                   resamples = train_cv,
                                   initial = glmnet_initial, # pass the output from the initial run
                                   iter = 25,
                                   metrics = metric_set(mae),
                                   control = control_sim_anneal(restart = 5L))  
glmnet_anneal %>% show_best(metric = 'mae', n = 1)





# team winner ridge regression model ----
# k-fold cross-validation
train_cv <- vfold_cv(train, v = 10, repeats = 5)

# Model specification
glmnet_spec <- linear_reg(penalty = tune(), mixture = tune()) %>%
    set_engine("glmnet") %>%
    set_mode("regression")

# Create recipe
ts_recipe <- recipe(team_score ~ ., data = train) %>%
    step_dummy(location, is_b2b_first:opp_is_b2b_second) %>%
    step_corr(all_numeric_predictors()) %>%
    step_YeoJohnson(all_numeric_predictors()) %>%
    step_nzv(all_predictors())

# Create workflow
workflow <- workflow() %>%
    add_recipe(ts_recipe) %>%
    add_model(glmnet_spec)

# Grid options
manual_grid <- crossing(penalty = 10^seq(2, -3, by = -.1),
                        mixture = c(0, 1))

latin_grid <- glmnet_spec %>%
    parameters() %>%
    grid_latin_hypercube(size = 100)

entropy_grid <- glmnet_spec %>%
    parameters() %>%
    grid_max_entropy(size = 100)

# Test different grids
diff_params <- function(name, x){
    tune_grid(workflow,
              resamples = train_cv,
              grid = x,
              metrics = metric_set(mae)) %>% 
        show_best(metric = 'mae', n = 1) %>% 
        mutate(param_set = name)
}

latin <- diff_params("latin", latin_grid)
entropy <- diff_params("entropy", entropy_grid)
manual <- diff_params("manual", manual_grid)

tibble(bind_rows(latin, entropy, manual))[-8]

# Control
ctrl <- control_grid(verbose = T)

# Initial tune
glmnet_initial <- tune_grid(workflow,
                            resamples = train_cv,
                            grid = latin_grid,
                            metrics = metric_set(mae),
                            control = ctrl)
glmnet_initial %>% show_best(metric = 'mae', n = 5)

# Anneal tune
glmnet_anneal <- tune_sim_anneal(workflow,
                                 resamples = train_cv,
                                 initial = glmnet_initial,
                                 iter = 25,
                                 metrics = metric_set(mae),
                                 control = control_sim_anneal(restart = 5L))  
glmnet_anneal %>% show_best(metric = 'mae', n = 1)

# Select best model
best_mod <- select_best(glmnet_anneal, "mae")

# Finalize model
final_mod <- finalize_model(glmnet_spec, best_mod)
final_mod

# Create final workfloor
final_wf <- workflow() %>%
    add_recipe(ts_recipe) %>%
    add_model(final_mod)

# Train and test
final_res <- final_wf %>%
    last_fit(train_test_split, metrics = metric_set(mae))

# Test metrics
final_res %>%
    collect_metrics()

# Variable importance
vip::vi(extract_fit_parsnip(final_res))  # extract the variable importance scores
vip::vip(extract_fit_parsnip(final_res)) + # plot them
    theme(text = element_text(size = 15)) 





































