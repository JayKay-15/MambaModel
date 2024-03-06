### model tuning ----

library(tidyverse)
library(RSQLite) # db
library(DBI) # db
# library(caret) # model training
library(tidymodels) # model eval
library(finetune)   # package for more advanced hyperparameter tuning 
library(ggfortify) # autoplot
library(doParallel) # parallel

library(embed)      # use the create embeddings for categorical features
library(stacks)     # used for stacking models 
library(rules)      # contains the cubist modelling algorithm
library(vip)        # variable importance plots
library(brulee)     # torch model

# library(MLeval) # model eval
# library(pROC) # model eval

# library(glmnet) # regularization
# library(ranger) # rf
# library(xgboost) # xgb
# library(e1071) # svm
# library(nnet) # nn

# library(nbastatR)
# library(caTools)

# library(mctest) # correlations
# library(corrplot) # correlations
# library(corrgram) # correlations

# Check how many cores you have to work with
detectCores()

# Set the number of clusters tidymodels has to work with
cl <- makePSOCKcluster(8)  # Create 8 clusters
registerDoParallel(cl)
getDoParWorkers()

# Turn off the parallel processing once you're finished with it
stopCluster(cl)
registerDoSEQ()

# detectCores()
# cl <- makePSOCKcluster(4)
# registerDoParallel(cl)
# 
# stopCluster(cl)
# registerDoSEQ() # ends parallel processing

options(scipen = 999999)
set.seed(214)
# Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)

# https://www.stepbystepdatascience.com/ml-with-tidymodels

# pull all historical data
nba_final <- tbl(dbConnect(SQLite(), "../nba_sql_db/nba_db"), "mamba_long_odds") %>%
    collect() %>%
    filter(season_year >= 2020) %>%
    rename(team_winner = wl,
           team_score = pts,
           opp_score = opp_pts) %>%
    mutate(
        game_date = as_date(game_date, origin ="1970-01-01"),
        team_winner = if_else(team_winner == "W", "win", "loss"),
        team_winner = factor(team_winner, levels = c("win", "loss")),
        location = if_else(location == "away", 1, 0),
        across(c(location, is_b2b_first:opp_is_b2b_second), factor)
    )

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

# # Create train-test set
# train_test_split <- initial_split(nba_final,
#                                   prop = 0.1)      # take a 10% sample for Train
#                                   # strata = team_winner) # tell tidymodels to keep the distribution of team_winner the same in each sample
# train_test_split
# 
# test  <- testing(train_test_split)  # create the test set
# train <- training(train_test_split) # create the train set

skimr::skim(train)

# team_score histogram
ggplot(train, aes(x = team_score)) +
    geom_histogram()  +
    ggtitle(paste0("Histogram of team_score"))

# Check the correlation amongst the columns
cor(train %>% 
        select(team_fgm:opp_opp_pct_uast_fgm) %>%
        select(-contains("_rating")) %>% # only keep numeric columns
        filter(if_all(everything(), ~!is.na(.x)))) # filter out any rows that have missing data

# k-fold cross-validation - vfold_cv()
# train_cv <- vfold_cv(train, v = 5, repeats = 3, strata = price) # create resampling scheme on train
train_cv <- vfold_cv(train, v = 5, repeats = 3) # create resampling scheme on train
train_cv

# Create a recipe for our linear model
ts_recipe <- recipe(team_score ~ ., data = train) %>%
    step_unorder(location, is_b2b_first:opp_is_b2b_second) %>%
    step_dummy(location, is_b2b_first:opp_is_b2b_second) %>%
    step_corr(all_numeric_predictors()) %>%
    step_normalize(all_numeric_predictors()) %>%
    step_nzv(all_predictors())
ts_recipe

# Create the specification for a glmnet regression model
glmnet_spec <- linear_reg(mode = "regression", 
               engine = "glmnet", 
               penalty = 0.0001, 
               mixture = 0)
glmnet_spec

# Create a workflow that combines the recipe and the linear model specification
lm_workflow <- workflow() %>%
    add_recipe(ts_recipe) %>% # add in our newly created recipe
    add_model(glmnet_spec) # add the model spec we made earlier
lm_workflow

# Fit the model with a workflow
lm_model <- fit_resamples(lm_workflow,         # use our newly made workflow
                          resamples = train_cv,    # use our k-fold cross-validation we made earlier
                          control = control_resamples(save_pred = T,   # save the predictions for each of the resamples
                                                      save_workflow = T,  # append the workflow ID to the outcome
                                                      verbose = T),           # print extra info as the model trains
                          metrics = metric_set(mae))   # use MAE as the assessment metric
lm_model

# Helper functions
collect_metrics(lm_model) # collect the model performance (average across resamples)

# Other helper functions
show_best(lm_model, metric = "mae")       # bring back the best performing model
collect_notes(lm_model)   # collect any notes

# Have a look at the results
collect_predictions(lm_model)

# Plot the results
collect_predictions(lm_model) %>% 
    filter(.config == pull(show_best(lm_model, metric = 'mae', n =1 ),.config)) %>% # only bring back predictions for the best model
    ggplot(aes(x = .pred, y = team_score )) + 
    geom_point(shape = 1)  +
    geom_abline(slope = 1, linetype = "dashed", colour = 'blue') +
    coord_obs_pred() +
    ggtitle("Actuals vs Prediction on Resamples") + 
    theme(text = element_text(size = 20))

# Plot the residuals
collect_predictions(lm_model) %>% 
    filter(.config == pull(show_best(lm_model, metric = 'mae', n = 1),.config)) %>% 
    mutate(residuals = team_score-.pred) %>% 
    ggplot(aes(x = team_score, y = residuals )) + 
    geom_point(shape = 1)  +
    geom_abline(slope = 0, linetype = "dashed", colour = 'blue') +
    ggtitle("Actuals vs Prediction on Resamples") + 
    theme(text = element_text(size = 20))


# Fit the final model
last_fit <- lm_workflow %>% # workflow that creates our chosen model
    last_fit(split = train_test_split, metrics = metric_set(mae)) # fit the model on 100% Train and predict on the Test set

# See how it performs on the Test set
collect_metrics(last_fit)

# Compare Test v Resamples
inner_join(collect_metrics(lm_model) %>%
               select(.metric, mean) %>%
               rename(.estimate = mean),
           collect_metrics(last_fit) %>%
               select(.metric, .estimate),
           by = ".metric",
           suffix = c("_train", "_test")) %>% 
    data.frame() # tibbles round values so they can appear the same

# Plot predictions on Test
collect_predictions(last_fit) %>% 
    ggplot(aes(x = .pred, y = team_score )) + 
    geom_point(shape = 1)  +
    geom_abline(slope = 1, linetype = "dashed", colour = 'blue') +
    coord_obs_pred() +
    ggtitle("Actuals vs Prediction on Test") + 
    theme(text = element_text(size=20))

# Variable importance
vip::vi(extract_fit_parsnip(last_fit))  # extract the variable importance scores
vip::vip(extract_fit_parsnip(last_fit)) + # plot them
    theme(text = element_text(size = 20)) 



# Create default model specs for lots of different algorithms
create_spec <- function(x){
    x(mode="regression")
}
bag_mars_spec <- create_spec(bag_mars)
bag_tree_spec <- create_spec(bag_tree)
bart_spec <- create_spec(bart)
boost_tree_spec <- create_spec(boost_tree)
cubist_rules_spec <- create_spec(cubist_rules)
decision_tree_spec <- create_spec(decision_tree)
linear_reg_spec <- create_spec(linear_reg)
mars_spec <- create_spec(mars)
mlp_spec <- create_spec(mlp)
knn_spec <- create_spec(nearest_neighbor)
rand_forest_spec <- create_spec(rand_forest)
svm_linear <- create_spec(svm_linear)
svm_poly_spec <- create_spec(svm_poly)
svm_rbf_spec <- create_spec(svm_rbf)

# Recipe
dummy_norm <- recipe(team_score ~ ., data=train) %>% 
    step_unorder(location, is_b2b_first:opp_is_b2b_second) %>%
    step_dummy(location, is_b2b_first:opp_is_b2b_second) %>%
    step_corr(all_numeric_predictors()) %>%
    step_normalize(all_numeric_predictors()) %>%
    step_nzv(all_predictors())

# Have a look at the recipe
dummy_norm %>% prep(train) %>% bake(train)

# Workflow sets
all_models_set <- workflow_set(preproc = list(dummy_norm = dummy_norm),
                               models = list(bag_mars_spec,
                                             bag_tree_spec,
                                             bart_spec,
                                             boost_tree_spec,
                                             cubist_rules_spec,
                                             decision_tree_spec,
                                             glmnet = glmnet_spec, # name glmnet so tidymodels can tell it apart from the linear model
                                             linear_reg_spec,
                                             mars_spec,
                                             mlp_spec,
                                             knn_spec,
                                             rand_forest_spec,
                                             svm_linear,
                                             svm_poly_spec,
                                             svm_rbf_spec),
                               cross = TRUE)

# Have a look at our workflow set
all_models_set

# Run the workflow set
all_models <- all_models_set %>%   # use our workflowset
    workflow_map("fit_resamples", # tell the workflow map what function to run
                 resamples = train_cv, # use our k-fold cross-validation scheme for resampling
                 verbose = T,   # print a more detailed output in the console
                 metrics = metric_set(mae))

# Have a look at the results
collect_metrics(all_models) %>% filter(.metric == "mae") 

# Rank the best models and only return the top X
rank_results(all_models, rank_metric = "mae") %>% 
    filter(.metric == "mae")

# Plot our results
autoplot(all_models) + theme(text = element_text(size = 20))











