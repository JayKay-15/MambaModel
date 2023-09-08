### 2022 Model Training - Created by @MambaMetrics ----
library(tidyverse)
library(caTools)
library(readxl)
library(tidymodels)
library(caret) # model training
library(ggfortify) # autoplot
library(glmnet) # regularization
library(ranger) # rf
library(xgboost) # xgb
library(e1071) # svm
library(nnet) # nn

# library(RSQLite) # db
# library(DBI) # db

# library(mctest) # correlations
# library(corrplot) # correlations
# library(corrgram) # correlations

rm(list=ls())

setwd("/Users/Jesse/Documents/MyStuff/NBA Betting/NBAdb/")

NBAdb <- dplyr::tbl(DBI::dbConnect(RSQLite::SQLite(),
                                     "/Users/Jesse/Documents/MyStuff/NBA Betting/NBAdb/NBAdb.sqlite"),"GameLogsAdj")

nba <- NBAdb %>% collect() %>% mutate(date = as_date(date, origin ="1970-01-01"))

train <- nba %>% filter(season <= 2020) %>% select(-1)
test <- nba %>% filter(season >= 2021) %>% select(-1)

nba <- rbind(train, test)

### Preprocess Stats ----
# master_db <- NBAdb %>% collect() %>% mutate(date = as_date(date, origin ="1970-01-01")) %>% select(-1)
#
# all_db <- master_db %>% select(8,9:78)
# ts_db <- master_db %>% select(8,14,15,16,18,19,21,24,30,34,35,37,40,43,
#                               49,50,51,53,54,56,59,65,69,70,72,75,78)
# 
# pre_proc_val_all <- preProcess(all_db[,-1], method = c("center", "scale"))
# pre_proc_val_ts <- preProcess(ts_db[,-1], method = c("center", "scale"))
# 
# saveRDS(pre_proc_val_all, "pre_proc_val_all.rds")
# saveRDS(pre_proc_val_ts, "pre_proc_val_ts.rds")

### Correlations ----
# Win

# All
nba_cla <- nba %>%
    select(8,9:78) %>%
    mutate(result = if_else(result == "W", 1, 0))
cor_mx <- cor(nba_cla)
# corrplot(cor_mx, method = "color", title = "Correlation Matrix",
#          mar=c(0,0,1,0))
cor_win <- cor(nba_cla, nba_cla$result)
cor_win <- as.matrix(cor_win[order(cor_win[,1], decreasing = T),])
cor_win

# eFG
nba_cla <- nba %>%
    select(8,
           14,15,16,18,19,21,23,30,34,35,37,39,43,
           49,50,51,53,54,56,58,65,69,70,72,74,78) %>%
    mutate(result = if_else(result == "W", 1, 0))
cor_mx <- cor(nba_cla)
# corrplot(cor_mx, method = "color", title = "Correlation Matrix",
#          mar=c(0,0,1,0))
cor_win <- cor(nba_cla, nba_cla$result)
cor_win <- as.matrix(cor_win[order(cor_win[,1], decreasing = T),])
cor_win

# TS
nba_cla <- nba %>%
    select(8,
           14,15,16,18,19,21,24,30,34,35,37,40,43,
           49,50,51,53,54,56,59,65,69,70,72,75,78) %>%
    mutate(result = if_else(result == "W", 1, 0))
cor_mx <- cor(nba_cla)
# corrplot(cor_mx, method = "color", title = "Correlation Matrix",
#          mar=c(0,0,1,0))
cor_win <- cor(nba_cla, nba_cla$result)
cor_win <- as.matrix(cor_win[order(cor_win[,1], decreasing = T),])
cor_win

# as

# All
nba_reg <- nba %>%
    select(5,9:78)
cor_mx <- cor(nba_reg)
# corrplot(cor_mx, method = "color", title = "Correlation Matrix",
#          mar=c(0,0,1,0))
cor_as <- cor(nba_reg, nba_reg$as)
cor_as <- as.matrix(cor_as[order(cor_as[,1], decreasing = T),])
cor_as

# eFG
nba_reg <- nba %>%
    select(5,
           14,15,16,18,19,21,23,30,34,35,37,39,43,
           49,50,51,53,54,56,58,65,69,70,72,74,78)
cor_mx <- cor(nba_reg)
# corrplot(cor_mx, method = "color", title = "Correlation Matrix",
#          mar=c(0,0,1,0))
cor_as <- cor(nba_reg, nba_reg$as)
cor_as <- as.matrix(cor_as[order(cor_as[,1], decreasing = T),])
cor_as

# TS
nba_reg <- nba %>%
    select(5,
           14,15,16,18,19,21,24,30,34,35,37,40,43,
           49,50,51,53,54,56,59,65,69,70,72,75,78)
cor_mx <- cor(nba_reg)
# corrplot(cor_mx, method = "color", title = "Correlation Matrix",
#          mar=c(0,0,1,0))
cor_as <- cor(nba_reg, nba_reg$as)
cor_as <- as.matrix(cor_as[order(cor_as[,1], decreasing = T),])
cor_as

# hs

# All
nba_reg <- nba %>%
    select(6,9:78)
cor_mx <- cor(nba_reg)
# corrplot(cor_mx, method = "color", title = "Correlation Matrix",
#          mar=c(0,0,1,0))
cor_as <- cor(nba_reg, nba_reg$hs)
cor_as <- as.matrix(cor_as[order(cor_as[1], decreasing = T),])
cor_as

# eFG
nba_reg <- nba %>%
    select(6,
           14,15,16,18,19,21,23,30,34,35,37,39,43,
           49,50,51,53,54,56,58,65,69,70,72,74,78)
cor_mx <- cor(nba_reg)
# corrplot(cor_mx, method = "color", title = "Correlation Matrix",
#          mar=c(0,0,1,0))
cor_as <- cor(nba_reg, nba_reg$hs)
cor_as <- as.matrix(cor_as[order(cor_as[,1], decreasing = T),])
cor_as

# TS
nba_reg <- nba %>%
    select(6,
           14,15,16,18,19,21,24,30,34,35,37,40,43,
           49,50,51,53,54,56,59,65,69,70,72,75,78)
cor_mx <- cor(nba_reg)
# corrplot(cor_mx, method = "color", title = "Correlation Matrix",
#          mar=c(0,0,1,0))
cor_as <- cor(nba_reg, nba_reg$hs)
cor_as <- as.matrix(cor_as[order(cor_as[,1], decreasing = T),])
cor_as

### Least Squares ----
# Win

# All
# nba_cla <- nba %>%
#     select(8,9:78)

# eFG
# nba_cla <- nba %>%
#     select(8,
#            14,15,16,18,19,21,23,30,34,35,37,39,43,
#            49,50,51,53,54,56,58,65,69,70,72,74,78)

# TS
nba_cla <- nba %>%
    select(8,
           14,15,16,18,19,21,24,30,34,35,37,40,43,
           49,50,51,53,54,56,59,65,69,70,72,75,78)
train <- nba_cla

set.seed(214)
train <- nba_cla[1:7006,]
test <- nba_cla[7007:8887,]
pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))

train[,-1] = predict(pre_proc_val, train[,-1])
test[,-1] = predict(pre_proc_val, test[,-1])
# summary(train)

# Model
ctrl <- trainControl(method = "cv", number = 5, verboseIter = T, 
                     classProbs = T, summaryFunction = twoClassSummary)
log_win <- train(as.factor(result) ~., data = train,
                 trControl = ctrl,
                 method = "glm",
                 metric = "ROC",
                 family = "binomial")
log_win
log_win$resample
log_win$results
summary(log_win) # Model Components
confusionMatrix(log_win) # Confusion Matrix
glance(log_win$finalModel) # Entire Model
tidy(log_win$finalModel) # Model Components
augment(log_win$finalModel) # Observations

saveRDS(log_win, "log_win_model.rds")

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

# as

# All
# nba_reg <- nba %>%
#     select(5,9:78)

# eFG
# nba_reg <- nba %>%
#     select(5,
#            14,15,16,18,19,21,23,30,34,35,37,39,43,
#            49,50,51,53,54,56,58,65,69,70,72,74,78)

# TS
nba_reg <- nba %>%
    select(5,
           14,15,16,18,19,21,24,30,34,35,37,40,43,
           49,50,51,53,54,56,59,65,69,70,72,75,78)
train <- nba_reg

set.seed(214)
train <- nba_reg[1:7006,]
test <- nba_reg[7007:8887,]
pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))

train[,-1] = predict(pre_proc_val, train[,-1])
test[,-1] = predict(pre_proc_val, test[,-1])
# summary(train)

# Model
ctrl <- trainControl(method = "cv", number = 5, verboseIter = T)
lin_as <- train(as ~., data = train,
                 trControl = ctrl,
                 method = "lm")
lin_as
summary(lin_as) # Model Components
autoplot(lin_as$finalModel) # Viz
glance(lin_as$finalModel) # Entire Model
tidy(lin_as$finalModel) # Model Components
augment(lin_as$finalModel) # Observations
# plot(lin_as$finalModel) # Viz

saveRDS(lin_as, "lin_as_model.rds")

as_pred <- predict(lin_as, test)

rmse <- RMSE(as_pred, test$as)
mae <- MAE(as_pred, test$as)
r2 <- R2(as_pred, test$as)

cat("RMSE: ", rmse, "MAE: ", mae, " R2: ", r2)

lin_as_imp <- rownames_to_column(varImp(lin_as)[["importance"]], "Var") %>%
    arrange(desc(Overall)) %>%
    head()


# hs

# All
# nba_reg <- nba %>%
#     select(6,9:78)

# eFG
# nba_reg <- nba %>%
#     select(6,
#            14,15,16,18,19,21,23,30,34,35,37,39,43,
#            49,50,51,53,54,56,58,65,69,70,72,74,78)

# TS
nba_reg <- nba %>%
    select(6,
           14,15,16,18,19,21,24,30,34,35,37,40,43,
           49,50,51,53,54,56,59,65,69,70,72,75,78)
train <- nba_reg

set.seed(214)
train <- nba_reg[1:7006,]
test <- nba_reg[7007:8887,]
pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))

train[,-1] = predict(pre_proc_val, train[,-1])
test[,-1] = predict(pre_proc_val, test[,-1])
# summary(train)

# Model
ctrl <- trainControl(method = "cv", number = 5,  verboseIter = T)
lin_hs <- train(hs ~., data = train,
                 trControl = ctrl,
                 method = "lm")
lin_hs
summary(lin_hs) # Model Components
autoplot(lin_hs$finalModel) # Viz
glance(lin_hs$finalModel) # Entire Model
tidy(lin_hs$finalModel) # Model Components
augment(lin_hs$finalModel) # Observations
# plot(lin_hs) # Viz

saveRDS(lin_hs, "lin_hs_model.rds")

hs_pred <- predict(lin_hs, test)

rmse <- RMSE(hs_pred, test$hs)
mae <- MAE(hs_pred, test$hs)
r2 <- R2(hs_pred, test$hs)

cat("RMSE: ", rmse, "MAE: ", mae, " R2: ", r2)

lin_hs_imp <- rownames_to_column(varImp(lin_hs)[["importance"]], "Var") %>%
    arrange(desc(Overall)) %>%
    head()


### Regularization ----
# Win

# All
nba_cla <- nba %>%
    select(8,9:78)
train <- nba_cla

# eFG
# nba_cla <- nba %>%
#     select(8,
#            14,15,16,18,19,21,23,30,34,35,37,39,43,
#            49,50,51,53,54,56,58,65,69,70,72,74,78)

# TS
# nba_cla <- nba %>%
#     select(8,
#            14,15,16,18,19,21,24,30,34,35,37,40,43,
#            49,50,51,53,54,56,59,65,69,70,72,75,78)

set.seed(214)
train <- nba_cla[1:7006,]
test <- nba_cla[7007:8887,]
pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))

train[,-1] = predict(pre_proc_val, train[,-1])
test[,-1] = predict(pre_proc_val, test[,-1])
# summary(train)

# Model
ctrl <- trainControl(method = "cv", number = 5, verboseIter = T, 
                     classProbs = T, summaryFunction = twoClassSummary)
grid <- expand.grid(
    alpha = 0, # ridge = 0 / lasso = 1
    lambda = 10^seq(2, -3, by = -.1)
)
reg_win <- train(as.factor(result) ~., data = train,
                 method = "glmnet",
                 metric = "ROC",
                 trControl = ctrl,
                 tuneGrid = grid)
reg_win
reg_win$resample
reg_win$results
summary(reg_win)
confusionMatrix(reg_win)
glance(reg_win$finalModel)
tidy(reg_win$finalModel)

saveRDS(reg_win, "reg_win_model.rds")

win_pred <- predict(reg_win, test, type = "prob")
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

reg_win_imp <- rownames_to_column(varImp(reg_win)[["importance"]], "Var") %>%
    arrange(desc(Overall)) %>%
    head()


# as

# All
nba_reg <- nba %>%
    select(5,9:78)
train <- nba_reg    

# eFG
# nba_reg <- nba %>%
#     select(5,
#            14,15,16,18,19,21,23,30,34,35,37,39,43,
#            49,50,51,53,54,56,58,65,69,70,72,74,78)

# TS
# nba_reg <- nba %>%
#     select(5,
#            14,15,16,18,19,21,24,30,34,35,37,40,43,
#            49,50,51,53,54,56,59,65,69,70,72,75,78)

set.seed(214)
train <- nba_reg[1:7006,]
test <- nba_reg[7007:8887,]
pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))

train[,-1] = predict(pre_proc_val, train[,-1])
test[,-1] = predict(pre_proc_val, test[,-1])
# summary(train)

# Model
ctrl <- trainControl(method = "cv", number = 5, verboseIter = T)
grid <- expand.grid(
    alpha = 0, 
    lambda = 10^seq(2, -3, by = -.1)
)
reg_as <- train(as ~., data = train,
                method = "glmnet",
                trControl = ctrl,
                tuneGrid = grid)
reg_as
summary(reg_as)
autoplot(reg_as$finalModel)
glance(reg_as$finalModel)
tidy(reg_as$finalModel)
augment(reg_as$finalModel)
# plot(reg_as$finalModel)

saveRDS(reg_as, "reg_as_model.rds")

as_pred <- predict(reg_as, test)

rmse <- RMSE(as_pred, test$as)
mae <- MAE(as_pred, test$as)
r2 <- R2(as_pred, test$as)

cat("RMSE: ", rmse, "MAE: ", mae, " R2: ", r2)

reg_as_imp <- rownames_to_column(varImp(reg_as)[["importance"]], "Var") %>%
    arrange(desc(Overall)) %>%
    head()


# hs

# All
nba_reg <- nba %>%
    select(6,9:78)
train <- nba_reg

# eFG
# nba_reg <- nba %>%
#     select(6,
#            14,15,16,18,19,21,23,30,34,35,37,39,43,
#            49,50,51,53,54,56,58,65,69,70,72,74,78)

# TS
# nba_reg <- nba %>%
#     select(6,
#            14,15,16,18,19,21,24,30,34,35,37,40,43,
#            49,50,51,53,54,56,59,65,69,70,72,75,78)

set.seed(214)
train <- nba_reg[1:7006,]
test <- nba_reg[7007:8887,]
pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))

train[,-1] = predict(pre_proc_val, train[,-1])
test[,-1] = predict(pre_proc_val, test[,-1])
# summary(train)

# Model
ctrl <- trainControl(method = "cv", number = 5, verboseIter = T)
grid <- expand.grid(
    alpha = 0, 
    lambda = 10^seq(2, -3, by = -.1)
)
reg_hs <- train(hs ~., data = train,
                method = "glmnet",
                trControl = ctrl,
                tuneGrid = grid)
reg_hs
summary(reg_hs)
autoplot(reg_hs$finalModel)
glance(reg_hs$finalModel)
tidy(reg_hs$finalModel)
augment(reg_hs$finalModel)
# plot(reg_hs)

saveRDS(reg_hs, "reg_hs_model.rds")

hs_pred <- predict(reg_hs, test)

rmse <- RMSE(hs_pred, test$hs)
mae <- MAE(hs_pred, test$hs)
r2 <- R2(hs_pred, test$hs)

cat("RMSE: ", rmse, "MAE: ", mae, " R2: ", r2)

reg_hs_imp <- rownames_to_column(varImp(reg_hs)[["importance"]], "Var") %>%
    arrange(desc(Overall)) %>%
    head()


### KNN ----
# Win

# All
# nba_cla <- nba %>%
#     select(8,9:78)

# eFG
# nba_cla <- nba %>%
#     select(8,
#            14,15,16,18,19,21,23,30,34,35,37,39,43,
#            49,50,51,53,54,56,58,65,69,70,72,74,78)

# TS
nba_cla <- nba %>%
    select(8,
           14,15,16,18,19,21,24,30,34,35,37,40,43,
           49,50,51,53,54,56,59,65,69,70,72,75,78)
train <- nba_cla

set.seed(214)
train <- nba_cla[1:7006,]
test <- nba_cla[7007:8887,]
pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))

train[,-1] = predict(pre_proc_val, train[,-1])
test[,-1] = predict(pre_proc_val, test[,-1])
# summary(train)

# Model
ctrl <- trainControl(method = "cv", number = 5, verboseIter = T, 
                     classProbs = T, summaryFunction = twoClassSummary)
grid <- expand.grid(
    k = seq(5, 125, 5)
)
knn_win <- train(as.factor(result) ~., data = train, 
                 method = "knn",
                 metric = "ROC",
                 trControl = ctrl,
                 tuneGrid = grid)
knn_win
knn_win$resample
knn_win$results
summary(knn_win)
confusionMatrix(knn_win)
plot(knn_win)

saveRDS(knn_win, "knn_win_model.rds")

win_pred <- predict(knn_win, test, type = "prob")
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

knn_win_imp <- rownames_to_column(varImp(knn_win)[["importance"]], "Var") %>%
    arrange(desc(Overall)) %>%
    head()


# as

# All
# nba_reg <- nba %>%
#     select(5,9:78)

# eFG
# nba_reg <- nba %>%
#     select(5,
#            14,15,16,18,19,21,23,30,34,35,37,39,43,
#            49,50,51,53,54,56,58,65,69,70,72,74,78)

# TS
nba_reg <- nba %>%
    select(5,
           14,15,16,18,19,21,24,30,34,35,37,40,43,
           49,50,51,53,54,56,59,65,69,70,72,75,78)
train <- nba_reg

set.seed(214)
train <- nba_reg[1:7006,]
test <- nba_reg[7007:8887,]
pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))

train[,-1] = predict(pre_proc_val, train[,-1])
test[,-1] = predict(pre_proc_val, test[,-1])
# summary(train)

# Model
ctrl <- trainControl(method = "cv", number = 5, verboseIter = T)
grid <- expand.grid(
    k = seq(2, 50, 2)
)
knn_as <- train(as ~., data = train, 
                method = "knn",
                trControl = ctrl,
                tuneGrid = grid)
knn_as
summary(knn_as)
plot(knn_as)

saveRDS(knn_as, "knn_as_model.rds")

as_pred <- predict(knn_as, test)

rmse <- RMSE(as_pred, test$as)
mae <- MAE(as_pred, test$as)
r2 <- R2(as_pred, test$as)

cat("RMSE: ", rmse, "MAE: ", mae, " R2: ", r2)

knn_as_imp <- rownames_to_column(varImp(knn_as)[["importance"]], "Var") %>%
    arrange(desc(Overall)) %>%
    head()


# hs

# All
# nba_reg <- nba %>%
#     select(6,9:78)

# eFG
# nba_reg <- nba %>%
#     select(6,
#            14,15,16,18,19,21,23,30,34,35,37,39,43,
#            49,50,51,53,54,56,58,65,69,70,72,74,78)

# TS
nba_reg <- nba %>%
    select(6,
           14,15,16,18,19,21,24,30,34,35,37,40,43,
           49,50,51,53,54,56,59,65,69,70,72,75,78)
train <- nba_reg

set.seed(214)
train <- nba_reg[1:7006,]
test <- nba_reg[7007:8887,]
pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))

train[,-1] = predict(pre_proc_val, train[,-1])
test[,-1] = predict(pre_proc_val, test[,-1])
# summary(train)

# Model
ctrl <- trainControl(method = "cv", number = 5, verboseIter = T)
grid <- expand.grid(
    k = seq(2, 50, 2)
)
knn_hs <- train(hs ~., data = train, 
                method = "knn",
                trControl = ctrl,
                tuneGrid = grid)
knn_hs
summary(knn_hs)
plot(knn_hs)

saveRDS(knn_hs, "knn_hs_model.rds")

hs_pred <- predict(knn_hs, test)

rmse <- RMSE(hs_pred, test$hs)
mae <- MAE(hs_pred, test$hs)
r2 <- R2(hs_pred, test$hs)

cat("RMSE: ", rmse, "MAE: ", mae, " R2: ", r2)

knn_hs_imp <- rownames_to_column(varImp(knn_hs)[["importance"]], "Var") %>%
    arrange(desc(Overall)) %>%
    head()


### Random Forest ----
# Win

# All
nba_cla <- nba %>%
    select(8,9:78)
train <- nba_cla

# eFG
# nba_cla <- nba %>%
#     select(8,
#            14,15,16,18,19,21,23,30,34,35,37,39,43,
#            49,50,51,53,54,56,58,65,69,70,72,74,78)

# TS
# nba_cla <- nba %>%
#     select(8,
#            14,15,16,18,19,21,24,30,34,35,37,40,43,
#            49,50,51,53,54,56,59,65,69,70,72,75,78)

set.seed(214)
train <- nba_cla[1:7006,]
test <- nba_cla[7007:8887,]
pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))

train[,-1] = predict(pre_proc_val, train[,-1])
test[,-1] = predict(pre_proc_val, test[,-1])
# summary(train)

# Model
ctrl <- trainControl(method = "cv", number = 5, verboseIter = T, 
                     classProbs = T, summaryFunction = twoClassSummary)
grid <- expand.grid(
    .mtry = 1:6,
    .splitrule = "gini",
    .min.node.size = 1
)
rf_win <- train(as.factor(result) ~., data = train,
                method = "ranger",
                metric = "ROC",
                trControl = ctrl,
                tuneGrid = grid)
rf_win
rf_win$resample
summary(rf_win)
plot(rf_win)
confusionMatrix(rf_win)

saveRDS(rf_win, "rf_win_model.rds")

win_pred <- predict(rf_win, test, type = "prob")
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

rf_win_imp <- rownames_to_column(varImp(rf_win)[["importance"]], "Var") %>%
    arrange(desc(Overall)) %>%
    head()


# as

# All
nba_reg <- nba %>%
    select(5,9:78)
train <- nba_reg

# eFG
# nba_reg <- nba %>%
#     select(5,
#            14,15,16,18,19,21,23,30,34,35,37,39,43,
#            49,50,51,53,54,56,58,65,69,70,72,74,78)

# TS
# nba_reg <- nba %>%
#     select(5,
#            14,15,16,18,19,21,24,30,34,35,37,40,43,
#            49,50,51,53,54,56,59,65,69,70,72,75,78)

set.seed(214)
train <- nba_reg[1:7006,]
test <- nba_reg[7007:8887,]
pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))

train[,-1] = predict(pre_proc_val, train[,-1])
test[,-1] = predict(pre_proc_val, test[,-1])
# summary(train)

# Model
ctrl <- trainControl(method = "cv", number = 5, verboseIter = T)
grid <- expand.grid(
    .mtry = seq(2, 18, 2),
    .splitrule = "variance",
    .min.node.size = 1
)
rf_as <- train(as ~., data = train,
               method = "ranger",
               trControl = ctrl,
               tuneGrid = grid)
rf_as
rf_as$resample
summary(rf_as)
plot(rf_as)

saveRDS(rf_as, "rf_as_model.rds")

as_pred <- predict(rf_as, test)

rmse <- RMSE(as_pred, test$as)
mae <- MAE(as_pred, test$as)
r2 <- R2(as_pred, test$as)

cat("RMSE: ", rmse, "MAE: ", mae, " R2: ", r2)

rf_as_imp <- rownames_to_column(varImp(rf_as)[["importance"]], "Var") %>%
    arrange(desc(Overall)) %>%
    head()


# hs

# All
nba_reg <- nba %>%
    select(6,9:78)
train <- nba_reg

# eFG
# nba_reg <- nba %>%
#     select(6,
#            14,15,16,18,19,21,23,30,34,35,37,39,43,
#            49,50,51,53,54,56,58,65,69,70,72,74,78)

# TS
# nba_reg <- nba %>%
#     select(6,
#            14,15,16,18,19,21,24,30,34,35,37,40,43,
#            49,50,51,53,54,56,59,65,69,70,72,75,78)

set.seed(214)
train <- nba_reg[1:7006,]
test <- nba_reg[7007:8887,]
pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))

train[,-1] = predict(pre_proc_val, train[,-1])
test[,-1] = predict(pre_proc_val, test[,-1])
# summary(train)

# Model
ctrl <- trainControl(method = "cv", number = 5, verboseIter = T)
grid <- expand.grid(
    .mtry = seq(2, 18, 2),
    .splitrule = "variance",
    .min.node.size = 1
)
rf_hs <- train(hs ~., data = train,
               method = "ranger",
               trControl = ctrl,
               tuneGrid = grid)
rf_hs
rf_hs$resample
summary(rf_hs)
plot(rf_hs)

saveRDS(rf_hs, "rf_hs_model.rds")

hs_pred <- predict(rf_hs, test)

rmse <- RMSE(hs_pred, test$hs)
mae <- MAE(hs_pred, test$hs)
r2 <- R2(hs_pred, test$hs)

cat("RMSE: ", rmse, "MAE: ", mae, " R2: ", r2)

rf_hs_imp <- rownames_to_column(varImp(rf_hs)[["importance"]], "Var") %>%
    arrange(desc(Overall)) %>%
    head()


### SVM ----
# Win

# All
# nba_cla <- nba %>%
#     select(8,9:78)

# eFG
# nba_cla <- nba %>%
#     select(8,
#            14,15,16,18,19,21,23,30,34,35,37,39,43,
#            49,50,51,53,54,56,58,65,69,70,72,74,78)

# TS
nba_cla <- nba %>%
    select(8,
           14,15,16,18,19,21,24,30,34,35,37,40,43,
           49,50,51,53,54,56,59,65,69,70,72,75,78)
train <- nba_cla

set.seed(214)
train <- nba_cla[1:7006,]
test <- nba_cla[7007:8887,]
pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))

train[,-1] = predict(pre_proc_val, train[,-1])
test[,-1] = predict(pre_proc_val, test[,-1])
# summary(train)

# Model
ctrl <- trainControl(method = "cv", number = 5, verboseIter = T, 
                     classProbs = T, summaryFunction = twoClassSummary)
grid <- expand.grid(
    sigma = c(0.005, 0.01, 0.05),
    C = c(0.25, 0.5, 0.75)
)
svm_win <- train(as.factor(result) ~., data = train,
                 method = "svmRadial",
                 metric = "ROC",
                 trControl = ctrl,
                 tuneGrid = grid)
svm_win
svm_win$resample
svm_win$results
summary(svm_win)
plot(svm_win)
confusionMatrix(svm_win)

saveRDS(svm_win, "svm_win_model.rds")

win_pred <- predict(svm_win, test, type = "prob")
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

svm_win_imp <- rownames_to_column(varImp(svm_win)[["importance"]], "Var") %>%
    arrange(desc(Overall)) %>%
    head()


# as

# All
# nba_reg <- nba %>%
#     select(5,9:78)

# eFG
# nba_reg <- nba %>%
#     select(5,
#            14,15,16,18,19,21,23,30,34,35,37,39,43,
#            49,50,51,53,54,56,58,65,69,70,72,74,78)

# TS
nba_reg <- nba %>%
    select(5,
           14,15,16,18,19,21,24,30,34,35,37,40,43,
           49,50,51,53,54,56,59,65,69,70,72,75,78)
train <- nba_reg

set.seed(214)
train <- nba_reg[1:7006,]
test <- nba_reg[7007:8887,]
pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))

train[,-1] = predict(pre_proc_val, train[,-1])
test[,-1] = predict(pre_proc_val, test[,-1])
# summary(train)

# Model
ctrl <- trainControl(method = "cv", number = 5, verboseIter = T)
grid <- expand.grid(
    sigma = c(0.005, 0.01, 0.05),
    C = c(0.25, 0.5)
)
svm_as <- train(as ~., data = train,
                method = "svmRadial",
                trControl = ctrl,
                tuneGrid = grid)
svm_as
svm_as$resample
summary(svm_as)
plot(svm_as)

saveRDS(svm_as, "svm_as_model.rds")

as_pred <- predict(svm_as, test)

rmse <- RMSE(as_pred, test$as)
mae <- MAE(as_pred, test$as)
r2 <- R2(as_pred, test$as)

cat("RMSE: ", rmse, "MAE: ", mae, " R2: ", r2)

svm_as_imp <- rownames_to_column(varImp(svm_as)[["importance"]], "Var") %>%
    arrange(desc(Overall)) %>%
    head()


# hs

# All
# nba_reg <- nba %>%
#     select(6,9:78)

# eFG
# nba_reg <- nba %>%
#     select(6,
#            14,15,16,18,19,21,23,30,34,35,37,39,43,
#            49,50,51,53,54,56,58,65,69,70,72,74,78)

# TS
nba_reg <- nba %>%
    select(6,
           14,15,16,18,19,21,24,30,34,35,37,40,43,
           49,50,51,53,54,56,59,65,69,70,72,75,78)
train <- nba_reg

set.seed(214)
train <- nba_reg[1:7006,]
test <- nba_reg[7007:8887,]
pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))

train[,-1] = predict(pre_proc_val, train[,-1])
test[,-1] = predict(pre_proc_val, test[,-1])
# summary(train)

# Model
ctrl <- trainControl(method = "cv", number = 5, verboseIter = T)
grid <- expand.grid(
    sigma = c(0.005, 0.01, 0.05),
    C = c(0.25, 0.5)
)
svm_hs <- train(hs ~., data = train,
                method = "svmRadial",
                trControl = ctrl,
                tuneGrid = grid)
svm_hs
svm_hs$resample
summary(svm_hs)
plot(svm_hs)

saveRDS(svm_hs, "svm_hs_model.rds")

hs_pred <- predict(svm_hs, test)

rmse <- RMSE(hs_pred, test$hs)
mae <- MAE(hs_pred, test$hs)
r2 <- R2(hs_pred, test$hs)

cat("RMSE: ", rmse, "MAE: ", mae, " R2: ", r2)

svm_hs_imp <- rownames_to_column(varImp(svm_hs)[["importance"]], "Var") %>%
    arrange(desc(Overall)) %>%
    head()


### Neural Net ----
# Win

# All
# nba_cla <- nba %>%
#     select(8,9:78)

# eFG
# nba_cla <- nba %>%
#     select(8,
#            14,15,16,18,19,21,23,30,34,35,37,39,43,
#            49,50,51,53,54,56,58,65,69,70,72,74,78)

# TS
nba_cla <- nba %>%
    select(8,
           14,15,16,18,19,21,24,30,34,35,37,40,43,
           49,50,51,53,54,56,59,65,69,70,72,75,78)
train <- nba_cla

set.seed(214)
train <- nba_cla[1:7006,]
test <- nba_cla[7007:8887,]
pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))

train[,-1] = predict(pre_proc_val, train[,-1])
test[,-1] = predict(pre_proc_val, test[,-1])
# summary(train)

# Model
ctrl <- trainControl(method = "cv", number = 5, verboseIter = T, 
                     classProbs = T, summaryFunction = twoClassSummary)
grid <- expand.grid(
    decay = c(0.75, 0.5, 0.1, 1e-2, 1e-3, 1e-4, 1e-5),
    size = c(1, 3, 5, 10)
)
nn_win <- train(as.factor(result) ~., data = train,
                method = "nnet",
                metric = "ROC",
                trControl = ctrl,
                tuneGrid = grid)
nn_win
nn_win$resample
nn_win$results
summary(nn_win)
plot(nn_win)
confusionMatrix(nn_win)

saveRDS(nn_win, "nn_win_model.rds")

win_pred <- predict(nn_win, test, type = "prob")
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

nn_win_imp <- rownames_to_column(varImp(nn_win)[["importance"]], "Var") %>%
    arrange(desc(Overall)) %>%
    head()


# as

# All
# nba_reg <- nba %>%
#     select(5,9:78)

# eFG
# nba_reg <- nba %>%
#     select(5,
#            14,15,16,18,19,21,23,30,34,35,37,39,43,
#            49,50,51,53,54,56,58,65,69,70,72,74,78)

# TS
nba_reg <- nba %>%
    select(5,
           14,15,16,18,19,21,24,30,34,35,37,40,43,
           49,50,51,53,54,56,59,65,69,70,72,75,78)
train <- nba_reg

set.seed(214)
train <- nba_reg[1:7006,]
test <- nba_reg[7007:8887,]
pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))

train[,-1] = predict(pre_proc_val, train[,-1])
test[,-1] = predict(pre_proc_val, test[,-1])
# summary(train)

# Model
ctrl <- trainControl(method = "cv", number = 5, verboseIter = T)
grid <- expand.grid(
    decay = c(0.75, 0.5, 0.1, 1e-2, 1e-3, 1e-4, 1e-5, 1e-6, 1e-7),
    size = c(1, 3, 5, 10, 20)
)
nn_as <- train(as ~., data = train,
               method = "nnet",
               trControl = ctrl,
               tuneGrid = grid,
               linout = 1)
nn_as
nn_as$resample
summary(nn_as)
plot(nn_as)

saveRDS(nn_as, "nn_as_model.rds")

as_pred <- predict(nn_as, test)

rmse <- RMSE(as_pred, test$as)
mae <- MAE(as_pred, test$as)
r2 <- R2(as_pred, test$as)

cat("RMSE: ", rmse, "MAE: ", mae, " R2: ", r2)

nn_as_imp <- rownames_to_column(varImp(nn_as)[["importance"]], "Var") %>%
    arrange(desc(Overall)) %>%
    head()


# hs

# All
# nba_reg <- nba %>%
#     select(6,9:78)

# eFG
# nba_reg <- nba %>%
#     select(6,
#            14,15,16,18,19,21,23,30,34,35,37,39,43,
#            49,50,51,53,54,56,58,65,69,70,72,74,78)

# TS
nba_reg <- nba %>%
    select(6,
           14,15,16,18,19,21,24,30,34,35,37,40,43,
           49,50,51,53,54,56,59,65,69,70,72,75,78)
train <- nba_reg

set.seed(214)
train <- nba_reg[1:7006,]
test <- nba_reg[7007:8887,]
pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))

train[,-1] = predict(pre_proc_val, train[,-1])
test[,-1] = predict(pre_proc_val, test[,-1])
# summary(train)

# Model
ctrl <- trainControl(method = "cv", number = 5, verboseIter = T)
grid <- expand.grid(
    decay = c(0.75, 0.5, 0.1, 1e-2, 1e-3, 1e-4, 1e-5, 1e-6, 1e-7),
    size = c(1, 3, 5, 10, 20)
)
nn_hs <- train(hs ~., data = train,
               method = "nnet",
               trControl = ctrl,
               tuneGrid = grid,
               linout = 1)
nn_hs
nn_hs$resample
summary(nn_hs)
plot(nn_hs)

saveRDS(nn_hs, "nn_hs_model.rds")

hs_pred <- predict(nn_hs, test)

rmse <- RMSE(hs_pred, test$hs)
mae <- MAE(hs_pred, test$hs)
r2 <- R2(hs_pred, test$hs)

cat("RMSE: ", rmse, "MAE: ", mae, " R2: ", r2)

nn_hs_imp <- rownames_to_column(varImp(nn_hs)[["importance"]], "Var") %>%
    arrange(desc(Overall)) %>%
    head()


### XGBoost ----
# Win

# All
# nba_cla <- nba %>%
#     select(8,9:78)

# eFG
# nba_cla <- nba %>%
#     select(8,
#            14,15,16,18,19,21,23,30,34,35,37,39,43,
#            49,50,51,53,54,56,58,65,69,70,72,74,78)

# TS
nba_cla <- nba %>%
    select(8,
           14,15,16,18,19,21,24,30,34,35,37,40,43,
           49,50,51,53,54,56,59,65,69,70,72,75,78)
train <- nba_cla

set.seed(214)
train <- nba_cla[1:7006,]
test <- nba_cla[7007:8887,]
pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))

train[,-1] = predict(pre_proc_val, train[,-1])
test[,-1] = predict(pre_proc_val, test[,-1])
# summary(train)

# Model
ctrl <- trainControl(method = "cv", number = 5, verboseIter = T, 
                     classProbs = T, summaryFunction = twoClassSummary)
grid <- expand.grid(
    nrounds = seq(300, 800, 100),
    eta = c(0.015, 0.025, 0.035),
    max_depth = c(1,2,3),
    gamma = c(1,2,3,4),
    colsample_bytree = seq(0.5, 0.9, 0.1),
    min_child_weight = 1,
    subsample = seq(0.5, 0.8, 0.1)
)
xgb_win <- train(as.factor(result) ~., data = train,
                 method = "xgbTree",
                 metric = "ROC",
                 trControl = ctrl,
                 tuneGrid = grid)
xgb_win
xgb_win$resample
xgb_win$results
summary(xgb_win)
plot(xgb_win)
confusionMatrix(xgb_win)

saveRDS(xgb_win, "xgb_win_model.rds")

win_pred <- predict(xgb_win, test, type = "prob")
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

xgb_win_imp <- rownames_to_column(varImp(xgb_win)[["importance"]], "Var") %>%
    arrange(desc(Overall)) %>%
    head()


# as

# All
# nba_reg <- nba %>%
#     select(5,9:78)

# eFG
# nba_reg <- nba %>%
#     select(5,
#            14,15,16,18,19,21,23,30,34,35,37,39,43,
#            49,50,51,53,54,56,58,65,69,70,72,74,78)

# TS
nba_reg <- nba %>%
    select(5,
           14,15,16,18,19,21,24,30,34,35,37,40,43,
           49,50,51,53,54,56,59,65,69,70,72,75,78)
train <- nba_reg

set.seed(214)
train <- nba_reg[1:7006,]
test <- nba_reg[7007:8887,]
pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))

train[,-1] = predict(pre_proc_val, train[,-1])
test[,-1] = predict(pre_proc_val, test[,-1])
# summary(train)

# Model
ctrl <- trainControl(method = "cv", number = 5, verboseIter = T)
grid <- expand.grid(
    nrounds = seq(300,800,50),
    eta = c(0.015, 0.025, 0.035),
    max_depth = c(1,2,3),
    gamma = c(1,2,3,4),
    colsample_bytree = seq(0.5, 0.9, 0.1),
    min_child_weight = 1,
    subsample = seq(0.5, 0.8, 0.1)
)
xgb_as <- train(as ~., data = train,
                method = "xgbTree",
                trControl = ctrl,
                tuneGrid = grid)
xgb_as
xgb_as$resample
summary(xgb_as)
plot(xgb_as)

saveRDS(xgb_as, "xgb_as_model.rds")

as_pred <- predict(xgb_as, test)

rmse <- RMSE(as_pred, test$as)
mae <- MAE(as_pred, test$as)
r2 <- R2(as_pred, test$as)

cat("RMSE: ", rmse, "MAE: ", mae, " R2: ", r2)

xgb_as_imp <- rownames_to_column(varImp(xgb_as)[["importance"]], "Var") %>%
    arrange(desc(Overall)) %>%
    head()


# hs

# All
# nba_reg <- nba %>%
#     select(6,9:78)

# eFG
# nba_reg <- nba %>%
#     select(6,
#            14,15,16,18,19,21,23,30,34,35,37,39,43,
#            49,50,51,53,54,56,58,65,69,70,72,74,78)

# TS
nba_reg <- nba %>%
    select(6,
           14,15,16,18,19,21,24,30,34,35,37,40,43,
           49,50,51,53,54,56,59,65,69,70,72,75,78)
train <- nba_reg

set.seed(214)
train <- nba_reg[1:7006,]
test <- nba_reg[7007:8887,]
pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))

train[,-1] = predict(pre_proc_val, train[,-1])
test[,-1] = predict(pre_proc_val, test[,-1])
# summary(train)

# Model
ctrl <- trainControl(method = "cv", number = 5, verboseIter = T)
grid <- expand.grid(
    nrounds = seq(300,800,50),
    eta = c(0.015, 0.025, 0.035),
    max_depth = c(1,2,3),
    gamma = c(1,2,3,4),
    colsample_bytree = seq(0.5, 0.9, 0.1),
    min_child_weight = 1,
    subsample = seq(0.5, 0.8, 0.1)
)
xgb_hs <- train(hs ~., data = train,
                method = "xgbTree",
                trControl = ctrl,
                tuneGrid = grid)
xgb_hs
xgb_hs$resample
summary(xgb_hs)
plot(xgb_hs)

saveRDS(xgb_hs, "xgb_hs_model.rds")

hs_pred <- predict(xgb_hs, test)

rmse <- RMSE(hs_pred, test$hs)
mae <- MAE(hs_pred, test$hs)
r2 <- R2(hs_pred, test$hs)

cat("RMSE: ", rmse, "MAE: ", mae, " R2: ", r2)

xgb_hs_imp <- rownames_to_column(varImp(xgb_hs)[["importance"]], "Var") %>%
    arrange(desc(Overall)) %>%
    head()


### Visualizations----
# InformationValue::sensitivity(actuals=test$Win, predictedScores=predictions)
# InformationValue::specificity(actuals=test$Win, predictedScores=predictions)
# 
# InformationValue::precision(actuals=test$Win, predictedScores=predictions)
# InformationValue::npv(actuals=test$Win, predictedScores=predictions)
# 
# InformationValue::youdensIndex(actuals=test$Win, predictedScores=predictions)
# InformationValue::misClassError(actuals=test$Win, predictedScores=predictions)
# 
# InformationValue::Concordance(actuals=test$Win, predictedScores=predictions)
# InformationValue::somersD(actuals=test$Win, predictedScores=predictions)
# 
# InformationValue::ks_stat(actuals=test$Win, predictedScores=predictions)
# InformationValue::ks_plot(actuals=test$Win, predictedScores=predictions)
# 
# InformationValue::optimalCutoff(actuals=test$Win, predictedScores=predictions) # returns cutoff that gives minimum misclassification error
# InformationValue::optimalCutoff(actuals=test$Win, predictedScores=predictions, optimiseFor = "Both")  # returns cutoff that gives maximum of Youden's J Index)


### Calibration ----
# log_mod <- rms::lrm(Win ~., data = train, x=T, y=T)
# cal <- rms::calibrate(log_mod)
# plot(cal, legend=F)
# legend(x=.6, y=.6, legend=c("Apparent", "Bias-corrected", "Ideal"), 
#        lty=c(3, 1, 2), bty="n")
# 
# lin_mod <- rms::ols(Margin ~., data = train, x=T, y=T)
# cal <- rms::calibrate(lin_mod)
# plot(cal, legend=T)


# ### Ridge Regression ----
# # Win
# 
# # All
# # nba_cla <- nba %>%
# #     select(8,9:78)
# 
# # eFG
# # nba_cla <- nba %>%
# #     select(8,
# #            14,15,16,18,19,21,23,30,34,35,37,39,43,
# #            49,50,51,53,54,56,58,65,69,70,72,74,78)
# 
# # TS
# # nba_cla <- nba %>%
# #     select(8,
# #            14,15,16,18,19,21,24,30,34,35,37,40,43,
# #            49,50,51,53,54,56,59,65,69,70,72,75,78)
# 
# set.seed(214)
# train <- nba_cla[1:7006,]
# test <- nba_cla[7007:8887,]
# pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))
# 
# train[,-1] = predict(pre_proc_val, train[,-1])
# test[,-1] = predict(pre_proc_val, test[,-1])
# # summary(train)
# 
# # Model
# lambdas <- 10^seq(2, -3, by = -.1)
# 
# ridge_win <- glmnet(as.matrix(train[-1]), train$result, alpha = 0,
#                     lambda = lambdas, family = "binomial")
# 
# par(mfrow = c(1, 2))
# plot(ridge_win)
# plot(ridge_win, xvar = "lambda", label = TRUE)
# summary(ridge_win)
# 
# ridge_win_cv <- cv.glmnet(as.matrix(train[-1]), train$result, alpha = 0, 
#                           lambda = lambdas, family = 'binomial')
# ridge_win_cv
# plot(ridge_win_cv)
# tidy(ridge_win_cv)
# 
# optimal_lambda <- ridge_win_cv$lambda.min
# optimal_lambda
# 
# win_pred <- predict(ridge_win, s = optimal_lambda, as.matrix(test[-1]), type = "response")
# confusionMatrix(as.factor(test$Win), as.factor(ifelse(win_pred > 0.5, 1, 0)))
# 
# Y <- as.numeric(win_pred)
# N <- 1 - Y
# obs <- factor(ifelse(test$result == 1, "Y", "N"))
# pred <- factor(ifelse(Y > 0.5, "Y", "N"))
# obs_pred <- data.frame(obs = obs,
#                        pred = pred,
#                        N = N, 
#                        Y = Y,
#                        act = ifelse(obs == "Y", 1, 0))
# 
# twoClassSummary(obs_pred, lev = levels(obs))
# prSummary(obs_pred, lev = levels(obs))
# InformationValue::plotROC(obs_pred$act, obs_pred$Y, returnSensitivityMat = T)
# 
# obs_pred %>%
#     metrics(obs, pred)
# obs_pred %>%
#     roc_auc(obs, N)
# 
# 
# # as
# 
# # All
# # nba_reg <- nba %>%
# #     select(5,9:78)
# 
# # eFG
# # nba_reg <- nba %>%
# #     select(5,
# #            14,15,16,18,19,21,23,30,34,35,37,39,43,
# #            49,50,51,53,54,56,58,65,69,70,72,74,78)
# 
# # TS
# # nba_reg <- nba %>%
# #     select(5,
# #            14,15,16,18,19,21,24,30,34,35,37,40,43,
# #            49,50,51,53,54,56,59,65,69,70,72,75,78)
# 
# set.seed(214)
# train <- nba_reg[1:7006,]
# test <- nba_reg[7007:8887,]
# pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))
# 
# train[,-1] = predict(pre_proc_val, train[,-1])
# test[,-1] = predict(pre_proc_val, test[,-1])
# # summary(train)
# 
# # Model
# lambdas <- 10^seq(2, -3, by = -.1)
# ridge_as <- glmnet(as.matrix(train[-1]), train$as, alpha = 0, family = 'gaussian', lambda = lambdas)
# 
# par(mfrow = c(1, 2))
# plot(ridge_as)
# plot(ridge_as, xvar = "lambda", label = TRUE)
# summary(ridge_as)
# 
# ridge_as_cv <- cv.glmnet(as.matrix(train[-1]), train$as, alpha = 0, lambda = lambdas)
# 
# ridge_as_cv
# plot(ridge_as_cv)
# tidy(ridge_as_cv) 
# 
# optimal_lambda <- ridge_as_cv$lambda.min
# optimal_lambda
# 
# as_pred <- predict(ridge_as, s = optimal_lambda, as.matrix(test[-1]))
# 
# rmse <- RMSE(as_pred, test$as)
# mae <- MAE(as_pred, test$as)
# r2 <- R2(as_pred, test$as)
# 
# cat("RMSE: ", rmse, "MAE: ", mae, " R2: ", r2)
# 
# 
# # hs
# 
# # All
# # nba_reg <- nba %>%
# #     select(6,9:78)
# 
# # eFG
# # nba_reg <- nba %>%
# #     select(6,
# #            14,15,16,18,19,21,23,30,34,35,37,39,43,
# #            49,50,51,53,54,56,58,65,69,70,72,74,78)
# 
# # TS
# # nba_reg <- nba %>%
# #     select(6,
# #            14,15,16,18,19,21,24,30,34,35,37,40,43,
# #            49,50,51,53,54,56,59,65,69,70,72,75,78)
# 
# set.seed(214)
# train <- nba_reg[1:7006,]
# test <- nba_reg[7007:8887,]
# pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))
# 
# train[,-1] = predict(pre_proc_val, train[,-1])
# test[,-1] = predict(pre_proc_val, test[,-1])
# # summary(train)
# 
# # Model
# lambdas <- 10^seq(2, -3, by = -.1)
# ridge_hs <- glmnet(as.matrix(train[-1]), train$hs, alpha = 0, family = 'gaussian', lambda = lambdas)
# 
# par(mfrow = c(1, 2))
# plot(ridge_hs)
# plot(ridge_hs, xvar = "lambda", label = TRUE)
# summary(ridge_hs)
# 
# ridge_hs_cv <- cv.glmnet(as.matrix(train[-1]), train$hs, alpha = 0, lambda = lambdas)
# ridge_hs_cv
# plot(ridge_hs_cv)
# tidy(ridge_hs_cv) 
# 
# optimal_lambda <- ridge_hs_cv$lambda.min
# optimal_lambda
# 
# hs_pred <- predict(ridge_hs, s = optimal_lambda, as.matrix(test[-1]))
# 
# rmse <- RMSE(hs_pred, test$hs)
# mae <- MAE(hs_pred, test$hs)
# r2 <- R2(hs_pred, test$hs)
# 
# cat("RMSE: ", rmse, "MAE: ", mae, " R2: ", r2)
# 
# 
# ### Lasso Regression ----
# # Win
# 
# # All
# # nba_cla <- nba %>%
# #     select(8,9:78)
# 
# # eFG
# # nba_cla <- nba %>%
# #     select(8,
# #            14,15,16,18,19,21,23,30,34,35,37,39,43,
# #            49,50,51,53,54,56,58,65,69,70,72,74,78)
# 
# # TS
# # nba_cla <- nba %>%
# #     select(8,
# #            14,15,16,18,19,21,24,30,34,35,37,40,43,
# #            49,50,51,53,54,56,59,65,69,70,72,75,78)
# 
# set.seed(214)
# train <- nba_cla[1:7006,]
# test <- nba_cla[7007:8887,]
# pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))
# 
# train[,-1] = predict(pre_proc_val, train[,-1])
# test[,-1] = predict(pre_proc_val, test[,-1])
# # summary(train)
# 
# # Model
# lambdas <- 10^seq(2, -3, by = -.1)
# 
# lasso_win <- glmnet(as.matrix(train[-1]), train$Win, alpha = 1,
#                     lambda = lambdas, family = "binomial")
# 
# par(mfrow = c(1, 2))
# plot(lasso_win)
# plot(lasso_win, xvar = "lambda", label = TRUE)
# summary(lasso_win)
# 
# lasso_win_cv <- cv.glmnet(as.matrix(train[-1]), train$Win, alpha = 1, 
#                           lambda = lambdas, family = 'binomial')
# lasso_win_cv
# plot(lasso_win_cv)
# tidy(lasso_win_cv)
# 
# optimal_lambda <- lasso_win_cv$lambda.min
# optimal_lambda
# 
# win_pred <- predict(lasso_win, s = optimal_lambda, as.matrix(test[-1]), type = "response")
# confusionMatrix(as.factor(test$Win), as.factor(ifelse(win_pred > 0.5, 1, 0)))
# 
# Y <- as.numeric(win_pred)
# N <- 1 - Y
# obs <- factor(ifelse(test$Win == 1, "Y", "N"))
# pred <- factor(ifelse(Y > 0.5, "Y", "N"))
# obs_pred <- data.frame(obs = obs,
#                        pred = pred,
#                        N = N, 
#                        Y = Y,
#                        act = ifelse(obs == "Y", 1, 0))
# 
# twoClassSummary(obs_pred, lev = levels(obs))
# prSummary(obs_pred, lev = levels(obs))
# InformationValue::plotROC(obs_pred$act, obs_pred$Y, returnSensitivityMat = T)
# 
# obs_pred %>%
#     metrics(obs, pred)
# obs_pred %>%
#     roc_auc(obs, N)
# 
# # as
# 
# # All
# # nba_reg <- nba %>%
# #     select(5,9:78)
# 
# # eFG
# # nba_reg <- nba %>%
# #     select(5,
# #            14,15,16,18,19,21,23,30,34,35,37,39,43,
# #            49,50,51,53,54,56,58,65,69,70,72,74,78)
# 
# # TS
# # nba_reg <- nba %>%
# #     select(5,
# #            14,15,16,18,19,21,24,30,34,35,37,40,43,
# #            49,50,51,53,54,56,59,65,69,70,72,75,78)
# 
# set.seed(214)
# train <- nba_reg[1:7006,]
# test <- nba_reg[7007:8887,]
# pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))
# 
# train[,-1] = predict(pre_proc_val, train[,-1])
# test[,-1] = predict(pre_proc_val, test[,-1])
# # summary(train)
# 
# # Model
# lambdas <- 10^seq(2, -3, by = -.1)
# lasso_as <- glmnet(as.matrix(train[-1]), train$as, alpha = 1, family = 'gaussian', lambda = lambdas)
# 
# par(mfrow = c(1, 2))
# plot(lasso_as)
# plot(lasso_as, xvar = "lambda", label = TRUE)
# summary(lasso_as)
# 
# lasso_as_cv <- cv.glmnet(as.matrix(train[-1]), train$as, alpha = 1, lambda = lambdas)
# lasso_as_cv
# plot(lasso_as_cv)
# tidy(lasso_as_cv) 
# 
# optimal_lambda <- lasso_as_cv$lambda.min
# optimal_lambda
# 
# as_pred <- predict(lasso_as, s = optimal_lambda, as.matrix(test[-1]))
# 
# rmse <- RMSE(as_pred, test$as)
# mae <- MAE(as_pred, test$as)
# r2 <- R2(as_pred, test$as)
# 
# cat("RMSE: ", rmse, "MAE: ", mae, " R2: ", r2)
# 
# # hs
# 
# # All
# # nba_reg <- nba %>%
# #     select(6,9:78)
# 
# # eFG
# # nba_reg <- nba %>%
# #     select(6,
# #            14,15,16,18,19,21,23,30,34,35,37,39,43,
# #            49,50,51,53,54,56,58,65,69,70,72,74,78)
# 
# # TS
# # nba_reg <- nba %>%
# #     select(6,
# #            14,15,16,18,19,21,24,30,34,35,37,40,43,
# #            49,50,51,53,54,56,59,65,69,70,72,75,78)
# 
# set.seed(214)
# train <- nba_reg[1:7006,]
# test <- nba_reg[7007:8887,]
# pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))
# 
# train[,-1] = predict(pre_proc_val, train[,-1])
# test[,-1] = predict(pre_proc_val, test[,-1])
# # summary(train)
# 
# # Model
# lambdas <- 10^seq(2, -3, by = -.1)
# lasso_hs <- glmnet(as.matrix(train[-1]), train$hs, alpha = 1, family = 'gaussian', lambda = lambdas)
# 
# par(mfrow = c(1, 2))
# plot(lasso_hs)
# plot(lasso_hs, xvar = "lambda", label = TRUE)
# summary(lasso_hs)
# 
# lasso_hs_cv <- cv.glmnet(as.matrix(train[-1]), train$hs, alpha = 1, lambda = lambdas)
# lasso_hs_cv
# plot(lasso_hs_cv)
# tidy(lasso_hs_cv) 
# 
# optimal_lambda <- lasso_hs_cv$lambda.min
# optimal_lambda
# 
# hs_pred <- predict(lasso_hs, s = optimal_lambda, as.matrix(test[-1]))
# 
# rmse <- RMSE(hs_pred, test$hs)
# mae <- MAE(hs_pred, test$hs)
# r2 <- R2(hs_pred, test$hs)
# 
# cat("RMSE: ", rmse, "MAE: ", mae, " R2: ", r2)