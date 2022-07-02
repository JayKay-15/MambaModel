if (!require("pacman")) install.packages("pacman"); library(pacman)
pacman::p_load(tidyverse, readxl, na.tools, caTools, Amelia, lubridate, hms,
               ggthemes, ggrepel, ggimage, XML, RCurl, openxlsx,
               rvest, nflfastR, nbastatR, nbaTools, data.table,
               here, skimr, janitor, SimDesign, zoo, future,
               corrgram, corrplot, tidymodels, broom, ggfortify)
library(glmnet)
library(caret)
library(InformationValue)
library(class)
# nortest, multcomp, agricolae, coin, DTK, mutoss)

# install.packages("devtools")
# library(devtools)
# devtools::install_github("abresler/nbastatR")
# devtools::install_github("ccagrawal/nbaTools")

rm(list=ls())
# nba <- read_xlsx("../Jesse/Documents/MyStuff/NBA Betting/NBAdb/NBAdb1722.xlsx")
# setwd("/Users/Jesse/Documents/MyStuff/NBA Analysis")

nba <- read_xlsx("/Users/Jesse/Documents/MyStuff/NBA Betting/NBAdb/NBAdb1722_noadj.xlsx")
nba <- read_xlsx("/Users/Jesse/Documents/MyStuff/NBA Betting/NBAdb/NBAdb1722_oneadj.xlsx")
# nba <- read_xlsx("/Users/Jesse/Documents/MyStuff/NBA Betting/NBAdb/NBAdb1722_twoadj.xlsx")
# nba <- read_xlsx("/Users/Jesse/Documents/MyStuff/NBA Betting/NBAdb/NBAdb1722.xlsx")

    
##### Kendall - Linear Model #####

# nba_lin <- nba %>%
#     select(7,13:16,18:22,42:43,48:51,53:57,77:78,23,58)
# nba_lin <- nba %>%
#     select(7,13:16,18:22,42:43,48:51,53:57,77:78,24,59)
nba_lin <- nba %>%
    select(7,14,15,16,18,19,21,23,30,34,35,37,39,43,
             49,50,51,53,54,56,58,65,69,70,72,74,78)

#################################################################################
#### https://www.machinelearningplus.com/machine-learning/feature-selection/ ####
#################################################################################

## 1. Boruta ***
library(Boruta)

# Perform Boruta search
boruta_output <- Boruta(Margin ~ ., data=nba_lin, doTrace=2, maxRuns = 15) 

# Get significant variables including tentatives
boruta_signif <- getSelectedAttributes(boruta_output, withTentative = TRUE)
print(boruta_signif)  

# Do a tentative rough fix
roughFixMod <- TentativeRoughFix(boruta_output)
boruta_signif <- getSelectedAttributes(roughFixMod)
print(boruta_signif)

# Variable Importance Scores
imps <- attStats(roughFixMod)
imps2 = imps[imps$decision != 'Rejected', c('meanImp', 'decision')]
head(imps2[order(-imps2$meanImp), ])  # descending sort

# Plot variable importance
plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance") 


## 2. Variable Importance from Machine Learning Algorithms - use short list
library(caret)

# Train an rpart model and compute variable importance
set.seed(100)
rPartMod <- train(Margin ~ ., data=nba_lin, method="rpart")
rpartImp <- varImp(rPartMod)
print(rpartImp)

# Train an RRF model and compute variable importance.
set.seed(100)
rrfMod <- train(Margin ~ ., data=nba_lin, method="RRF")
rrfImp <- varImp(rrfMod, scale=F)
rrfImp

plot(rrfImp, top = 20, main='Variable Importance')



## 3. Lasso Regression ***
library(glmnet)

x <- as.matrix(nba_lin[,-1]) # all X vars
y <- as.double(as.matrix(nba_lin[, 1])) # Only Class

# Fit the LASSO model (Lasso: Alpha = 1)
set.seed(100)
cv.lasso <- cv.glmnet(x, y, family='gaussian', alpha=1, type.measure='mse')

# Results
plot(cv.lasso)

# plot(cv.lasso$glmnet.fit, xvar="lambda", label=TRUE)
cat('Min Lambda: ', cv.lasso$lambda.min, '\n 1Sd Lambda: ', cv.lasso$lambda.1se)
df_coef <- round(as.matrix(coef(cv.lasso, s=cv.lasso$lambda.min)), 2)

# See all contributing variables
df_coef[df_coef[, 1] != 0, ]



## 4. Step wise Forward and Backward Selection ***
# Step 1: Define base intercept only model
base.mod <- lm(Margin ~ 1 , data=nba_lin)  

# Step 2: Full model with all predictors
all.mod <- lm(Margin ~ . , data= nba_lin) 

# Step 3: Perform step-wise algorithm. direction='both' implies both forward and backward stepwise
stepMod <- stats::step(base.mod, scope = list(lower = base.mod, upper = all.mod), direction = "both", trace = 0, steps = 1000)  

# Step 4: Get the shortlisted variable.
shortlistedVars <- names(unlist(stepMod[[1]])) 
shortlistedVars <- shortlistedVars[!shortlistedVars %in% "(Intercept)"] # remove intercept

# Show
print(shortlistedVars)


## 5. Relative Importance from Linear Regression - bootsub is long computing
library(relaimpo)

# Build linear regression model
lmMod <- lm(Margin~., data=nba_lin)

# calculate relative importance
relImportance <- calc.relimp(lmMod, type = "lmg", rela = F)  

# Sort
cat('Relative Importances: \n')
sort(round(relImportance$lmg, 3), decreasing=TRUE)


bootsub <- boot.relimp(Margin~., data=nba_lin,
                       b = 1000, type = 'lmg', rank = TRUE, diff = TRUE)

plot(booteval.relimp(bootsub, level=.95))


## 6. Recursive Feature Elimination (RFE) - long computing
library(caret)

set.seed(100)
options(warn=-1)

subsets <- c(1:5, 10, 15, 18)

ctrl <- rfeControl(functions = rfFuncs,
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)

lmProfile <- rfe(x=nba_lin[,-1], y=nba_lin$Margin,
                 sizes = subsets,
                 rfeControl = ctrl)

lmProfile



## 7. Genetic Algorithm - long computing
# Define control function
ga_ctrl <- gafsControl(functions = rfGA,  # another option is `caretGA`.
                       method = "cv",
                       repeats = 3)

# Genetic Algorithm feature selection
set.seed(100)
ga_obj <- gafs(x=nba_lin[, -1], 
               y=nba_lin[[1]], 
               iters = 3,   # normally much higher (100+)
               gafsControl = ga_ctrl)

ga_obj

# Optimal variables
ga_obj$optVariables



## 8. Simulated Annealing - long computing
# Define control function
sa_ctrl <- safsControl(functions = rfSA,
                       method = "repeatedcv",
                       repeats = 3,
                       improve = 5) # n iterations without improvement before a reset

# Genetic Algorithm feature selection
set.seed(100)
sa_obj <- safs(x=nba_lin[, -1], 
               y=nba_lin[[1]],
               safsControl = sa_ctrl)

sa_obj

# Optimal variables
print(sa_obj$optVariables)



## 9. Information Value and Weights of Evidence - doesn't work
library(InformationValue)
inputData <- read.csv("http://rstatistics.net/wp-content/uploads/2015/09/adult.csv")
print(head(inputData))

# Choose Categorical Variables to compute Info Value.
cat_vars <- c ("WORKCLASS", "EDUCATION", "MARITALSTATUS", "OCCUPATION", "RELATIONSHIP", "RACE", "SEX", "NATIVECOUNTRY")  # get all categorical variables

# Init Output
df_iv <- data.frame(VARS=cat_vars, IV=numeric(length(cat_vars)), STRENGTH=character(length(cat_vars)), stringsAsFactors = F)  # init output dataframe

# Get Information Value for each variable
for (factor_var in factor_vars){
    df_iv[df_iv$VARS == factor_var, "IV"] <- InformationValue::IV(X=inputData[, factor_var], Y=inputData$ABOVE50K)
    df_iv[df_iv$VARS == factor_var, "STRENGTH"] <- attr(InformationValue::IV(X=inputData[, factor_var], Y=inputData$ABOVE50K), "howgood")
}

# Sort
df_iv <- df_iv[order(-df_iv$IV), ]

df_iv

WOETable(X=inputData[, 'WORKCLASS'], Y=inputData$ABOVE50K)



## 10. DALEX Package ***
library(randomForest)
library(DALEX)

# Load data
inputData <- nba_lin

# Train random forest model
rf_mod <- randomForest(Margin ~ ., data=inputData, ntree=100)
rf_mod

# Variable importance with DALEX
explained_rf <- explain(rf_mod, data=inputData, y=inputData$Margin)

# Get the variable importances
varimps = variable_importance(explained_rf, type='raw')

# variable_dropout() -> variable_importance()
# single_variable() -> variable_response()
# single_prediction() -> prediction_breakdown()

print(varimps)

plot(varimps)




##############################################################################
#### http://r-statistics.co/Variable-Selection-and-Importance-With-R.html ####
##############################################################################


## 1. Random Forest Method
library(party)

cf1 <- cforest(Margin ~ . , 
               data = nba_lin, control=cforest_unbiased(mtry=2,ntree=500)) # fit the random forest

varimp(cf1) # get variable importance, based on mean decrease in accuracy

varimp(cf1, conditional=TRUE)  # conditional=True, adjusts for correlations between predictors

varimpAUC(cf1)  # more robust towards class imbalance.



## 4. MARS
library(earth)

marsModel <- earth(Margin ~ ., data=nba_lin) # build model
ev <- evimp(marsModel) # estimate variable importance
ev

plot(ev)



##############################################################################################################
#### https://towardsdatascience.com/selecting-the-best-predictors-for-linear-regression-in-r-f385bf3d93e9 ####
##############################################################################################################

## leaps
library(leaps)

best_lin <- regsubsets(Margin~.,
                       data = nba_lin,
                       nbest = 1,      # 1 best model for each number of predictors
                       nvmax = NULL,    # NULL for no limit on number of variables
                       force.in = NULL, force.out = NULL,
                       really.big = T,
                       method = "exhaustive")

summary_best_subset <- summary(best_lin)
as.data.frame(summary_best_subset$outmat)

which.max(summary_best_subset$adjr2)
summary_best_subset$which[10,]



##### Kendall - Logistic Model #####

nba_log <- nba %>%
    select(8,14,15,16,18,19,21,23,30,34,35,37,39,43,
           49,50,51,53,54,56,58,65,69,70,72,74,78)


#################################################################################
#### https://www.machinelearningplus.com/machine-learning/feature-selection/ ####
#################################################################################


## 3. Lasso Regression
library(glmnet)

x <- as.matrix(nba_log[,-1]) # all X vars
y <- as.double(as.matrix(nba_log[, 1])) # Only Class

# Fit the LASSO model (Lasso: Alpha = 1)
set.seed(100)
cv.lasso <- cv.glmnet(x, y, family='binomial', alpha=1, parallel=TRUE, standardize=TRUE, type.measure='auc')

# Results
plot(cv.lasso)

# plot(cv.lasso$glmnet.fit, xvar="lambda", label=TRUE)
cat('Min Lambda: ', cv.lasso$lambda.min, '\n 1Sd Lambda: ', cv.lasso$lambda.1se)
df_coef <- round(as.matrix(coef(cv.lasso, s=cv.lasso$lambda.min)), 2)

# See all contributing variables
df_coef[df_coef[, 1] != 0, ]



## Information
library(Information)
library(gridExtra)

set.seed(100)

sample <- sample.split(nba_log$Win, SplitRatio = .70)
train <- nba_log %>% filter(sample == TRUE)
test <- nba_log %>% filter(sample == FALSE)

### Ranking variables using penalized IV  
IV <- create_infotables(data=train,
                   y="Win")

print(IV$Summary, row.names=FALSE)

print(IV$Tables$N_OPEN_REV_ACTS, row.names=FALSE)




##############################################################
#### http://r-statistics.co/Information-Value-With-R.html ####
##############################################################
library(InformationValue)

set.seed(100)
sample <- sample.split(nba_log$Win, SplitRatio = .70)
train <- nba_log %>% filter(sample == TRUE)
test <- nba_log %>% filter(sample == FALSE)

win_fit <- glm(Win ~ ., data = train, family = "binomial")
summary(win_fit)

win_pred <- predict(win_fit, newdata = test, type = "response")



plotROC(actuals=test$Win, predictedScores=win_pred, returnSensitivityMat = TRUE)

InformationValue::sensitivity(actuals=test$Win, predictedScores=win_pred)
InformationValue::specificity(actuals=test$Win, predictedScores=win_pred)

InformationValue::precision(actuals=test$Win, predictedScores=win_pred)
InformationValue::npv(actuals=test$Win, predictedScores=win_pred)

InformationValue::youdensIndex(actuals=test$Win, predictedScores=win_pred)
InformationValue::misClassError(actuals=test$Win, predictedScores=win_pred)

InformationValue::Concordance(actuals=test$Win, predictedScores=win_pred)
InformationValue::somersD(actuals=test$Win, predictedScores=win_pred)    

InformationValue::ks_stat(actuals=test$Win, predictedScores=win_pred)    
InformationValue::ks_plot(actuals=test$Win, predictedScores=win_pred)    

InformationValue::optimalCutoff(actuals=test$Win, predictedScores=win_pred) # returns cutoff that gives minimum misclassification error
InformationValue::optimalCutoff(actuals=test$Win, predictedScores=win_pred, optimiseFor = "Both")  # returns cutoff that gives maximum of Youden's J Index)



## MASS
library(MASS)
step_model <- win_fit %>% MASS::stepAIC(trace = FALSE)
coef(step_model)


# plots
res_wf <- as.data.frame(residuals(win_fit))
ggplot(res_wf,aes(residuals(win_fit))) +  geom_histogram(fill='blue',alpha=0.5, bins = 30)
plot(win_fit)

anova(win_fit)
coef(win_fit)








###### Correlation #####
library(mctest)

model <- lm(Margin ~., data = nba_lin)

## all individual diagnostic measures with correlation matrix
mctest(model, type="i", corr=TRUE)

## VIF and correlation matrix with collinearity detection indication
mctest(model, type="i", method="VIF", corr=TRUE)

## both overall and individual collinearity diagnostics
mctest(model, type="b")
mctest(model, type="b", method="VIF", cor=TRUE)

## all overall and vif with correlation matrix
## VIF and CN desired threshold
## eigenvalues without intercept term
mctest(model, type="b", method="VIF", Inter=FALSE, vif=15, cn=35)

## Individual collinearity diagnostic measures in matrix of 0 or 1
mctest(model, all = TRUE)
mctest(model, method = "VIF", all = TRUE)
mctest(model, type="b", all = TRUE)




cor_mx <- cor(nba[9:78])
corrplot(cor_mx, method = "color", title = "Correlation Matrix", 
         mar=c(0,0,1,0))

cor_marg <- cor(nba_lin, nba_lin$Margin)
cor_marg <- as.matrix(cor_marg[order(cor_marg[,1], decreasing = T),]) # ordered by highest correlation
cor_marg


cor_mx <- cor(nba_log)
corrplot(cor_mx, method = "color", title = "Correlation Matrix", 
         mar=c(0,0,1,0))




setwd("/Users/Jesse/Desktop/")
wb <- createWorkbook()
addWorksheet(wb, sheetName = "cor")
writeData(wb, sheet = "cor", x = cor_mx)
saveWorkbook(wb, file = "/Users/Jesse/Desktop/cor_full2.xlsx")



###################
##### Testing #####
###################


set.seed(100)
sample <- sample.split(nba_lin$Margin, SplitRatio = .70)
train <- nba_lin %>% filter(sample == TRUE)
test <- nba_lin %>% filter(sample == FALSE)

lin_mod <- lm(Margin ~., data = train)
lin_pred <- predict(lin_mod, newdata = test)
sum((test$Margin - lin_pred)^2)/nrow(test)


# Regression diagnostics
model.diag.metrics <- augment(lin_mod)
head(model.diag.metrics)

par(mfrow = c(2, 2))
plot(lin_mod)
autoplot(lin_mod)

# Add observations indices and
# drop some columns (.se.fit, .sigma) for simplification
model.diag.metrics <- model.diag.metrics %>%
    mutate(index = 1:nrow(model.diag.metrics)) %>%
    select(index, everything(), -.sigma)
# Inspect the data
head(model.diag.metrics, 4)

# Cook's distance
plot(lin_mod, 4)
# Residuals vs Leverage
plot(lin_mod, 5)

model.diag.metrics %>%
    top_n(3, wt = .cooksd)






set.seed(100)
sample <- sample.split(nba_log$Win, SplitRatio = .70)
train <- nba_log %>% filter(sample == TRUE)
test <- nba_log %>% filter(sample == FALSE)

log_mod <- glm(Win ~., data = nba_log, family = "binomial")
log_pred <- predict(log_mod, newdata = test, type = "response")
mean(test$Win != round(log_pred,0))









### Backtesting OLS -------------------------------------------------------
if (!require("pacman")) install.packages("pacman"); library(pacman)
pacman::p_load(tidyverse, readxl, lubridate, openxlsx, nbastatR, rvest, progress, XLConnect)

Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)

rm(list=ls())
setwd("/Users/Jesse/Documents/MyStuff/NBA Betting/Backtest 2022/")

game_logs(seasons = 2022, result_types = c("team","players"))

dataGameLogsTeam <- dataGameLogsTeam %>% arrange(dateGame,idGame)
dataGameLogsTeam$dateGame <- as_date(dataGameLogsTeam$dateGame)
dataGameLogsPlayer <- dataGameLogsPlayer %>% arrange(dateGame,idGame)

master_db_all <- read_xlsx("/Users/Jesse/Documents/MyStuff/NBA Betting/NBAdb/NBAdb1722.xlsx") 

odds <- read_xlsx("/Users/Jesse/Documents/MyStuff/NBA Betting/Backtest 2022/nba odds 2021-22.xlsx") 
odds <- odds %>%
    filter(Test == 1)

# odds <- odds %>%
#     filter(Date == as_date('2022-01-31'))

dates_distinct <- odds %>%
    distinct(Date) %>%
    mutate(Stat_Date = as_date(Date - 1))

odds$Date <- as.character(odds$Date)
odds$`Stat Date` <- as.character(odds$`Stat Date`)

final_backtest <- data.frame()

#### ADJ + Weights ####

b <- 1
h <- nrow(dates_distinct)

for (b in b:h) {
    
    dates_distinct$Date <- as.character(dates_distinct$Date)
    dates_distinct$Stat_Date <- as.character(dates_distinct$Stat_Date)
    
    stats_end_gxg <- as.character(dates_distinct[b,2])
    gm_day_gxg <- as.character(dates_distinct[b,1])
    
    ### Attach game logs to itself to get all stats for each game in one row
    
    gl <- left_join(dataGameLogsTeam, dataGameLogsTeam, by = c("idGame" = "idGame", "slugTeam" = "slugOpponent"))
    
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
    
    ## Filter date if needed
    
    sched <- gl
    
    gl <- gl %>%
        filter(Date <= as_date(stats_end_gxg))
    
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
    
    ######### ROUND 2 ADJUSTMENTS ########
    
    #Joining for oppt stats
    raw_adj_home_2 <- raw_final %>%
        left_join(away_adj_round_1, by = c("opptName" = "teamName")) %>%
        left_join(., home_adj_round_1, by = c("teamName" = "teamName")) %>%
        filter(teamLoc == "H")
    
    #Joining for team stats
    raw_adj_away_2 <- raw_final %>%
        left_join(home_adj_round_1, by = c("opptName" = "teamName")) %>%
        left_join(., away_adj_round_1, by = c("teamName" = "teamName")) %>%
        filter(teamLoc == "A")
    
    raw_adj_2 <- bind_rows(raw_adj_home_2,raw_adj_away_2)
    
    #.x = oppt avg, .y = team avg, no tail = game actual
    
    raw_adj_2$FG_adj <- (raw_adj_2$FG - (raw_adj_2$oFG_adj.x - season_lg_avg$FG))
    raw_adj_2$SR2_adj <- (raw_adj_2$SR2 - (raw_adj_2$oSR2_adj.x - season_lg_avg$SR2))
    raw_adj_2$FG3_adj <- (raw_adj_2$FG3 - (raw_adj_2$oFG3_adj.x - season_lg_avg$FG3 ))
    raw_adj_2$SR3_adj <- (raw_adj_2$SR3 - (raw_adj_2$oSR3_adj.x - season_lg_avg$SR3))
    raw_adj_2$FT_adj <- (raw_adj_2$FT - (raw_adj_2$oFT_adj.x - season_lg_avg$FT))
    raw_adj_2$FTR_adj <- (raw_adj_2$FTR - (raw_adj_2$oFTR_adj.x - season_lg_avg$FTR))
    raw_adj_2$ORB_adj <- (raw_adj_2$ORB + (raw_adj_2$oDRB_adj.x - season_lg_avg$DRB))
    raw_adj_2$DRB_adj <- (raw_adj_2$DRB - (raw_adj_2$oORB_adj.x - season_lg_avg$ORB))
    raw_adj_2$TRB_adj <- (raw_adj_2$TRB + (raw_adj_2$oTRB_adj.x - season_lg_avg$TRB))
    raw_adj_2$AST_adj <- (raw_adj_2$AST - (raw_adj_2$oAST_adj.x - season_lg_avg$AST))
    raw_adj_2$TOV_adj <- (raw_adj_2$TOV - (raw_adj_2$oTOV_adj.x - season_lg_avg$TOV))
    raw_adj_2$STL_adj <- (raw_adj_2$STL - (raw_adj_2$oSTL_adj.x - season_lg_avg$STL))
    raw_adj_2$BLK_adj <- (raw_adj_2$BLK - (raw_adj_2$oBLK_adj.x - season_lg_avg$BLK))
    raw_adj_2$PF_adj <- (raw_adj_2$PF - (raw_adj_2$oPF_adj.x - season_lg_avg$PF)) 
    raw_adj_2$eFG_adj <- (raw_adj_2$eFG - (raw_adj_2$oeFG_adj.x - season_lg_avg$eFG))
    raw_adj_2$TS_adj <- (raw_adj_2$TS - (raw_adj_2$oTS_adj.x - season_lg_avg$TS))
    raw_adj_2$ExpPace <- (season_lg_avg$Pace + (raw_adj_2$Pace - season_lg_avg$Pace) + 
                              (raw_adj_2$Pace_adj.x - season_lg_avg$Pace))
    raw_adj_2$PaceDiff <- (raw_adj_2$Pace - raw_adj_2$ExpPace)
    raw_adj_2$PaceR <- (raw_adj_2$Pace_adj.y / (raw_adj_2$Pace_adj.y + raw_adj_2$Pace_adj.x))
    raw_adj_2$oPaceR <- (raw_adj_2$Pace_adj.x / (raw_adj_2$Pace_adj.y + raw_adj_2$Pace_adj.x))
    raw_adj_2$Pace_adj <- (raw_adj_2$Pace_adj.y + (raw_adj_2$PaceDiff * raw_adj_2$PaceR))
    raw_adj_2$ORtg_adj <- (raw_adj_2$ORtg - (raw_adj_2$DRtg_adj.x - season_lg_avg$DRtg))
    raw_adj_2$DRtg_adj <- (raw_adj_2$DRtg - (raw_adj_2$ORtg_adj.x - season_lg_avg$ORtg))
    
    raw_adj_2$oFG_adj <- (raw_adj_2$oFG - (raw_adj_2$FG_adj.y - season_lg_avg$FG))
    raw_adj_2$oSR2_adj <- (raw_adj_2$oSR2 - (raw_adj_2$SR2_adj.y - season_lg_avg$SR2))
    raw_adj_2$oFG3_adj <- (raw_adj_2$oFG3 - (raw_adj_2$FG3_adj.y - season_lg_avg$FG3 ))
    raw_adj_2$oSR3_adj <- (raw_adj_2$oSR3 - (raw_adj_2$SR3_adj.y - season_lg_avg$SR3))
    raw_adj_2$oFT_adj <- (raw_adj_2$oFT - (raw_adj_2$FT_adj.y - season_lg_avg$FT))
    raw_adj_2$oFTR_adj <- (raw_adj_2$oFTR - (raw_adj_2$FTR_adj.y - season_lg_avg$FTR))
    raw_adj_2$oORB_adj <- (raw_adj_2$oORB + (raw_adj_2$DRB_adj.y - season_lg_avg$DRB))
    raw_adj_2$oDRB_adj <- (raw_adj_2$oDRB - (raw_adj_2$ORB_adj.y - season_lg_avg$ORB))
    raw_adj_2$oTRB_adj <- (raw_adj_2$oTRB + (raw_adj_2$TRB_adj.y - season_lg_avg$TRB))
    raw_adj_2$oAST_adj <- (raw_adj_2$oAST - (raw_adj_2$AST_adj.y - season_lg_avg$AST))
    raw_adj_2$oTOV_adj <- (raw_adj_2$oTOV - (raw_adj_2$TOV_adj.y - season_lg_avg$TOV))
    raw_adj_2$oSTL_adj <- (raw_adj_2$oSTL - (raw_adj_2$STL_adj.y - season_lg_avg$STL))
    raw_adj_2$oBLK_adj <- (raw_adj_2$oBLK - (raw_adj_2$BLK_adj.y - season_lg_avg$BLK))
    raw_adj_2$oPF_adj <- (raw_adj_2$oPF - (raw_adj_2$PF_adj.y - season_lg_avg$PF)) 
    raw_adj_2$oeFG_adj <- (raw_adj_2$oeFG - (raw_adj_2$eFG_adj.y - season_lg_avg$eFG))
    raw_adj_2$oTS_adj <- (raw_adj_2$oTS - (raw_adj_2$TS_adj.y - season_lg_avg$TS))
    
    ### GROUP ROUND 2 ADJUSTMENTS
    
    home_adj_round_2 <- raw_adj_2 %>%
        select(1,2,109:124,132:147,130,131,129) %>%
        filter(teamLoc == "H") %>%
        group_by(teamName) %>%
        summarise(across(where(is.numeric), mean))
    
    away_adj_round_2 <- raw_adj_2 %>%
        select(1,2,109:124,132:147,130,131,129) %>%
        filter(teamLoc == "A") %>%
        group_by(teamName) %>%
        summarise(across(where(is.numeric), mean))
    
    ######### ROUND 3 ADJUSTMENTS ########
    
    #Joining for oppt stats
    raw_adj_home_3 <- raw_final %>%
        left_join(away_adj_round_2, by = c("opptName" = "teamName")) %>%
        left_join(., home_adj_round_2, by = c("teamName" = "teamName")) %>%
        filter(teamLoc == "H")
    
    #Joining for team stats
    raw_adj_away_3 <- raw_final %>%
        left_join(home_adj_round_2, by = c("opptName" = "teamName")) %>%
        left_join(., away_adj_round_2, by = c("teamName" = "teamName")) %>%
        filter(teamLoc == "A")
    
    raw_adj_3 <- bind_rows(raw_adj_home_3,raw_adj_away_3)
    
    #.x = oppt avg, .y = team avg, no tail = game actual
    
    raw_adj_3$FG_adj <- (raw_adj_3$FG - (raw_adj_3$oFG_adj.x - season_lg_avg$FG))
    raw_adj_3$SR2_adj <- (raw_adj_3$SR2 - (raw_adj_3$oSR2_adj.x - season_lg_avg$SR2))
    raw_adj_3$FG3_adj <- (raw_adj_3$FG3 - (raw_adj_3$oFG3_adj.x - season_lg_avg$FG3 ))
    raw_adj_3$SR3_adj <- (raw_adj_3$SR3 - (raw_adj_3$oSR3_adj.x - season_lg_avg$SR3))
    raw_adj_3$FT_adj <- (raw_adj_3$FT - (raw_adj_3$oFT_adj.x - season_lg_avg$FT))
    raw_adj_3$FTR_adj <- (raw_adj_3$FTR - (raw_adj_3$oFTR_adj.x - season_lg_avg$FTR))
    raw_adj_3$ORB_adj <- (raw_adj_3$ORB + (raw_adj_3$oDRB_adj.x - season_lg_avg$DRB))
    raw_adj_3$DRB_adj <- (raw_adj_3$DRB - (raw_adj_3$oORB_adj.x - season_lg_avg$ORB))
    raw_adj_3$TRB_adj <- (raw_adj_3$TRB + (raw_adj_3$oTRB_adj.x - season_lg_avg$TRB))
    raw_adj_3$AST_adj <- (raw_adj_3$AST - (raw_adj_3$oAST_adj.x - season_lg_avg$AST))
    raw_adj_3$TOV_adj <- (raw_adj_3$TOV - (raw_adj_3$oTOV_adj.x - season_lg_avg$TOV))
    raw_adj_3$STL_adj <- (raw_adj_3$STL - (raw_adj_3$oSTL_adj.x - season_lg_avg$STL))
    raw_adj_3$BLK_adj <- (raw_adj_3$BLK - (raw_adj_3$oBLK_adj.x - season_lg_avg$BLK))
    raw_adj_3$PF_adj <- (raw_adj_3$PF - (raw_adj_3$oPF_adj.x - season_lg_avg$PF)) 
    raw_adj_3$eFG_adj <- (raw_adj_3$eFG - (raw_adj_3$oeFG_adj.x - season_lg_avg$eFG))
    raw_adj_3$TS_adj <- (raw_adj_3$TS - (raw_adj_3$oTS_adj.x - season_lg_avg$TS))
    raw_adj_3$ExpPace <- (season_lg_avg$Pace + (raw_adj_3$Pace - season_lg_avg$Pace) + 
                              (raw_adj_3$Pace_adj.x - season_lg_avg$Pace))
    raw_adj_3$PaceDiff <- (raw_adj_3$Pace - raw_adj_3$ExpPace)
    raw_adj_3$PaceR <- (raw_adj_3$Pace_adj.y / (raw_adj_3$Pace_adj.y + raw_adj_3$Pace_adj.x))
    raw_adj_3$oPaceR <- (raw_adj_3$Pace_adj.x / (raw_adj_3$Pace_adj.y + raw_adj_3$Pace_adj.x))
    raw_adj_3$Pace_adj <- (raw_adj_3$Pace_adj.y + (raw_adj_3$PaceDiff * raw_adj_3$PaceR))
    raw_adj_3$ORtg_adj <- (raw_adj_3$ORtg - (raw_adj_3$DRtg_adj.x - season_lg_avg$DRtg))
    raw_adj_3$DRtg_adj <- (raw_adj_3$DRtg - (raw_adj_3$ORtg_adj.x - season_lg_avg$ORtg))
    
    raw_adj_3$oFG_adj <- (raw_adj_3$oFG - (raw_adj_3$FG_adj.y - season_lg_avg$FG))
    raw_adj_3$oSR2_adj <- (raw_adj_3$oSR2 - (raw_adj_3$SR2_adj.y - season_lg_avg$SR2))
    raw_adj_3$oFG3_adj <- (raw_adj_3$oFG3 - (raw_adj_3$FG3_adj.y - season_lg_avg$FG3 ))
    raw_adj_3$oSR3_adj <- (raw_adj_3$oSR3 - (raw_adj_3$SR3_adj.y - season_lg_avg$SR3))
    raw_adj_3$oFT_adj <- (raw_adj_3$oFT - (raw_adj_3$FT_adj.y - season_lg_avg$FT))
    raw_adj_3$oFTR_adj <- (raw_adj_3$oFTR - (raw_adj_3$FTR_adj.y - season_lg_avg$FTR))
    raw_adj_3$oORB_adj <- (raw_adj_3$oORB + (raw_adj_3$DRB_adj.y - season_lg_avg$DRB))
    raw_adj_3$oDRB_adj <- (raw_adj_3$oDRB - (raw_adj_3$ORB_adj.y - season_lg_avg$ORB))
    raw_adj_3$oTRB_adj <- (raw_adj_3$oTRB + (raw_adj_3$TRB_adj.y - season_lg_avg$TRB))
    raw_adj_3$oAST_adj <- (raw_adj_3$oAST - (raw_adj_3$AST_adj.y - season_lg_avg$AST))
    raw_adj_3$oTOV_adj <- (raw_adj_3$oTOV - (raw_adj_3$TOV_adj.y - season_lg_avg$TOV))
    raw_adj_3$oSTL_adj <- (raw_adj_3$oSTL - (raw_adj_3$STL_adj.y - season_lg_avg$STL))
    raw_adj_3$oBLK_adj <- (raw_adj_3$oBLK - (raw_adj_3$BLK_adj.y - season_lg_avg$BLK))
    raw_adj_3$oPF_adj <- (raw_adj_3$oPF - (raw_adj_3$PF_adj.y - season_lg_avg$PF)) 
    raw_adj_3$oeFG_adj <- (raw_adj_3$oeFG - (raw_adj_3$eFG_adj.y - season_lg_avg$eFG))
    raw_adj_3$oTS_adj <- (raw_adj_3$oTS - (raw_adj_3$TS_adj.y - season_lg_avg$TS))
    
    ### GROUP ROUND 3 ADJUSTMENTS
    
    home_adj_round_3 <- raw_adj_3 %>%
        select(1,2,109:124,132:147,130,131,129) %>%
        filter(teamLoc == "H") %>%
        group_by(teamName) %>%
        summarise(across(where(is.numeric), mean))
    
    away_adj_round_3 <- raw_adj_3 %>%
        select(1,2,109:124,132:147,130,131,129) %>%
        filter(teamLoc == "A") %>%
        group_by(teamName) %>%
        summarise(across(where(is.numeric), mean))
    
    ### Weighting Data frames ###
    
    home_uw <- raw_adj_3 %>%
        select(1,2,109:124,132:147,130,131,129) %>%
        filter(teamLoc == "H")
    
    away_uw <- raw_adj_3 %>%
        select(1,2,109:124,132:147,130,131,129) %>%
        filter(teamLoc == "A")
    
    ##### WEIGHTING - AWAY ####
    
    wt_holder_away <- data.frame()
    
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
        
        wt_df <- data.frame(act_id,FG_wt,SR2_wt,FG3_wt,SR3_wt,FT_wt,
                            FTR_wt,ORB_wt,DRB_wt,TRB_wt,AST_wt,TOV_wt,
                            STL_wt,BLK_wt,PF_wt,eFG_wt,TS_wt,oFG_wt,oSR2_wt,oFG3_wt,oSR3_wt,
                            oFT_wt,oFTR_wt,oORB_wt,oDRB_wt,oTRB_wt,oAST_wt,
                            oTOV_wt,oSTL_wt,oBLK_wt,oPF_wt,oeFG_wt,oTS_wt,
                            ORtg_wt,DRtg_wt,Pace_wt)
        
        wt_holder_away <- bind_rows(wt_holder_away,wt_df)
        
    }
    
    away_final_wt <- wt_holder_away
    
    colnames(away_final_wt) <- c("Team","FG","SR2","FG3","SR3","FT","FTR","ORB","DRB","TRB",
                                 "AST","TOV","STL","BLK","PF","eFG","TS",
                                 "oFG","oSR2","oFG3","oSR3","oFT","oFTR","oORB","oDRB","oTRB",
                                 "oAST","oTOV","oSTL","oBLK","oPF","oeFG","oTS",
                                 "ORtg","DRtg","Pace")
    
    ##### WEIGHTING - HOME ####
    
    wt_holder_home <- data.frame()
    
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
        
        wt_df <- data.frame(act_id,FG_wt,SR2_wt,FG3_wt,SR3_wt,FT_wt,
                            FTR_wt,ORB_wt,DRB_wt,TRB_wt,AST_wt,TOV_wt,
                            STL_wt,BLK_wt,PF_wt,eFG_wt,TS_wt,oFG_wt,oSR2_wt,oFG3_wt,oSR3_wt,
                            oFT_wt,oFTR_wt,oORB_wt,oDRB_wt,oTRB_wt,oAST_wt,
                            oTOV_wt,oSTL_wt,oBLK_wt,oPF_wt,oeFG_wt,oTS_wt,
                            ORtg_wt,DRtg_wt,Pace_wt)
        
        wt_holder_home <- bind_rows(wt_holder_home,wt_df)
        
    }
    
    home_final_wt <- wt_holder_home
    
    colnames(home_final_wt) <- c("Team","FG","SR2","FG3","SR3","FT","FTR","ORB","DRB","TRB",
                                 "AST","TOV","STL","BLK","PF","eFG","TS",
                                 "oFG","oSR2","oFG3","oSR3","oFT","oFTR","oORB","oDRB","oTRB",
                                 "oAST","oTOV","oSTL","oBLK","oPF","oeFG","oTS",
                                 "ORtg","DRtg","Pace")
    
    ### Run Models ###
    
    
    td <- as_date(gm_day_gxg)
    
    slate <- sched %>%
        filter(Date == td & teamLoc == 'H') %>%
        select(1,4,3)
    
    colnames(slate) <- c("Date","Away","Home")
    
    slate <- slate %>%
        mutate(across(where(is.character), str_replace_all, pattern = "Los Angeles Clippers", 
                      replacement = "LA Clippers"))
    
    ### Read in Database ###
    
    master_lin <- master_db_all %>%
        filter(Date < as_date(gm_day_gxg)) %>%
        select(7,
               14,15,16,18,19,21,23,30,34,35,37,39,43,
               49,50,51,53,54,56,58,65,69,70,72,74,78)
    master_as <- master_db_all %>%
        filter(Date < as_date(gm_day_gxg)) %>%
        select(5,
               14,15,16,18,19,21,23,30,34,35,37,39,43,
               49,50,51,53,54,56,58,65,69,70,72,74,78)
    master_hs <- master_db_all %>%
        filter(Date < as_date(gm_day_gxg)) %>%
        select(6,
               14,15,16,18,19,21,23,30,34,35,37,39,43,
               49,50,51,53,54,56,58,65,69,70,72,74,78)
    master_log <- master_db_all %>%
        filter(Date < as_date(gm_day_gxg)) %>%
        select(8,
               14,15,16,18,19,21,23,30,34,35,37,39,43,
               49,50,51,53,54,56,58,65,69,70,72,74,78)
    
    
    #### Tyra #### - Least Squares
    
    pb <- progress_bar$new(
        format = "  running model... [:bar] :percent eta: :eta",
        total = nrow(slate), clear = FALSE, width = 60, show_after = 0)
    invisible(pb$tick(0))
    
    lin_fit <- lm(Margin ~ ., data = master_lin)
    lmas_fit <- lm(AS ~ ., data = master_as)
    lmhs_fit <- lm(HS ~ ., data = master_hs)
    
    log_fit <- glm(Win ~ ., data = master_log, family = "binomial")
    
    tyra_predict <- data.frame() # Predictions frame
    
    a <- 1
    g <- nrow(slate)
    
    for (a in a:g) {
        
        slate_away <- as.character(slate[a,2])
        slate_home <- as.character(slate[a,3])
        
        tyra_away_lin <- away_final_wt %>%
            filter(Team == slate_away) %>%
            select(7,8,9,11,12,14,16,
                   23,27,28,30,32,36)
        
        tyra_home_lin <- home_final_wt %>%
            filter(Team == slate_home) %>%
            select(7,8,9,11,12,14,16,
                   23,27,28,30,32,36)
        
        tyra_away_log <- away_final_wt %>%
            filter(Team == slate_away) %>%
            select(7,8,9,11,12,14,16,
                   23,27,28,30,32,36)
        
        tyra_home_log <- home_final_wt %>%
            filter(Team == slate_home) %>%
            select(7,8,9,11,12,14,16,
                   23,27,28,30,32,36)
        
        lin_input <- cbind(tyra_away_lin, tyra_home_lin)
        
        colnames(lin_input) <- c("FTR_away", "ORB_away", "DRB_away", "AST_away", 
                                 "TOV_away", "BLK_away", "eFG_away", "oFTR_away",
                                 "oAST_away", "oTOV_away", "oBLK_away", "oeFG_away",
                                 "Pace_away",
                                 "FTR_home", "ORB_home", "DRB_home", "AST_home", 
                                 "TOV_home", "BLK_home", "eFG_home", "oFTR_home",
                                 "oAST_home", "oTOV_home", "oBLK_home", "oeFG_home",
                                 "Pace_home")
        
        log_input <- cbind(tyra_away_log, tyra_home_log)
        
        colnames(log_input) <- c("FTR_away", "ORB_away", "DRB_away", "AST_away", 
                                 "TOV_away", "BLK_away", "eFG_away", "oFTR_away",
                                 "oAST_away", "oTOV_away", "oBLK_away", "oeFG_away",
                                 "Pace_away",
                                 "FTR_home", "ORB_home", "DRB_home", "AST_home", 
                                 "TOV_home", "BLK_home", "eFG_home", "oFTR_home",
                                 "oAST_home", "oTOV_home", "oBLK_home", "oeFG_home",
                                 "Pace_home")
        
        tyra_margin <- as.numeric(predict(lin_fit, newdata = lin_input))
        tyra_ascore <- as.numeric(predict(lmas_fit, newdata = lin_input))
        tyra_hscore <- as.numeric(predict(lmhs_fit, newdata = lin_input))
        
        tyra_awin <- as.numeric(predict(log_fit, newdata = log_input, type = "response"))
        tyra_hwin <- 1 - tyra_awin
        
        holder <- slate[a,2:3]
        holder$Away_Margin <- tyra_margin
        holder$Home_Margin <- tyra_margin*-1
        holder$Away_Margin2 <- tyra_ascore - tyra_hscore
        holder$Home_Margin2 <- tyra_hscore - tyra_ascore
        holder$Away_Win <- tyra_awin
        holder$Home_Win <- tyra_hwin
        holder$Total <- tyra_ascore + tyra_hscore
        
        
        tyra_predict <- bind_rows(tyra_predict,holder)
        
        pb$tick()
        Sys.sleep(1 / nrow(slate))
        
    }
    
    tyra_predict <- tyra_predict %>%
        mutate(across(where(is.numeric), round, 3))

    tyra_predict$Model <- "Tyra Banks - Least Squares"

    
    all_models <- rbind(tyra_predict)
    
    slate <- slate %>%
        left_join(., dataGameLogsTeam, by = c("Date" = "dateGame", "Home" = "nameTeam")) %>%
        select(1:3,9)
    
    plays_a <- slate %>%
        mutate(Loc = "A") %>%
        select(4,1,5,3,2)
    colnames(plays_a) <- c("idGame", "Date", "Loc", "oppTeam", "Team")
    
    plays_a <- plays_a %>%
        left_join(tyra_predict, by = c("Team" = "Away"))
    
    plays_a <- plays_a %>%
        select(1:5, 
               7, 9, 11, 13)
    
    colnames(plays_a) <- c("idGame", "Date", "Loc", "oppTeam", "Team", 
                           "Tyra_Margin", "Tyra_Margin2", "Tyra_Win", "Tyra_Total")
    
    plays_h <- slate %>%
        mutate(Loc = "H") %>%
        select(4,1,5,2,3)
    colnames(plays_h) <- c("idGame", "Date", "Loc", "oppTeam", "Team")
    
    plays_h <- plays_h %>%
        left_join(tyra_predict, by = c("oppTeam" = "Away"))
    
    plays_h <- plays_h %>%
        select(1:5, 
               8, 10, 12, 13)
    
    colnames(plays_h) <- c("idGame", "Date", "Loc", "oppTeam", "Team", 
                           "Tyra_Margin", "Tyra_Margin2", "Tyra_Win", "Tyra_Total")
    
    plays <- bind_rows(plays_a, plays_h)
    
    plays <- plays %>%
        arrange(idGame)
    
    
    # bring in odds
    odds <- read_xlsx("/Users/Jesse/Documents/MyStuff/NBA Betting/Backtest 2022/nba odds 2021-22.xlsx")
    
    odds <- odds %>%
        mutate(across(where(is.character), str_replace_all, pattern = "L.A. Clippers", replacement = "LA Clippers"))
    
    plays <- plays %>%
        left_join(odds, by = c("Team", "Date")) %>%
        select(1:5, 12, 13, 14, 6:9)
    
    plays$Tyra_Spread_Edge <- with(plays, Tyra_Margin + Spread)
    plays$Tyra_Spread2_Edge <- with(plays, Tyra_Margin2 + Spread)
    plays$Tyra_ML_Edge <- with(plays, Tyra_Win - round((if_else(ML<0,((ML*-1)/((ML*-1)+100)),(100/(ML+100)))), 3))
    plays$Tyra_Over_Edge <- with(plays, Tyra_Total - Total)
    plays$Tyra_Under_Edge <- with(plays, Total - Tyra_Total)
    
    
    # plays[, c(29:32,62:66)] <- sapply(plays[, c(29:32,62:66)], as.numeric)
    
    final_backtest <- bind_rows(final_backtest,plays)
    
    print(paste0(round((b/h)*100,1),"%"))
    
    
    
}

detach("package:XLConnect", unload = TRUE)

fn <- "Backtest_2021_2022_ols"
u <- paste0("/Users/Jesse/Documents/MyStuff/NBA Betting/Backtest 2022/",fn,".xlsx")

wb <- createWorkbook()
addWorksheet(wb, sheetName = "Backtest")
writeData(wb, sheet = "Backtest", x = final_backtest)
saveWorkbook(wb, file = u)













### backtest results -------------------------------------------------
if (!require("pacman")) install.packages("pacman"); library(pacman)
pacman::p_load(tidyverse, readxl, lubridate, openxlsx, nbastatR, rvest, progress, XLConnect)

Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)

rm(list=ls())
setwd("/Users/Jesse/Documents/MyStuff/NBA Betting/Backtest 2022/")

##### CREATE BACKTEST RESULTS BOOK #####

game_logs(seasons = c(2019:2022), result_types = c("team","players"))

dataGameLogsTeam <- dataGameLogsTeam %>% arrange(dateGame,idGame)
dataGameLogsTeam$dateGame <- as_date(dataGameLogsTeam$dateGame)
dataGameLogsPlayer <- dataGameLogsPlayer %>% arrange(dateGame,idGame)


backtest19 <- read_xlsx("/Users/Jesse/Documents/MyStuff/NBA Betting/Backtest 2022/Backtest_2018_2019_ols.xlsx")
backtest19$Date <- as_date(backtest19$Date)

backtest20 <- read_xlsx("/Users/Jesse/Documents/MyStuff/NBA Betting/Backtest 2022/Backtest_2019_2020_ols.xlsx")
backtest20$Date <- as_date(backtest20$Date)

backtest21 <- read_xlsx("/Users/Jesse/Documents/MyStuff/NBA Betting/Backtest 2022/Backtest_2020_2021_ols.xlsx")
backtest21$Date <- as_date(backtest21$Date)

backtest22 <- read_xlsx("/Users/Jesse/Documents/MyStuff/NBA Betting/Backtest 2022/Backtest_2021_2022_ols.xlsx")
backtest22$Date <- as_date(backtest22$Date)

backtest <- bind_rows(backtest19, backtest20, backtest21, backtest22)

# backtest <- backtest %>%
#     filter(Date >= max(Date)-30)

### Attach game logs to itself to get all stats for each game in one row

gl <- left_join(dataGameLogsTeam, dataGameLogsTeam, by = c("idGame" = "idGame", "slugTeam" = "slugOpponent"))

gl <- gl %>%
    select(6,13,8,62,17,90,45)
colnames(gl) <- c("idGame", "Date", "Loc", "oppTeam", "Team", "oppScore", "Score")

results_book <- gl %>%
    arrange(idGame, Loc)

backtest <- backtest[c(1,4:17)]

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
    select(1:7, 9:11, 21:27, 12:20) %>%
    rename(oppTeam = oppTeam.x)


results_book$Tyra_Spread_Result <- with(results_book, ifelse(Tyra_Spread_Edge>0,ATS_Result,0))
results_book$Tyra_Spread2_Result <- with(results_book, ifelse(Tyra_Spread2_Edge>0,ATS_Result,0))
results_book$Tyra_ML_Result <- with(results_book, ifelse(Tyra_ML_Edge>0,ML_Result,0))
results_book$Tyra_Over_Result <- with(results_book, ifelse(Tyra_Over_Edge>0,Over_Result,0))
results_book$Tyra_Under_Result <- with(results_book, ifelse(Tyra_Under_Edge>0,Under_Result,0))



results_book2 <- results_book

fn <- "Backtest_Full_Results"
u <- paste0("/Users/Jesse/Documents/MyStuff/NBA Betting/Backtest 2022/",fn,".xlsx")

wb <- createWorkbook()
addWorksheet(wb, sheetName = "Results")
writeData(wb, sheet = "Results", x = results_book)
saveWorkbook(wb, file = u)

##### BACKTEST ANALYSIS #####

if (!require("pacman")) install.packages("pacman"); library(pacman)
pacman::p_load(tidyverse, readxl, lubridate, openxlsx, nbastatR, rvest, data.table)

backtest <- read_xlsx("/Users/Jesse/Documents/MyStuff/NBA Betting/Backtest 2022/Backtest_Full_Results.xlsx")
backtest$Date <- as_date(backtest$Date)

results_book <- bind_rows(backtest)

results_book <- results_book %>%
    filter(Date >= '2018-10-01' & Date <= '2019-05-01')

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

combos_s <- lapply(1:1, function(x) combn(c("Tyra_Spread_Edge"), x, simplify = FALSE))
combos_s <- unlist(combos_s, recursive = F)

combos_result_s <- lapply(1:1, function(x) combn(c("Tyra_Spread_Result"), x, simplify = FALSE))
combos_result_s <- unlist(combos_result_s, recursive = F)

combos_s2 <- lapply(1:1, function(x) combn(c("Tyra_Spread2_Edge"), x, simplify = FALSE))
combos_s2 <- unlist(combos_s2, recursive = F)

combos_result_s2 <- lapply(1:1, function(x) combn(c("Tyra_Spread2_Result"), x, simplify = FALSE))
combos_result_s2 <- unlist(combos_result_s2, recursive = F)

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

spread_peak_filtered <- spread_peak %>%
    mutate(num_models = rowSums(is.na(spread_peak))) %>%
    filter(num_models == 0) %>%
    select(1:4)

spread_peak_filtered <- spread_peak_filtered %>%
    pivot_longer(cols = !gameNum:ROI, names_to = "Model", values_to = "Key") %>%
    drop_na()




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
spread2_peak <- spread2_peak %>% select(1,4:6) %>% arrange(desc(Cume))

spread2_peak_filtered <- spread2_peak %>%
    mutate(num_models = rowSums(is.na(spread2_peak))) %>%
    filter(num_models == 0) %>%
    select(1:4)

spread2_peak_filtered <- spread2_peak_filtered %>%
    pivot_longer(cols = !gameNum:ROI, names_to = "Model", values_to = "Key") %>%
    drop_na()



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

ml_peak_filtered <- ml_peak %>%
    mutate(num_models = rowSums(is.na(ml_peak))) %>%
    filter(num_models == 0) %>%
    select(1:4)

ml_peak_filtered <- ml_peak_filtered %>%
    pivot_longer(cols = !gameNum:ROI, names_to = "Model", values_to = "Key") %>%
    drop_na()



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

over_peak_filtered <- over_peak %>%
    mutate(num_models = rowSums(is.na(over_peak))) %>%
    filter(num_models == 0) %>%
    select(1:4)

over_peak_filtered <- over_peak_filtered %>%
    pivot_longer(cols = !gameNum:ROI, names_to = "Model", values_to = "Key") %>%
    drop_na()


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

under_peak_filtered <- under_peak %>%
    mutate(num_models = rowSums(is.na(under_peak))) %>%
    filter(num_models == 0) %>%
    select(1:4)

under_peak_filtered <- under_peak_filtered %>%
    pivot_longer(cols = !gameNum:ROI, names_to = "Model", values_to = "Key") %>%
    drop_na()



#### Print to Excel ####

#### Creating workbook - results
detach("package:XLConnect", unload = TRUE)

fn <- "backtest__results"
u <- paste0("/Users/Jesse/Documents/MyStuff/NBA Betting/Backtest 2022/",fn,".xlsx")

wb <- createWorkbook()
addWorksheet(wb, sheetName = "Results")
writeData(wb, sheet = "Results", x = results_book)
saveWorkbook(wb, u)



#### Creating workbook - keys
detach("package:XLConnect", unload = TRUE)

fn <- "backtest_keys_2019_new"
u <- paste0("/Users/Jesse/Documents/MyStuff/NBA Betting/Backtest 2022/",fn,".xlsx")

wb <- createWorkbook()
addWorksheet(wb, sheetName = "Spread Peak")
addWorksheet(wb, sheetName = "Spread2 Peak")
addWorksheet(wb, sheetName = "ML Peak")
addWorksheet(wb, sheetName = "Over Peak")
addWorksheet(wb, sheetName = "Under Peak")
addWorksheet(wb, sheetName = "Spread Peak Filtered")
addWorksheet(wb, sheetName = "Spread2 Peak Filtered")
addWorksheet(wb, sheetName = "ML Peak Filtered")
addWorksheet(wb, sheetName = "Over Peak Filtered")
addWorksheet(wb, sheetName = "Under Peak Filtered")
writeData(wb, sheet = "Spread Peak", x = spread_peak)
writeData(wb, sheet = "Spread2 Peak", x = spread2_peak)
writeData(wb, sheet = "ML Peak", x = ml_peak)
writeData(wb, sheet = "Over Peak", x = over_peak)
writeData(wb, sheet = "Under Peak", x = under_peak)
writeData(wb, sheet = "Spread Peak Filtered", x = spread_peak_filtered)
writeData(wb, sheet = "Spread2 Peak Filtered", x = spread2_peak_filtered)
writeData(wb, sheet = "ML Peak Filtered", x = ml_peak_filtered)
writeData(wb, sheet = "Over Peak Filtered", x = over_peak_filtered)
writeData(wb, sheet = "Under Peak Filtered", x = under_peak_filtered)

saveWorkbook(wb, file = u)












## all
# nba_lin <- nba %>%
#     select(7,9:78)

## no correlation - eFG
nba_lin <- nba %>%
    select(7,
           14,15,16,18,19,21,23,30,34,35,37,39,43,
           49,50,51,53,54,56,58,65,69,70,72,74,78)

## no correlation - ts
nba_lin <- nba %>%
    select(7,
           14,15,16,18,19,21,24,30,34,35,37,40,43,
           49,50,51,53,54,56,59,65,69,70,72,75,78)

## no correlation - rtg
nba_lin <- nba %>%
    select(7,
           14,15,16,18,19,21,30,34,35,37,43,41,42,
           49,50,51,53,54,56,65,69,70,72,78,76,77)











set.seed(214)
sample <- sample.split(nba_lin$Margin, SplitRatio = .70)
train <- nba_lin %>% filter(sample == TRUE)
test <- nba_lin %>% filter(sample == FALSE)


pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))

train[,-1] = predict(pre_proc_val, train[,-1])
test[,-1] = predict(pre_proc_val, test[,-1])

summary(train)


### linear model

lin_mod <- lm(Margin ~., data = train)
summary(lin_mod)


#Step 1 - create the evaluation metrics function
eval_metrics <- function(model, df, predictions, target) {
        resids = df[,target] - predictions
        resids2 = resids**2
        N = length(predictions)
        r2 = as.character(round(summary(model)$r.squared, 4))
        adj_r2 = as.character(round(summary(model)$adj.r.squared, 4))
        print(adj_r2) #Adjusted R-squared
        print(as.character(round(sqrt(sum(resids2)/N), 4))) #RMSE
    }
# Step 2 - predicting and evaluating the model on train data
predictions = predict(lin_mod, newdata = train)
eval_metrics(lin_mod, train, predictions, target = 'Margin')

# Step 3 - predicting and evaluating the model on test data
predictions = predict(lin_mod, newdata = test)
eval_metrics(lin_mod, test, predictions, target = 'Margin')



### ridge model

dummies <- dummyVars(Margin ~ ., data = nba_lin)
train_dummies = predict(dummies, newdata = train)
test_dummies = predict(dummies, newdata = test)
print(dim(train_dummies)); print(dim(test_dummies))



x = as.matrix(train_dummies)
y_train = train$Margin

x_test = as.matrix(test_dummies)
y_test = test$Margin

lambdas <- 10^seq(2, -3, by = -.1)
ridge_reg = glmnet(x, y_train, nlambda = 25, alpha = 0, family = 'gaussian', lambda = lambdas)

summary(ridge_reg)


cv_ridge <- cv.glmnet(x, y_train, alpha = 0, lambda = lambdas)
optimal_lambda <- cv_ridge$lambda.min
optimal_lambda




# Compute R^2 from true and predicted values
eval_results <- function(true, predicted, df) {
      SSE <- sum((predicted - true)^2)
      SST <- sum((true - mean(true))^2)
      R_square <- 1 - SSE / SST
      RMSE = sqrt(SSE/nrow(df))
    
      
      # Model performance metrics
    data.frame(
          Rsquare = R_square,
          RMSE = RMSE
        )
      
    }

# Prediction and evaluation on train data
predictions_train <- predict(ridge_reg, s = optimal_lambda, newx = x)
eval_results(y_train, predictions_train, train)

# Prediction and evaluation on test data
predictions_test <- predict(ridge_reg, s = optimal_lambda, newx = x_test)
eval_results(y_test, predictions_test, test)




### lasso model

lambdas <- 10^seq(2, -3, by = -.1)

# Setting alpha = 1 implements lasso regression
lasso_reg <- cv.glmnet(x, y_train, alpha = 1, lambda = lambdas, standardize = TRUE, nfolds = 5,
                       family='gaussian', type.measure='mse')
plot(lasso_reg)
plot(lasso_reg$glmnet.fit, xvar="lambda", label=TRUE)

cat('Min Lambda: ', lasso_reg$lambda.min, '\n 1Sd Lambda: ', lasso_reg$lambda.1se)
df_coef <- round(as.matrix(coef(lasso_reg, s=lasso_reg$lambda.min)), 2)

# See all contributing variables
df_coef[df_coef[, 1] != 0, ]

# Best 
lambda_best <- lasso_reg$lambda.min 
lambda_best


lasso_model <- glmnet(x, y_train, alpha = 1, lambda = lambda_best, standardize = TRUE, family='gaussian')

predictions_train <- predict(lasso_model, s = lambda_best, newx = x)
eval_results(y_train, predictions_train, train)

predictions_test <- predict(lasso_model, s = lambda_best, newx = x_test)
eval_results(y_test, predictions_test, test)




### elastic net

# Set training control
train_cont <- trainControl(method = "repeatedcv",
                                                          number = 10,
                                                          repeats = 5,
                                                          search = "random",
                                                          verboseIter = TRUE)

# Train the model
elastic_reg <- train(Margin ~ .,
                                                 data = train,
                                                 method = "glmnet",
                                                 preProcess = c("center", "scale"),
                                                 tuneLength = 10,
                                                 trControl = train_cont)



# Best tuning parameter
elastic_reg$bestTune

# Make predictions on training set
predictions_train <- predict(elastic_reg, x)
eval_results(y_train, predictions_train, train) 

# Make predictions on test set
predictions_test <- predict(elastic_reg, x_test)
eval_results(y_test, predictions_test, test)

















## no correlation - eFG
nba_log <- nba %>%
    select(8,
           14,15,16,18,19,21,23,30,34,35,37,39,43,
           49,50,51,53,54,56,58,65,69,70,72,74,78)

## no correlation - ts
nba_log <- nba %>%
    select(8,
           14,15,16,18,19,21,24,30,34,35,37,40,43,
           49,50,51,53,54,56,59,65,69,70,72,75,78)

## no correlation - rtg
nba_log <- nba %>%
    select(8,
           14,15,16,18,19,21,30,34,35,37,43,41,42,
           49,50,51,53,54,56,65,69,70,72,78,76,77)





set.seed(214)
sample <- sample.split(nba_log$Win, SplitRatio = .70)
train <- nba_log %>% filter(sample == TRUE)
test <- nba_log %>% filter(sample == FALSE)


pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))

train[,-1] = predict(pre_proc_val, train[,-1])
test[,-1] = predict(pre_proc_val, test[,-1])



### logistic model

log_mod <- lm(Win ~., data = train)
summary(log_mod)


#Step 1 - create the evaluation metrics function
eval_metrics <- function(model, df, predictions, target) {
    # resids = df[,target] - predictions
    # resids2 = resids**2
    # N = length(predictions)
    r2 = as.character(round(summary(model)$r.squared, 4))
    adj_r2 = as.character(round(summary(model)$adj.r.squared, 4))
    print(adj_r2) #Adjusted R-squared
    print(as.character(mean(df != round(predictions,0)))) #RMSE
}
# Step 2 - predicting and evaluating the model on train data
predictions = predict(log_mod, newdata = train, type = 'response')
eval_metrics(log_mod, train$Win, predictions, target = 'Win')

# Step 3 - predicting and evaluating the model on test data
predictions = predict(log_mod, newdata = test, type = 'response')
eval_metrics(log_mod, test$Win, predictions, target = 'Win')


plotROC(actuals=test$Win, predictedScores=predictions, returnSensitivityMat = F) # .7-.8 = good; .8+ = great
confusionMatrix(test$Win, predictions)
misClassError(test$Win, predictions)





### ridge model

dummies <- dummyVars(Win ~ ., data = nba_log)
train_dummies = predict(dummies, newdata = train)
test_dummies = predict(dummies, newdata = test)
print(dim(train_dummies)); print(dim(test_dummies))



x = as.matrix(train_dummies)
y_train = train$Win

x_test = as.matrix(test_dummies)
y_test = test$Win

lambdas <- 10^seq(2, -3, by = -.1)
ridge_reg = glmnet(x, y_train, nlambda = 25, alpha = 0, family = 'binomial', lambda = lambdas)


cv_ridge <- cv.glmnet(x, y_train, alpha = 0, family = 'binomial', lambda = lambdas)
optimal_lambda <- cv_ridge$lambda.min
optimal_lambda


# Prediction and evaluation on train data
predictions_train <- as.numeric(predict(ridge_reg, s = optimal_lambda, newx = x, type = 'response'))

# Prediction and evaluation on test data
predictions_test <- as.numeric(predict(ridge_reg, s = optimal_lambda, newx = x_test, type = 'response'))


plotROC(actuals=y_train, predictedScores=predictions_train, returnSensitivityMat = F)
confusionMatrix(train$Win, predictions_train)
misClassError(train$Win, predictions_train)


plotROC(actuals=y_test, predictedScores=predictions_test, returnSensitivityMat = F)
confusionMatrix(test$Win, predictions_test)
misClassError(test$Win, predictions_test)






### lasso model

lambdas <- 10^seq(2, -3, by = -.1)

# Setting alpha = 1 implements lasso regression
lasso_reg <- cv.glmnet(x, y_train, alpha = 1, family = 'binomial', lambda = lambdas, standardize = T, nfolds = 5)

# Best 
lambda_best <- lasso_reg$lambda.min 
lambda_best



lasso_model <- glmnet(x, y_train, alpha = 1, family = 'binomial', lambda = lambda_best, standardize = TRUE)
assess.glmnet(lasso_model, x, y_train)

# Prediction and evaluation on train data
predictions_train <- as.numeric(predict(lasso_model, s = lambda_best, newx = x, type = 'response'))

# Prediction and evaluation on test data
predictions_test <- as.numeric(predict(lasso_model, s = lambda_best, newx = x_test, type = 'response'))


plotROC(actuals=y_train, predictedScores=predictions_train, returnSensitivityMat = F)
confusionMatrix(train$Win, predictions_train)
misClassError(train$Win, predictions_train)


plotROC(actuals=y_test, predictedScores=predictions_test, returnSensitivityMat = F)
confusionMatrix(test$Win, predictions_test)
misClassError(test$Win, predictions_test)




# logistic - in depth
plotROC(actuals=test$Win, predictedScores=predictions, returnSensitivityMat = TRUE)

InformationValue::sensitivity(actuals=test$Win, predictedScores=predictions)
InformationValue::specificity(actuals=test$Win, predictedScores=predictions)

InformationValue::precision(actuals=test$Win, predictedScores=predictions)
InformationValue::npv(actuals=test$Win, predictedScores=predictions)

InformationValue::youdensIndex(actuals=test$Win, predictedScores=predictions)
InformationValue::misClassError(actuals=test$Win, predictedScores=predictions)

InformationValue::Concordance(actuals=test$Win, predictedScores=predictions)
InformationValue::somersD(actuals=test$Win, predictedScores=predictions)    

InformationValue::ks_stat(actuals=test$Win, predictedScores=predictions)    
InformationValue::ks_plot(actuals=test$Win, predictedScores=predictions)    

InformationValue::optimalCutoff(actuals=test$Win, predictedScores=predictions) # returns cutoff that gives minimum misclassification error
InformationValue::optimalCutoff(actuals=test$Win, predictedScores=predictions, optimiseFor = "Both")  # returns cutoff that gives maximum of Youden's J Index)




#############
#### KNN #### - regression
#############
## no correlation - eFG
nba_knnr <- nba %>%
    select(7,
           14,15,16,18,19,21,23,30,34,35,37,39,43,
           49,50,51,53,54,56,58,65,69,70,72,74,78)

## no correlation - ts
nba_knnr <- nba %>%
    select(7,
           14,15,16,18,19,21,24,30,34,35,37,40,43,
           49,50,51,53,54,56,59,65,69,70,72,75,78)

## no correlation - rtg
nba_knnr <- nba %>%
    select(7,
           14,15,16,18,19,21,30,34,35,37,43,41,42,
           49,50,51,53,54,56,65,69,70,72,78,76,77)


set.seed(214)
sample <- sample.split(nba_knnr$Margin, SplitRatio = .70)
train <- nba_knnr %>% filter(sample == TRUE)
test <- nba_knnr %>% filter(sample == FALSE)

pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))

train[,-1] <- predict(pre_proc_val, train[,-1])
test[,-1] <- predict(pre_proc_val, test[,-1])


# predicted_margin = NULL
# error_rate = NULL

# for(i in 1:100){
#     set.seed(214)
#     predicted_margin = FNN::knn.reg(train[-1], test[-1], as.numeric(train$Margin), k=i)
#     knnr_margin <- predicted_margin[["pred"]] * (max(nba_knnr$Margin) - min(nba_knnr$Margin)) + min(nba_knnr$Margin)
#     error_rate[i] = mean((knnr_margin - test$Margin ) ^ 2)
# }
# 
# print(error_rate)
# 
# k_values <- 1:100
# error_df <- data.frame(error_rate, k_values)
# order(error_df$error_rate)
# 
# ggplot(error_df, aes(k_values, error_rate)) + geom_point()+ geom_line(lty="dotted", color='red')
# ggplot(test,aes(x=Margin, y=pred)) + geom_point() + stat_smooth(method = "lm") + coord_obs_pred()










# train_x = train[, -1]
# train_y = as.numeric(unlist(train[,1]))
# 
# test_x = test[, -1]
# test_y = as.numeric(unlist(test[,1]))


# knnmodel = knnreg(train_x, train_y, k = 32)
# str(knnmodel)
# 
# pred_y = predict(knnmodel, data.frame(test_x))
# 
# mse = mean((test_y - pred_y)^2)
# mae = caret::MAE(test_y, pred_y)
# rmse = caret::RMSE(test_y, pred_y)
# 
# cat("MSE: ", mse, "MAE: ", mae, " RMSE: ", rmse)


# x = 1:length(test_y)
# x = 1:100
# plot(x, test_y[1:100], col = "red", type = "l", lwd=2,
#      main = "KNN Regression Analysis")
# lines(x, pred_y[1:100], col = "blue", lwd=2)
# legend("topright",  legend = c("actual", "predicted"), 
#        fill = c("red", "blue"), col = 2:3,  adj = c(0, 0.6))
# grid() 






# rmse function
rmse = function(actual, predicted) {
    sqrt(mean((actual - predicted) ^ 2))
}

# define helper function for getting knn.reg predictions
# note: this function is highly specific to this situation and dataset
make_knn_pred = function(k = 1, training, predicting) {
    pred = FNN::knn.reg(as.data.frame(training[,-1]), 
                        as.data.frame(predicting[,-1]), 
                        y = as.numeric(train$Margin), k = k)$pred
    act  = predicting$Margin
    rmse(predicted = pred, actual = act)
}


# define values of k to evaluate
k = seq(21,91,5)

# get requested train RMSEs
knn_trn_rmse = sapply(k, make_knn_pred, 
                      training = train, 
                      predicting = train)
# get requested test RMSEs
knn_tst_rmse = sapply(k, make_knn_pred, 
                      training = train, 
                      predicting = test)

# determine "best" k
best_k = k[which.min(knn_tst_rmse)]

# find overfitting, underfitting, and "best"" k
fit_status = ifelse(k < best_k, "Over", ifelse(k == best_k, "Best", "Under"))


# summarize results
knn_results = data.frame(
    k,
    round(knn_trn_rmse, 2),
    round(knn_tst_rmse, 2),
    fit_status
)
colnames(knn_results) = c("k", "Train RMSE", "Test RMSE", "Fit?")

# display results
knitr::kable(knn_results, escape = FALSE, booktabs = TRUE)





knn_model <- FNN::knn.reg(train = as.data.frame(train[,-1]), 
                          test = as.data.frame(test[,-1]), 
                          y = as.numeric(train$Margin), 
                          k = best_k)
pred_y  = knn_model$pred


mse = mean((as.numeric(unlist(test[,1])) - pred_y)^2)
mae = caret::MAE(as.numeric(unlist(test[,1])), pred_y)
rmse = caret::RMSE(as.numeric(unlist(test[,1])), pred_y)

cat("MSE: ", mse, "MAE: ", mae, " RMSE: ", rmse)



#############
#### KNN #### - class
#############
## no correlation - eFG
nba_knn <- nba %>%
    select(8,
           14,15,16,18,19,21,23,30,34,35,37,39,43,
           49,50,51,53,54,56,58,65,69,70,72,74,78)

## no correlation - ts
nba_knn <- nba %>%
    select(8,
           14,15,16,18,19,21,24,30,34,35,37,40,43,
           49,50,51,53,54,56,59,65,69,70,72,75,78)

## no correlation - rtg
nba_knn <- nba %>%
    select(8,
           14,15,16,18,19,21,30,34,35,37,43,41,42,
           49,50,51,53,54,56,65,69,70,72,78,76,77)





set.seed(214)
sample <- sample.split(nba_knn$Win, SplitRatio = .70)
train <- nba_knn %>% filter(sample == TRUE)
test <- nba_knn %>% filter(sample == FALSE)


pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))

train[,-1] = predict(pre_proc_val, train[,-1])
test[,-1] = predict(pre_proc_val, test[,-1])


# error
calc_class_err = function(actual, predicted) {
    mean(actual != predicted)
}

calc_class_err(actual = test$Win,
               predicted = knn(train = train[,-1],
                               test  = test[,-1],
                               cl    = train$Win,
                               k     = 125))



set.seed(214)
k_to_try = 1:250
err_k = rep(x = 0, times = length(k_to_try))

for (i in seq_along(k_to_try)) {
    pred = knn(train = train[,-1],
               test  = test[,-1],
               cl    = train$Win,
               k     = k_to_try[i])
    err_k[i] = calc_class_err(test$Win, pred)
}


# plot error vs choice of k
plot(err_k, type = "b", col = "dodgerblue", cex = 1, pch = 20, 
     xlab = "k, number of neighbors", ylab = "classification error",
     main = "(Test) Error Rate vs Neighbors")
# add line for min error seen
abline(h = min(err_k), col = "darkorange", lty = 3)
# add line for minority prevalence in test set
abline(h = mean(test$Win == "Yes"), col = "grey", lty = 2)

min(err_k)

which(err_k == min(err_k))

max(which(err_k == min(err_k)))

table(test$Win)

mean(test$Win == "Yes")










# ridge part 2
X = model.matrix(Margin ~ ., nba_lin)[, -1]
y = nba_lin$Margin

fit = lm(Margin ~ ., nba_lin)
coef(fit)

sum(abs(coef(fit)[-1]))

sum(coef(fit)[-1] ^ 2)



par(mfrow = c(1, 2))
fit_ridge = glmnet(X, y, alpha = 0)
plot(fit_ridge)
plot(fit_ridge, xvar = "lambda", label = TRUE)

fit_ridge_cv = cv.glmnet(X, y, alpha = 0)
plot(fit_ridge_cv)

# fitted coefficients, using 1-SE rule lambda, default behavior
coef(fit_ridge_cv)

# fitted coefficients, using minimum lambda
coef(fit_ridge_cv, s = "lambda.min")

# penalty term using minimum lambda
sum(coef(fit_ridge_cv, s = "lambda.min")[-1] ^ 2)

# fitted coefficients, using 1-SE rule lambda
coef(fit_ridge_cv, s = "lambda.1se")

# penalty term using 1-SE rule lambda
sum(coef(fit_ridge_cv, s = "lambda.1se")[-1] ^ 2)

# predict using minimum lambda
predict(fit_ridge_cv, X, s = "lambda.min")

# predict using 1-SE rule lambda, default behavior
predict(fit_ridge_cv, X)

# calcualte "train error"
mean((y - predict(fit_ridge_cv, X)) ^ 2)

# CV-RMSEs
sqrt(fit_ridge_cv$cvm)

# CV-RMSE using minimum lambda
sqrt(fit_ridge_cv$cvm[fit_ridge_cv$lambda == fit_ridge_cv$lambda.min])

# CV-RMSE using 1-SE rule lambda
sqrt(fit_ridge_cv$cvm[fit_ridge_cv$lambda == fit_ridge_cv$lambda.1se]) 




# lasso part 2
par(mfrow = c(1, 2))
fit_lasso = glmnet(X, y, alpha = 1)
plot(fit_lasso)
plot(fit_lasso, xvar = "lambda", label = TRUE)

fit_lasso_cv = cv.glmnet(X, y, alpha = 1)
plot(fit_lasso_cv)

# fitted coefficients, using 1-SE rule lambda, default behavior
coef(fit_lasso_cv)

# fitted coefficients, using minimum lambda
coef(fit_lasso_cv, s = "lambda.min")

# penalty term using minimum lambda
sum(coef(fit_lasso_cv, s = "lambda.min")[-1] ^ 2)

# fitted coefficients, using 1-SE rule lambda
coef(fit_lasso_cv, s = "lambda.1se")

# penalty term using 1-SE rule lambda
sum(coef(fit_lasso_cv, s = "lambda.1se")[-1] ^ 2)

# predict using minimum lambda
predict(fit_lasso_cv, X, s = "lambda.min")

# predict using 1-SE rule lambda, default behavior
predict(fit_lasso_cv, X)

# calcualte "train error"
mean((y - predict(fit_lasso_cv, X)) ^ 2)

# CV-RMSEs
sqrt(fit_lasso_cv$cvm)

# CV-RMSE using minimum lambda
sqrt(fit_lasso_cv$cvm[fit_lasso_cv$lambda == fit_lasso_cv$lambda.min])

# CV-RMSE using 1-SE rule lambda
sqrt(fit_lasso_cv$cvm[fit_lasso_cv$lambda == fit_lasso_cv$lambda.1se]) 



library(broom)
# the output from the commented line would be immense
# fit_lasso_cv
tidy(fit_lasso_cv)

# the two lambda values of interest
glance(fit_lasso_cv) 





# lasso ML
X = model.matrix(Win ~ ., nba_log)[, -1]
y = nba_log$Win

fit = lm(Win ~ ., nba_log)
coef(fit)

sum(abs(coef(fit)[-1]))

sum(coef(fit)[-1] ^ 2)


library(glmnet)
fit_cv = cv.glmnet(X, y, family = "binomial", alpha = 1)
plot(fit_cv)

head(coef(fit_cv), n = 10)

fit_1se = glmnet(X, y, family = "binomial", lambda = fit_cv$lambda.1se)
which(as.vector(as.matrix(fit_1se$beta)) != 0)

par(mfrow = c(1, 2))
plot(glmnet(X, y, family = "binomial"))
plot(glmnet(X, y, family = "binomial"), xvar = "lambda")

fit_cv$lambda.min

fit_cv$lambda.1se

library(caret)
cv_5 = trainControl(method = "cv", number = 5)
lasso_grid = expand.grid(alpha = 1, 
                         lambda = c(fit_cv$lambda.min, fit_cv$lambda.1se))
lasso_grid

sim_data = data.frame(y, X)
fit_lasso = train(
    factor(y) ~ ., data = sim_data,
    method = "glmnet",
    trControl = cv_5,
    tuneGrid = lasso_grid
)
fit_lasso$results






##################
#### Ensemble ####
##################


# ensemble regression style
calc_rmse = function(actual, predicted) {
    sqrt(mean((actual - predicted) ^ 2))
}

library(rpart)
library(rpart.plot)
library(randomForest)
library(gbm)
library(caret)
library(MASS)
library(ISLR)

set.seed(214)
sample <- sample.split(nba_reg$Margin, SplitRatio = .70)
train <- nba_reg %>% filter(sample == TRUE)
test <- nba_reg %>% filter(sample == FALSE)

pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))

train[,-1] <- predict(pre_proc_val, train[,-1])
test[,-1] <- predict(pre_proc_val, test[,-1])



# tree
nba_tree = rpart(Margin ~ ., data = train)

nba_tree_tst_pred = predict(nba_tree, newdata = test)
plot(nba_tree_tst_pred, test$Margin, 
     xlab = "Predicted", ylab = "Actual", 
     main = "Predicted vs Actual: Single Tree, Test Data",
     col = "dodgerblue", pch = 20)
grid()
abline(0, 1, col = "darkorange", lwd = 2)

(tree_tst_rmse = calc_rmse(nba_tree_tst_pred, test$Margin))

# linear
nba_lm = lm(Margin ~ ., data = train)

nba_lm_tst_pred = predict(nba_lm, newdata = test)

plot(nba_lm_tst_pred, test$Margin,
     xlab = "Predicted", ylab = "Actual",
     main = "Predicted vs Actual: Linear Model, Test Data",
     col = "dodgerblue", pch = 20)
grid()
abline(0, 1, col = "darkorange", lwd = 2)

(lm_tst_rmse = calc_rmse(nba_lm_tst_pred, test$Margin))

# bagging
nba_bag = randomForest(Margin ~ ., data = train, mtry = 26, 
                          importance = TRUE, ntrees = 500)
nba_bag

nba_bag_tst_pred = predict(nba_bag, newdata = test)
plot(nba_bag_tst_pred,test$Margin,
     xlab = "Predicted", ylab = "Actual",
     main = "Predicted vs Actual: Bagged Model, Test Data",
     col = "dodgerblue", pch = 20)
grid()
abline(0, 1, col = "darkorange", lwd = 2)

(bag_tst_rmse = calc_rmse(nba_bag_tst_pred, test$Margin))

plot(nba_bag, col = "dodgerblue", lwd = 2, main = "Bagged Trees: Error vs Number of Trees")
grid()

# random forest
nba_forest = randomForest(Margin ~ ., data = train, mtry = 7, 
                             importance = TRUE, ntrees = 500)
nba_forest

importance(nba_forest, type = 1)

varImpPlot(nba_forest, type = 1)

nba_forest_tst_pred = predict(nba_forest, newdata = test)
plot(nba_forest_tst_pred, test$Margin,
     xlab = "Predicted", ylab = "Actual",
     main = "Predicted vs Actual: Random Forest, Test Data",
     col = "dodgerblue", pch = 20)
grid()
abline(0, 1, col = "darkorange", lwd = 2)

(forest_tst_rmse = calc_rmse(nba_forest_tst_pred, test$Margin))

nba_forest_trn_pred = predict(nba_forest, newdata = train)
forest_trn_rmse = calc_rmse(nba_forest_trn_pred, train$Margin)
forest_oob_rmse = calc_rmse(nba_forest$predicted, train$Margin)

# boosting
nba = gbm(Margin ~ ., data = train, distribution = "gaussian", 
                    n.trees = 1500, interaction.depth = 3, shrinkage = 0.01)
nba_boost

tibble::as_tibble(summary(nba_boost))

par(mfrow = c(1, 3))
plot(nba_boost, i = "oeFG_home", col = "dodgerblue", lwd = 2)

plot(nba_boost, i = "oeFG_away", col = "dodgerblue", lwd = 2)

plot(nba_boost, i = "eFG_home", col = "dodgerblue", lwd = 2)

plot(nba_boost, i = "eFG_away", col = "dodgerblue", lwd = 2)

nba_boost_tst_pred = predict(nba_boost, newdata = test, n.trees = 1500)
(boost_tst_rmse = calc_rmse(nba_boost_tst_pred, test$Margin))

plot(nba_boost_tst_pred, test$Margin,
     xlab = "Predicted", ylab = "Actual", 
     main = "Predicted vs Actual: Boosted Model, Test Data",
     col = "dodgerblue", pch = 20)
grid()
abline(0, 1, col = "darkorange", lwd = 2)

# results
(nba_rmse = data.frame(
    Model = c("Single Tree", "Linear Model", "Bagging",  "Random Forest",  "Boosting"),
    TestError = c(tree_tst_rmse, lm_tst_rmse, bag_tst_rmse, forest_tst_rmse, boost_tst_rmse)
)
)




# ensemble classification style 
calc_acc = function(actual, predicted) {
    mean(actual == predicted)
}

set.seed(214)
sample <- sample.split(nba_cla$Win, SplitRatio = .70)
train <- nba_cla %>% filter(sample == TRUE)
test <- nba_cla %>% filter(sample == FALSE)

pre_proc_val <- preProcess(train[,-1], method = c("center", "scale"))

train[,-1] <- predict(pre_proc_val, train[,-1])
test[,-1] <- predict(pre_proc_val, test[,-1])

train$Win <- factor(train$Win)
test$Win <- factor(test$Win)



# tree
nba_tree = rpart(Win ~ ., data = train)

rpart.plot(nba_tree)

nba_tree_tst_pred = predict(nba_tree, test, type = "class")
table(predicted = nba_tree_tst_pred, actual = test$Win)

(tree_tst_acc = calc_acc(predicted = nba_tree_tst_pred, actual = test$Win))

# logistic
nba_glm = glm(Win ~ ., data = train, family = "binomial")

nba_glm_tst_pred = ifelse(predict(nba_glm, test, "response") > 0.5, 
                           "1", "0")
table(predicted = nba_glm_tst_pred, actual = test$Win)

(glm_tst_acc = calc_acc(predicted = nba_glm_tst_pred, actual = test$Win))

# bagging
nba_bag = randomForest(Win ~ ., data = train, mtry = 20, 
                        importance = TRUE, ntrees = 500)
nba_bag

nba_bag_tst_pred = predict(nba_bag, newdata = test)
table(predicted = nba_bag_tst_pred, actual = test$Win)

(bag_tst_acc = calc_acc(predicted = nba_bag_tst_pred, actual = test$Win))

# random forest
nba_forest = randomForest(Win ~ ., data = train, mtry = 20, importance = TRUE, ntrees = 500)
nba_forest

nba_forest_tst_perd = predict(nba_forest, newdata = test)
table(predicted = nba_forest_tst_perd, actual = test$Win)

(forest_tst_acc = calc_acc(predicted = nba_forest_tst_perd, actual = test$Win))

# boosting
nba_trn_mod = train
nba_trn_mod$Win = as.numeric(ifelse(nba_trn_mod$Win == 1, "1", "0"))

nba_boost = gbm(Win ~ ., data = nba_trn_mod, distribution = "bernoulli", 
                 n.trees = 1500, interaction.depth = 2, shrinkage = 0.01)
nba_boost

nba_boost_tst_pred = ifelse(predict(nba_boost, test, n.trees = 1500, "response") > 0.5, 
                             "1", "0")
table(predicted = nba_boost_tst_pred, actual = test$Win)

(boost_tst_acc = calc_acc(predicted = nba_boost_tst_pred, actual = test$Win))

# results
(nba_acc = data.frame(
    Model = c("Single Tree", "Logistic Regression", "Bagging",  "Random Forest",  "Boosting"),
    TestAccuracy = c(tree_tst_acc, glm_tst_acc, bag_tst_acc, forest_tst_acc, boost_tst_acc)
)
)



#################
##### Tuning ####
#################

# regression
# random forest
oob = trainControl(method = "oob")
cv_5 = trainControl(method = "cv", number = 5)

dim(train)

rf_grid =  expand.grid(mtry = 1:26)

set.seed(214)
nba_rf_tune = train(Margin ~ ., data = train,
                    method = "rf", #ranger
                    trControl = oob,
                    verbose = T,
                    tuneGrid = rf_grid)
nba_rf_tune

calc_rmse(predict(nba_rf_tune, test), test$Margin)

nba_rf_tune$bestTune

# boosting
gbm_grid =  expand.grid(interaction.depth = 1:5,
                        n.trees = (1:6) * 500,
                        shrinkage = c(0.001, 0.01, 0.1),
                        n.minobsinnode = 10)

nba_gbm_tune = train(Margin ~ ., data = train,
                     method = "gbm",
                     trControl = cv_5,
                     verbose = FALSE,
                     tuneGrid = gbm_grid)

plot(nba_gbm_tune)

calc_rmse(predict(nba_gbm_tune, test), test$Margin)

nba_gbm_tune$bestTune











# classification
# random forest
oob = trainControl(method = "oob")
cv_5 = trainControl(method = "cv", number = 5)

dim(train)

rf_grid =  expand.grid(mtry = 1:26)

set.seed(214)
nba_rf_tune = train(Win ~ ., data = train,
                     method = "rf", #ranger
                     trControl = oob,
                     verbose = T,
                     tuneGrid = rf_grid)
nba_rf_tune

calc_acc(predict(nba_rf_tune, test), test$Win)

nba_rf_tune$bestTune

# boosting
gbm_grid =  expand.grid(interaction.depth = 1:5,
                        n.trees = (1:6) * 500,
                        shrinkage = c(0.001, 0.01, 0.1),
                        n.minobsinnode = 10)

nba_gbm_tune = train(Win ~ ., data = train,
                      method = "gbm",
                      trControl = cv_5,
                      verbose = FALSE,
                      tuneGrid = gbm_grid)

plot(nba_gbm_tune)

calc_acc(predict(nba_gbm_tune, test), test$Win)

nba_gbm_tune$bestTune








