library(dplyr)
library(ggplot2)
library(stringr)
library(htmlwidgets)
library(htmltools)
library(caret)
library(reshape2)
library(ggd3)
library(doMC)
library(MASS)
library(pROC)
library(scales)


registerDoMC(cores = 4)
source('shinyUtil.R', local = TRUE)

data(nba_wl_games)
data(team_summary)
data(modelOutput)
# establish all variable groups, then get on with
# eliminating colinear variables, splitting, training
## period 1 models/variables
p1_vars <- c('P1_diff', grep('[^n]_1$', names(nba_wl_games),
                             value = TRUE))

## period 2 models/variables
p2_vars <- c(p1_vars, 'P2_diff', grep('[^n]_2$', names(nba_wl_games),
                                      value = TRUE))



## period 3 models/variables
p3_vars <- c(p2_vars, 'P3_diff', grep('[^n]_3$', names(nba_wl_games),
                                      value = TRUE))



# nothing.
nba_wl_games <- nba_wl_games[complete.cases(nba_wl_games[,p3_vars]), ]
f1 <- WIN ~ P1_diff
f2 <- WIN ~ P1_diff + P2_diff
f3 <- WIN ~ P1_diff + P2_diff + P3_diff
m1 <- glm(f1, data=nba_wl_games, family=binomial, model = FALSE)
m2 <- glm(f2, data=nba_wl_games, family=binomial, model = FALSE)
m3 <- glm(f3, data=nba_wl_games, family=binomial, model = FALSE)

nba_wl_games$p1_win_prob <- m1$fitted.values
nba_wl_games$p2_win_prob <- m2$fitted.values
nba_wl_games$p3_win_prob <- m3$fitted.values

# dataset for ML
pd <- nba_wl_games[,p3_vars]
scaleCenter <- preProcess(pd[modelOutput$index, ])
wins <- factor(ifelse(nba_wl_games$WIN ==1 , "WIN", "LOSS"))
pdtest <- predict(scaleCenter, pd[-modelOutput$index, p3_vars])
pdtest$wins <- wins[-modelOutput$index]
rm(scaleCenter)
modelOutput$index <- NULL
models <- c('glmnet', 'gbm', 'rf')
varImps <- plyr::ldply(modelOutput, function(obj) {
  out <- data.frame()
  for(m in models){
    tmp <- varImp(obj[[m]]$model)[[1]]
    tmp$variable <- row.names(tmp)
    tmp$model <- m
    out <- plyr::rbind.fill(out, tmp)
  }
  out
})
names(varImps)[1] <- 'period'
varImps <- varImps[order(varImps$model, varImps$Overall,
                         decreasing = FALSE),]
varOrder <- unique(subset(varImps, model == 'rf')$variable)
