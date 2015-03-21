library(dplyr)
library(ggplot2)
library(stringr)
library(bbscrapeR)
library(ggvis)
library(htmlwidgets)
library(htmltools)
library(caret)
# library(metricsgraphics)
library(reshape2)
library(ggd3)
library(doMC)
library(MASS)
library(scales)
registerDoMC(cores = 2)
source('shinyUtil.R', local = TRUE)

data(players)
data(nba_wl_games)
data(team_summary)

# nothing.
allvars <- c('WIN', 'P1_diff', 'P2_diff', 'P3_diff')
nba_wl_games <- nba_wl_games[complete.cases(nba_wl_games[,allvars]), ]
f1 <- WIN ~ P1_diff
f2 <- WIN ~ P1_diff + P2_diff
f3 <- WIN ~ P1_diff + P2_diff + P3_diff
m1 <- glm(f1, data=nba_wl_games, family=binomial, model = FALSE)
m2 <- glm(f2, data=nba_wl_games, family=binomial, model = FALSE)
m3 <- glm(f3, data=nba_wl_games, family=binomial, model = FALSE)

nba_wl_games$p1_win_prob <- m1$fitted.values
nba_wl_games$p2_win_prob <- m2$fitted.values
nba_wl_games$p3_win_prob <- m3$fitted.values
