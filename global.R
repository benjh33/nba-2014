library(dplyr)
library(ggplot2)
library(stringr)
library(bbscrapeR)
library(ggvis)
library(htmlwidgets)
library(htmltools)
# library(metricsgraphics)
library(ggd3)
library(doMC)
library(MASS)

registerDoMC(cores = 4)
source('shinyUtil.R', local = TRUE)

data(players)
data(nba_wl_games)
data(team_summary)

