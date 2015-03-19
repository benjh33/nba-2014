library(bbscrapeR)
library(htmlwidgets)
library(htmltools)
library(dplyr)
library(ggd3)
library(ggplot2)
library(stringr)
library(doMC)
library(MASS)

registerDoMC(4)

data(players)
data(nba_wl_games)

