## setting up datasets
library(bbscrapeR)
library(htmlwidgets)
library(htmltools)
library(dplyr)
library(ggd3)
library(ggplot2)
library(stringr)
library(reshape2)
library(doMC)
registerDoMC(cores = 4)

team_lookup <- list(
  BKN = "nets",
  DAL = "mavericks",
  GSW = "warriors",
  HOU = "rockets",
  LAL = "lakers",
  POR = "blazers",
  ATL = "hawks",
  BOS = "celtics",
  CHA = "bobcats",
  CHI = "bulls",
  CLE = "cavaliers",
  DEN = "nuggets",
  DET = "pistons",
  IND = "pacers",
  LAC = "clippers",
  MEM = "grizzlies",
  MIA = "heat",
  MIL = "bucks",
  MIN = "timberwolves",
  NOP = "pelicans",
  NYK = "knicks",
  ORL = "magic",
  PHI = "sixers",
  PHX = "suns",
  SAC = "kings",
  SAS = "spurs",
  TOR = "raptors",
  WAS = "wizards",
  OKC = "thunder",
  UTA = "jazz"
)
## iterate through season, weekly
files <- c("boxscore.xml", "pbp_all.xml", "shotchart_all.xml")
# regular season starts 2013-10-29, ends 2014-04-16
startDate <- as.Date("2013-10-29")
endDate <- as.Date('2014-04-16')
curDate <- startDate
boxes <- list()
while(endDate > (curDate + 6)) {
  boxes[[as.character(curDate)]] <- rebound(strftime(curDate, "%Y%m%d"),
                                            strftime(curDate, "%Y%m%d"),
                                            suffix=files)
  curDate <- curDate + 7
}


#### end single week processing
# ---
# get all shots for 2013-2014 season
ids <- players[, 'PlayerID']
lshots <- lapply(ids, function(x) acquire(PlayerID = x))
allshots <- do.call("rbind", lshots)
rm(lshots)
allshots <- as.data.frame(allshots)
allshots$date <- strptime(str_extract(allshots$MATCHUP, '[A-Z]+ [0-9]{1,2}, [0-9]{4}'), "%B %d, %Y")

numbers <- c("FINAL_MARGIN",
             "SHOT_NUMBER",
             "PERIOD",
             "SHOT_CLOCK",
             "DRIBBLES",
             "TOUCH_TIME",
             "SHOT_DIST",
             "PTS_TYPE",
             "CLOSE_DEF_DIST",
             "FGM",
             "PTS")
for(n in numbers){
  allshots[,n] <- as.numeric(levels(allshots[ ,n])[allshots[,n]])
}
rownames(players) <- players[,2]
allshots$PlayerName <- players[as.character(allshots$PlayerID), 1]
allshots$MINUTES <- as.numeric(str_extract(allshots$GAME_CLOCK,
                                "([0-9]){1,2}"))
allshots$SECONDS <- as.numeric(str_extract(allshots$GAME_CLOCK,
                                           "([0-9]){2}$"))
devtools::use_data(allshots, overwrite = TRUE)



# boxes is an unfortunately named dataset that initially would
# hold boxscores. It holds all three available datasets from
# bbscrapeR's 'rebound' function for the 2013-2014 season
data(boxes) # currently list holding weekly tables from bbscrapeR::rebound

namesplit <- function(n){
  n <- str_split(n, "//")[[1]]
  f <- list(
    "shotchart" = "sc",
    "boxscore" = "bs",
    "pbp" = "pbp"
  )
  s <- list(
    "message" = 'ms',
    "game" = "g",
    "event" = 'ev',
    "teams" = 't',
    "officials" = "of",
    "players" = "pl"
  )
  return(paste0(f[[n[1]]], s[[n[2]]]))
}
w1 <- boxes[[1]]
rm(boxes)
for(w in names(w1)){
  assign(namesplit(w), w1[[w]])
}
rm(w1)
bsof <- as.data.frame(bsof)
bspl <- as.data.frame(bspl)
bst <- as.data.frame(bst)

# boxscore//players has data on the team's highest performing players
# and the scores by quarter in pipe-delimited column "scr"

# iterate through boxscore//players and split "scr" by "|"
prep_bs_player <- function(bspl) {
  scores <- str_split(bspl$scr, "\\|")
  scores <- apply(do.call(rbind, scores)[, c(1:4, 15)], 2, as.numeric)
  colnames(scores) <- c('P1', 'P2', 'P3', 'P4', 'FINAL')
  bspl <- cbind(bspl, scores)
  bspl <- plyr::ddply(bspl, plyr::.(url), function(d) {
    homeWin <- d[1, 'FINAL'] > d[2, 'FINAL']
    if(homeWin){
      d$WIN <- c(1, 0)
    } else {
      d$WIN <- c(0, 1)
    }
    qs <- paste0('P', seq(1, 4))
    d[ ,paste0(qs, '_diff')] <- NA
    d[ ,paste0(qs, '_cumulative')] <- NA
    d$final_margin <- NA
    d[1 ,paste0(qs, '_diff')] <- d[1, qs] - d[2, qs]
    d[2 ,paste0(qs, '_diff')] <- -d[1 ,paste0(qs, '_diff')]
    d[1 ,paste0(qs, '_cumulative')] <- teamCumsum(d[1, ], d[2, ])
    d[2 ,paste0(qs, '_cumulative')] <- -d[1 ,paste0(qs, '_cumulative')]
    margin <- d$FINAL[1] - d$FINAL[2]
    d[1, 'final_margin'] <- margin
    d[2, 'final_margin'] <- -margin
    d
  }, .parallel = TRUE)
  cols <- c("ald", "bld", "rld", "pld", "gstat", "rcd", "stat", "std", "tcd",
            "timout", "tcd", "home_away", "gcd", "WIN", colnames(scores),
            grep('(diff)|(cumulative)|(final_margin)', names(bspl), value = TRUE))
  dates <- str_extract(bspl$url, "[0-9]{8}")
  bspl$date <- strptime(dates, "%Y%m%d")
  bspl$gcd <- paste0(dates, "/", str_extract(bspl$url, "[A-Z]{6}"))
  bspl <- bspl[, cols]
  names(bspl)[which(names(bspl) == 'tcd')] <- 'tm'
  bspl
}

teamCumsum <- function(t1, t2){
  t(apply(t1[, paste0('P', 1:4)], 1, cumsum)) -
    t(apply(t2[, paste0('P', 1:4)], 1, cumsum))
}

prep_pbpev <- function(df) {
  pbpev <- group_by(df, gcd, tm, prd)
  pbpev <- summarise(pbpev,
                     foul = sum(action=='foul'),
                     made_shot = sum(action=='made shot'),
                     missed_shot = sum(action=='missed shot'),
                     rebound = sum(action=='rebound'),
                     substitution = sum(action=='substitution'),
                     timout = sum(action == 'timeout'),
                     turnover = sum(action == 'turnover'),
                     violation = sum(action == 'violation')
  )
  filter(pbpev, prd %in% c(1,2,3,4))
}
data(boxes)

out <- plyr::ldply(boxes, function(d) {
  bs <- d[['boxscore//players']] %>% as.data.frame %>% prep_bs_player
  pbp <- prep_pbpev(d[['pbp//event']])
  pbp <- dcast(melt(pbp, id.vars = c("gcd", "tm", "prd")),
               gcd + tm ~ prd + variable)
  return(inner_join(bs, pbp, by = c('gcd', 'tm')))
}, .parallel = TRUE)

## Game level scores/stats aggregated by quarter
nba_wl_games <- out
## getting highlight url
for(b in boxes){
  d <- unique(b[['boxscore//game']][,c('gcd', 'vid')])
  rownames(d) <- d$gcd
  inds <- nba_wl_games$gcd %in% rownames(d)
  nba_wl_games$video[inds] <- d[nba_wl_games$gcd[inds], 'vid']
}
rm(boxes)
rm(out)


# get aggregated shot data from allshots,
# and merge with nba_wl_games to be used as predictors
data(allshots)
# allshots$date <- as.character(allshots$date)
# allshots$tm <- sapply(allshots$MATCHUP, function(s) {
#   str_extract_all(s, "([A-Z]{3})")[[1]][2]
# })
allshots$tm <- as.character(team_lookup[allshots$tm])
allshots <- filter(allshots, tm != "NULL")
devtools::use_data(allshots, overwrite = TRUE)
allshots_period_sum <- plyr::ddply(allshots, plyr::.(MATCHUP, PERIOD, tm, date),
                                   summarise,
                                   SHOT_CLOCK = mean(SHOT_CLOCK, na.rm=TRUE),
                                   SHOT_DIST = mean(SHOT_DIST, na.rm=TRUE),
                                   DRIBBLES = mean(DRIBBLES, na.rm=TRUE),
                                   TOUCH_TIME = mean(TOUCH_TIME, na.rm=TRUE),
                                   SHOTS = length(SHOT_NUMBER),
                                   DEF_DISTANCE = mean(CLOSE_DEF_DIST, na.rm=TRUE),
                                   PROP_3 = sum(PTS_TYPE == 3)/length(PTS_TYPE),
                                   PROP_MADE = sum(SHOT_RESULT=='made')/length(SHOT_RESULT),
                                   PROP_3_MADE = sum(SHOT_RESULT=='made' & PTS_TYPE==3)/sum(PTS_TYPE == 3),
                                   n = length(MATCHUP),
                                   .parallel = TRUE
)

allshots_game_sum <- plyr::ddply(allshots, plyr::.(MATCHUP, tm, date), summarise,
                                 SHOT_CLOCK = mean(SHOT_CLOCK, na.rm=TRUE),
                                 SHOT_DIST = mean(SHOT_DIST, na.rm=TRUE),
                                 DRIBBLES = mean(DRIBBLES, na.rm=TRUE),
                                 TOUCH_TIME = mean(TOUCH_TIME, na.rm=TRUE),
                                 SHOTS = length(SHOT_NUMBER),
                                 DEF_DISTANCE = mean(CLOSE_DEF_DIST, na.rm=TRUE),
                                 PROP_3 = sum(PTS_TYPE == 3)/length(PTS_TYPE),
                                 PROP_MADE = sum(SHOT_RESULT=='made')/length(SHOT_RESULT),
                                 PROP_3_MADE = sum(SHOT_RESULT=='made' & PTS_TYPE==3)/sum(PTS_TYPE == 3),
                                 n = length(MATCHUP),
                                 .parallel = TRUE
)

allshots_team_sum <- plyr::ddply(allshots_game_sum, plyr::.(tm), summarise,
                                 SHOT_CLOCK = weighted.mean(SHOT_CLOCK, n, na.rm=TRUE),
                                 SHOT_DIST = weighted.mean(SHOT_DIST, n, na.rm=TRUE),
                                 DRIBBLES = weighted.mean(DRIBBLES, na.rm=TRUE),
                                 TOUCH_TIME = weighted.mean(TOUCH_TIME, n, na.rm=TRUE),
                                 SHOTS = mean(SHOTS),
                                 PROP_MADE = weighted.mean(PROP_MADE, n, na.rm=TRUE),
                                 PROP_3_MADE = weighted.mean(PROP_3_MADE, n,
                                                             na.rm = TRUE),
                                 DEF_DISTANCE = weighted.mean(DEF_DISTANCE,
                                                              n, na.rm=TRUE),
                                 PROP_3 = weighted.mean(PROP_3, n, na.rm = TRUE),
                                 .parallel = TRUE
)
team_summary <- allshots_team_sum
devtools::use_data(team_summary, overwrite = TRUE)

allshots_period_sum <- dcast(melt(allshots_period_sum,
                                  id.vars = c('MATCHUP', 'tm', 'date', 'PERIOD')),
                             MATCHUP + tm + date ~ PERIOD + variable)
allshots_period_sum <- allshots_period_sum[, -grep(c("5_|6_|7_|NA_"), names(allshots_period_sum))]
allshots_period_sum$date <- as.Date(allshots_period_sum$date)
allshots_game_sum$date <- as.Date(allshots_game_sum$date)
allshots_period_sum$MATCHUP <- NULL

names(allshots_game_sum)[4:13] <- paste0(names(allshots_game_sum)[4:13], "_game")
nba_wl_games$date <- as.Date(str_extract(nba_wl_games$gcd, "[0-9]{8}"), "%Y%m%d")
nba_wl_games <- inner_join(nba_wl_games, allshots_period_sum)
nba_wl_games <- inner_join(nba_wl_games, allshots_game_sum)

# put <PERIOD>_ at the end of variable names
changeNames <- function(period, names_){
  names_ <- gsub(pattern = paste0("^", period, "_"), "", names_)
  paste0(names_, paste0("_", period))
}

for(i in 1:4){
  n <- grep(paste0("^", i, "_"), names(nba_wl_games))
  names(nba_wl_games)[n] <- changeNames(i, names(nba_wl_games)[n])
}

devtools::use_data(nba_wl_games, overwrite=TRUE)

