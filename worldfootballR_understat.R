##install worldfootballR
devtools::install_github("JaseZiv/worldfootballR")

##load worldfootballR and tidverse
library(worldfootballR)
library(tidyverse)

##top 5 leagues to pull shot data
leagues <- 
  c("EPL",
    "La liga",
    "Bundesliga",
    "Serie A",
    "Ligue 1")

##extract x/y shot data from understat for the 2020/21 season
df <- 
  map_df(leagues,
         ~understat_league_season_shots(league = .x,
                                       season_start_year = 2020))



