library(tidyverse) 
library(understatr)
library(here)

##pull available leagues
leagues<-get_leagues_meta()

##remove RFPL to leave top 5 European leagues
leagues=leagues%>%
  filter(!league_name == "RFPL")

##pull team match data
team_data<-map_dfr(unique(leagues$league_name), get_league_teams_stats, year = 2020)

##pull player meta data
player_data<-map_dfr(unique(team_data$team_name), get_team_players_stats, year = 2020) 

##create playId vector
players<-c(player_data$player_id)

##pull all location x/y for playerId's
shot_data <- players %>% 
  map_dfr(.,possibly(get_player_shots,otherwise=NULL))

##filter 2020 shots only
shots_data_2020<-shot_data %>% 
  filter(year == 2020)

##save player meta data .csv
write_csv(player_data, here("player_meta_2020.csv"))

##save shot location data .csv
write_csv(shots_data_2020, here("understat_shot_data_2020.csv"))
