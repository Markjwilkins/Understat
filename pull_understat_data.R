library(tidyverse) 
library(understatr)
library(here) ##for saving output - more stable than set_wd()
library(ggrepel)
library(ggsoccer)

##pull available leagues
leagues<-get_leagues_meta()

unique(leagues$league_name)

##remove RFPL to leave top 5 European leagues
leagues<-leagues%>%
  filter(!league_name == "RFPL")

##pull team match data
team_data<-map_dfr(unique(leagues$league_name), get_league_teams_stats, year = 2020)

##pull player meta data
player_data<-map_dfr(unique(team_data$team_name), get_team_players_stats, year = 2020)

##create some summary stats p90
summary<-player_data %>% 
  mutate(nineties = time/90,
         npxg_p90 = npxG/nineties,
         xa_p90 = xA/nineties) %>% 
  filter(time>900)

##save player meta data
write_csv(summary, here("player_meta_2020.csv"))

##quick plot npxg_p90 & xa_p90
summary %>% 
  filter(time>900) %>% 
  ggplot(aes(x=npxg_p90, y=xa_p90))+
  geom_point() +
  geom_text_repel(data = summary %>% 
                    filter(npxg_p90>0.4 | xa_p90>0.4),
                  aes(label = player_name))+
  labs(title = "Attacking Contribution",
       subtitle = "European Big 5 Leagues // >900mins",
       x = "NPxG P90",
       y = "xA P90")

##create playId vector
players<-c(player_data$player_id)

##pull all location x/y for playerId's
shot_data <- players %>% 
  map_dfr(.,possibly(get_player_shots,otherwise=NULL))

##filter 2020 shots only
shots_data_2020<-shot_data %>% 
  filter(year == 2020)

##save 2020 shot data
write_csv(shots_data_2020, here("shots_data_2020.csv"))

##plot Lewandowski non-penalty shots
shots_data_2020 %>% 
  filter(player=="Robert Lewandowski",
         !situation=="Penalty") %>% 
  mutate(X = X*100,
         Y = Y*100) %>% 
  ggplot(aes(x = X, y = 100-Y))+
    annotate_pitch()+
    geom_point(aes(colour = result, size = xG)) +
  coord_flip(xlim = c(50,100),
             ylim = c(0,100)) +
  theme_pitch() +
  labs(title = "Robert Lewandowski",
       subtitle = "Shot Locations 2020/2021 - Penalties Removed")
