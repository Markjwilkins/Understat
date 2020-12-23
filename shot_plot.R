library(tidyverse)
library(ggsoccer) ##pitch plot
library(janitor)
library(here)

##download yearly .csv files in https://github.com/Markjwilkins/Understat and add to local folder

##select file path where understat files are saved e.g:
file_path<-"understat_shot_data"

##bind all .csv files and uniforn column names
all_shots <- list.files(path = file_path,     
                       pattern = "*.csv", full.names = TRUE) %>% 
  lapply(read_csv) %>%                                            
  bind_rows %>% 
  clean_names()

##filter player and remove penalties
player_plot<-all_shots %>% 
  filter(player=="Jamie Vardy",
         !situation=="Penalty") %>% 
  mutate(outcome = ifelse(result=="Goal", "Goal", "No Goal")) %>% 
  mutate(league_year = glue::glue("{year}: {team_name}"))

##create some summary stats
player_summary<-player_plot %>% 
  group_by(league_year) %>% 
  summarise(total_shots = n(),
            total_xg = sum(round(x_g, digits = 2)))

##create some summary stats
player_summary_1<-player_plot %>% 
  group_by(league_year) %>% 
  summarise(total_goals = sum(outcome=="Goal"))

##join summary stats together and create new xg_per_shot variable
player_summary_1<-left_join(player_summary,player_summary_1, by = "league_year") %>% 
  mutate(xg_per_shot = signif(total_xg/total_shots, digits = 2))

##set primary colour
primary_col<-"grey97"

##plot!
p1<-ggplot(player_plot) +
  annotate_pitch(fill = primary_col) +
  geom_point(aes(x=x, y=y, size = x_g, colour = outcome)) +
  geom_point(data = player_plot %>% 
               filter(outcome=="Goal"),
             aes(x=x, y=y, size = x_g, colour = outcome), shape = 21, fill = "#A8E6CE", stroke = 0.5, colour = "black") +
  scale_colour_manual(values = c("#A8E6CE", "#FF8C94"), labels = c("Goal", "No Goal"), name = "Shot Outcome")+
  coord_flip(xlim = c(49, 100),
             ylim = c(-5, 105)) +
  theme_pitch() +
  labs(title = paste(player_plot$player),
       subtitle = paste(min(player_plot$year), "-", max(player_plot$year), "// All Penalties Removed"),
       size = "Expected Goals") +
  geom_label(data = player_summary_1, size=4, fill = primary_col, aes(x=65, y=20, label = paste0("Total Shots: ", total_shots)))+
  geom_label(data = player_summary_1, size=4, fill = primary_col, aes(x=58.5, y=20, label = paste0("Total Goals: ", total_goals)))+
  geom_label(data = player_summary_1, size=4, fill = primary_col, aes(x=65, y=80, label = paste0("Total xG: ", total_xg)))+
  geom_label(data = player_summary_1, size=4, fill = primary_col, aes(x=58.5, y=80, label = paste0("xG Per Shot: ", xg_per_shot)))+
  facet_wrap(~league_year) +
  
  theme(plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        strip.background = element_blank(),
        strip.text = element_text(size = 10, face = "bold"),
        legend.key = element_rect(fill = primary_col),
        legend.background = element_rect(fill = primary_col),
        legend.position = "bottom",
        legend.title = element_text(face = "bold"),
        plot.background = element_rect(fill = primary_col))

p1
