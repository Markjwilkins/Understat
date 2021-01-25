Train an xG model on ~180,000 Understat shots following steps from: https://www.thesignificantgame.com/portfolio/expected-goals-model-with-tidymodels/

library(tidyverse)
library(ggsoccer)
library(tidymodels)
library(readr)
library(viridis)

##standardise coords from 100x100 to 105x68
https://raw.githubusercontent.com/RobWHickman/Rteta/master/R/standardize_coordinates.R

##105x68 pitch for plotting
custom <- list(
  length = 105,
  width = 68,
  penalty_box_length = 16.5,
  penalty_box_width = 40.32,
  six_yard_box_length = 5.5,
  six_yard_box_width = 18.32,
  penalty_spot_distance = 11,
  goal_width = 7.32,
  origin_x = 0,
  origin_y = 0
)

##load understat shot data 2014-2019 & standardise x/y
data<-read_csv("understat_shots_2014-2019.csv") %>% 
  standardize_opta_x(cols = c("x")) %>% 
  standardize_opta_y(cols = c("y"))

##filter data - leave only open play shots - remove blocked shots/own goals
data<-data %>% 
  filter(situation=="OpenPlay",
         shottype%in%c("RightFoot","LeftFoot"),
         !result%in%c("BlockedShot", "OwnGoal"))

##calculate distance
calc_dist <- function(x_pos, y_pos){
  
  x_shift <- (105 - x_pos)
  y_shift <- abs(34 - y_pos)
  
  distance <- sqrt(x_shift*x_shift + y_shift*y_shift)
}

##calculate angle
calc_angle <- function(x_pos, y_pos){
  
  x_shift <- (105 - x_pos)
  y_shift <- abs(34 - y_pos)
  
  angle <- atan((7.32*x_shift)/(x_shift*x_shift + y_shift*y_shift - (7.32/2)*(7.32/2)))
  angle <- ifelse(angle < 0, angle + pi, angle)
  
  angle_degrees <- angle*180/pi
}

##calc dist/angle based on x/y
data<-data %>% 
  mutate(distance = goal_distance(x,y),
         angle = goal_angle(x,y))

##create goal outcome
data$is_goal <- ifelse(data$result=="Goal", 1,0)
data$is_goal <- factor(data$is_goal, levels = c("1", "0"))

#################################################################

set.seed(seed = 1234) 

##split data - test/train
train_test_split <- initial_split(data = data_1, prop = 0.80) 

train_data <- train_test_split %>% training() 
test_data  <- train_test_split %>% testing()

##create predictors
xg_recipe <- 
  recipe(is_goal ~ distance + angle + x + y, data = train_data) %>% 
  update_role(x, y, new_role = "ID") 

##logistic regression
model <- logistic_reg() %>% 
  set_engine("glm")

xg_wflow <- 
  workflow() %>% 
  add_model(model) %>% 
  add_recipe(xg_recipe)

##train
xg_fit <- 
  xg_wflow %>% 
  fit(data = train_data)

xg_fit %>% 
  pull_workflow_fit() %>% 
  tidy()

##create artificial data set 105x68
artificial_shots <- crossing(x = seq(50, 105, by = 0.5), y = seq(0, 68, by = 0.5)) %>%
  mutate(distance = goal_distance(x, y),
         angle = goal_angle(x, y))

##use trained model to predict artificial xG
data_to_plot <- predict(xg_fit, artificial_shots, type = "prob") %>% 
  bind_cols(artificial_shots) %>%
  rename("xG" = ".pred_1")

##plot artificial data to track xG assigned
ggplot(data = data_to_plot, aes(x=x, y=y))+
  annotate_pitch(fill = "black", dimensions = custom)+
  coord_flip(xlim = c(60, 105),
             ylim = c(0, 68))+
  geom_tile(aes(fill = xG))+
  scale_fill_viridis(option = "inferno") +
  annotate_pitch_one(fill = "NA", dimensions = custom) +
  theme_pitch()+
  theme(legend.position = c(.5, .2),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.text = element_text(colour = "white"),
        legend.title = element_text(colour = "white", face = "bold"),
        plot.background = element_rect(fill = "black"))
