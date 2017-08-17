library(dplyr)
library(tidyr)

ginf <- read.csv("ginf.csv") 
# 10112 observations, 18 variables 

events <- read.csv("events.csv")
# 941009 observations, 22 variables

# function to binarize a given vector   
binarize <- function(x){
  ifelse(is.na(x), 0, 1)
}

# join ginf and events data frames into a single data frame 
ginf_events <- left_join(events, ginf, by = "id_odsp")

# filter and clean up missing values 
ginf_events <- ginf_events %>%
  subset(season != 2017) %>% 
#  observations, 39 variables 

  subset(select = -c(link_odsp, odd_over, odd_under, odd_bts, odd_bts_n)) %>% 
  subset(time <= 90) %>% 
  subset(adv_stats == TRUE) %>% 

  mutate(
    player = replace(player, player == "", NA),
    player2 = replace(player2, player2 == "", NA), 
    location = replace(location, is.na(location), 19), 
    
    has_event_type2 = binarize(event_type2), 
    has_player = binarize(player), 
    has_player2 = binarize(player2), 
    has_player_in = binarize(player_in), 
    has_player_out = binarize(player_out), 
    has_shot_place = binarize(shot_place), 
    has_shot_outcome = binarize(shot_outcome), 
    has_location = binarize(location), 
    has_bodypart = binarize(bodypart), 
    has_situation = binarize(situation) 
  ) 
  
# create clean data set 
write.csv(ginf_events, file = "ginf_events.csv", row.names = FALSE)
