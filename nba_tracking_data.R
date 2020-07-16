# Set up 
library(RCurl)
library(jsonlite)
library(tidyverse)
library(trajr)
library(lubridate)
library(hms)
library(janitor)
source('C:/Users/alexl/Downloads/_functions.R')
source('C:/Users/alexl/Downloads/fullcourt.R')
nba_tracking <- sportvu_convert_json("C:/Users/alexl/Desktop/0021500440.json")
str(nba_tracking)
plays <- read.csv("C:/Users/alexl/Downloads/LAL_LAC_playbyplay.csv") %>% 
  clean_names()
full_track <- merge(nba_tracking, plays, 
                    by.x = "event.id",
                    by.y = "eventnum", 
                    keep = TRUE)
full_track <- full_track %>% 
  unite("play", c(homedescription, visitordescription), na.rm = TRUE, remove = FALSE)
unique(full_track$score)
full_track <- separate(full_track, col = "score", into = c("Clippers", "Lakers"), sep = "-")
full_track$Clippers <- as.numeric(full_track$Clippers)
full_track$Lakers <- as.numeric(full_track$Lakers)
full_track <- full_track %>% 
  fill(Lakers) %>% 
  fill(Clippers)
sum(is.na(full_track$Lakers))
sum(is.na(full_track$Clippers))
full_track[0:42350, ]$Lakers = 0
full_track[0:42350, ]$Clippers = 0
unique(full_track$Lakers)
full_track$scoremargin <- full_track$Clippers - full_track$Lakers
full_track <- full_track %>% 
  mutate(game_clock = hms::as_hms(game_clock))

game_moments <- function(range){
  full_track %>% 
    filter(event.id %in% range) %>% 
    group_by(event.id) %>% 
    summarise(start_time = max(game_clock),
              end_time = min(game_clock)) %>% 
    ggplot() +
    geom_segment(aes(x = start_time, 
                     xend = end_time, 
                     y = event.id,
                     yend = event.id),
                     size = 100 / length(range)) +
    scale_x_reverse(breaks = as.numeric(ms("12:00", "11:00", "10:00", "09:00", "08:00", "07:00", "06:00", "05:00", "04:00", "03:00", "02:00", "01:00", "00:00")),
                    labels = function(l) strftime(hms(l),"%M:%S")) +
    scale_y_reverse()+
    labs(x = "Game Clock",
         y = "Moment") + 
    theme_bw()
}

game_moments(1:20)

full_track %>% 
  group_by(quarter, event.id, game_clock) %>% 
  summarise(n()) %>% 
  filter('n()'!= 11) %>% 
  head(n = 15)
  


ball_distance <- player_dist_matrix(nba_tracking, 2) %>% 
  select(starts_with("ball"))

selected_play <- full_track %>% 
  filter(event.id == 2, lastname == "ball") %>% 
  mutate(ball_carrier = apply(ball_distance, 1, which.min),
         ball_carrier_distance = apply(ball_distance, 1, min))
coords <- selected_play %>% 
  select(x_loc, y_loc, game_clock) %>% 
  TrajFromCoords(fps = 25)
traj_data <- coords %>% 
  mutate(inst_speed = c(NA, TrajDerivatives(coords)[[1]]),
         inst_acceleration = c(NA, NA, TrajDerivatives(coords)[[3]]),
         dir_change = c(NA, NA, TrajDirectionalChange(coords)))

fullcourt() + 
  geom_point(data = selected_play, 
             aes(x_loc, y_loc)) + 
  scale_color_manual(values = c("Lakers"="#fdb927",
                                "Clippers"="#1D428A",
                                "ball" = "#FA8320")) +  scale_y_reverse()
