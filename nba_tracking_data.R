library(RCurl)
library(jsonlite)
library(plotly)
library(tidyverse)
source('C:/Users/alexl/Downloads/_functions.R')
source('C:/Users/alexl/Downloads/fullcourt.R')
nba_tracking <- sportvu_convert_json("C:/Users/alexl/Desktop/0021500440.json")
plays <- read.csv("C:/Users/alexl/Downloads/LAL_LAC_playbyplay.csv")
full_track <- merge(nba_tracking, plays, 
                    by.x = "event.id",
                    by.y = "EVENTNUM", 
                    keep = TRUE)
full_track <- full_track %>% 
  unite("Play", c(HOMEDESCRIPTION, VISITORDESCRIPTION), na.rm = TRUE, remove = FALSE)
unique(full_track$SCORE)
full_track <- separate(full_track, col = "SCORE", into = c("Clippers", "Lakers"), sep = "-")
full_track$Clippers <- as.numeric(full_track$Clippers)
full_track$Lakers <- as.numeric(full_track$Lakers)
full_track$SCOREMARGIN <- full_track$Clippers - full_track$Lakers
unique(full_track$Lakers)
sum(is.na(full_track$Lakers))
sum(is.na(full_track$Clippers))
full_track[0:42350, ]$Lakers = 0
full_track[0:42350, ]$Clippers = 0
unique(full_track$Lakers)
head(full_track)
