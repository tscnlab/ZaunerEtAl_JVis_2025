#this script produces a GIF file of viewing distances across time
library(LightLogR)
library(tidyverse)
library(gganimate)
load("data/cleaned/data.RData")

p <- 
  TOF_long %>% 
  ggplot(aes(x=x.pos, y=y.pos, fill = dist1)) + 
  labs(title = "TOF at {frame_time}") +
  extras + transition_time(Datetime)

animate(p, duration = 60, fps = 5)