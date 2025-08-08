#this script produces a GIF file of viewing distances across time
library(LightLogR)
library(tidyverse)
library(gganimate)
load("data/cleaned/data.RData")

#set visualization parameters
extras <- list(
  geom_tile(),
  scale_fill_viridis_c(direction = -1, limits = c(0, 200),
                       oob = scales::oob_squish_any),
  theme_minimal(),
  guides(colour = "none"),
  coord_fixed(),
  labs(x = "X position (°)", y = "Y position (°)", 
       fill = "Distance (cm)", alpha = "Confidence (0-255)"))

p <-
  dataVEET3 |> filter_Datetime(start = "2024-06-10 13:10:00", length = "5 mins") |> 
    add_Time_col() |> 
  mutate(dist1 = ifelse(dist1 == 0, Inf, dist1)) |> #replace 0 distances with 5m
  filter(conf1 >= 0.1 | dist1 == Inf) |> #remove data that has less than 10% confidence
  ggplot(aes(x=x.pos, y=y.pos, fill = dist1/10))+ #plot the data
  labs(title = "Spatially resolved distance from eye-level at {closes_state}") +
  extras + transition_time(Time)

anim <- animate(p, duration = 60, fps = 24)
anim
