#this script produces a GIF file of viewing distances across time
library(LightLogR)
library(tidyverse)
library(gganimate)
library(magick)
library(legendry)
load("data/cleaned/data.RData")

#set visualization parameters
extras <- list(
  geom_tile(),
  scale_fill_viridis_c(breaks = c(0, 50, 100, 150, 200),
    direction = -1, limits = c(0, 200),
                       oob = scales::oob_squish_any,
    guide = "colbar"),
  theme_minimal(),
  guides(colour = "none"),
  coord_fixed(),
  theme(legend.position = "bottom"),
  labs(x = "X position (°)", y = "Y position (°)", 
       fill = "Distance (cm)", alpha = "Confidence (0-255)"))

p <-
  dataVEET3 |> 
  # filter_Datetime(start = "2024-06-10 13:10:00", length = "15 mins") |> 
  cut_Datetime(unit = "5 secs", New.colname = Datetime) |> 
  filter_Time(start = "13:10:00", length = "00:19:59") |> 
  add_Time_col() |> 
  mutate(dist1 = ifelse(dist1 == 0, 5000, dist1)) |> #replace 0 distances with 5m
  # filter(conf1 >= 0.1 | dist1 == Inf) |> #remove data that has less than 10% confidence
  ggplot(aes(x=x.pos, y=y.pos, fill = dist1/10, group = seq_along(Time)))+ #plot the data
  labs(title = "Spatially resolved distance from eye-level. Time: {frame_time}") +
  extras + transition_time(Time) +
  facet_wrap(~Date)

p

Plot <- animate(p, height = 500, width = 500, res = 80, fps = 4, duration = 60, renderer = magick_renderer())

image_write_gif(Plot,"output/abstract.gif", delay = 1/10)
image_write_video(Plot, "output/abstract.mp4", framerate = 5)  
