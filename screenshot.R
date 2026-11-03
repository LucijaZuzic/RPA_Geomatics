# Clear environment
rm(list=ls())

## The R script for Žužić et al. manuscript

# Set path to the working directory containing prepared data set

library(tidyverse)
filter <- dplyr::filter
select <- dplyr::select
getCurrentFileLocation <-  function()
{
  this_file <- commandArgs() %>% 
    tibble::enframe(name = NULL) %>%
    tidyr::separate(col = value, into = c("key", "value"), sep = "=", fill = 'right') %>%
    dplyr::filter(key == "--file") %>%
    dplyr::pull(value)
  if (length(this_file) == 0) {
    this_file <- rstudioapi::getSourceEditorContext()$path
  }
  return(dirname(this_file))
}

setwd(getCurrentFileLocation())

# Load required libraries
library(ggplot2)
library(maps)
library(mapdata)
library(patchwork) # For combining the 3 maps into one figure

# High-resolution world map data (matches screenshot coastline detail)
world_hires <- map_data("worldHires")

# Official IGS station coordinates and regional bounding boxes
stations <- data.frame(
  id    = c("matera", "iqaluit", "darwin"),
  label = c("Matera, Italy", "Iqaluit, Canada", "Darwin, Australia"),
  lon   = c(16.704458, -68.510497, 131.132744),
  lat   = c(40.649131,  63.755964, -12.843697),
  # Regional zoom bounds (xlim and ylim)
  xmin  = c(  4.5, -85.0, 112.0),
  xmax  = c( 24.5, -52.0, 154.0),
  ymin  = c( 36.0,  55.0, -30.0),
  ymax  = c( 49.0,  72.0,  -5.0),
  # Vertical text offset below the red square
  v_off = c( -0.65, -0.90, -1.30)
)

# Function to create the exact style from your screenshot
make_station_map <- function(st_row) {
  ggplot() +
    geom_polygon(
      data = world_hires,
      aes(x = long, y = lat, group = group),
      fill = "white",
      color = "black",
      linewidth = 0.35
    ) +
    # Red square marker
    geom_point(
      data = st_row,
      aes(x = lon, y = lat),
      color = "red",
      shape = 15,
      size = 2.8
    ) +
    # Red label below marker
    geom_label(
      data = st_row,
      aes(x = lon, y = lat + v_off, label = label),
      color = "white",
      fill = "red",
      size = 4.2,
      fontface = "plain"
    ) +
    coord_quickmap(
      xlim = c(st_row$xmin, st_row$xmax),
      ylim = c(st_row$ymin, st_row$ymax),
      expand = FALSE
    ) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(5, 5, 5, 5)
    )
}

# 1. Generate and save each individual map
p_matera  <- make_station_map(stations[1, ])
p_iqaluit <- make_station_map(stations[2, ])
p_darwin  <- make_station_map(stations[3, ])

ggsave("map_matera.png",  p_matera,  width = 6.5, height = 4.2, dpi = 300)
ggsave("map_iqaluit.png", p_iqaluit, width = 6.5, height = 4.2, dpi = 300)
ggsave("map_darwin.png",  p_darwin,  width = 6.5, height = 4.2, dpi = 300)

# 2. Combine all 3 regional maps side-by-side for the manuscript
p_combined <- p_iqaluit + p_matera + p_darwin + plot_layout(ncol = 3)
ggsave("map_all_three_panels.png", p_combined, width = 15, height = 4.2, dpi = 300)

world_map <- map_data("world") %>%
  dplyr::filter(region != "Antarctica")

p_global <- ggplot() +
  geom_polygon(
    data = world_map,
    aes(x = long, y = lat, group = group),
    fill = "white",
    color = "black",
    linewidth = 0.25
  ) +
  geom_point(
    data = stations,
    aes(x = lon, y = lat),
    color = "red",
    shape = 15,
    size = 3
  ) +
  geom_label(
    data = stations,
    aes(x = lon, y = lat - 5.5, label = label),
    color = "white",
    fill = "red",
    size = 4
  ) +
  coord_quickmap(xlim = c(-170, 175), ylim = c(-55, 82), expand = FALSE) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

ggsave("map_global_three_stations.png", p_global, width = 10, height = 5.2, dpi = 300)