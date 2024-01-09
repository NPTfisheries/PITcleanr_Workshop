# Author: Kevin See
# Purpose: create figures for PITcleanr presentation
# Created: 12/1/2023
# Last Modified: 12/1/2023
# Notes:

#-----------------------------------------------------------------
# load needed libraries
library(tidyverse)
library(sf)
library(nhdplusTools)
# library(magrittr)
library(here)
# library(usethis)
library(PITcleanr)

#-----------------------------------------------------------------
# where should figures be saved?
save_loc = here("rmd/images")

#-----------------------------------------------------------------
# set the root site
root_site = c("TUM",
              "PRA",
              "PRO",
              "GRA",
              "LEMTRP")[2]

# vector of all files in PITcleanr package for that root site
all_data_files <- 
  list.files(system.file("extdata", package = 'PITcleanr'), recursive = T, full.names = T)
all_data_files <-
  all_data_files[str_detect(all_data_files, root_site)]


# load sites, flowlines, parent-child and configuration tables
system.file("extdata", 
            paste0(root_site, "_site_config.Rdata"), 
            package = "PITcleanr") |> 
  load()


#-----------------------------------------------------------------
# Site map
site_map <-
  ggplot() +
  geom_sf(data = flowlines,
          aes(color = as.factor(StreamOrde),
              size = StreamOrde)) +
  scale_color_viridis_d(direction = -1,
                        option = "D",
                        name = "Stream\nOrder",
                        end = 0.8) +
  scale_size_continuous(range = c(1, 4),
                        guide = 'none') +
  # geom_sf(data = nhd_list$basin,
  #         fill = NA,
  #         lwd = 2) +
  geom_sf(data = sites_sf,
          size = 4,
          color = "black") +
  # ggrepel::geom_label_repel(
  #   data = sites_sf,
  #   aes(label = site_code, 
  #       geometry = geometry),
  #   size = 3,
  #   stat = "sf_coordinates",
  #   min.segment.length = 0,
  #   max.overlaps = 50
  # ) +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

site_map

ggsave(paste0(save_loc, "/",
             root_site, "_site_map.png"),
       site_map,
       width = 5,
       height = 8)
