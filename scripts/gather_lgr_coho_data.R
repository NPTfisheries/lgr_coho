# Purpose: Query coho salmon PIT tagged or observed at LGR as well as any subsequent 
# detections upstream
# Authors: Mike Ackerman and Ryan N. Kinzer 
# 
# Created: October 25, 2023
#   Last Modified: November 6, 2024

# install PITcleanr, if not already installed
# remotes::install_github("KevinSee/PITcleanr@develop")

# load necessary libraries
library(PITcleanr)
library(tidyverse)
library(here)
library(janitor)

# load configuration
load("data/site_config_LGR_20231117.rda") ; rm(sites_sf, flowlines, parent_child, pc_nodes, node_paths)
yr = 2024

# query DART to return all PIT tags observed at GRA, as well as any subsequent detections upstream,
# and compress them
dart_ls = compressDART(species = "Coho",
                       loc = "GRA",
                       spawn_year = yr,
                       configuration = configuration)

# extract all dart observations
dart_obs = dart_ls %>%
  pluck("dart_obs")

# extract mark data from the dart observations
mark_data = dart_obs %>%
  select(tag_code, 
         file_id, 
         contains('mark_'), 
         contains('event_'), 
         contains('rel_'), 
         flags,
         -event_type_name) %>%
  distinct(tag_code, 
           .keep_all = TRUE)

# just fish marked at lgr in 2023
mark_lgr_23 = mark_data %>%
  filter(mark_site == "LGRLDR",
         year(mark_date) == yr)

# number of coho pit tagged per day at LGR
pits_per_day = mark_lgr_23 %>%
  tabyl(mark_date) %>%
  adorn_totals()

# compile compressed obs for fish marked at lgr in 2023
compress_obs = dart_ls %>%
  pluck("compress_obs") %>%
  inner_join(mark_lgr_23) %>%
  # I need to figure out what these few observations are...
  filter(!is.na(node))

# quick summary
compress_obs %>%
  tabyl(node)

# how many tags have been detected somewhere other than LGR?
compress_obs %>%
  filter(!node %in% c("LGR", "GRS")) %>%
  distinct(tag_code) %>%
  nrow()

# and where?
compress_obs %>%
  filter(!node %in% c("LGR", "GRS")) %>%
  distinct(tag_code, node) %>%
  tabyl(node) %>%
  adorn_totals()

fallbacks = compress_obs %>%
  group_by(tag_code) %>%
  filter(any(node == "GRS")) %>%
  distinct(tag_code) %>%
  nrow() 

reascenders = compress_obs %>%
  group_by(tag_code) %>%
  filter(any(node == "GRS") & any(node == "LGR" & slot > min(slot[node == "GRS"]))) %>%
  distinct(tag_code) %>%
  nrow() 

# write out objects for analysis
write_csv(compress_obs,
          file = paste0("data/sy", yr, "_coho_lgr_dart_obs.csv"))
