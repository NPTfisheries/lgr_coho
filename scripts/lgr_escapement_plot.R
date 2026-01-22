# Purpose: Make Figure for LGR Escapement
#
# Authors: Mike Ackerman
# 
# Created: January 22, 2026
#   Last Modified:

# clear environment
rm(list = ls())

# load libraries
library(tidyverse)
library(readxl)

# read in data
lgr_df = read_excel(path = "C:/Git/SnakeRiverFishStatus/output/syntheses/LGR_Coho_all_summaries_2025-09-16.xlsx",
                    sheet = "LGR_Esc")

lgr_p = lgr_df %>%
  mutate(
    origin2 = case_when(
      origin %in% c("Hatchery Clip", "Hatchery No-Clip") ~ "Hatchery",
      origin == "Natural"                                ~ "Natural",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(origin2)) %>%
  group_by(spawn_yr, origin2) %>%
  summarize(estimate = sum(estimate), .groups = "drop") %>%
  mutate(spawn_yr = factor(spawn_yr)) %>%
  ggplot(aes(x = spawn_yr, y = estimate, fill = origin2)) +
  geom_col(color = "black") +
  scale_y_continuous(
    breaks = seq(0, 30000, by = 5000),
    labels = scales::comma
  ) +
  # scale_fill_manual(
  #   values = c(
  #     "Natural" = "#1b9e77",            # muted green
  #     "Hatchery" = "#d95f02"   # muted orange
  #   )
  # ) +
  labs(x = "Run Year",
       y = "Escapement",
       fill = "Origin") +
  theme_classic() +
  theme(
    axis.title = element_text(color = "black", size = 14, face = "bold"),
    axis.text  = element_text(color = "black", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_text(color = "black", size = 12),
    legend.text = element_text(color = "black", size = 10)
  )
lgr_p
ggsave(lgr_p, filename = "./output/figures/lgr_coho_esc.png")
