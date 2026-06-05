install.packages("nflreadr")

library(tidyverse)

nfl_passes <- read_csv("https://raw.githubusercontent.com/36-SURE/2026/main/data/nfl_passes.csv")

install.packages("nflfastR")
library(nflfastR)

install.packages("nflverse")
library(nflverse)

# Load packages
library(tidyverse)
library(nflreadr)
library(nflfastR)
library(nflverse)

# -----------------------------
# 1. Load play-by-play data
# -----------------------------
pbp_2022 <- load_pbp(2022)

# -----------------------------
# 2. Basic league summary
# -----------------------------
league_summary <- pbp_2022 %>%
	summarize(
		total_plays = n(),
    total_passes = sum(pass == 1, na.rm = TRUE),
    total_rushes = sum(rush == 1, na.rm = TRUE),
    total_epa = sum(epa, na.rm = TRUE),
    avg_epa_play = mean(epa, na.rm = TRUE)
  )

league_summary
