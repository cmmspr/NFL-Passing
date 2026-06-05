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

team_summary <- pbp_2022 %>%
	filter(!is.na(posteam)) %>%
	group_by(posteam) %>%
	summarize(
		plays = n(),
		pass_plays = sum(pass == 1, na.rm = TRUE),
		rush_plays = sum(rush == 1, na.rm = TRUE),
		total_epa = sum(epa, na.rm = TRUE),
		avg_epa = mean(epa, na.rm = TRUE),
		success_rate = mean(success == 1, na.rm = TRUE)
	) %>%
	arrange(desc(avg_epa))

team_summary

library(tidyverse)
library(ggplot2)
library(ggthemes)

library(tidyverse)
library(nflfastR)

# Load 2022 play-by-play
pbp_2022 <- load_pbp(2022)

# Filter to 4th-down plays
fourth_down <- pbp_2022 %>%
	filter(down == 4)

league_4th_summary <- fourth_down %>%
	summarize(
		total_4th_downs = n(),
		go_for_it = sum(play_type == "run" | play_type == "pass", na.rm = TRUE),
		punts = sum(play_type == "punt", na.rm = TRUE),
		field_goals = sum(play_type == "field_goal", na.rm = TRUE),
		conversion_rate = mean(first_down == 1, na.rm = TRUE)
	)

league_4th_summary

team_tendencies <- fourth_down %>%
	filter(!is.na(posteam)) %>%
	group_by(posteam) %>%
	summarize(
		attempts = n(),
		go_for_it = sum(play_type %in% c("run", "pass")),
		punt = sum(play_type == "punt"),
		fg = sum(play_type == "field_goal"),
		go_rate = go_for_it / attempts
	) %>%
	arrange(desc(go_rate))

team_tendencies
team_tendencies <- fourth_down %>%
	filter(!is.na(posteam)) %>%
	group_by(posteam) %>%
	summarize(
		attempts = n(),
		go_for_it = sum(play_type %in% c("run", "pass")),
		punt = sum(play_type == "punt"),
		fg = sum(play_type == "field_goal"),
		go_rate = go_for_it / attempts
	) %>%
	arrange(desc(go_rate))

team_tendencies

