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

pbp_2022 <- load_pbp(2022)

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

pbp_2022 <- load_pbp(2022)

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

team_success <- fourth_down %>%
	filter(play_type %in% c("run", "pass")) %>%
	group_by(posteam) %>%
	summarize(
		attempts = n(),
		conversions = sum(first_down == 1, na.rm = TRUE),
		conversion_rate = conversions / attempts,
		avg_epa = mean(epa, na.rm = TRUE)
	) %>%
	arrange(desc(conversion_rate))

team_success

library(ggplot2)

ggplot(team_tendencies, aes(x = reorder(posteam, go_rate), y = go_rate)) +
	geom_col(fill = "darkred") +
	coord_flip() +
	labs(
		title = "2022 4th-Down Go-For-It Rate by Team",
		x = "Team",
		y = "Go-For-It Rate"
	) +
	scale_y_continuous(labels = scales::percent_format()) +
	theme_minimal()

library(tidyverse)
library(nflfastR)

pbp_2022 <- load_pbp(2022)

# Filter to 4th-down plays
fourth_down <- pbp_2022 %>%
	filter(down == 4)

league_4th_pass <- fourth_down %>%
	filter(pass == 1) %>%
	summarize(
		total_4th_passes = n(),
		conversion_rate = mean(first_down == 1, na.rm = TRUE),
		avg_epa = mean(epa, na.rm = TRUE)
	)

league_4th_pass

team_4th_pass_tendencies <- fourth_down %>%
	filter(!is.na(posteam)) %>%
	group_by(posteam) %>%
	summarize(
		total_4th = n(),
		pass_4th = sum(pass == 1, na.rm = TRUE),
		run_4th = sum(rush == 1, na.rm = TRUE),
		pass_rate = pass_4th / total_4th
	) %>%
	arrange(desc(pass_rate))

team_4th_pass_tendencies

team_4th_pass_success <- fourth_down %>%
	filter(pass == 1) %>%
	group_by(posteam) %>%
	summarize(
		attempts = n(),
		conversions = sum(first_down == 1, na.rm = TRUE),
		conversion_rate = conversions / attempts,
		avg_epa = mean(epa, na.rm = TRUE)
	) %>%
	arrange(desc(conversion_rate))

team_4th_pass_success

library(ggplot2)

ggplot(team_4th_pass_tendencies, aes(x = reorder(posteam, pass_rate), y = pass_rate)) +
	geom_col(fill = "steelblue") +
	coord_flip() +
	labs(
		title = "2022 4th-Down Pass Rate by Team",
		x = "Team",
		y = "Pass Rate"
	) +
	scale_y_continuous(labels = scales::percent_format()) +
	theme_minimal()

install.packages("sportyR")
library(sportyR)

library(tidyverse)
library(nflfastR)
library(sportyR)

fourth_passes <- pbp_2022 %>%
	filter(down == 4, pass == 1, !is.na(air_yards), !is.na(pass_location))

fourth_passes %>%
	mutate(depth_bucket = case_when(
		air_yards <= 0 ~ "Behind LOS",
		air_yards <= 5 ~ "Short (0–5)",
		air_yards <= 10 ~ "Intermediate (6–10)",
		TRUE ~ "Deep (10+)"
	)) %>%
	group_by(depth_bucket) %>%
	summarize(
		attempts = n(),
		conversion_rate = mean(first_down == 1, na.rm = TRUE)
	) %>%
	arrange(desc(conversion_rate))

fourth_passes %>%
	group_by(pass_location) %>%
	summarize(
		attempts = n(),
		conversion_rate = mean(first_down == 1, na.rm = TRUE)
	) %>%
	arrange(desc(conversion_rate))

fourth_passes %>%
	mutate(at_sticks = air_yards >= ydstogo - 1 & air_yards <= ydstogo + 2) %>%
	group_by(at_sticks) %>%
	summarize(
		attempts = n(),
		conversion_rate = mean(first_down == 1, na.rm = TRUE)
	)

fourth_passes <- pbp_2022 %>%
	filter(down == 4, pass == 1) %>%
	mutate(
		success = if_else(first_down == 1, "Success", "Failure")
	)

ggplot(fourth_passes, aes(x = air_yards, y = pass_location, color = success)) +
	geom_jitter(alpha = 0.7, size = 2.5, height = 0.15) +
	scale_color_manual(values = c("Success" = "green3", "Failure" = "red3")) +
	labs(
		title = "4th Down Passing: Success vs Failure (2022)",
		subtitle = "Depth of target (air_yards) vs horizontal location (left/middle/right)",
		x = "Air Yards (Depth of Throw)",
		y = "Pass Location",
		color = "Outcome"
	) +
	theme_minimal()