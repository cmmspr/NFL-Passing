install.packages("nflreadr")

library(tidyverse)

nfl_passes <- read_csv("https://raw.githubusercontent.com/36-SURE/2026/main/data/nfl_passes.csv")

fourth_passes_clean <- fourth_passes %>%
	filter(!is.na(pass_location))

ggplot(fourth_passes_clean, aes(x = air_yards, y = pass_location, color = success)) +
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