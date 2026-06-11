install.packages("nflreadr")

library(tidyverse)

nfl_passes <- read_csv("https://raw.githubusercontent.com/36-SURE/2026/main/data/nfl_passes.csv")

nfl_passes %>%
	filter(down == 4) %>%
	count(complete_pass) %>%
	mutate( outcome = ifelse(complete_pass == 1, "Success", "Failure")) %>%
	ggplot(aes(x = outcome, y = n, fill = outcome)) +
	geom_col() +
	geom_text(aes(label = n), vjust = -0.5) +
	labs(
		title = "Passing: Success vs Failure (2022)",
		x = "Outcome",
		y = "Number of Pass Attempts"
	) +
	theme_minimal() +
	theme(legend.position = "none")