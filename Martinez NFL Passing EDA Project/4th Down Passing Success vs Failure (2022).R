library(tidyverse)
library(nflreadr)

pbp_2022 <- load_pbp(2022)

fourth_passes <- pbp_2022 %>%
	filter(down == 4, pass == 1) %>%
	mutate(
		pass_location = factor(pass_location, levels = c("left", "middle", "right")),
		success = if_else(first_down == 1, "Success", "Failure"),
		y_coord = case_when(
			pass_location == "left"   ~ 15,
			pass_location == "middle" ~ 26.65,
			pass_location == "right"  ~ 38
		),
		x_coord = air_yards + 50
	) %>%
	filter(!is.na(x_coord), !is.na(y_coord))

ggplot() +
	# SUCCESS HEATMAP (GREEN)
	stat_density_2d(
		data = fourth_passes %>% filter(success == "Success"),
		aes(x = x_coord, y = y_coord, fill = after_stat(level)),
		geom = "polygon",
		alpha = 0.6,
		contour = TRUE,
		fill = "green3"
	) +
	# FAILURE HEATMAP (RED)
	stat_density_2d(
		data = fourth_passes %>% filter(success == "Failure"),
		aes(x = x_coord, y = y_coord, fill = after_stat(level)),
		geom = "polygon",
		alpha = 0.6,
		contour = TRUE,
		fill = "red3"
	) +
	facet_wrap(~ pass_location, ncol = 3) +
	labs(
		title = "4th Down Pass Heatmaps by Location",
		subtitle = "Green = Success, Red = Failure",
		x = "Approximate Depth (yards)",
		y = "Horizontal Approximation"
	) +
	theme_minimal() +
	theme(legend.position = "none")