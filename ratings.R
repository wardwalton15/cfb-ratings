# Current-season predictive ratings, plot, and next-week predictions.
#
#   Rscript ratings.R          # current season (2026)
#   Rscript ratings.R 2025     # any season with data
#
# Needs CFBD_API_KEY in the environment (e.g. in ~/.Renviron).

source("R/pipeline.R")
suppressMessages({
  library(ggplot2)
  library(ggimage)
})

args <- commandArgs(trailingOnly = TRUE)
SEASON <- if (length(args)) as.integer(args[1]) else 2026L
params <- default_params
dir.create("output", showWarnings = FALSE)

# The current season is re-downloaded every run; past seasons come from cache.
seasons <- lapply(2016:SEASON, function(s) load_season(s, params, refresh = s == SEASON))
names(seasons) <- 2016:SEASON
pri <- build_priors(seasons, SEASON, params)[[1]]
cur <- seasons[[as.character(SEASON)]]

# Play-by-play can lag final scores by a day or two, so rate through the last
# week that has play-by-play.
through <- max(cur$epa$slot)
fit <- fit_both(cur, pri, through, params)
final <- blend_ratings(fit, params)
preseason <- blend_ratings(fit_both(cur, pri, 0, params), params)

info <- get_team_info(SEASON, refresh = TRUE)
ratings <- final$ratings %>%
  filter(!fcs) %>%
  left_join(preseason$ratings %>% select(team, preseason = net), by = "team") %>%
  left_join(info, by = "team") %>%
  arrange(desc(net)) %>%
  mutate(rank = row_number(), off_rank = min_rank(desc(off)), def_rank = min_rank(desc(def)))

write.csv(
  ratings %>% select(rank, team, conference, net, off, def, off_rank, def_rank, preseason) %>%
    mutate(across(c(net, off, def, preseason), ~ round(.x, 1))),
  sprintf("output/ratings_%d_wk%d.csv", SEASON, through), row.names = FALSE
)

cat(sprintf("\n%d ratings through week %d (points per game vs. average FBS team; HFA = %.1f)\n\n",
            SEASON, through, final$hfa))
print(ratings %>% select(rank, team, conference, net, off, def, preseason) %>%
        mutate(across(where(is.numeric) & !rank, ~ round(.x, 1))), n = 25)

# ---- next week's predictions ---------------------------------------------

upcoming <- cur$games %>% filter(slot == through + 1)
if (nrow(upcoming) > 0) {
  r <- final$ratings
  net <- function(t) r$net[match(t, r$team)]
  preds <- upcoming %>%
    left_join(get_lines(SEASON, refresh = TRUE), by = "game_id") %>%
    transmute(
      week = slot, home_team, away_team, neutral_site,
      pred_margin = round(net(home_team) - net(away_team) + final$hfa * !neutral_site, 1),
      vegas_margin = -spread,
      edge = round(pred_margin - vegas_margin, 1)
    ) %>%
    arrange(desc(abs(edge)))
  write.csv(preds, sprintf("output/predictions_%d_wk%d.csv", SEASON, through + 1), row.names = FALSE)
  cat(sprintf("\nWeek %d predictions (home margin; biggest disagreements with the line first):\n\n", through + 1))
  print(preds, n = 15)
}

# ---- plot ------------------------------------------------------------------

p4 <- c("SEC", "Big Ten", "Big 12", "ACC")
plot_df <- ratings %>% filter(conference %in% p4 | team == "Notre Dame")
lim <- range(c(plot_df$off, plot_df$def))
nets <- seq(floor(min(plot_df$net) / 5) * 5, ceiling(max(plot_df$net) / 5) * 5, by = 5)

plot <- ggplot(plot_df, aes(x = off, y = def)) +
  geom_abline(intercept = nets, slope = -1, linetype = "dashed", color = "gray75", linewidth = 0.4) +
  geom_hline(yintercept = 0, color = "gray60", linewidth = 0.3) +
  geom_vline(xintercept = 0, color = "gray60", linewidth = 0.3) +
  geom_image(aes(image = logo), size = 0.045, by = "width", asp = 1.0) +
  coord_cartesian(xlim = lim, ylim = lim) +
  theme_minimal() +
  labs(
    title = sprintf("%d CFB Predictive Ratings", SEASON),
    subtitle = sprintf("Through week %d  |  points per game vs. average FBS team  |  dashed lines = equal net rating (5-pt steps)", through),
    x = "Offense (points scored above average)",
    y = "Defense (points prevented vs. average)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 9, color = "gray40")
  )
ggsave(sprintf("output/ratings_plot_%d.png", SEASON), plot, width = 10, height = 10, dpi = 300, bg = "white")
