# Walk-forward backtest: for each week, fit using only earlier weeks and
# predict that week's games. Compares to actual margins and closing lines.

source("R/pipeline.R")

TRAIN_SEASONS <- c(2018:2019, 2021:2023)
TEST_SEASONS <- 2024:2025

load_all <- function(params = default_params, years = 2016:2025) {
  ss <- lapply(years, load_season, params = params)
  setNames(ss, years)
}

backtest <- function(params = default_params, ss = load_all(params),
                     targets = c(TRAIN_SEASONS, TEST_SEASONS)) {
  priors <- build_priors(ss, targets, params)
  preds <- list()
  for (t in targets) {
    s <- ss[[as.character(t)]]
    pri <- priors[[as.character(t)]]
    g <- s$games %>% filter(completed, !is.na(home_points))
    for (w in sort(unique(g$slot))) {
      fit <- fit_both(s, pri, through_slot = w - 1, params = params)
      preds[[paste(t, w)]] <- g %>%
        filter(slot == w) %>%
        transmute(season = t, slot, game_id, home_team, away_team, neutral_site,
                  margin = home_points - away_points,
                  pred_epa = predict_margin(fit$epa, home_team, away_team, neutral_site),
                  pred_pts = predict_margin(fit$pts, home_team, away_team, neutral_site))
    }
  }
  lines <- bind_rows(lapply(targets, get_lines))
  bind_rows(preds) %>%
    filter(!is.na(pred_epa)) %>%
    left_join(lines, by = "game_id") %>%
    mutate(vegas = -spread)
}

# Blend: margin ~ a * pred_epa + b * pred_pts, fit on training seasons only.
blend_weights <- function(bt, train = TRAIN_SEASONS) {
  coef(lm(margin ~ 0 + pred_epa + pred_pts, data = bt %>% filter(season %in% train)))
}

score_backtest <- function(bt, train = TRAIN_SEASONS, weights = blend_weights(bt, train)) {
  bt <- bt %>% mutate(pred = weights[["pred_epa"]] * pred_epa + weights[["pred_pts"]] * pred_pts)
  summ <- function(d) d %>% filter(!is.na(vegas)) %>% summarise(
    games = n(),
    mae = mean(abs(margin - pred)),
    rmse = sqrt(mean((margin - pred)^2)),
    su = mean(sign(pred) == sign(margin)),
    vegas_mae = mean(abs(margin - vegas)),
    ats = mean(sign(pred - vegas) == sign(margin - vegas)),
    .groups = "drop")
  list(weights = weights, data = bt,
       train = summ(bt %>% filter(season %in% train)),
       test = summ(bt %>% filter(!season %in% train)),
       by_season = summ(bt %>% group_by(season)),
       by_phase = summ(bt %>% mutate(phase = cut(slot, c(0, 1, 3, 6, 9, 13, 99))) %>% group_by(phase)))
}
