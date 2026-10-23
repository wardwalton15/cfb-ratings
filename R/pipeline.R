# Season-level plumbing shared by the backtest and the live ratings script.

source("R/data.R")
source("R/model.R")

load_season <- function(season, params = default_params, refresh = FALSE) {
  games <- prep_games(get_games(season, refresh))
  list(
    season = season,
    games = games,
    teams = team_divisions(games),
    epa = epa_obs(get_pbp(season, refresh), games, params),
    pts = points_obs(games),
    returning = get_returning(season),
    talent = get_talent(season)
  )
}

# Priors for each season in `targets`, built from the two seasons before.
# The prior regression for season S is trained on every other season
# (leave-one-season-out) so backtests don't peek. 2020 is never a training
# target (COVID schedules). `seasons` is a named list from load_season().
build_priors <- function(seasons, targets, params = default_params) {
  finals <- lapply(seasons, final_ratings)
  yrs <- as.integer(names(seasons))

  feats <- list()
  for (y in yrs[yrs - 2 >= min(yrs)]) {
    key <- as.character(y)
    cur <- finals[[key]]
    feats[[key]] <- prior_features(
      finals[[as.character(y - 1)]], finals[[as.character(y - 2)]],
      seasons[[key]]$returning, seasons[[key]]$talent, seasons[[key]]$teams
    ) %>%
      mutate(season = y) %>%
      left_join(cur$epa$ratings %>% select(team, epa_off = off, epa_def = def), by = "team") %>%
      left_join(cur$pts$ratings %>% select(team, pts_off = off, pts_def = def), by = "team")
  }
  hist <- bind_rows(feats) %>% filter(!is.na(epa_off), season != 2020)

  out <- list()
  for (t in targets) {
    key <- as.character(t)
    prev <- finals[[as.character(t - 1)]]
    model <- fit_prior_model(hist %>% filter(season != t))
    out[[key]] <- list(
      epa = component_prior(model$epa, feats[[key]], prev$epa, params),
      pts = component_prior(model$pts, feats[[key]], prev$pts, params),
      model = model
    )
  }
  out
}
