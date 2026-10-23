# Opponent-adjusted offense/defense ratings.
#
# Two component models share one ridge-regression engine:
#   * EPA model:    each team-game-side's weighted mean EPA/play
#   * Points model: points each team scored in each game
# Both have the form
#   y[g] = mu + hfa * home[g] + off[offense] + def[defense] + noise
# where off > 0 is a better-than-average offense and def > 0 is a defense that
# ALLOWS more than average (lower is better on the raw scale; flipped when
# converted to the final points ratings).
#
# Team effects shrink toward a preseason prior rather than toward zero, so
# early in the year the prior dominates and it fades as data accumulates.
# FCS teams also share group-level offsets so they aren't pulled toward the
# FBS average.

suppressMessages({
  library(dplyr)
  library(tidyr)
  library(Matrix)
})

# Tuned by walk-forward backtest (see R/tune.R).
default_params <- list(
  lambda      = 300,   # EPA prior strength, in plays
  decay       = 0.94,  # per-week weight multiplier for older games
  epa_cap     = 3.5,   # winsorize play EPA at +/- this (blunts fluky turnovers/TDs)
  garbage_wt  = 0.2,   # weight on garbage-time plays
  pts_lambda  = 4,     # points-model prior strength, in games
  fcs_carry   = 1,     # share of an FCS team's last-season rating carried over
  # Margin = w_epa * EPA-model margin + w_pts * points-model margin (fit by
  # regression on 2018-2025 backtest predictions).
  w_epa       = 11.93,
  w_pts       = 0.928
)

# ---- data preparation ------------------------------------------------------

# Garbage time (Connelly-style score margins by quarter).
is_garbage <- function(period, margin) {
  m <- abs(margin)
  (period == 1 & m > 43) | (period == 2 & m > 37) |
    (period == 3 & m > 27) | (period >= 4 & m > 21)
}

# Regular season weeks keep their number; all postseason games share one final
# slot. Only games with an FBS team are kept.
prep_games <- function(games) {
  last_reg <- max(games$week[games$season_type == "regular"], na.rm = TRUE)
  games %>%
    filter(home_division == "fbs" | away_division == "fbs") %>%
    mutate(slot = if_else(season_type == "postseason", last_reg + 1L, as.integer(week)),
           neutral_site = coalesce(neutral_site, FALSE))
}

team_divisions <- function(games) {
  bind_rows(
    games %>% select(team = home_team, division = home_division),
    games %>% select(team = away_team, division = away_division)
  ) %>%
    distinct(team, .keep_all = TRUE) %>%
    mutate(fcs = is.na(division) | division != "fbs") %>%
    arrange(team)
}

# One row per game/offense: weighted mean EPA (y) and weighted play count (wt).
epa_obs <- function(pbp, games, params = default_params) {
  g <- games %>% select(game_id, slot, neutral_site, home_team, away_team)
  pbp %>%
    select(-any_of(c("neutral_site", "week", "season_type"))) %>%
    inner_join(g, by = "game_id") %>%
    filter(pos_team %in% c(home_team, away_team), def_pos_team %in% c(home_team, away_team)) %>%
    mutate(
      epa = pmax(pmin(EPA, params$epa_cap), -params$epa_cap),
      w = if_else(is_garbage(period, pos_score_diff_start), params$garbage_wt, 1)
    ) %>%
    group_by(game_id, slot, offense = pos_team, defense = def_pos_team) %>%
    summarise(
      home = if_else(first(neutral_site), 0, if_else(first(offense) == first(home_team), 1, -1)),
      wt = sum(w),
      y = sum(w * epa) / sum(w),
      .groups = "drop"
    ) %>%
    filter(wt > 0)
}

# One row per game/offense: points scored.
points_obs <- function(games) {
  g <- games %>% filter(completed, !is.na(home_points), !is.na(away_points))
  bind_rows(
    g %>% transmute(game_id, slot, offense = home_team, defense = away_team,
                    home = if_else(neutral_site, 0, 1), y = home_points),
    g %>% transmute(game_id, slot, offense = away_team, defense = home_team,
                    home = if_else(neutral_site, 0, -1), y = away_points)
  ) %>%
    mutate(wt = 1)
}

# ---- fitting ---------------------------------------------------------------

# obs:   tibble(slot, offense, defense, home, y, wt)
# teams: tibble(team, fcs)
# prior: tibble(team, off, def). For FBS teams these are ratings; for FCS
#        teams they're deviations from the FCS group offsets. Missing -> 0.
# group: list(mu, hfa, fcs_off, fcs_def) prior means for the global terms.
# lambda: prior strength for team effects, in units of `wt`.
fit_ridge <- function(obs, teams, prior, group, lambda, decay, through_slot = Inf) {
  d <- obs %>% filter(slot <= through_slot)
  nt <- nrow(teams)
  idx <- setNames(seq_len(nt), teams$team)
  pr <- tibble(team = teams$team) %>%
    left_join(prior, by = "team") %>%
    mutate(off = coalesce(off, 0), def = coalesce(def, 0))

  # Columns: 1 mu, 2 hfa, 3 fcs_off, 4 fcs_def, team offenses, team defenses.
  # HFA is held near its prior (lots of weight); FCS offsets moderately.
  p <- 4 + 2 * nt
  beta0 <- c(group$mu, group$hfa, group$fcs_off, group$fcs_def, pr$off, pr$def)
  pen <- c(1e-6, 60 * lambda, lambda, lambda, rep(lambda, 2 * nt))

  if (nrow(d) == 0) {
    beta <- beta0
  } else {
    oi <- idx[d$offense]; di <- idx[d$defense]
    nr <- nrow(d); r <- seq_len(nr)
    X <- sparseMatrix(
      i = rep(r, 6),
      j = c(rep(1, nr), rep(2, nr), rep(3, nr), rep(4, nr), 4 + oi, 4 + nt + di),
      x = c(rep(1, nr), d$home, teams$fcs[oi], teams$fcs[di], rep(1, nr), rep(1, nr)),
      dims = c(nr, p)
    )
    w <- d$wt * decay^(max(d$slot) - d$slot)
    XtW <- t(Diagonal(x = w) %*% X)
    beta <- as.numeric(solve(XtW %*% X + Diagonal(p, pen), XtW %*% d$y + pen * beta0))
  }

  # Center on the FBS average so mu absorbs it.
  off <- beta[4 + seq_len(nt)] + teams$fcs * beta[3]
  def <- beta[4 + nt + seq_len(nt)] + teams$fcs * beta[4]
  mo <- mean(off[!teams$fcs]); md <- mean(def[!teams$fcs])
  list(
    ratings = tibble(team = teams$team, fcs = teams$fcs, off = off - mo, def = def - md),
    mu = beta[1] + mo + md, hfa = beta[2],
    fcs_off = beta[3] - mo, fcs_def = beta[4] - md
  )
}

# Expected home-minus-away margin on the model's own scale.
predict_margin <- function(fit, home, away, neutral) {
  r <- fit$ratings
  oh <- r$off[match(home, r$team)]; dh <- r$def[match(home, r$team)]
  oa <- r$off[match(away, r$team)]; da <- r$def[match(away, r$team)]
  (oh + da) - (oa + dh) + 2 * fit$hfa * !neutral
}

# Fit both components as of a given slot. `pri` comes from build_priors().
fit_both <- function(season, pri, through_slot = Inf, params = default_params) {
  list(
    epa = fit_ridge(season$epa, season$teams, pri$epa$prior, pri$epa$group,
                    params$lambda, params$decay, through_slot),
    pts = fit_ridge(season$pts, season$teams, pri$pts$prior, pri$pts$group,
                    params$pts_lambda, params$decay, through_slot)
  )
}

# Combine the two components into final ratings in points per game vs an
# average FBS team. Higher is better for both offense and defense, and
# predicted margin = net[home] - net[away] + hfa (0 at neutral sites).
blend_ratings <- function(fit, params = default_params) {
  a <- params$w_epa; b <- params$w_pts
  e <- fit$epa$ratings; p <- fit$pts$ratings
  ratings <- e %>%
    select(team, fcs) %>%
    mutate(
      off = a * e$off + b * p$off,
      def = -(a * e$def + b * p$def),
      net = off + def
    )
  list(ratings = ratings, hfa = 2 * (a * fit$epa$hfa + b * fit$pts$hfa))
}

# ---- preseason priors -----------------------------------------------------

# End-of-season ratings with a weak zero prior: what the priors try to hit.
final_ratings <- function(season) {
  empty <- tibble(team = character(), off = numeric(), def = numeric())
  list(
    epa = fit_ridge(season$epa, season$teams, empty,
                    list(mu = 0, hfa = 0.03, fcs_off = -0.15, fcs_def = 0.15),
                    lambda = 300, decay = 1),
    pts = fit_ridge(season$pts, season$teams, empty,
                    list(mu = 28, hfa = 1.5, fcs_off = -12, fcs_def = 12),
                    lambda = 2, decay = 1)
  )
}

prior_features <- function(prev, prev2, returning, talent, teams) {
  fbs_r <- function(f, prefix) {
    f$ratings %>% filter(!fcs) %>% select(team, off, def) %>%
      rename_with(~ paste0(prefix, .x), c(off, def))
  }
  teams %>%
    filter(!fcs) %>%
    select(team) %>%
    left_join(fbs_r(prev$epa, "prev_"), by = "team") %>%
    left_join(fbs_r(prev$pts, "prevpts_"), by = "team") %>%
    left_join(fbs_r(prev2$epa, "prev2_"), by = "team") %>%
    left_join(returning, by = "team") %>%
    left_join(talent, by = "team") %>%
    mutate(
      # New FBS teams start well below average.
      across(c(prev_off, prevpts_off), ~ coalesce(.x, quantile(.x, 0.1, na.rm = TRUE))),
      across(c(prev_def, prevpts_def), ~ coalesce(.x, quantile(.x, 0.9, na.rm = TRUE))),
      prev2_off = coalesce(prev2_off, prev_off),
      prev2_def = coalesce(prev2_def, prev_def),
      ret_ppa = coalesce(ret_ppa, median(ret_ppa, na.rm = TRUE)),
      ret_usage = coalesce(ret_usage, median(ret_usage, na.rm = TRUE)),
      talent_z = as.numeric(scale(talent)),
      talent_z = coalesce(talent_z, min(talent_z, na.rm = TRUE))
    )
}

# Regress final ratings on last season's (and the season before's) ratings,
# returning production, and roster talent. One pair of models per component.
fit_prior_model <- function(hist) {
  off_f <- ~ prev_off * ret_ppa + prevpts_off + prev2_off + talent_z
  def_f <- ~ prev_def * ret_usage + prevpts_def + prev2_def + talent_z
  list(
    epa = list(off = lm(update(off_f, epa_off ~ .), data = hist),
               def = lm(update(def_f, epa_def ~ .), data = hist)),
    pts = list(off = lm(update(off_f, pts_off ~ .), data = hist),
               def = lm(update(def_f, pts_def ~ .), data = hist))
  )
}

# FBS teams get the regression prior; FCS teams carry over part of last
# season's deviation from the FCS group. Also returns the group-level priors.
component_prior <- function(model, features, prev, params = default_params) {
  fbs <- features %>%
    mutate(off = predict(model$off, features), def = predict(model$def, features)) %>%
    mutate(off = off - mean(off), def = def - mean(def)) %>%
    select(team, off, def)
  fcs <- prev$ratings %>%
    filter(fcs, !team %in% fbs$team) %>%
    transmute(team,
              off = params$fcs_carry * (off - prev$fcs_off),
              def = params$fcs_carry * (def - prev$fcs_def))
  list(prior = bind_rows(fbs, fcs),
       group = list(mu = prev$mu, hfa = prev$hfa, fcs_off = prev$fcs_off, fcs_def = prev$fcs_def))
}
