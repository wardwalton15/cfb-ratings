# Data fetching + local caching. Play-by-play comes from the sportsdataverse
# releases (no key needed); everything else uses the CFBD API, which needs
# CFBD_API_KEY set (put it in ~/.Renviron, not in code).

suppressMessages({
  library(cfbfastR)
  library(dplyr)
})

DATA_DIR <- "data"

pbp_cols <- c(
  "season", "week", "season_type", "game_id", "pos_team", "def_pos_team",
  "home", "away", "neutral_site", "period", "pos_score_diff_start",
  "wp_before", "EPA", "success", "rush", "pass", "play_type"
)

cached <- function(name, fetch, refresh = FALSE) {
  path <- file.path(DATA_DIR, paste0(name, ".rds"))
  if (!refresh && file.exists(path)) return(readRDS(path))
  dir.create(DATA_DIR, showWarnings = FALSE)
  out <- fetch()
  saveRDS(out, path)
  out
}

# Only scrimmage plays with a valid EPA, trimmed to the columns the model uses.
get_pbp <- function(season, refresh = FALSE) {
  cached(paste0("pbp_", season), refresh = refresh, function() {
    load_cfb_pbp(season) %>%
      filter(rush == 1 | pass == 1, !is.na(EPA), !is.na(pos_team), !is.na(def_pos_team)) %>%
      select(any_of(pbp_cols)) %>%
      as_tibble()
  })
}

get_games <- function(season, refresh = FALSE) {
  cached(paste0("games_", season), refresh = refresh, function() {
    bind_rows(
      cfbd_game_info(season, season_type = "regular"),
      # Errors noisily (but harmlessly) before the postseason schedule exists.
      suppressMessages(cfbd_game_info(season, season_type = "postseason"))
    ) %>%
      select(game_id, season, week, season_type, completed, neutral_site,
             home_team, home_division, home_points,
             away_team, away_division, away_points) %>%
      as_tibble()
  })
}

# Consensus-ish closing spread (median across books), home perspective
# (negative = home favored, CFBD convention).
get_lines <- function(season, refresh = FALSE) {
  cached(paste0("lines_", season), refresh = refresh, function() {
    bind_rows(
      cfbd_betting_lines(year = season, season_type = "regular"),
      cfbd_betting_lines(year = season, season_type = "postseason")
    ) %>%
      mutate(spread = suppressWarnings(as.numeric(spread))) %>%
      filter(!is.na(spread)) %>%
      group_by(game_id) %>%
      summarise(spread = median(spread), .groups = "drop")
  })
}

get_returning <- function(season, refresh = FALSE) {
  cached(paste0("returning_", season), refresh = refresh, function() {
    cfbd_player_returning(season) %>%
      select(team, ret_ppa = percent_ppa, ret_pass = percent_passing_ppa,
             ret_usage = usage) %>%
      as_tibble()
  })
}

get_talent <- function(season, refresh = FALSE) {
  cached(paste0("talent_", season), refresh = refresh, function() {
    cfbd_team_talent(season) %>%
      transmute(team = school, talent = as.numeric(talent)) %>%
      as_tibble()
  })
}

get_team_info <- function(season, refresh = FALSE) {
  cached(paste0("teams_", season), refresh = refresh, function() {
    cfbd_team_info(year = season) %>%
      select(team = school, conference, logo) %>%
      as_tibble()
  })
}
