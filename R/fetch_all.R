source("R/data.R")
for (s in 2017:2026) {
  message("season ", s)
  get_pbp(s); get_games(s); get_lines(s); get_returning(s); get_talent(s)
}
