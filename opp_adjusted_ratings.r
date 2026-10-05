install.packages("cfbfastR")
install.packages("rlang")
library(rlang)
detach("package:cfbfastR", unload = TRUE)
library(cfbfastR)



library(tidyverse)

Sys.setenv(CFBD_API_KEY = "4NJiTunoAXYPiPWLFYr/+eZmdri00cgz5H55r/5ZOr+w7irK1ivFr/PRhcYnH+4K")


pbp <- load_cfb_pbp(seasons = 2026)




team_epa <- pbp %>%
  filter(!is.na(EPA)) %>%
  group_by(pos_team) %>%
  summarise(
    off_epa = mean(EPA, na.rm = TRUE),
    plays = n(),
    games = length(unique(game_id)),
    .groups = "drop"
  )
head(team_epa)

sp2025 <- cfbd_ratings_sp(year = 2025)
sp2024 <- cfbd_ratings_sp(year = 2024)
sp2023 <- cfbd_ratings_sp(year = 2023)

head(sp2025)
sp <- sp2025 %>%
  left_join(sp2024, by = "team") %>%
  left_join(sp2023, by = "team")
head(sp)

sp2 <- sp %>%
    group_by(team) %>%
    summarise(off_rating = (offense_rating.x + offense_rating.y + offense_rating)/3,
            def_rating = (defense_rating.x + defense_rating.y + defense_rating)/3)

sp2$off_rating[is.na(sp2$off_rating)] <- mean(sp2$off_rating, na.rm = TRUE)
sp2$def_rating[is.na(sp2$def_rating)] <- mean(sp2$def_rating, na.rm = TRUE)

head(sp2)

game_epa <- pbp %>%
  filter(!is.na(EPA)) %>%
  group_by(game_id, pos_team, def_pos_team) %>%
  summarise(
    epa = mean(EPA),
    plays = n(),
    .groups = "drop"
  ) %>%
  rename(offense = pos_team, defense = def_pos_team)

teams <- unique(c(game_epa$offense, game_epa$defense))
priors <- sp2 %>%
  filter(team %in% teams)
install.packages("brms")
library(brms)

# Add prior offsets
game_epa <- game_epa %>%
  left_join(priors %>% select(team, off_rating), by = c("offense" = "team")) %>%
  left_join(priors %>% select(team, def_rating), by = c("defense" = "team")) %>%
  mutate(
    off_prior = off_rating,
    def_prior = def_rating
  )

game_epa <- pbp %>%
  filter(!is.na(EPA)) %>%
  group_by(game_id, pos_team, def_pos_team, home_team) %>%
  summarise(
    epa = mean(EPA), 
    home_field = ifelse(first(pos_team) == first(home_team), 1, 0), 
    .groups = "drop"
  )
#install.packages("lfe")
library(lfe)
head(game_epa)
fit <- felm(epa ~ home_field | pos_team + def_pos_team, data = game_epa)

off_ratings <- getfe(fit) %>%
  filter(fe == "pos_team") %>%
  select(team = idx, off_adj = effect) 

def_ratings <- getfe(fit) %>%
  filter(fe == "def_pos_team") %>%
  select(team = idx, def_adj = effect)



k_off <- 4
k_def <- 4

ratings <- sp2 %>%
  left_join(off_ratings, by = "team") %>%
  left_join(def_ratings, by = "team") %>%
  left_join(
    team_epa %>% select(team = pos_team, games),
    by = "team"
  ) %>%
  mutate(
    games = coalesce(games, 0L),
    lambda_off = k_off / (k_off + games),
    lambda_def = k_def / (k_def + games),

    a = as.numeric(scale(off_rating)),
    b = as.numeric(scale(def_rating)),
    c = as.numeric(scale(off_adj)),
    d = as.numeric(scale(def_adj)),

    off_final = (1 - lambda_off) * coalesce(c, a) +
      lambda_off * a,
    def_final = (1 - lambda_def) * coalesce(d, b) +
      lambda_def * b
  )
head(ratings)
teams_meta <- cfbd_team_info(year = 2026)

p5 <- c("SEC", "Big Ten", "Big 12", "ACC")

ratings <- ratings %>%
  left_join(teams_meta %>% select(school, conference, logo), 
            by = c("team" = "school")) %>% filter((conference %in% p5) | team == "Notre Dame")

library(ggplot2)

#install.packages("ggimage")
#install.packages("ggrepel")
library(ggrepel)

library(ggimage)

plot <- ggplot(ratings, aes(x = off_final, y = def_final)) +
  geom_image(aes(image = logo), size = 0.05, by = "width", asp = 1.0) +
  geom_abline(intercept = seq(-2, 4, by = 1), slope = -1, 
              linetype = "dashed", color = "gray60", linewidth = 0.5) +
  theme_minimal() +
  labs(
    title = "Opponent-Adjusted CFB Ratings",
    x = "Offense Strength",
    y = "Defense Strength"
  ) + scale_y_reverse() +
    theme(
        plot.title = element_text(hjust = 0.5, size = 30, face = "bold")
    )

final <- ratings %>% mutate(rating = off_final - def_final) %>%
select(c(team,rating))
ggsave(plot, file = "opp_adjusted_ratings_plot.png", width = 8, height = 12, dpi = 300)
