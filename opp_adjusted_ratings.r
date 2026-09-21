install.packages("cfbfastR")
detach("package:cfbfastR", unload = TRUE)
library(cfbfastR)



library(tidyverse)

Sys.setenv(CFBD_API_KEY = "4NJiTunoAXYPiPWLFYr/+eZmdri00cgz5H55r/5ZOr+w7irK1ivFr/PRhcYnH+4K")


pbp <- load_cfb_pbp(seasons = 2025)




team_epa <- pbp %>%
  filter(!is.na(EPA)) %>%
  group_by(pos_team) %>%
  summarise(
    off_epa = mean(EPA, na.rm = TRUE),
    plays = n(),
    games = length(unique(game_id)),
    .groups = "drop"
  )
view(team_epa)

sp2024 <- cfbd_ratings_sp(year = 2024)
sp2023 <- cfbd_ratings_sp(year = 2023)
sp2022 <- cfbd_ratings_sp(year = 2022)

view(sp2024)
sp <- sp2024 %>%
  left_join(sp2023, by = "team") %>%
  left_join(sp2022, by = "team")
view(sp)

sp2 <- sp %>%
    group_by(team) %>%
    summarise(off_rating = (offense_rating.x + offense_rating.y + offense_rating)/3,
            def_rating = (defense_rating.x + defense_rating.y + defense_rating)/3)

sp2$off_rating[is.na(sp2$off_rating)] <- mean(sp2$off_rating, na.rm = TRUE)
sp2$def_rating[is.na(sp2$def_rating)] <- mean(sp2$def_rating, na.rm = TRUE)

view(sp2)

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
#install.packages("brms")
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
  group_by(game_id, pos_team, def_pos_team) %>%
  summarise(epa = mean(EPA), home_field = ifelse(pos_team == home_team,1,0), .groups = "drop")
library(lfe)
fit <- felm(epa ~ home_field | pos_team + def_pos_team, data = game_epa)

off_ratings <- getfe(fit) %>%
  filter(fe == "pos_team") %>%
  select(team = idx, off_adj = effect) 

def_ratings <- getfe(fit) %>%
  filter(fe == "def_pos_team") %>%
  select(team = idx, def_adj = effect)

lambda_off <- 0.15   # offense shrinkage
lambda_def <- 0.15# defense shrinkage


ratings <- sp2 %>%
  left_join(off_ratings, by = "team") %>%
  left_join(def_ratings, by = "team") %>%
  mutate(
    a = scale(off_rating),
    b = scale(def_rating),
    c = scale(off_adj),
    d = scale(def_adj),
    off_final = (1 - lambda_off) * (c) + lambda_off * (a),
    def_final = (1 - lambda_def) * (d) + lambda_def * (b)
  )
view(ratings)
teams_meta <- cfbd_team_info(year = 2025)

p5 <- c("SEC", "Big Ten", "Big 12", "ACC")

ratings <- ratings %>%
  left_join(teams_meta %>% select(school, conference, logo), 
            by = c("team" = "school")) %>% filter((!conference %in% p5) & team != "Notre Dame")

library(ggplot2)

library(ggrepel)

library(ggimage)

ggplot(ratings, aes(x = off_final, y = def_final)) +
  geom_image(aes(image = logo), size = 0.05, by = "width", asp = 1.0) +
  geom_abline(intercept = seq(-2, 4, by = 1), slope = -1, 
              linetype = "dashed", color = "gray60", linewidth = 0.5) +
  theme_minimal() +
  labs(
    title = "Opponent-Adjusted CFB Ratings",
    x = "Offense Strength (Adj EPA/Play)",
    y = "Defense Strength (Adj EPA/Play)"
  ) + scale_y_reverse() +
    theme(
        plot.title = element_text(hjust = 0.5, size = 30, face = "bold")
    )

final <- ratings %>% mutate(rating = off_final - def_final) %>%
select(c(team,rating))
view(final)
