library(readr)
library(purrr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(broom)
options(dplyr.width = Inf)

raw_schedule <- read_csv(file="cfb_schedule_05_15.csv",
                         col_types = cols(Date=col_date(format="%b %d, %Y")))

#need to add points for home and away teams and find season game belongs to
results <- raw_schedule %>%
  mutate(Home_Points = ifelse(Winner == Home, Winner_Points, Loser_Points),
         Away_Points = ifelse(Winner == Home, Loser_Point, Winner_Points),
         Home_MOV = Home_Points - Away_Points,
         Season = ifelse(month(Date) == 1, year(Date) - 1, year(Date))) %>%
  filter(!is.na(Home_MOV))

organize_game_results <- function(df, ...){
  home_teams <- df %>%
    select(Season, Game_Num, Home, Home_MOV, ...) %>%
    mutate(involved = 1) %>%
    rename(Team = Home)
  away_teams <- df %>%
    select(Season, Game_Num, Away, Home_MOV, ...) %>%
    mutate(involved = -1) %>%
    rename(Team = Away)
  return(bind_rows(home_teams, away_teams))
}

season_team_values <- results %>%
  organize_game_results %>%
  group_by(Season) %>%
  nest() %>%
  mutate(regression_df = map(data, spread, Team, involved, fill = 0),
         model = map(regression_df, ~ lm(Home_MOV ~ . - Game_Num, data = .)),
         team_values = map(model, tidy)) %>%
  unnest(team_values) %>%
  mutate(term = str_replace_all(term, fixed("'"), ""))

K <- 4 #number of cross validated fields
cv_results <- results %>%
  mutate(fold_id = sample(1:K, size = n(), replace=T)) %>%
  organize_game_results(fold_id) %>%
  group_by(Season) %>%
  nest() %>%
  crossing(fold = 1:K) %>%
  mutate(train = map2(data, fold, function(df, fold_num) filter(df, fold_id != fold_num)),
         test = map2(data, fold, function(df, fold_num) filter(df, fold_id == fold_num)),
         regression_df = map(train, spread, Team, involved, fill = 0),
         test_df = map(test, spread, Team, involved, fill = 0),
         model = map(regression_df, ~ lm(Home_MOV ~ . - Game_Num - fold_id, data = .)),
         preds = map2(model, test_df, safe_pred),
         error = map_dbl(preds, function(df) mean((df$Home_MOV - df$pred)^2))) %>%
  group_by(Season) %>%
  summarize(mean_mse = mean(error), sd_mse = sd(error))

c(mean(cv_results$mean_mse), mean(cv_results$sd_mse))


#adjusting for fcs

game_list <- results %>%
  mutate(fold_id = sample(1:K, size = n(), replace=T)) %>%
  organize_game_results(fold_id)

bad_teams <- game_list %>%
  count(Season, Team) %>%
  mutate(bad_team = n <= 2)

fcs_fix_results <- game_list %>%
  left_join(bad_teams, by = c("Season", "Team")) %>%
  mutate(Team = ifelse(bad_team, "FCS", Team)) %>%
  select(-n, -bad_team) %>%
  group_by(Season) %>%
  nest() %>%
  crossing(fold = 1:K) %>%
  mutate(train = map2(data, fold, function(df, fold_num) filter(df, fold_id != fold_num)),
         test = map2(data, fold, function(df, fold_num) filter(df, fold_id == fold_num)),
         regression_df = map(train, spread, Team, involved, fill = 0),
         test_df = map(test, spread, Team, involved, fill = 0),
         model = map(regression_df, ~ lm(Home_MOV ~ . - Game_Num - fold_id, data = .)),
         preds = map2(model, test_df, safe_pred),
         error = map_dbl(preds, function(df) mean((df$Home_MOV - df$pred) ^2))) %>%
  group_by(Season) %>%
  summarize(mean_mse = mean(error), sd_mse = sd(error))

c(mean(fcs_fix_results$mean_mse), mean(fcs_fix_results$sd_mse))


ridge_results <- game_list %>%
  group_by(Season) %>%
  nest() %>%
  crossing(fold = 1:K) %>%
  mutate(train = map2(data, fold, function(df, fold_num) filter(df, fold_id != fold_num)),
         test = map2(data, fold, function(df, fold_num) filter(df, fold_id == fold_num)),
         regression_df = map(train, spread, Team, involved, fill = 0),
         test_df = map(test, spread, Team, involved, fill = 0),
         model = map(regression_df, ridge_fit),
         preds = map2(model, test_df, ridge_pred),
         error = map_dbl(preds, function(df) mean((df$Home_MOV - df$pred)^2))) %>%
  group_by(Season) %>%
  summarize(mean_mse = mean(error), sd_mse = sd(error))

c(mean(ridge_results$mean_mse), mean(ridge_results$sd_mse))