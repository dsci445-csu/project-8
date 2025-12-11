set.seed(445)

library(tidyverse)

library(tidymodels)



clean_data <- read_csv("clean_movie_data.csv") 

clean_data_lm <- clean_data |>
  mutate(
    gross = log(gross + 1),
    budget = log(budget + 1),
    votes  = log(votes + 1),
    comp_freq = log(comp_freq + 1),
    runtime = log(runtime + 1),
    score= log(score + 1),
    writer_popularity = log(writer_popularity + 1),
    director_popularity = log(director_popularity + 1),
    star_popularity = log(star_popularity + 1)
  ) |>
  
  select(
    gross,
    budget, votes, runtime, score,
    comp_freq, writer_popularity, director_popularity, star_popularity,
    genre, rating, region, season,
    sentiment_best
  ) |>
  mutate(
    genre  = as.factor(genre),
    rating = as.factor(rating),
    region = as.factor(region),
    season = as.factor(season)
  ) |>

  drop_na(gross)



lm_full <- lm(gross ~ ., data = clean_data_lm)

preds_log <- fitted(lm_full)
preds_dollars <- exp(preds_log) - 1

obs_log <- lm_full$model$gross
obs_dollars <- exp(obs_log) - 1

rmse_dollars <- sqrt(mean((obs_dollars - preds_dollars)^2))

obs <- lm_full$model$gross
preds <- fitted(lm_full)   
rmse_lm <- sqrt(mean((obs- preds)^2))

rsq_lm <- 1- sum((obs- preds)^2) / sum((obs- mean(obs))^2)

sig_preds <- tidy(lm_full) %>%
  filter(p.value < 0.05) %>%              
  arrange(p.value)                        







