library(tidyverse)
library(tidymodels)
library(here)
library(kableExtra)

set.seed(445)

#set up lambda tune
lambda <- lambda <- 10^seq(-2, 10, length.out = 100)
tune_df <- data.frame(lambda = lambda)


# Pre CAA #####
## normalize data #####
prep_data <- recipe(load_yield_adj ~ ., data = pre_caa) |>
  step_dummy(all_nominal_predictors()) |>
  step_normalize(all_predictors())

ridge_ests <- data.frame()
for(lam in lambda) {
  ridge_spec <- linear_reg(mixture = 0, penalty = lam) |>
    set_mode("regression") |>
    set_engine("glmnet")
  
  workflow() |>
    add_model(ridge_spec) |>
    add_recipe(prep_data) |>
    fit(pre_caa) |>
    tidy() |>
    bind_rows(ridge_ests) -> ridge_ests
}

## see lambda penalties #####
ridge_ests |>
  filter(term != "(Intercept)") |>
  ggplot() +
  geom_line(aes(penalty, estimate, group = term, colour = term)) +
  coord_trans(x = "log10")%>%
  labs(title = 'Pre CAA')

ggsave(filename = here('out/ridge_lambda_test_pre.png'))

## perform CV to pick best lambda #####
pre_10foldcv <- vfold_cv(pre_caa, v = 10)

ridge_spec <- linear_reg(mixture = 0, penalty = tune("lambda")) |>
  set_mode("regression") |>
  set_engine("glmnet")

workflow() |>
  add_model(ridge_spec) |>
  add_recipe(prep_data) |>
  tune_grid(resamples = pre_10foldcv, grid = tune_df) -> ridge_tune

ridge_tune |>
  collect_metrics() |>
  select(lambda, .metric, mean) |>
  pivot_wider(names_from = .metric, values_from = mean) |>
  ggplot() +
  geom_line(aes(lambda, rmse^2)) +
  geom_point(aes(lambda, rmse^2)) +
  coord_trans(x = "log10")

ggsave(filename = here('out/ridge_tune_pre.png'))

show_best(ridge_tune, metric = "rmse", n = 1)

ridge_spec_tuned <- linear_reg(mixture = 0, penalty = 0.01) |>
  set_mode("regression") |>
  set_engine("glmnet")

glmnet_res <- workflow() |>
  add_model(ridge_spec_tuned) |>
  add_recipe(prep_data)

# using code from https://community.rstudio.com/t/tidymodels-plotting-predicted-vs-true-values-using-the-functions-collect-predictions-and-ggplot-in-r/90752 for plotting
fit_glm <- fit_resamples(
  glmnet_res,
  pre_10foldcv,
  metrics = metric_set(rmse, rsq),
  control = control_resamples(save_pred = TRUE)
)



##Collect model predictions for each fold for the frequency 

Predictions<-fit_glm %>% 
  collect_predictions()

Predictions

##Plot the predicted and true values 
fit_glm %>%
  collect_predictions() %>%
  ggplot(aes(load_yield_adj, .pred, color = id)) +
  geom_abline(lty = 2, color = "gray80", size = 1.5) +
  geom_point(alpha = 0.3) +
  labs(
    x = "Truth",
    y = "Predicted yield",
    color = NULL,
    title = "Pre: Predicted vs True",
    subtitle = "Each Cross-validation Fold is Shown in a Different Color"
  )
ggsave(here('out/ridge_cv_plot_pre.png'))

fit_glm <- fit(
  glmnet_res,
  data = pre_caa
) 

fit_glm %>%
  predict(pre_caa) %>%
  bind_cols(pre_caa) %>%
  rsq(truth = load_yield_adj, estimate = .pred)

fit_glm %>%
  predict(pre_caa) %>%
  bind_cols(pre_caa) %>%
  rsq(truth = load_yield_adj, estimate = .pred)

fit_glm %>%
  extract_fit_parsnip() %>%
  tidy() %>%
  kbl(width = 4) %>%
  kable_paper(latex_options = c("striped", "scale_down")) %>%
  kable_styling(full_width = F) %>%
  save_kable(here('out/ridge_estimates_table_pre.png'))

pre_tbl <- fit_glm %>%
  extract_fit_parsnip() %>%
  tidy()

# Post CAA #####
## normalize data #####
prep_data <- recipe(load_yield_adj ~ ., data = post_caa) |>
  step_dummy(all_nominal_predictors()) |>
  step_normalize(all_predictors())

ridge_ests <- data.frame()
for(lam in lambda) {
  ridge_spec <- linear_reg(mixture = 0, penalty = lam) |>
    set_mode("regression") |>
    set_engine("glmnet")
  
  workflow() |>
    add_model(ridge_spec) |>
    add_recipe(prep_data) |>
    fit(pre_caa) |>
    tidy() |>
    bind_rows(ridge_ests) -> ridge_ests
}

## see lambda penalties #####
ridge_ests |>
  filter(term != "(Intercept)") |>
  ggplot() +
  geom_line(aes(penalty, estimate, group = term, colour = term)) +
  coord_trans(x = "log10") %>%
  labs(title = 'Post CAA')

ggsave(filename = here('out/ridge_lambda_test_post.png'))

## perform CV to pick best lambda #####
post_10foldcv <- vfold_cv(post_caa, v = 10)

ridge_spec <- linear_reg(mixture = 0, penalty = tune("lambda")) |>
  set_mode("regression") |>
  set_engine("glmnet")

workflow() |>
  add_model(ridge_spec) |>
  add_recipe(prep_data) |>
  tune_grid(resamples = post_10foldcv, grid = tune_df) -> ridge_tune

ridge_tune |>
  collect_metrics() |>
  select(lambda, .metric, mean) |>
  pivot_wider(names_from = .metric, values_from = mean) |>
  ggplot() +
  geom_line(aes(lambda, rmse^2)) +
  geom_point(aes(lambda, rmse^2)) +
  coord_trans(x = "log10")

ggsave(filename = here('out/ridge_tune_post.png'))

show_best(ridge_tune, metric = "rmse", n = 1)

ridge_spec_tuned <- linear_reg(mixture = 0, penalty = 0.01) |>
  set_mode("regression") |>
  set_engine("glmnet")

glmnet_res <- workflow() |>
  add_model(ridge_spec_tuned) |>
  add_recipe(prep_data)

# using code from https://community.rstudio.com/t/tidymodels-plotting-predicted-vs-true-values-using-the-functions-collect-predictions-and-ggplot-in-r/90752 for plotting
fit_glm <- fit_resamples(
  glmnet_res,
  pre_10foldcv,
  metrics = metric_set(rmse, rsq),
  control = control_resamples(save_pred = TRUE)
)



##Collect model predictions for each fold for the frequency 

Predictions<-fit_glm %>% 
  collect_predictions()

Predictions

##Plot the predicted and true values 
fit_glm %>%
  collect_predictions() %>%
  ggplot(aes(load_yield_adj, .pred, color = id)) +
  geom_abline(lty = 2, color = "gray80", size = 1.5) +
  geom_point(alpha = 0.3) +
  labs(
    x = "Truth",
    y = "Predicted yield",
    color = NULL,
    title = "Post: Predicted vs True",
    subtitle = "Each Cross-validation Fold is Shown in a Different Color"
  )
ggsave(here('out/ridge_cv_plot_post.png'))

fit_glm <- fit(
  glmnet_res,
  data = post_caa
) 

fit_glm %>%
  predict(post_caa) %>%
  bind_cols(post_caa) %>%
  rsq(truth = load_yield_adj, estimate = .pred)

fit_glm %>%
  predict(post_caa) %>%
  bind_cols(post_caa) %>%
  rsq(truth = load_yield_adj, estimate = .pred)

fit_glm %>%
  extract_fit_parsnip() %>%
  tidy() %>%
  kbl(width = 4) %>%
  kable_paper(latex_options = c("striped", "scale_down")) %>%
  kable_styling(full_width = F) %>%
  save_kable(here('out/ridge_estimates_table_post.png'))

post_tbl <- fit_glm %>%
  extract_fit_parsnip() %>%
  tidy() %>%
  select(estimate) %>%
  rename('Estimate Post CAA' = estimate)

pre_tbl %>%
  rename('Estimate Pre CAA' = estimate) %>%
  cbind(post_tbl) %>%
  select(term, `Estimate Pre CAA`, `Estimate Post CAA`)%>%
  kbl(width = 4) %>%
  kable_paper(latex_options = c("striped", "scale_down")) %>%
  kable_styling(full_width = F) %>%
  save_kable(here('out/ridge_estimates_table_comparison.png'))

