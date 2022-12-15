library(tidyverse) 
library(tidymodels)
library(knitr)

## Reproducibility
set.seed(445)

lm_spec <- linear_reg()

## all sites combined


## all site mlr recipe
w3_linear_rec = recipe(load_yield_adj ~ ., data = pre_caa)

## all site workflow
w3_linear_wf <- workflow() |>
  add_model(lm_spec) |>
  add_recipe(w3_linear_rec)

## all site fit
w3_linear_fit <- w3_linear_wf |>
  fit(data = pre_caa)

w3_linear_fit |> tidy()

## adding the predicted values to a helper data frame
augment(w3_linear_fit, new_data = pre_caa) -> helpdf

##ploting this
helpdf %>%
  ggplot(aes(x = .pred, y = load_yield_adj, colour = site))+
  geom_point()+
  labs(x = "Predicted Values based on model", y = "Real Values Produced", title = "Pre:All Three Sites")
ggsave(filename = here('out/PRE3MLR.png'))
##sub setting by site

pre_caa_Co = pre_caa%>%
  filter(site == "Colorado")%>%
  select(-site)

## CO sites combined


## CO site mlr recipe
w3_linear_rec_co = recipe(load_yield_adj ~ ., data = pre_caa_Co)

## CO site workflow
w3_linear_wf_co <- workflow() |>
  add_model(lm_spec) |>
  add_recipe(w3_linear_rec_co)

## CO site fit
w3_linear_fit_co <- w3_linear_wf_co |>
  fit(data = pre_caa_Co)

w3_linear_fit_co |> tidy()

## adding the predicted values to a helper data frame
augment(w3_linear_fit_co, new_data = pre_caa_Co) -> helpdf

##ploting this
helpdf %>%
  ggplot(aes(x = .pred, y = load_yield_adj))+
  geom_point()+
  labs(x = "Predicted Values based on model", y = "Real Values Produced", title = "pre:Colorado")
ggsave(filename = here('out/PRECoMLR.png'))


##sub setting by site

pre_caa_Vt = pre_caa%>%
  filter(site == "Vermont")%>%
  select(-site)

## VT sites combined


## VT site mlr recipe
w3_linear_rec_vt = recipe(load_yield_adj ~ ., data = pre_caa_Vt)

## VT site workflow
w3_linear_wf_vt <- workflow() |>
  add_model(lm_spec) |>
  add_recipe(w3_linear_rec_vt)

## VT site fit
w3_linear_fit_vt <- w3_linear_wf_vt |>
  fit(data = pre_caa_Vt)

w3_linear_fit_vt |> tidy()

## adding the predicted values to a helper data frame
augment(w3_linear_fit_vt, new_data = pre_caa_Vt) -> helpdf

##ploting this
helpdf %>%
  ggplot(aes(x = .pred, y = load_yield_adj, colour = pH_mean))+
  geom_point()+
  labs(x = "Predicted Values based on model", y = "Real Values Produced", title = "pre:Vermont")
ggsave(filename = here('out/PREVtMLR.png'))


##sub setting by site

pre_caa_Ca = pre_caa%>%
  filter(site == "California")%>%
  select(-site)

## CA sites combined


## CA site mlr recipe
w3_linear_rec_ca = recipe(load_yield_adj ~ ., data = pre_caa_Ca)

## CA site workflow
w3_linear_wf_ca <- workflow() |>
  add_model(lm_spec) |>
  add_recipe(w3_linear_rec_ca)

## CA site fit
w3_linear_fit_ca <- w3_linear_wf_ca |>
  fit(data = pre_caa_Ca)

w3_linear_fit_ca |> tidy()

## adding the predicted values to a helper data frame
augment(w3_linear_fit_ca, new_data = pre_caa_Ca) -> helpdf

##ploting this
helpdf %>%
  ggplot(aes(x = .pred, y = load_yield_adj, colour = yield))+
  geom_point()+
  labs(x = "Predicted Values based on model", y = "Real Values Produced", title = "pre:California")
ggsave(filename = here('out/PRECaMLR.png'))











w3_linear_rec = recipe(load_yield_adj ~ ., data = post_caa)

## all site workflow
w3_linear_wf <- workflow() |>
  add_model(lm_spec) |>
  add_recipe(w3_linear_rec)

## all site fit
w3_linear_fit <- w3_linear_wf |>
  fit(data = post_caa)

w3_linear_fit |> tidy()

## adding the predicted values to a helper data frame
augment(w3_linear_fit, new_data = post_caa) -> helpdf

##ploting this
helpdf %>%
  ggplot(aes(x = .pred, y = load_yield_adj, colour = site))+
  geom_point()+
  labs(x = "Predicted Values based on model", y = "Real Values Produced", title = "Post:All Three Sites")
ggsave(filename = here('out/PRE3MLR.png'))
##sub setting by site

post_caa_Co = post_caa%>%
  filter(site == "Colorado")%>%
  select(-site)

## CO sites combined


## CO site mlr recipe
w3_linear_rec_co = recipe(load_yield_adj ~ ., data = post_caa_Co)

## CO site workflow
w3_linear_wf_co <- workflow() |>
  add_model(lm_spec) |>
  add_recipe(w3_linear_rec_co)

## CO site fit
w3_linear_fit_co <- w3_linear_wf_co |>
  fit(data = post_caa_Co)

w3_linear_fit_co |> tidy()

## adding the predicted values to a helper data frame
augment(w3_linear_fit_co, new_data = post_caa_Co) -> helpdf

##ploting this
helpdf %>%
  ggplot(aes(x = .pred, y = load_yield_adj, colour = pH_mean))+
  geom_point()+
  labs(x = "Predicted Values based on model", y = "Real Values Produced", title = "Post:Colorado")
ggsave(filename = here('out/PRECoMLR.png'))


##sub setting by site

post_caa_Vt = post_caa%>%
  filter(site == "Vermont")%>%
  select(-site)

## VT sites combined


## VT site mlr recipe
w3_linear_rec_vt = recipe(load_yield_adj ~ ., data = post_caa_Vt)

## VT site workflow
w3_linear_wf_vt <- workflow() |>
  add_model(lm_spec) |>
  add_recipe(w3_linear_rec_vt)

## VT site fit
w3_linear_fit_vt <- w3_linear_wf_vt |>
  fit(data = post_caa_Vt)

w3_linear_fit_vt |> tidy()

## adding the predicted values to a helper data frame
augment(w3_linear_fit_vt, new_data = pre_caa_Vt) -> helpdf

##ploting this
helpdf %>%
  ggplot(aes(x = .pred, y = load_yield_adj, colour = pH_mean))+
  geom_point()+
  labs(x = "Predicted Values based on model", y = "Real Values Produced", title = "Post:Vermont")


##sub setting by site

post_caa_Ca = post_caa%>%
  filter(site == "California")%>%
  select(-site)

## CA sites combined


## CA site mlr recipe
w3_linear_rec_ca = recipe(load_yield_adj ~ ., data = post_caa_Ca)

## CA site workflow
w3_linear_wf_ca <- workflow() |>
  add_model(lm_spec) |>
  add_recipe(w3_linear_rec_ca)

## CA site fit
w3_linear_fit_ca <- w3_linear_wf_ca |>
  fit(data = post_caa_Ca)

w3_linear_fit_ca |> tidy()

## adding the predicted values to a helper data frame
augment(w3_linear_fit_ca, new_data = pre_caa_Ca) -> helpdf

##ploting this
helpdf %>%
  ggplot(aes(x = .pred, y = load_yield_adj, colour = yield))+
  geom_point()+
  labs(x = "Predicted Values based on model", y = "Real Values Produced", title = "Post:California")








