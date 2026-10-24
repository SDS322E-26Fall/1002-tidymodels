library(tidyverse)
library(tidymodels)

##################################################################
# Example 1: linear regression with tidymodels
# Goal: predict penguin `body_mass` from `flipper_len`, `bill_len`, and `bill_dep`

# Step 1: specify the model
lm_mod <- parsnip::linear_reg() |> parsnip::set_engine("lm")
lm_mod

# Step 2: specify the pre-processing
lm_rec <- recipes::recipe(body_mass ~ flipper_len + bill_len + bill_dep, data = datasets::penguins) |>
  recipes::step_naomit(recipes::all_predictors())
lm_rec

# Step 3: build a workflow
lm_wf <- workflows::workflow() |> workflows::add_recipe(lm_rec) |> workflows::add_model(lm_mod)
lm_wf

# Step 4: fit the model
lm_fit <- lm_wf |> infer::fit(data = datasets::penguins)
lm_fit
broom::tidy(lm_fit)

# Step 5: predict
res_lm <- broom::augment(lm_fit, datasets::penguins)
res_lm |> select(.pred, .resid, body_mass, flipper_len, bill_len, bill_dep)

# Step 6: evaluate
res <- yardstick::metrics(res_lm, truth = body_mass, estimate = .pred)
res

##################################################################
# Your time: logistic regression with tidymodels
# Goal: predict penguin `sex` from `flipper_len`, `bill_len`, and `bill_dep`
# Hint: `sex` needs to stay a factor (female/ male) - no need to recode it to 0/1

# Step 1: specify the model - you will be using `logistic_reg()`, which engine to use?


# Step 2: specify the pre-processing


# Step 3: build a workflow to combine the model and recipe


# Step 4: fit the model


# Step 5: obtain the prediction


# Step 6: obtain the accuracy metric


# Extra time:
# * read the documentation of `yardstick::conf_mat()` and obtain the confusion matrix
# * read the documentation of `yardstick::roc_curve()` and plot the ROC curve
