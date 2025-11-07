library(tidyverse)
library(kknn)
library(tidymodels)

##################################################################
# Example 1: Linear model: basics
lm_mod <- parsnip::linear_reg() |> parsnip::set_engine("lm")
lm_rec <- recipes::recipe(body_mass ~ flipper_len + bill_len + bill_dep, data = datasets::penguins) |>
  recipes::step_naomit(recipes::all_predictors())
lm_wf <- workflows::workflow() |> workflows::add_recipe(lm_rec) |> workflows::add_model(lm_mod)
lm_fit <- lm_wf |> infer::fit(data = datasets::penguins)
res_lm <- broom::augment(lm_fit, datasets::penguins)
res <- yardstick::metrics(res_lm, truth = body_mass, estimate = .pred)


##################################################################
# Your time
# Can you replicate the example to fit a logistic regression






##################################################################
# Example 2: Logistic regression with training-testing split
set.seed(123)
penguins_split <- rsample::initial_split(datasets::penguins, prop = 0.8)
penguins_train <- rsample::training(penguins_split)
penguins_test  <- rsample::testing(penguins_split)

lr_mod <- parsnip::logistic_reg() %>% parsnip::set_engine("glm")
lr_rec <- recipes::recipe(sex ~ flipper_len + bill_len + bill_dep, data = penguins_train) |>
  recipes::step_naomit(recipes::all_predictors())
lr_wf <- workflows::workflow() |> workflows::add_recipe(lr_rec) |> workflows::add_model(lr_mod)


lr_fit <- lr_wf |> infer::fit(data = penguins_train)

res_split <- broom::augment(lr_fit, penguins_test)


confmax <- yardstick::conf_mat(res_split, truth = sex, estimate = .pred_class)
roc_data <- yardstick::roc_curve(res_split, truth = sex, .pred_female)
autoplot(roc_data)


###################################################################
# Your time
# Classification with KNN: training-testing split






###################################################################
# Example 3: Cross validation with KNN

# Step 1/2/3: Specify the pre-processing recipe, the model, and the workflow - same
penguins_clean <- datasets::penguins |> na.omit()
set.seed(123)
penguins_split <- rsample::initial_split(penguins_clean, prop = 0.8)
penguins_train <- rsample::training(penguins_split)
penguins_test  <- rsample::testing(penguins_split)

penguins_rec <- recipe(sex ~ bill_len + bill_dep + flipper_len, data = penguins_train) |>
  step_normalize(all_predictors())
knn_mod <- nearest_neighbor(mode = "classification", neighbors = 7) |> set_engine("kknn")
wf <- workflow() |> add_recipe(penguins_rec) |> add_model(knn_mod)

## Step 4: Generate the cross validation sampling/ fit the model

## Step 5/6: Predict and calculate classification metrics

###################################################################
# Example 4: hyperparameter tuning

# Step 1/2/3: Specify the pre-processing recipe, the model, and the workflow - same
penguins_clean <- datasets::penguins |> na.omit()
set.seed(123)
penguins_split <- initial_split(penguins_clean, prop = 0.8)
penguins_train <- training(penguins_split)
penguins_test  <- testing(penguins_split)

penguins_rec <- recipe(sex ~ bill_len + bill_dep, data = penguins_train) |>
  step_normalize(all_predictors())

knn_spec <- nearest_neighbor(mode = "classification", neighbors = tune::tune()) |> set_engine("kknn")
knn_wf <- workflow() |> add_recipe(penguins_rec) |> add_model(knn_spec)


# Step 4: Generate the cross validation sampling


# Step 5/6: Predict and calculate classification metrics
