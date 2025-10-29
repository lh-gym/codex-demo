skip_if_not_installed("VGAM")

source("ppo_full_pipeline.R")

example <- simulate_example(seed = 99, n = 400)
example_data <- example$data
predictors <- c("age", "sex", "smoke")

test_that("fit_ppo defaults to proportional odds", {
  fit_po <- fit_ppo(
    data = example_data,
    response = Y,
    predictors = predictors,
    non_parallel = NULL,
    keep_po = FALSE
  )

  age_slopes <- fit_po$effective_slopes$estimate[fit_po$effective_slopes$term == "age"]
  expect_equal(length(unique(round(age_slopes, 8))), 1L)
})

test_that("non_parallel variables obtain free slopes", {
  fit_np <- fit_ppo(
    data = example_data,
    response = Y,
    predictors = predictors,
    non_parallel = c("age"),
    keep_po = FALSE
  )

  age_slopes <- fit_np$effective_slopes$estimate[fit_np$effective_slopes$term == "age"]
  expect_gt(length(unique(round(age_slopes, 6))), 1L)
})

test_that("trend constraints respect supplied fj", {
  fj_age <- c(0, 1, 2)
  fit_trend <- fit_ppo(
    data = example_data,
    response = Y,
    predictors = predictors,
    fj_list = list(age = fj_age),
    non_parallel = NULL,
    keep_po = FALSE
  )

  coef_mat <- as.matrix(coef(fit_trend$model))
  beta_age <- coef_mat["beta_age", 1]
  gamma_age <- coef_mat["gamma_age", 1]

  expected <- beta_age + gamma_age * fj_age
  eff_age <- fit_trend$effective_slopes$estimate[fit_trend$effective_slopes$term == "age"]

  expect_equal(round(eff_age, 6), round(expected, 6))
})
