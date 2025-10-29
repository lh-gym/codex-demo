#' Partial Proportional Odds (PPO) workflow demo
#'
#' This script mirrors the four-step process described in `PPO_playground.Rmd`:
#'   1. generate or load an ordered-response data set and define cut-point trends (f_j)
#'   2. fit the fully unconstrained PPO model (model 5)
#'   3. fit the common-trend constrained PPO model (model 6)
#'   4. fit the variable-specific trend constrained PPO model (model 7)
#'
#' It concludes by exporting the “effective slopes” per cutpoint so that
#' the results can be compared side-by-side.
#'
#' The current implementation uses the simulation design from the R Markdown
#' playground, but the `run_ppo_pipeline()` function is written so that you can
#' supply your own data, response column, and f_j specification.

suppressPackageStartupMessages({
  required_pkgs <- c("VGAM", "rlang", "stats")
  missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkgs)) {
    install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
  }
})

library(VGAM)
library(rlang)

# ---- helper utilities -----------------------------------------------------

#' Construct the default intercept constraint matrix for K cutpoints.
make_intercept_constraint <- function(K) {
  diag(1, nrow = K, ncol = K, dimnames = list(NULL, paste0("alpha_", seq_len(K))))
}

#' Build a VGAM constraint matrix tying a variable's coefficients together.
#'
#' @param weights numeric vector of length K describing the f_j pattern.
#' @param name    base name to use for the constrained coefficient.
make_trend_constraint <- function(weights, name) {
  matrix(weights, nrow = length(weights), dimnames = list(NULL, name))
}

#' Calculate effective slopes (beta + gamma * f_j) from a fitted model.
#'
#' @param coef_matrix matrix returned by `coef()`.
#' @param base_name name of the parallel term.
#' @param trend_name name of the trend term.
#' @param weights f_j vector used in the constraints.
compute_effective_slopes <- function(coef_matrix, base_name, trend_name, weights) {
  beta_idx <- grep(paste0("^", base_name, "(?::[0-9]+)?$"), rownames(coef_matrix), perl = TRUE)
  if (length(beta_idx) != length(weights)) {
    stop(
      sprintf(
        "Unable to locate %d coefficients for '%s' in the fitted model.",
        length(weights),
        base_name
      )
    )
  }

  beta <- coef_matrix[beta_idx, 1]

  if (!trend_name %in% rownames(coef_matrix)) {
    stop(sprintf("Trend term '%s' not found in the fitted model.", trend_name))
  }

  gamma <- coef_matrix[trend_name, 1]
  as.numeric(beta + gamma * weights)
}

# ---- main pipeline --------------------------------------------------------

#' Run the PPO workflow on a data frame.
#'
#' @param data data.frame containing the ordered response and predictors.
#' @param response unquoted name of the ordered response column.
#' @param predictors character vector of predictor column names.
#' @param fj_list named list whose elements are numeric vectors of the f_j pattern
#'   for each predictor (used in model 7); the special name "common" is used for
#'   the shared pattern in model 6.
#' @return list with fitted models, effective slopes, and raw coefficients.
run_ppo_pipeline <- function(data, response, predictors, fj_list) {
  response <- ensym(response)
  response_name <- as_string(response)

  if (!is.ordered(data[[response_name]])) {
    stop("The response column must be an ordered factor.")
  }

  K <- length(levels(data[[response_name]])) - 1L
  if (K <= 0) {
    stop("The response column must have at least two ordered levels.")
  }

  formula_base <- as.formula(
    paste(response_name, "~", paste(predictors, collapse = " + "))
  )

  # Step 2: fully unconstrained PPO (model 5)
  model5 <- vglm(
    formula_base,
    family = cumulative(parallel = FALSE, reverse = TRUE),
    data = data
  )

  # Prepare additional columns for trend components
  trend_cols <- paste0(predictors, "_tr")
  data[trend_cols] <- data[predictors]

  intercept_constraint <- list("(Intercept)" = make_intercept_constraint(K))
  parallel_constraints <- lapply(predictors, function(var) {
    matrix(1, nrow = K, dimnames = list(NULL, paste0("beta_", var)))
  })
  names(parallel_constraints) <- predictors

  # Step 3: common trend constraint (model 6)
  common_fj <- fj_list[["common"]]
  if (length(common_fj) != K) {
    stop("The common f_j vector must have length equal to the number of cutpoints.")
  }
  trend_constraints_common <- stats::setNames(
    lapply(trend_cols, function(var_tr) {
      make_trend_constraint(common_fj, paste0("gamma_", sub("_tr$", "", var_tr)))
    }),
    trend_cols
  )

  formula_trend <- as.formula(
    paste(response_name, "~", paste(c(predictors, trend_cols), collapse = " + "))
  )

  model6 <- vglm(
    formula_trend,
    family = cumulative(parallel = FALSE, reverse = TRUE),
    data = data,
    constraints = c(intercept_constraint, trend_constraints_common, parallel_constraints)
  )

  # Step 4: variable-specific trend constraint (model 7)
  trend_constraints_specific <- stats::setNames(
    lapply(seq_along(predictors), function(i) {
      var <- predictors[[i]]
      fj <- fj_list[[var]]
      if (length(fj) != K) {
        stop(sprintf("f_j for predictor '%s' must have length %d", var, K))
      }
      make_trend_constraint(fj, paste0("gamma_", var))
    }),
    trend_cols
  )

  model7 <- vglm(
    formula_trend,
    family = cumulative(parallel = FALSE, reverse = TRUE),
    data = data,
    constraints = c(intercept_constraint, trend_constraints_specific, parallel_constraints)
  )

  # Build effective slopes table
  coef5 <- as.matrix(coef(model5))
  coef6 <- as.matrix(coef(model6))
  coef7 <- as.matrix(coef(model7))

  eff_table <- data.frame(cutpoint = seq_len(K))

  # Model 5 effective slopes are already per-cutpoint coefficients
  for (var in predictors) {
    eff_table[[paste0("m5_", var)]] <- coef5[grep(paste0("^", var), rownames(coef5)), 1]
  }

  for (var in predictors) {
    eff_table[[paste0("m6_", var)]] <- compute_effective_slopes(
      coef6, var, paste0(var, "_tr"), common_fj
    )
    eff_table[[paste0("m7_", var)]] <- compute_effective_slopes(
      coef7, var, paste0(var, "_tr"), fj_list[[var]]
    )
  }

  list(
    model5 = model5,
    model6 = model6,
    model7 = model7,
    effective_slopes = eff_table,
    coefficients = list(model5 = coef5, model6 = coef6, model7 = coef7)
  )
}

# ---- demonstration --------------------------------------------------------

simulate_example <- function(seed = 2025L, n = 2000L) {
  set.seed(seed)
  K <- 3L
  fj_age   <- c(0, 1, 2)
  fj_sex   <- c(0, 0, 1)
  fj_smoke <- c(0, 1, 1)

  age <- scale(rnorm(n, 50, 10))[ , 1]
  sex <- rbinom(n, 1, 0.5)
  smoke <- rbinom(n, 1, 0.4)

  beta_age   <- 0.20; gamma_age   <- +0.15
  beta_sex   <- 0.80; gamma_sex   <- -0.60
  beta_smoke <- 0.70; gamma_smoke <- -0.30

  alpha <- c(0.5, -0.3, -1.2)

  lin_pred <- sapply(seq_len(K), function(j) {
    b_age_j   <- beta_age   + gamma_age   * fj_age[j]
    b_sex_j   <- beta_sex   + gamma_sex   * fj_sex[j]
    b_smk_j   <- beta_smoke + gamma_smoke * fj_smoke[j]
    eta <- alpha[j] + age * b_age_j + sex * b_sex_j + smoke * b_smk_j
    plogis(eta)
  })

  p0 <- 1 - lin_pred[, 1]
  p1 <- lin_pred[, 1] - lin_pred[, 2]
  p2 <- lin_pred[, 2] - lin_pred[, 3]
  p3 <- lin_pred[, 3]
  probs <- cbind(p0, p1, p2, p3)

  Y <- apply(probs, 1, function(p) sample(0:3, size = 1, prob = p, replace = TRUE))

  list(
    data = data.frame(
      Y = ordered(Y),
      age = as.numeric(age),
      sex = sex,
      smoke = smoke
    ),
    fj_spec = list(
      common = 0:(K - 1),
      age = fj_age,
      sex = fj_sex,
      smoke = fj_smoke
    )
  )
}

if (sys.nframe() == 0L) {
  example <- simulate_example()
  example_data <- example$data
  fj_spec <- example$fj_spec

  predictors <- c("age", "sex", "smoke")

  fits <- run_ppo_pipeline(
    data = example_data,
    response = Y,
    predictors = predictors,
    fj_list = fj_spec
  )

  eff_path <- "effective_slopes_table.csv"
  write.csv(round(fits$effective_slopes, 4), eff_path, row.names = FALSE)

  message("Effective slopes written to ", eff_path)
  print(fits$effective_slopes)
}
