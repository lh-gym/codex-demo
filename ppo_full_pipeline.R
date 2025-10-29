#' Partial Proportional Odds (PPO) workflow demo
#'
#' 本脚本围绕 Peterson (1990, JRSS-C) 对有序响应 PPO 模型的三个要点展开：
#' 1. 截距参数控制各 cutpoint 的阈值，需保持单调递增；
#' 2. 斜率部分可拆分为“平行斜率”(β) 与“松弛项”(γ × f_j)；
#' 3. 通过约束矩阵选择性地让协变量放松平行性。
#'
#' 核心函数 `fit_ppo()` 提供：
#' * 与纯比例优势模型兼容的默认行为（`non_parallel = NULL`）；
#' * 为给定协变量解锁非平行斜率（`non_parallel`）或按指定 f_j 模式放松（`fj_list`）；
#' * 返回易读的系数表、有效斜率与基于 LR 的平行性检验。
#'
#' `run_ppo_pipeline()` 在教学/报告场景中一次性给出 PO、全非平行（模型 5）、
#' 统一趋势（模型 6）与变量特定趋势（模型 7）的结果。

suppressPackageStartupMessages({
  required_pkgs <- c("VGAM", "rlang", "stats")
  missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkgs)) {
    install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
  }
})

library(VGAM)
library(rlang)

`%||%` <- function(x, y) {
  if (!is.null(x)) x else y
}

# ---- helper utilities -----------------------------------------------------

#' 构造截距的单位矩阵约束，确保每个 cutpoint 拥有独立阈值。
make_intercept_constraint <- function(K) {
  mat <- diag(1, nrow = K, ncol = K)
  colnames(mat) <- paste0("alpha_", seq_len(K))
  mat
}

#' 构造 trend 约束矩阵：第 j 行为 f_j，对应 γ × f_j。
#'
#' @param weights numeric vector of length K describing the f_j pattern.
#' @param name    base name to use for the constrained coefficient.
make_trend_constraint <- function(weights, name) {
  matrix(weights, nrow = length(weights), dimnames = list(NULL, name))
}

#' 将 `coef()` 输出整理为 data.frame，区分平行、趋势与自由斜率。
#'
#' @param coef_matrix 矩阵形式的系数。
#' @return data.frame(term, component, cutpoint, estimate)。
tidy_vglm_coefficients <- function(coef_matrix) {
  rows <- rownames(coef_matrix)
  estimates <- as.numeric(coef_matrix[, 1])

  # 拆解 rowname 中的 cutpoint 标记（形式如 term:1）
  term <- sub(":[0-9]+$", "", rows)
  cutpoint <- suppressWarnings(as.integer(sub("^.*:(\\d+)$", "\\1", rows)))
  has_cutpoint <- grepl(":[0-9]+$", rows)

  component <- ifelse(grepl("^alpha_", term), "intercept",
    ifelse(grepl("^gamma_", term), "trend",
      ifelse(grepl("^beta_", term), "parallel",
        ifelse(has_cutpoint, "non_parallel", "parallel")
      )
    )
  )

  data.frame(
    term = term,
    component = component,
    cutpoint = ifelse(component %in% c("parallel", "trend") & !has_cutpoint, NA_integer_, cutpoint),
    estimate = estimates,
    row.names = NULL
  )
}

#' 根据 β、γ 与 f_j 计算有效斜率（β + γ × f_j）。
#'
#' @param coef_matrix `coef(fit)` 的矩阵形式。
#' @param predictors 协变量名称向量。
#' @param K cutpoint 数（等级数 - 1）。
#' @param non_parallel 字符向量，标记自由斜率变量。
#' @param trend_info 命名列表：变量 → f_j 向量。
compute_effective_slopes_table <- function(coef_matrix, predictors, K, non_parallel, trend_info) {
  result <- lapply(predictors, function(var) {
    if (var %in% names(trend_info)) {
      beta_name <- paste0("beta_", var)
      trend_name <- paste0("gamma_", var)
      beta <- coef_matrix[beta_name, 1]
      gamma <- coef_matrix[trend_name, 1]
      data.frame(
        term = var,
        cutpoint = seq_len(K),
        estimate = as.numeric(beta + gamma * trend_info[[var]]),
        component = "structured",
        row.names = NULL
      )
    } else if (var %in% non_parallel) {
      idx <- grep(paste0("^", var, "(?::[0-9]+)?$"), rownames(coef_matrix), perl = TRUE)
      if (length(idx) != K) {
        stop(sprintf("未能为变量 %s 找到 %d 个 cutpoint 斜率。", var, K))
      }
      data.frame(
        term = var,
        cutpoint = seq_len(K),
        estimate = as.numeric(coef_matrix[idx, 1]),
        component = "non_parallel",
        row.names = NULL
      )
    } else {
      # 纯平行项：同一个值重复 K 次
      beta_name <- paste0("beta_", var)
      if (!beta_name %in% rownames(coef_matrix)) {
        beta_name <- var
      }
      beta <- coef_matrix[beta_name, 1]
      data.frame(
        term = var,
        cutpoint = seq_len(K),
        estimate = rep(as.numeric(beta), K),
        component = "parallel",
        row.names = NULL
      )
    }
  })

  do.call(rbind, result)
}

# ---- model fitting --------------------------------------------------------

#' Fit a partial proportional odds model with optional non-parallel slopes.
#'
#' 函数遵循 Peterson (1990) 提出的“β + γ × f_j” 参数化：
#' 默认拟合纯比例优势（β 平行）；指定 `non_parallel` 后，选定协变量改为
#' cutpoint 特异的自由斜率；若同时提供 `fj_list`，则为变量创建趋势列，
#' 通过 γ × f_j 形式松弛平行性，兼顾可解释性与统计稳定性。
#'
#' @param data 数据框，需包含有序因变量与协变量。
#' @param response 未引用的有序因变量名称。
#' @param predictors 字符向量，列出模型中的协变量。
#' @param non_parallel 字符向量，哪些协变量允许非平行斜率。
#' @param fj_list 命名列表：变量 → f_j 向量；可含 `common` 作为默认模式。
#' @param link 链接函数，传递给 `VGAM::cumulative()`。
#' @param reverse 是否按 `VGAM` 的 `reverse` 选项重排等级。
#' @param keep_po 是否同时拟合 PO 基准模型，以便进行 LR 检验。
#' @param ... 传递给 `VGAM::vglm()` 的其他参数。
#'
#' @return 列表：`model`、`po_model`、`coefficients`、`effective_slopes`、`lr_test`、
#'   `fitted_probabilities`、`predictor_info`。
#' @examples
#' \dontrun{
#'   fit <- fit_ppo(dat, response = Y, predictors = c("age", "sex"),
#'                  non_parallel = "age")
#' }
fit_ppo <- function(
    data,
    response,
    predictors,
    non_parallel = NULL,
    fj_list = NULL,
    link = "logit",
    reverse = TRUE,
    keep_po = TRUE,
    ...) {
  response <- ensym(response)
  response_name <- as_string(response)

  if (!response_name %in% names(data)) {
    stop(sprintf("数据中不存在响应变量 %s。", response_name))
  }
  if (!is.ordered(data[[response_name]])) {
    stop("响应变量必须是有序因子。")
  }

  K <- length(levels(data[[response_name]])) - 1L
  if (K <= 0) {
    stop("响应变量至少需要两个有序等级。")
  }

  predictors <- unique(predictors)
  missing_pred <- setdiff(predictors, names(data))
  if (length(missing_pred)) {
    stop(sprintf("以下协变量不存在：%s", paste(missing_pred, collapse = ", ")))
  }

  # 构建 trend 信息：若未显式给定则为空列表
  trend_info <- list()
  if (!is.null(fj_list)) {
    if (!is.null(fj_list[["common"]])) {
      # 为未指定的变量使用共同 f_j
      common_pattern <- fj_list[["common"]]
      for (var in predictors) {
        trend_info[[var]] <- fj_list[[var]] %||% common_pattern
      }
    } else {
      overlap <- intersect(names(fj_list), predictors)
      trend_info <- fj_list[overlap]
    }
  }

  # 清理 NA 与重复
  trend_info <- trend_info[!vapply(trend_info, is.null, logical(1))]
  if (length(trend_info)) {
    invalid_len <- vapply(trend_info, length, integer(1)) != K
    if (any(invalid_len)) {
      bad <- names(trend_info)[invalid_len]
      stop(sprintf("f_j 长度必须等于 cutpoint 数 (K=%d)：%s", K, paste(bad, collapse = ", ")))
    }
    conflict_vars <- intersect(non_parallel %||% character(), names(trend_info))
    if (length(conflict_vars)) {
      stop(sprintf(
        "以下变量同时出现在 non_parallel 与 fj_list 中，模型含义冲突：%s",
        paste(conflict_vars, collapse = ", ")
      ))
    }
  }

  data_work <- data
  trend_cols <- character()
  if (length(trend_info)) {
    for (var in names(trend_info)) {
      new_col <- paste0(var, "_trend")
      data_work[[new_col]] <- data_work[[var]]
      trend_cols <- c(trend_cols, new_col)
    }
  }

  formula_terms <- c(predictors, trend_cols)
  formula_base <- as.formula(
    paste(response_name, "~", paste(formula_terms, collapse = " + "))
  )

  intercept_constraint <- list("(Intercept)" = make_intercept_constraint(K))
  parallel_vars <- setdiff(predictors, non_parallel %||% character())
  parallel_constraints <- lapply(parallel_vars, function(var) {
    matrix(1, nrow = K, dimnames = list(NULL, paste0("beta_", var)))
  })
  if (length(parallel_constraints)) {
    names(parallel_constraints) <- parallel_vars
  }

  trend_constraints <- lapply(names(trend_info), function(var) {
    make_trend_constraint(trend_info[[var]], paste0("gamma_", var))
  })
  if (length(trend_constraints)) {
    names(trend_constraints) <- paste0(names(trend_info), "_trend")
  }

  constraints <- c(intercept_constraint, parallel_constraints, trend_constraints)

  model <- vglm(
    formula_base,
    family = cumulative(link = link, parallel = FALSE, reverse = reverse),
    data = data_work,
    constraints = constraints,
    ...
  )

  po_model <- NULL
  if (isTRUE(keep_po)) {
    po_model <- vglm(
      as.formula(paste(response_name, "~", paste(predictors, collapse = " + "))),
      family = cumulative(link = link, parallel = TRUE, reverse = reverse),
      data = data,
      ...
    )
  }

  coef_matrix <- as.matrix(coef(model))
  coef_tidy <- tidy_vglm_coefficients(coef_matrix)
  effective_slopes <- compute_effective_slopes_table(
    coef_matrix = coef_matrix,
    predictors = predictors,
    K = K,
    non_parallel = non_parallel,
    trend_info = trend_info
  )

  lr_test <- NULL
  if (!is.null(po_model)) {
    lr_test <- tryCatch(
      VGAM::lrtest(po_model, model),
      error = function(e) {
        warning("LR 检验失败：", conditionMessage(e))
        NULL
      }
    )
  }

  fitted_probabilities <- predict(model, type = "response")

  list(
    call = match.call(),
    model = model,
    po_model = po_model,
    coefficients = coef_tidy,
    effective_slopes = effective_slopes,
    lr_test = lr_test,
    fitted_probabilities = fitted_probabilities,
    predictor_info = list(
      non_parallel = non_parallel,
      structured = names(trend_info),
      parallel = setdiff(parallel_vars, names(trend_info))
    ),
    K = K
  )
}

#' 汇总多个 `fit_ppo()` 结果，生成方便对比的有效斜率表。
#'
#' @param fits 命名列表，每个元素为 `fit_ppo()` 的返回值。
#' @param predictors 协变量名称。
combine_effective_slopes <- function(fits, predictors) {
  if (!length(fits)) {
    return(data.frame())
  }
  K <- fits[[1]]$K
  table <- data.frame(cutpoint = seq_len(K))
  for (name in names(fits)) {
    slopes <- fits[[name]]$effective_slopes
    for (var in predictors) {
      idx <- slopes$term == var
      col_name <- paste(name, var, sep = "_")
      table[[col_name]] <- slopes$estimate[idx]
    }
  }
  table
}

# ---- pipeline -------------------------------------------------------------

#' Run the PPO workflow on a data frame.
#'
#' @param data 数据框。
#' @param response 未引用的有序响应列名。
#' @param predictors 字符向量，协变量。
#' @param fj_list 命名列表：用于模型 (6)/(7) 的 f_j 设定，允许 `common`。
#' @param non_parallel 字符向量，演示模型 (7) 时额外放开的变量。
#'
#' @return 列表：包含四类模型、有效斜率表与原始系数。
run_ppo_pipeline <- function(data, response, predictors, fj_list, non_parallel = predictors) {
  response <- ensym(response)

  fits <- list(
    po = fit_ppo(data, !!response, predictors, non_parallel = NULL, keep_po = FALSE),
    model5 = fit_ppo(data, !!response, predictors, non_parallel = predictors, keep_po = FALSE)
  )

  if (!is.null(fj_list)) {
    common_fj <- fj_list[["common"]]
    if (!is.null(common_fj)) {
      common_list <- stats::setNames(rep(list(common_fj), length(predictors)), predictors)
      fits$model6 <- fit_ppo(
        data,
        !!response,
        predictors,
        non_parallel = NULL,
        fj_list = common_list,
        keep_po = FALSE
      )
    }

    specific_keys <- intersect(predictors, names(fj_list))
    if (length(specific_keys)) {
      specific_list <- fj_list[specific_keys]
      fits$model7 <- fit_ppo(
        data,
        !!response,
        predictors,
        non_parallel = NULL,
        fj_list = specific_list,
        keep_po = FALSE
      )
    }
  }

  if (!"model7" %in% names(fits)) {
    fits$model7 <- fit_ppo(
      data,
      !!response,
      predictors,
      non_parallel = non_parallel,
      keep_po = FALSE
    )
  }

  effective_slopes <- combine_effective_slopes(fits, predictors)
  coefficient_matrices <- lapply(fits, function(x) as.matrix(coef(x$model)))

  list(
    fits = fits,
    effective_slopes = effective_slopes,
    coefficients = coefficient_matrices
  )
}

# ---- demonstration --------------------------------------------------------


#' 生成与 Rmd 相同的模拟数据，便于脚本直接运行。
simulate_example <- function(seed = 2025L, n = 2000L) {
  set.seed(seed)
  K <- 3L
  fj_age <- c(0, 1, 2)
  fj_sex <- c(0, 0, 1)
  fj_smoke <- c(0, 1, 1)

  age <- scale(rnorm(n, 50, 10))[ , 1]
  sex <- rbinom(n, 1, 0.5)
  smoke <- rbinom(n, 1, 0.4)

  beta_age <- 0.20; gamma_age <- +0.15
  beta_sex <- 0.80; gamma_sex <- -0.60
  beta_smoke <- 0.70; gamma_smoke <- -0.30
  alpha <- c(0.5, -0.3, -1.2)

  lin_pred <- sapply(seq_len(K), function(j) {
    b_age_j <- beta_age + gamma_age * fj_age[j]
    b_sex_j <- beta_sex + gamma_sex * fj_sex[j]
    b_smk_j <- beta_smoke + gamma_smoke * fj_smoke[j]
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
    fj_list = fj_spec,
    non_parallel = c("age", "smoke")
  )

  eff_path <- "effective_slopes_table.csv"
  write.csv(round(fits$effective_slopes, 4), eff_path, row.names = FALSE)

  message("Effective slopes written to ", eff_path)
  print(fits$effective_slopes)
}
