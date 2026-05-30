# Pure functions for between-condition power vs sequencing depth.
# Dispatcher pattern: add new models in pow_compute() and pow_model_*().

`%||%` <- function(x, y) if (is.null(x)) y else x

# m1, m2: expected Poisson means (total counts per condition) for one variant.
pow_poisson_lfc_se <- function(m1, m2) {
  if (m1 <= 0 || m2 <= 0) return(Inf)
  sqrt(1 / m1 + 1 / m2) / log(2)
}

#' Two-sided normal-approximation power for detecting |true log2 FC| = |delta|.
pow_normal_two_sided_power <- function(delta, se, alpha) {
  if (!is.finite(se) || se <= 0 || !is.finite(delta)) return(NA_real_)
  z <- stats::qnorm(1 - alpha / 2)
  ncp <- abs(delta) / se
  1 - stats::pnorm(z - ncp) + stats::pnorm(-z - ncp)
}

#' Means for Poisson LFC model: ref vs alt with true log2 fold-change `delta_log2`.
pow_poisson_lfc_means <- function(mean_depth, n_rep, delta_log2) {
  m1 <- n_rep * mean_depth
  m2 <- n_rep * mean_depth * (2 ^ delta_log2)
  list(m1 = m1, m2 = m2)
}

pow_model_poisson_lfc <- function(params) {
  mean_depth <- as.numeric(params$mean_depth)
  n_rep <- as.numeric(params$n_rep)
  delta_log2 <- as.numeric(params$delta_log2)
  alpha <- as.numeric(params$alpha)

  if (length(mean_depth) != 1L || length(n_rep) != 1L || length(delta_log2) != 1L ||
      length(alpha) != 1L) {
    stop("pow_model_poisson_lfc: scalar mean_depth, n_rep, delta_log2, alpha required")
  }

  mm <- pow_poisson_lfc_means(mean_depth, n_rep, delta_log2)
  m1 <- mm$m1
  m2 <- mm$m2
  se <- pow_poisson_lfc_se(m1, m2)
  pow <- pow_normal_two_sided_power(delta_log2, se, alpha)

  curve_max <- params$curve_depth_max
  if (is.null(curve_max) || !is.finite(curve_max) || curve_max <= 0) {
    curve_max <- max(200, mean_depth * 3, na.rm = TRUE)
  }
  curve_min <- max(0.5, params$curve_depth_min %||% 0.5)
  depths <- exp(seq(log(curve_min), log(curve_max), length.out = 80L))
  curve_power <- vapply(depths, function(d) {
    mm2 <- pow_poisson_lfc_means(d, n_rep, delta_log2)
    se_d <- pow_poisson_lfc_se(mm2$m1, mm2$m2)
    pow_normal_two_sided_power(delta_log2, se_d, alpha)
  }, numeric(1L))

  target <- as.numeric(params$target_power %||% 0.8)
  min_d <- pow_poisson_lfc_min_depth(n_rep, delta_log2, alpha, target)

  low_count <- m1 < 5 || m2 < 5

  list(
    model_id = "poisson_lfc",
    model_label = "Poisson counts, delta-method SE of log2 fold change",
    mean_depth = mean_depth,
    n_rep = n_rep,
    delta_log2 = delta_log2,
    alpha = alpha,
    m1 = m1,
    m2 = m2,
    se_lfc = se,
    power = pow,
    curve = data.frame(mean_depth = depths, power = curve_power),
    min_depth_for_target = min_d$depth,
    min_depth_target_power = target,
    min_depth_ok = min_d$ok,
    min_depth_message = min_d$message,
    low_count_warning = isTRUE(low_count),
    assumptions_bullets = c(
      "One variant; counts per condition are Poisson with means m1 = R x depth, m2 = R x depth x 2^LFC.",
      "R is independent biological replicates per condition; depth is mean total reads per variant per replicate.",
      "SE(log2 FC) uses the delta method: sqrt(1/m1 + 1/m2) / log(2).",
      "Power uses a two-sided normal test at the stated alpha (large-mean approximation).",
      "Not a substitute for your full scoring pipeline; overdispersion and hierarchy are not modeled in v1."
    )
  )
}

pow_poisson_lfc_min_depth <- function(n_rep, delta_log2, alpha, target_power) {
  if (n_rep <= 0 || !is.finite(delta_log2) || delta_log2 == 0 ||
      !is.finite(target_power) || target_power <= 0 || target_power >= 1) {
    return(list(depth = NA_real_, ok = FALSE, message = "Invalid inputs for min-depth search."))
  }

  f <- function(log_d) {
    d <- exp(log_d)
    mm <- pow_poisson_lfc_means(d, n_rep, delta_log2)
    se <- pow_poisson_lfc_se(mm$m1, mm$m2)
    pow_normal_two_sided_power(delta_log2, se, alpha) - target_power
  }

  lo <- log(0.05)
  hi <- log(1e7)
  flo <- f(lo)
  fhi <- f(hi)
  if (!is.finite(flo) || !is.finite(fhi)) {
    return(list(depth = NA_real_, ok = FALSE, message = "Could not evaluate power curve endpoints."))
  }
  if (fhi < 0) {
    return(list(
      depth = NA_real_,
      ok = FALSE,
      message = "Even at very high depth, target power is not reached (try more replicates or a larger effect)."
    ))
  }
  if (flo > 0) {
    return(list(
      depth = exp(lo),
      ok = TRUE,
      message = "Target power already met at minimal search depth."
    ))
  }

  root <- tryCatch(
    stats::uniroot(f, interval = c(lo, hi), extendInt = "upX", tol = 1e-4),
    error = function(e) NULL
  )
  if (is.null(root)) {
    return(list(depth = NA_real_, ok = FALSE, message = "Root search failed."))
  }
  list(depth = exp(root$root), ok = TRUE, message = "")
}

# model_id: v1 supports "poisson_lfc". params: named list, model-specific.
pow_compute <- function(model_id, params) {
  switch(model_id,
    poisson_lfc = pow_model_poisson_lfc(params),
    stop("Unknown pow model_id: ", model_id, call. = FALSE)
  )
}
