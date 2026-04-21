get_effect_sign <- function(model, coef_name = NULL) {
  if (is.null(coef_name)) {
    return(sign(model$beta[[1]])) # main effect
  } else {
    return(sign(model$beta[[coef_name]])) # moderator slope
  }
}

# -------------------------
# CATEGORICAL: per level
# -------------------------
# sim_power_cat_level <- function(df,
#                                 mod,
#                                 lvl,
#                                 b1,
#                                 b0 = 0,
#                                 nsim = 500,
#                                 alpha = 0.05,
#                                 seed = 123,
#                                 verbose = TRUE) {
#   if (!is.null(seed)) set.seed(seed)
#
#   sim_df <- df %>%
#     filter(!is.na(.data[[mod]]), .data[[mod]] == lvl)
#
#   if (nrow(sim_df) == 0) {
#     if (verbose) message("No rows for level '", lvl, "'. Returning NA.")
#     return(list(power = NA_real_, ci = c(NA_real_, NA_real_)))
#   }
#
#   sim_df$true_es <- b0 + b1
#
#   plan(multisession, workers = max(1, parallel::detectCores() - 1))
#
#   rej <- future_map_lgl(
#     seq_len(nsim),
#     ~ {
#       sim_df$yi <- rnorm(
#         nrow(sim_df),
#         mean = sim_df$true_es,
#         sd   = sqrt(sim_df$vi)
#       )
#
#       res <- tryCatch(
#         metafor::rma.mv(
#           yi, vi,
#           random = ~ 1 | participant_id/efN_id,
#           data   = sim_df,
#           method = "REML"
#         ),
#         error = function(e) NULL
#       )
#
#       if (is.null(res)) return(NA)
#
#       pval <- coef(summary(res))[1, "pval"]
#       !is.na(pval) && pval < alpha
#     },
#     .options = furrr_options(seed = seed)
#   )
#
#   plan(sequential)
#
#   n_good <- sum(!is.na(rej))
#   if (n_good == 0) return(list(power = NA_real_, ci = c(NA_real_, NA_real_)))
#
#   power_hat <- mean(rej, na.rm = TRUE)
#
#   x <- sum(rej, na.rm = TRUE)
#   n <- n_good
#   z <- qnorm(0.975)
#   phat <- x / n
#   denom <- 1 + z^2 / n
#   center <- (phat + z^2 / (2 * n)) / denom
#   half <- z * sqrt((phat * (1 - phat) / n + z^2 / (4 * n^2))) / denom
#
#   list(
#     power = power_hat,
#     ci = c(max(0, center - half), min(1, center + half))
#   )
# }

sim_power_cat_level <- function(df,
                                model,
                                mod,
                                lvl,
                                b1,
                                b0 = 0,
                                nsim = 500,
                                alpha = 0.05,
                                seed = 123,
                                verbose = TRUE) {
  model <- model$model_without_intercept$fit

  if (!is.null(seed)) set.seed(seed)

  # Filter to level
  sim_df <- df %>%
    dplyr::filter(!is.na(.data[[mod]]), .data[[mod]] == lvl)

  if (nrow(sim_df) == 0) {
    if (verbose) message("No rows for level '", lvl, "'. Returning NA.")
    return(list(power = NA_real_, ci = c(NA_real_, NA_real_)))
  }

  # Extract variance components from fitted model
  tau_sample <- model$sigma2[1]
  tau_es <- model$sigma2[2]

  samples <- unique(sim_df$participant_id)
  effects <- unique(sim_df$efN_id)

  sim_df$true_es <- b0 + b1

  # Start parallel workers
  plan(multisession, workers = max(1, parallel::detectCores() - 1))

  rej <- future.apply::future_sapply(seq_len(nsim), function(i) {
    # Random effects
    u_sample <- rnorm(length(samples), 0, sqrt(tau_sample))
    u_es <- rnorm(length(effects), 0, sqrt(tau_es))

    sim_df$u_sample <- u_sample[match(sim_df$participant_id, samples)]
    sim_df$u_es <- u_es[match(sim_df$efN_id, effects)]

    # Simulated effect sizes
    sim_df$yi <- sim_df$true_es +
      sim_df$u_sample +
      sim_df$u_es +
      rnorm(nrow(sim_df), 0, sqrt(sim_df$vi))

    # Fit model
    res <- tryCatch(
      metafor::rma.mv(
        yi, vi,
        random = ~ 1 | participant_id / efN_id,
        data = sim_df,
        method = "REML"
      ),
      error = function(e) NULL
    )

    if (is.null(res)) {
      return(NA)
    }

    pval <- coef(summary(res))[1, "pval"]
    return(!is.na(pval) && pval < alpha)
  })

  plan(sequential)

  good <- !is.na(rej)
  if (sum(good) == 0) {
    return(list(power = NA_real_, ci = c(NA_real_, NA_real_)))
  }

  power_hat <- mean(rej[good])


  list(power = power_hat)
}


find_mdes_level <- function(df,
                            model,
                            mod,
                            lvl,
                            target_power = 0.8,
                            b0 = 0,
                            mdes_min = NULL,
                            mdes_max = NULL,
                            tol = 0.01,
                            power_tol = 0.02, # NEW
                            nsim = 200,
                            seed = 123,
                            verbose = TRUE,
                            save_prefix = NULL) {
  fit <- model$model_without_intercept[["fit"]]

  beta_vec <- fit$beta
  beta_names <- rownames(beta_vec)

  coef_name <- paste0("factor(", mod, ")", lvl)
  coef_idx <- match(coef_name, beta_names)
  if (is.na(coef_idx)) stop("Coefficient not found: ", coef_name)

  b1_raw <- beta_vec[coef_idx, 1]
  b1_hat <- abs(b1_raw)
  sign_hat <- sign(b1_raw)

  if (is.null(mdes_min)) mdes_min <- max(0.001, b1_hat * 0.25)
  if (is.null(mdes_max)) mdes_max <- max(b1_hat * 2, 0.5) # WIDER UPPER BOUND

  lo <- mdes_min
  hi <- mdes_max
  iter <- 0
  max_iter <- 50

  results_iter <- list()

  while ((hi - lo) > tol && iter < max_iter) {
    iter <- iter + 1
    mid <- (lo + hi) / 2
    b1_signed <- sign_hat * mid

    res_mid <- sim_power_cat_level(
      df = df,
      model = model,
      mod = mod,
      lvl = lvl,
      b1 = b1_signed,
      b0 = b0,
      nsim = nsim,
      seed = seed,
      verbose = FALSE
    )

    pow_mid <- res_mid$power

    results_iter[[iter]] <- tibble(
      mod   = mod,
      level = lvl,
      iter  = iter,
      mdes  = mid,
      power = pow_mid
    )

    if (verbose) {
      message(
        "Bisection iteration ", iter,
        ": tested effect size = ", round(mid, 4),
        ". Estimated power = ", round(pow_mid, 3),
        " Interval width = ", round(hi - lo, 4), "."
      )
    }

    # if (is.na(pow_mid)) break

    if (pow_mid >= 0.790 && pow_mid <= 0.809) {
      lo <- hi <- mid
      break
    }


    if (pow_mid >= target_power) hi <- mid else lo <- mid
  }

  # final_b1 <- sign_hat * hi
  # final_res <- sim_power_cat_level(
  #   df    = df,
  #   model = model,
  #   mod   = mod,
  #   lvl   = lvl,
  #   b1    = final_b1,
  #   b0    = b0,
  #   nsim  = nsim * 2,
  #   seed  = seed,
  #   verbose = FALSE
  # )
  #
  # status <- if (abs(final_res$power - target_power) > 0.1) "far_from_target" else "ok"


  # final <- tibble(
  #   mod   = mod,
  #   level = lvl,
  #   mdes  = hi,
  #   power = final_res$power,
  #   status = status
  # )

  # list(final = final, path = bind_rows(results_iter))
  final <- tibble(
    mod   = mod,
    mdes  = hi,
    power = results_iter[[iter]]$power
  )
  return(list(final = final, path = bind_rows(results_iter)))
}


# find_mdes_cat_mod <- function(df,
#                               mod,
#                               cat_levels,
#                               target_power = 0.8,
#                               b0 = 0,
#                               b1_min = 0,
#                               b1_max = 1,
#                               tol = 0.01,
#                               nsim = 200,
#                               seed = 123,
#                               verbose = TRUE,
#                               save_prefix = NULL) {
#
#   res_list <- map(cat_levels, ~ find_mdes_level(
#     df           = df,
#     mod          = mod,
#     lvl          = .x,
#     target_power = target_power,
#     b0           = b0,
#     b1_min       = b1_min,
#     b1_max       = b1_max,
#     tol          = tol,
#     nsim         = nsim,
#     seed         = seed,
#     verbose      = verbose,
#     save_prefix  = save_prefix
#   ))
#
#   finals <- map_dfr(res_list, "final")
#   paths  <- map_dfr(res_list, "path")
#
#   list(final = finals, path = paths)
# }

# -------------------------
# CONTINUOUS MODERATOR
# -------------------------
# sim_power_cont <- function(df,
#                            mod,
#                            b1,
#                            b0 = 0,
#                            nsim = 200,
#                            alpha = 0.05,
#                            seed = 123,
#                            verbose = TRUE) {
#
#   if (!is.null(seed)) set.seed(seed)
#
#   if (!is.numeric(df[[mod]])) {
#     converted <- suppressWarnings(as.numeric(df[[mod]]))
#     bad_idx <- which(is.na(converted) & !is.na(df[[mod]]))
#     if (length(bad_idx) > 0) {
#       bad_vals <- unique(df[[mod]][bad_idx])
#       stop(
#         "Non-numeric values in moderator '", mod, "': ",
#         paste(bad_vals, collapse = ", ")
#       )
#     }
#     df[[mod]] <- converted
#   }
#
#   sim_df <- df %>% filter(!is.na(.data[[mod]]))
#
#   if (nrow(sim_df) == 0) {
#     return(list(power = NA_real_, ci = c(NA_real_, NA_real_)))
#   }
#
#   sim_df$true_es <- b0 + b1 * sim_df[[mod]]
#
#   plan(multisession, workers = max(1, parallel::detectCores() - 1))
#
#   rej <- future_map_lgl(
#     seq_len(nsim),
#     ~ {
#       sim_df$yi <- rnorm(
#         nrow(sim_df),
#         mean = sim_df$true_es,
#         sd   = sqrt(sim_df$vi)
#       )
#
#       res <- tryCatch(
#         metafor::rma.mv(
#           yi, vi,
#           mods   = as.formula(paste0("~ ", mod)),
#           random = ~ 1 | participant_id/efN_id,
#           data   = sim_df,
#           method = "REML"
#         ),
#         error = function(e) NULL
#       )
#
#       if (is.null(res)) return(NA)
#
#       pval <- coef(summary(res))[2, "pval"]
#       if (is.na(pval)) return(NA)
#       pval < alpha
#     },
#     .options = furrr_options(seed = seed)
#   )
#
#   plan(sequential)
#
#   n_good <- sum(!is.na(rej))
#   if (n_good == 0) return(list(power = NA_real_, ci = c(NA_real_, NA_real_)))
#
#   power_hat <- mean(rej, na.rm = TRUE)
#
#   x <- sum(rej, na.rm = TRUE)
#   n <- n_good
#   z <- qnorm(0.975)
#   phat <- x / n
#   denom <- 1 + z^2 / n
#   center <- (phat + z^2 / (2 * n)) / denom
#   half <- z * sqrt((phat * (1 - phat) / n + z^2 / (4 * n^2))) / denom
#
#   list(
#     power = power_hat,
#     ci = c(max(0, center - half), min(1, center + half))
#   )
# }

sim_power_cont <- function(df,
                           model,
                           mod,
                           b1,
                           b0 = 0,
                           nsim = 500, spare_cores = 1,
                           alpha = 0.05,
                           seed = 123, verbose = TRUE) {
  if (!is.null(seed)) set.seed(seed)

  # Ensure moderator is numeric
  df[[mod]] <- suppressWarnings(as.numeric(df[[mod]]))
  df <- df[!is.na(df[[mod]]), ]
  plan(multisession, workers = max(1, parallel::detectCores() - spare_cores))

  # Extract variance components
  tau_sample <- model$sigma2[1]
  tau_es <- model$sigma2[2]

  samples <- unique(df$participant_id)
  effects <- unique(df$efN_id)

  rej <- future.apply::future_sapply(seq_len(nsim), function(s) {
    sim_df <- df
    u_sample <- rnorm(length(samples), 0, sqrt(tau_sample))
    u_es <- rnorm(length(effects), 0, sqrt(tau_es))

    sim_df$u_sample <- u_sample[match(sim_df$participant_id, samples)]
    sim_df$u_es <- u_es[match(sim_df$efN_id, effects)]

    sim_df$yi <- b0 + b1 * sim_df[[mod]] + sim_df$u_sample + sim_df$u_es +
      rnorm(nrow(sim_df), 0, sqrt(sim_df$vi))

    fit <- tryCatch(
      metafor::rma.mv(
        yi, vi,
        mods = as.formula(paste0("~ ", mod)),
        random = ~ 1 | participant_id / efN_id,
        data = sim_df,
        method = "REML"
      ),
      error = function(e) NULL
    )

    if (is.null(fit)) {
      return(NA)
    }

    # slope is row 2
    p <- coef(summary(fit))[2, "pval"]
    return(!is.na(p) && p < alpha)
  },
  future.seed=TRUE)

  good <- !is.na(rej)
  power_hat <- mean(rej[good])

  plan(sequential)

  list(
    power = power_hat,
    tau_sample = tau_sample,
    tau_es = tau_es
  )
}
find_mdes_cont <- function(df,
                           model,
                           mod,
                           target_power = 0.80,
                           b0 = 0,
                           mdes_min = NULL,
                           mdes_max = NULL,
                           tol = 0.01,
                           nsim = 200,
                           seed = 123,
                           verbose = TRUE,
                           save_prefix = NULL) {
  model <- model$model_with_intercept$fit

  # slope is row 2
  b1_hat <- abs(model$beta[[2]])
  sign_hat <- sign(model$beta[[2]])

  # default interval (same logic as main)
  if (is.null(mdes_min)) mdes_min <- max(0.001, b1_hat * 0.25)
  if (is.null(mdes_max)) mdes_max <- b1_hat * 2

  lo <- mdes_min
  hi <- mdes_max
  iter <- 0
  max_iter <- 50

  results_iter <- list()
  while ((hi - lo) > tol && iter < max_iter) {
    iter <- iter + 1
    mid <- (lo + hi) / 2
    b1_signed <- sign_hat * mid

    res_mid <- sim_power_cont(
      df = df,
      model = model,
      mod = mod,
      b1 = b1_signed,
      b0 = b0,
      nsim = nsim,
      seed = seed,
      verbose = verbose
    )

    pow_mid <- res_mid$power

    results_iter[[iter]] <- tibble(
      mod   = mod,
      iter  = iter,
      mdes  = mid,
      power = pow_mid
    )

    if (verbose) {
      message(
        "Iter ", iter,
        "  | MDES=", round(mid, 4),
        "  | Power=", round(pow_mid, 3),
        "  | Width=", round(hi - lo, 4)
      )
    }

    # if (is.na(pow_mid)) break

    if (pow_mid >= 0.790 && pow_mid <= 0.809) {
      lo <- hi <- mid
      break
    }

    if (pow_mid >= target_power) hi <- mid else lo <- mid
  }

  # # final evaluation
  # final_b1 <- sign_hat * hi
  # final_res <- sim_power_cont(
  #   df    = df,
  #   model = model,
  #   mod   = mod,
  #   b1    = final_b1,
  #   b0    = b0,
  #   nsim  = nsim * 2,
  #   seed  = seed,
  #   verbose = verbose
  # )
  #
  # final <- tibble(
  #   mod   = mod,
  #   mdes  = hi,
  #   power = final_res$power
  # )
  #
  # list(final = final, path = bind_rows(results_iter))
  final <- tibble(
    mod   = mod,
    mdes  = mid,
    power = results_iter[[iter]]$power
  )
  return(list(final = final, path = bind_rows(results_iter)))
}

# -------------------------
# MAIN MODEL
# -------------------------
# sim_power_main <- function(df,
#                            model,
#                            b1,
#                            b0 = 0,
#                            nsim = 500,
#                            alpha = 0.05,
#                            seed = 123,
#                            verbose = TRUE) {
#
#   if (!is.null(seed)) set.seed(seed)
#
#
#   df$true_es <- b0 + b1
#
#   plan(multisession, workers = max(1, parallel::detectCores() - 1))
#
#   rej <- future_map_lgl(
#     seq_len(nsim),
#     ~ {
#       df$yi <- rnorm(
#         nrow(df),
#         mean = df$true_es,
#         sd   = sqrt(df$vi)
#       )
#
#       res <- tryCatch(
#         metafor::rma.mv(
#           yi, vi,
#           random = ~ 1 | participant_id/efN_id,
#           data   = df,
#           method = "REML"
#         ),
#         error = function(e) NULL
#       )
#
#       if (is.null(res)) return(NA)
#
#       pval <- coef(summary(res))[1, "pval"]
#       !is.na(pval) && pval < alpha
#     },
#     .options = furrr_options(seed = seed)
#   )
#
#   plan(sequential)
#
#   n_good <- sum(!is.na(rej))
#   if (n_good == 0) return(list(power = NA_real_, ci = c(NA_real_, NA_real_)))
#
#   power_hat <- mean(rej, na.rm = TRUE)
#
#   x <- sum(rej, na.rm = TRUE)
#   n <- n_good
#   z <- qnorm(0.975)
#   phat <- x / n
#   denom <- 1 + z^2 / n
#   center <- (phat + z^2 / (2 * n)) / denom
#   half <- z * sqrt((phat * (1 - phat) / n + z^2 / (4 * n^2))) / denom
#
#   list(
#     power = power_hat,
#     ci = c(max(0, center - half), min(1, center + half))
#   )
# }

sim_power_main <- function(df,
                           model,
                           b1,
                           b0 = 0,
                           nsim = 500, spare_cores = 1,
                           alpha = 0.05,
                           seed = 123, verbose = TRUE) {
  if (!is.null(seed)) set.seed(seed)

  plan(multisession, workers = max(1, parallel::detectCores() - 1))

  # --- Extract variance components from the fitted model ---
  tau_sample <- model$sigma2[1] # participant-level variance
  tau_es <- model$sigma2[2] # effect-size-level variance (likely ~0)

  # --- Extract cluster structure from df ---
  samples <- unique(df$participant_id)
  effects <- unique(df$efN_id)

  rej <- future.apply::future_sapply(seq_len(nsim), function(s) {
    u_sample <- rnorm(length(samples), 0, sqrt(tau_sample))
    u_es <- rnorm(length(effects), 0, sqrt(tau_es))

    df$u_sample <- u_sample[match(df$participant_id, samples)]
    df$u_es <- u_es[match(df$efN_id, effects)]

    df$yi <- b0 + b1 + df$u_sample + df$u_es +
      rnorm(nrow(df), 0, sqrt(df$vi))

    fit <- tryCatch(
      metafor::rma.mv(
        yi, vi,
        random = ~ 1 | participant_id / efN_id,
        data = df,
        method = "REML"
      ),
      error = function(e) NULL
    )

    if (is.null(fit)) {
      return(NA)
    }

    p <- coef(summary(fit))[1, "pval"]
    return(!is.na(p) && p < alpha)
  })


  # --- 4. Compute power ---
  good <- !is.na(rej)
  power_hat <- mean(rej[good])

  out <- list(
    power = power_hat,
    tau_sample = tau_sample,
    tau_es = tau_es
  )
  plan(sequential)
  return(out)
}


find_mdes_main <- function(df,
                           model,
                           target_power = 0.80,
                           b0 = 0,
                           mdes_min = NULL,
                           mdes_max = NULL,
                           tol = 0.01,
                           nsim = 200,
                           seed = 123,
                           verbose = TRUE,
                           save_prefix = NULL) {
  # magnitude of observed effect
  b1_hat <- abs(model$beta[[1]])
  sign_hat <- sign(model$beta[[1]])

  # default interval (wide, symmetric)
  if (is.null(mdes_min)) mdes_min <- max(0.001, b1_hat * 0.25)
  if (is.null(mdes_max)) mdes_max <- b1_hat * 2

  lo <- mdes_min
  hi <- mdes_max
  iter <- 0
  max_iter <- 50

  results_iter <- list()

  while ((hi - lo) > tol && iter < max_iter) {
    iter <- iter + 1
    mid <- (lo + hi) / 2
    b1_signed <- sign_hat * mid

    res_mid <- sim_power_main(
      df = df,
      model = model,
      b1 = b1_signed,
      b0 = b0,
      nsim = nsim,
      seed = seed,
      verbose = verbose
    )

    pow_mid <- res_mid$power

    results_iter[[iter]] <- tibble(
      mod   = "main",
      iter  = iter,
      mdes  = mid,
      power = pow_mid
    )

    if (verbose) {
      message(
        "Iter ", iter,
        "  | MDES=", round(mid, 4),
        "  | Power=", round(pow_mid, 3),
        "  | Width=", round(hi - lo, 4)
      )
    }

    # if (is.na(pow_mid)) break
    if (pow_mid >= 0.790 && pow_mid <= 0.809) {
      lo <- hi <- mid
      break
    }

    if (pow_mid >= target_power) hi <- mid else lo <- mid
  }

  # # final evaluation
  # final_b1 <- sign_hat * hi
  # final_res <- sim_power_main(
  #   df    = df,
  #   model = model,
  #   b1    = final_b1,
  #   b0    = b0,
  #   nsim  = nsim * 2,
  #   seed  = seed,
  #   verbose = verbose
  # )
  #
  # final <- tibble(
  #   mod   = "main",
  #   mdes  = hi,
  #   power = final_res$power
  # )
  #
  # list(final = final, path = bind_rows(results_iter))
  final <- tibble(
    mod   = "main",
    mdes  = hi,
    power = results_iter[[iter]]$power
  )
  return(list(final = final, path = bind_rows(results_iter)))
}


#### combining mdes results
# combining main ones
combine_main_mdes_results <- function(full, trimmed) {
  full_path <- full$path
  trimmed_path <- trimmed$path
  ## change value of mod col in _path dfs to full vs trimmed (respectively)
  full_path$mod <- "Full"
  trimmed_path$mod <- "Trimmed"
  ## combine path files
  all_paths <- rbind(full_path, trimmed_path)

  ## save mdes values
  full_mdes <- full$final$mdes
  trimmed_mdes <- trimmed$final$mdes
  # Repeat each mdes value to match the number of rows in its path
  mdes_expanded <- mapply(
    function(df, mdes) rep(mdes, nrow(df)),
    list(full_path, trimmed_path),
    c(full_mdes, trimmed_mdes),
    SIMPLIFY = FALSE
  )

  # Bind all paths together
  res <- dplyr::bind_rows(all_paths)

  # Add the expanded mdes column
  res$final_mdes <- unlist(mdes_expanded)

  # Ensure consistent factor ordering
  res$mod <- factor(res$mod, levels = unique(res$mod))

  res
}

combine_mdes_results <- function(...) {
  dots <- list(...)

  # Extract all path data frames
  all_paths <- lapply(dots, `[[`, "path")

  # Extract the mdes value from each "final"
  mdes_values <- sapply(dots, function(x) x$final$mdes)

  # Repeat each mdes value to match the number of rows in its path
  mdes_expanded <- mapply(
    function(df, mdes) rep(mdes, nrow(df)),
    all_paths,
    mdes_values,
    SIMPLIFY = FALSE
  )

  # Bind all paths together
  res <- dplyr::bind_rows(all_paths)

  # Add the expanded mdes column
  res$final_mdes <- unlist(mdes_expanded)

  # Ensure consistent factor ordering
  res$mod <- factor(res$mod, levels = unique(res$mod))

  res
}

### plot
plot_main_mdes_results <- function(meta,
                                   title = "",
                                   x_label = expression("Effect Size (Hedge's " * italic("g") * ")"),
                                   y_label = "Power") {
  # ---- DEFINE COLOURS FOR BOTH MODELS ----
  model_cols <- c(
    "Full"    = "#A6CEE3",
    "Trimmed" = "#1F78B4"
  )

  # ---- LABEL DATA ----
  mdes_labels <- meta %>%
    distinct(mod, final_mdes) %>%
    mutate(
      label_text = paste0(mod, ": ", round(final_mdes, 3)),
      y = case_when(
        mod == "Full" ~ 0.92,
        mod == "Trimmed" ~ 0.88,
        TRUE ~ 0.9
      )
    )

  p <- ggplot(meta, aes(mdes, power, colour = mod, group = mod)) +

    # shading
    geom_rect(
      xmin = -Inf, xmax = Inf, ymin = 0.8, ymax = 1.5,
      fill = "#E6F4EA", alpha = 0.05, inherit.aes = FALSE
    ) +
    geom_rect(
      xmin = 0.19, xmax = 0.21, ymin = -Inf, ymax = Inf,
      fill = "#FDEAEA", alpha = 0.05, inherit.aes = FALSE
    ) +
    geom_line() +
    geom_point() +

    # ---- USE ONE COLOUR SCALE FOR EVERYTHING ----
    scale_colour_manual(values = model_cols, name = "Model") +
    scale_fill_manual(values = model_cols, guide = "none") +
    geom_hline(yintercept = .80, linetype = "dashed", colour = "#33A02C") +

    # squares at y = 0.8
    geom_point(
      data = meta,
      aes(x = final_mdes, y = 0.8),
      shape = 22, fill = "white", size = 3, stroke = 1.2
    ) +

    # axis precision
    scale_y_continuous(
      limits = c(0.5, 1),
      breaks = seq(0, 1, 0.2)
    ) +
    scale_x_continuous(
      limits = c(
        min(c(meta$mdes, meta$final_mdes)) - 0.01,
        max(c(meta$mdes, meta$final_mdes)) + 0.01
      ),
      breaks = seq(0, 0.25, 0.01),
      minor_breaks = seq(0, 0.25, 0.005)
    ) +
    labs(title = title, y = y_label, x = x_label) +
    annotate("text",
      x = min(c(meta$mdes, meta$final_mdes)) - 0.01,
      y = 0.82,
      label = "Power >= 0.8",
      family = "Georgia", colour = "#33A02C", hjust = 0
    ) +


    # ---- VERTICAL LINES USING SAME COLOUR SCALE ----
    geom_vline(
      data = mdes_labels,
      aes(xintercept = final_mdes, colour = mod),
      linetype = "dotted",
      alpha = 0.6,
      inherit.aes = FALSE
    ) +
    theme_bw(base_family = "Georgia") +
    theme(
      text = element_text(family = "Georgia", size = 9),
      plot.title = element_blank(),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 9)
    )

  # ---- LABELS (ALSO USING SAME COLOUR SCALE) ----
  p +
    geom_label_repel(
      data = mdes_labels,
      aes(x = final_mdes, y = y, label = label_text, fill = mod),
      colour = "black",
      box.padding = 0.5,
      family = "Georgia",
      size = 3,
      inherit.aes = FALSE
    )
}

plot_mdes_results <- function(meta,
                              colour_palette = RColorBrewer::brewer.pal(12, "Paired"),
                              title = "Power Sensitivity Curves",
                              x_label = expression("Effect Size (Hedge's " * italic("g") * ")"),
                              y_label = "Power") {
  ggplot(meta, aes(mdes, power, color = mod, group = mod)) +
    geom_rect(
      xmin = -Inf, xmax = Inf, ymin = 0.8, ymax = 1.5,
      fill = "#E6F4EA", alpha = 0.05, inherit.aes = FALSE
    ) +
    geom_rect(
      xmin = 0.2, xmax = 0.35, ymin = -Inf, ymax = Inf,
      fill = "#FDEAEA", alpha = 0.05, inherit.aes = FALSE
    ) +
    geom_line() +
    geom_point() +
    scale_color_manual(values = colour_palette, name = "Model") +
    geom_hline(yintercept = .80, linetype = "dashed", color = "#33A02C") +
    geom_point(aes(x = mdes, y = 0.8), shape = 22, fill = "white", size = 3, stroke = 1.2) +
    scale_y_continuous(
      limits = c(min(meta$power), 1),
      breaks = seq(0, 1, 0.2)
    ) +
    scale_x_continuous(
      limits = c(min(meta$mdes), 0.3),
      breaks = seq(0, 0.3, 0.05),
      minor_breaks = seq(0.025, 0.325, 0.05)
    ) +
    labs(title = title, y = y_label, x = x_label) +
    annotate("text",
      x = 0.245, y = 0.82, label = "Power >= 0.8",
      family = "Georgia", color = "#33A02C", hjust = 0
    ) +
    geom_vline(xintercept = .2, lty = 2, color = "#FB9A99", alpha = 0.5) +
    annotate("text",
      x = 0.21, y = 0.45, label = "Small Effect",
      family = "Georgia", color = "#FB9A99", hjust = 0, angle = 270
    ) +
    theme_bw(base_family = "Georgia") +
    theme(
      text = element_text(family = "Georgia", size = 11),
      plot.title = element_text(hjust = 0.5, size = 11, face = "bold"),
      axis.title = element_text(size = 11),
      axis.text = element_text(size = 9)
    )
}

create_pwr_curve_for_all_mods <- function(meta) {
  mycolours <- RColorBrewer::brewer.pal(12, "Paired")
  maincolours <- RColorBrewer::brewer.pal(4, "Set2")
  meta <- meta %>%
    mutate(
      level = case_when(
        is.na(level) ~ mod, # copy mod into level for continuous moderators
        TRUE ~ level # keep existing values
      )
    )

  meta$level <- stringr::str_to_title(meta$level)
  # make some manual adjustments
  meta$level <- forcats::fct_recode(
    meta$level,
    "AI" = "Ai",
    "Means-to-End" = "Meane",
    "Side-Effect" = "Sidee",
    "Not Implied" = "Notimplied",
    "Not Harm" = "Notharm",
    "PMA" = "Pma",
    "PMC" = "Pmc",
    "RQ" = "Rq",
    "Responsibility" = "Responsible"
  )
  mdes_labels <- meta %>%
    distinct(mod, level, final_mdes) %>%
    mutate(
      label_text = paste0(level, " = ", round(final_mdes, 3)),

      # ---- COLOURS BY MODERATOR GROUP ----
      colour = case_when(
        mod == "harm" ~ "#E31A1C",
        mod == "in_action" ~ "#FF7F00",
        mod == "agent_intel" ~ "#FDBF6F",
        mod == "intent" ~ "#D1D100",
        mod == "aiType_a" ~ "#B2DF8A",
        mod == "aiType_b" ~ "#33A02C",
        mod == "dv_synonym" ~ "#A6CEE3",
        mod == "PMA" ~ "#1F78B4",
        mod == "PMC" ~ "#CAB2D6",
        mod == "responsible" ~ "#6A3D9A",
        mod == "RQ" ~ "#FB9A99",
        TRUE ~ "#000000"
      ),

      # ---- LINETYPES (customise as needed) ----
      linetype = case_when(
        TRUE ~ "dashed"
      )
    )

  p <- ggplot(meta, aes(mdes, power, colour = mod, group = mod)) +
    geom_rect(
      xmin = -Inf, xmax = Inf, ymin = 0.8, ymax = 1.5,
      fill = "#E6F4EA", alpha = 0.05, inherit.aes = FALSE
    ) +
    geom_rect(
      xmin = 0.16, xmax = 0.4, ymin = -Inf, ymax = Inf,
      fill = "#FDEAEA", alpha = 0.05, inherit.aes = FALSE
    ) +
    geom_rect(xmin = 0.4, xmax = 0.75, ymin = -Inf, ymax = Inf, fill = "#E9E2F0", alpha = 0.03, inherit.aes = FALSE) +
    geom_line(alpha = 0.1) +
    geom_point(alpha = 0.1) +
    scale_color_manual(
      values = mycolours,
      name = expression("Min. " * italic(g)),
      labels = c(
        "Moral Domain", "Decision Type", "Agent Intel.", "Intent", "2-category AI", "3-category AI",
        "DV Wording", "PMA", "PMC", "Responsibility", "Research Quality"
      )
    ) +
    geom_hline(yintercept = .80, linetype = "dashed", color = "#33A02C") +

    # squares at y = 0.8, one per level
    geom_point(
      data = mdes_labels,
      aes(x = final_mdes, y = 0.8, colour = mod),
      shape = 22, fill = "white", size = 3, stroke = 1.2,
      inherit.aes = FALSE
    ) +
    scale_y_continuous(limits = c(min(meta$power) - 0.1, 1), breaks = seq(0, 1, 0.2)) +
    scale_x_continuous(
      limits = c(min(meta$mdes) - 0.013, 0.605),
      breaks = seq(0, 1, 0.05),
      minor_breaks = seq(0, 1, 0.025)
    ) +
    labs(
      title = "",
      y = "Power",
      x = bquote("Effect Size (Hedge's " * italic("g") * ")")
    ) +
    # annotate("text", x = 0.583, y = 0.813, label = "Power >= 0.8",
    #        family = "Georgia", color = "#33A02C", hjust = 0, size=3) +
    geom_vline(xintercept = .4, lty = 16, color = "#6A3D9A", alpha = 0.5) +
    annotate("text",
      x = 0.605, y = 0.52, label = "Medium Effect",
      family = "Georgia", color = "#6A3D9A", hjust = 0.5, vjust = -2, angle = 270, size = 4
    ) +
    geom_vline(xintercept = .16, lty = 16, color = "#FB9A99", alpha = 0.5) +
    annotate("text",
      x = 0.39, y = 0.52, label = "Small Effect",
      family = "Georgia", color = "#FB9A99", hjust = 0.5, angle = 270, size = 4
    ) +
    theme_bw(base_family = "Georgia") +
    theme(
      text = element_text(family = "Georgia", size = 2),
      plot.title = element_blank(),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 9),
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 9)
    )
  p <- p +
    geom_vline(
      data = mdes_labels,
      aes(xintercept = final_mdes, colour = mod),
      linetype = "dotted",
      alpha = 0.5,
      inherit.aes = FALSE
    )
  p <- p +
    geom_label_repel(
      data = mdes_labels,
      aes(
        x = final_mdes,
        y = 0.8,
        label = label_text,
        fill = mod
      ),
      direction = c("y"),
      xlim = c(-0.5, 0.7),
      ylim = c(0.07, 1.15),
      alpha = 0.8,
      colour = "black",
      box.padding = 5,
      max.iter = 10000,
      max.time = 500,
      family = "Georgia",
      size = 2.5,
      inherit.aes = FALSE,
      seed = 123,
      max.overlaps = 50,
      force = 15,
      force_pull = 0
    )
  p <- p + scale_fill_manual(values = mycolours, guide = "none")


  font_add("Georgia",
    regular = "Georgia.ttf",
    bold = "Georgia Bold.ttf",
    italic = "Georgia Italic.ttf",
    bolditalic = "Georgia Bold Italic.ttf"
  )

  showtext_auto()
  fig_save_name <- paste0("figures/trimmed_mods_pwr_curve.pdf")
  pdf(fig_save_name, width = 11, height = 8) # A4 landscape in inches
  par(mar = c(2, 3, 3, 2)) # bottom, left, top, right margins
  par(oma = c(0, 6, 6, 5)) # outer margins
  par(family = "Georgia") # if using showtext

  print(p)

  dev.off()
}

######### for multi regs
find_mdes_level_for_multi_reg <- function(df,
                                          model,
                                          mod,
                                          lvl,
                                          target_power = 0.8,
                                          b0 = 0,
                                          mdes_min = NULL,
                                          mdes_max = NULL,
                                          tol = 0.01,
                                          power_tol = 0.02,
                                          nsim = 200,
                                          seed = 123,
                                          verbose = TRUE,
                                          save_prefix = NULL) {
  # use enriched summary table, not raw fit
  sum_tab <- model$model_without_intercept[["summaryTable"]]

  coef_name <- paste0(mod, ":", lvl)
  row_idx <- match(coef_name, sum_tab$Term)
  if (is.na(row_idx)) stop("Coefficient not found in summaryTable: ", coef_name)

  b1_raw <- sum_tab$estimate[row_idx]
  b1_hat <- abs(b1_raw)
  sign_hat <- sign(b1_raw)

  if (is.null(mdes_min)) mdes_min <- max(0.001, b1_hat * 0.25)
  if (is.null(mdes_max)) mdes_max <- max(b1_hat * 2, 0.5)

  lo <- mdes_min
  hi <- mdes_max
  iter <- 0
  max_iter <- 50

  results_iter <- list()

  while ((hi - lo) > tol && iter < max_iter) {
    iter <- iter + 1
    mid <- (lo + hi) / 2
    b1_signed <- sign_hat * mid

    res_mid <- sim_power_cat_level(
      df = df,
      model = model,
      mod = mod,
      lvl = lvl,
      b1 = b1_signed,
      b0 = b0,
      nsim = nsim,
      seed = seed,
      verbose = FALSE
    )

    pow_mid <- res_mid$power

    results_iter[[iter]] <- tibble::tibble(
      mod   = mod,
      level = lvl,
      iter  = iter,
      mdes  = mid,
      power = pow_mid
    )

    if (verbose) {
      message(
        "Bisection iteration ", iter,
        ": tested effect size = ", round(mid, 4),
        ". Estimated power = ", round(pow_mid, 3),
        " Interval width = ", round(hi - lo, 4), "."
      )
    }

    if (!is.na(pow_mid) && pow_mid >= 0.790 && pow_mid <= 0.809) {
      lo <- hi <- mid
      break
    }

    if (!is.na(pow_mid) && pow_mid >= target_power) hi <- mid else lo <- mid
  }

  list(
    mdes   = (lo + hi) / 2,
    iters  = dplyr::bind_rows(results_iter)
  )
}

find_mdes_cont_multi_mods <- function(df,
                                      model,
                                      mod,
                                      target_power = 0.80,
                                      b0 = 0,
                                      mdes_min = NULL,
                                      mdes_max = NULL,
                                      tol = 0.01,
                                      nsim = 200,
                                      seed = 123,
                                      verbose = TRUE,
                                      save_prefix = NULL) {
  model <- model$model_without_intercept$fit
  extract_val <- paste0("as.numeric(", mod, ")")


  # slope is row 2
  b1_hat <- abs(model$beta[[extract_val, 1]])
  sign_hat <- sign(model$beta[[extract_val, 1]])

  # default interval (same logic as main)
  if (is.null(mdes_min)) mdes_min <- max(0.001, b1_hat * 0.25)
  if (is.null(mdes_max)) mdes_max <- b1_hat * 2

  lo <- mdes_min
  hi <- mdes_max
  iter <- 0
  max_iter <- 50

  results_iter <- list()
  while ((hi - lo) > tol && iter < max_iter) {
    iter <- iter + 1
    mid <- (lo + hi) / 2
    b1_signed <- sign_hat * mid

    res_mid <- sim_power_cont(
      df = df,
      model = model,
      mod = mod,
      b1 = b1_signed,
      b0 = b0,
      nsim = nsim,
      seed = seed,
      verbose = verbose
    )

    pow_mid <- res_mid$power

    results_iter[[iter]] <- tibble(
      mod   = mod,
      iter  = iter,
      mdes  = mid,
      power = pow_mid
    )

    if (verbose) {
      message(
        "Iter ", iter,
        "  | MDES=", round(mid, 4),
        "  | Power=", round(pow_mid, 3),
        "  | Width=", round(hi - lo, 4)
      )
    }

    # if (is.na(pow_mid)) break

    if (pow_mid >= 0.790 && pow_mid <= 0.809) {
      lo <- hi <- mid
      break
    }

    if (pow_mid >= target_power) hi <- mid else lo <- mid
  }

  # # final evaluation
  # final_b1 <- sign_hat * hi
  # final_res <- sim_power_cont(
  #   df    = df,
  #   model = model,
  #   mod   = mod,
  #   b1    = final_b1,
  #   b0    = b0,
  #   nsim  = nsim * 2,
  #   seed  = seed,
  #   verbose = verbose
  # )
  #
  # final <- tibble(
  #   mod   = mod,
  #   mdes  = hi,
  #   power = final_res$power
  # )
  #
  # list(final = final, path = bind_rows(results_iter))
  final <- tibble(
    mod   = mod,
    mdes  = hi,
    power = results_iter[[iter]]$power
  )
  return(list(final = final, path = bind_rows(results_iter)))
}
