calc_main_toast <- function(extract_val, model, mdes_res, model_name) {
  mdes_val <- mdes_res[["final"]][["mdes"]]
  # Run TOST using the specified bound
  res <- TOSTmeta(
    model[["beta"]][[extract_val]],
    model[["se"]][[extract_val]],
    low_eqbound_d = -mdes_val,
    high_eqbound_d = mdes_val,
    alpha = 0.05
  )

  # Build tibble
  toast_tib <- tibble(
    model_name = model_name,
    estimate = res$ES,
    ci_low = res$LL_CI_TOST,
    ci_high = res$UL_CI_TOST,
    lower_bound = res$low_eqbound_d,
    upper_bound = res$high_eqbound_d,
    p_tost1 = res$TOST_p1,
    p_tost2 = res$TOST_p2,
    p_nhst = model[["pval"]][[extract_val]],
    NHST_significant = p_nhst < 0.05,
    TOST_equivalent = (res$TOST_p1 < 0.05 & res$TOST_p2 < 0.05),
    scenario_code = dplyr::case_when(
      TOST_equivalent & !NHST_significant ~ "A",
      !TOST_equivalent & NHST_significant ~ "B",
      TOST_equivalent & NHST_significant ~ "C",
      TRUE ~ "D"
    ),
    scenario_label = dplyr::case_when(
      scenario_code == "A" ~ "Equivalent, not different from zero",
      scenario_code == "B" ~ "Different from zero, not equivalent",
      scenario_code == "C" ~ "Different from zero and equivalent",
      scenario_code == "D" ~ "Undetermined (neither different nor equivalent)"
    )
  )

  library(crayon)

  colour_fun <- switch(toast_tib$scenario_code,
    "A" = bgGreen$black$bold,
    "B" = bgRed$white$bold,
    "C" = bgBlue$white$bold,
    "D" = bgYellow$black$bold
  )

  msg <- paste0(
    "The ", model_name, " model was found to be: ", toast_tib$scenario_label
  )

  message(colour_fun(paste("📌 ", msg)))


  # Return both objects in a list
  return(list(TOST_res = res, tibble = toast_tib))
}

# calc_toast <- function(extract_val, model, mdes_res, model_name) {
#   mdes_val <- as.numeric(mdes_res$mdes)
#   # Run TOST using the specified bound
#   res <- TOSTmeta(
#     model$model_without_intercept[["fit"]][["beta"]][[extract_val]],
#     model$model_without_intercept[["fit"]][["se"]][[extract_val]],
#     low_eqbound_d = -mdes_val,
#     high_eqbound_d = mdes_val,
#     alpha = 0.05
#   )
#
#   # Build tibble
#   toast_tib <- tibble(
#     model_name = model_name,
#     estimate = res$ES,
#     ci_low = res$LL_CI_ZTEST,
#     ci_high = res$UL_CI_ZTEST,
#     lower_bound = res$low_eqbound_d,
#     upper_bound = res$high_eqbound_d,
#     p_tost1 = res$TOST_p1,
#     p_tost2 = res$TOST_p2,
#     p_nhst = res$NHST_p,
#     NHST_significant = ifelse(res$NHST_p < 0.05, "Yes", "No"),
#     TOST_direction = ifelse(
#       res$ES < res$low_eqbound_d, "Smaller than bound",
#       ifelse(res$ES > res$high_eqbound_d, "Larger than bound", "Within bounds")
#     )
#   )
#
#   # Return both objects in a list
#   return(list(TOST_res = res, tibble = toast_tib))
# }
#
calc_toast <- function(level_name, model, mdes_res, model_name, factor_var = "aiType") {
  # Dynamic extraction by level name (replaces [[extract_val]])
  # Extract from rma.mv (NOT lme4!)
  fit <- model$model_without_intercept[["fit"]]
  # Build coef name dynamically
  # level_name <- paste0(factor_var, level_name)
  betas <- fit[["beta"]]
  ses <- fit[["se"]]
  pvals <- fit[["pval"]]

  # Get EXACT rownames from your output
  beta_names <- rownames(betas)

  # Find the full name containing your level (e.g. "factor(aiType_b)AI")
  coef_name <- paste0("factor(", factor_var, ")", level_name)
  coef_idx <- which(beta_names == coef_name)
  if (length(coef_idx) == 0) stop(paste("Level", level_name, "not found"))

  beta_val <- betas[coef_idx]
  se_val <- ses[coef_idx]
  p_val <- pvals[coef_idx]

  mdes_val <- mdes_res[["final"]][["mdes"]]
  # Run TOST
  res <- TOSTmeta(
    beta_val,
    se_val,
    low_eqbound_d = -mdes_val,
    high_eqbound_d = mdes_val,
    alpha = 0.05
  )

  # Build tibble (unchanged)
  toast_tib <- tibble(
    model_name = model_name,
    estimate = res$ES,
    ci_low = res$LL_CI_TOST,
    ci_high = res$UL_CI_TOST,
    lower_bound = res$low_eqbound_d,
    upper_bound = res$high_eqbound_d,
    p_tost1 = res$TOST_p1,
    p_tost2 = res$TOST_p2,
    p_nhst = p_val,
    NHST_significant = p_nhst < 0.05,
    TOST_equivalent = (res$TOST_p1 < 0.05 & res$TOST_p2 < 0.05),
    scenario_code = dplyr::case_when(
      TOST_equivalent & !NHST_significant ~ "A",
      !TOST_equivalent & NHST_significant ~ "B",
      TOST_equivalent & NHST_significant ~ "C",
      TRUE ~ "D"
    ),
    scenario_label = dplyr::case_when(
      scenario_code == "A" ~ "Equivalent, not different from zero",
      scenario_code == "B" ~ "Different from zero, not equivalent",
      scenario_code == "C" ~ "Different from zero and equivalent",
      scenario_code == "D" ~ "Undetermined (neither different nor equivalent)"
    )
  )

  library(crayon)

  colour_fun <- switch(toast_tib$scenario_code,
    "A" = bgGreen$black$bold,
    "B" = bgRed$white$bold,
    "C" = bgBlue$white$bold,
    "D" = bgYellow$black$bold
  )

  msg <- paste0(
    "The ", model_name, " analysis for ", level_name,
    " was found to be: ", toast_tib$scenario_label
  )

  message(colour_fun(paste("📌 ", msg)))


  return(list(TOST_res = res, tibble = toast_tib))
}


calc_toast_cont <- function(model, mdes_res, model_name) {
  # Dynamic extraction by level name (replaces [[extract_val]])
  # Extract from rma.mv (NOT lme4!)
  fit <- model$model_with_intercept[["fit"]]
  # Build coef name dynamically
  # level_name <- paste0(factor_var, level_name)
  beta_val <- fit[["beta"]][[2]]
  se_val <- fit[["se"]][[2]]
  p_val <- fit[["pval"]][[2]]


  mdes_val <- mdes_res[["final"]][["mdes"]]
  # Run TOST
  res <- TOSTmeta(
    beta_val,
    se_val,
    low_eqbound_d = -mdes_val,
    high_eqbound_d = mdes_val,
    alpha = 0.05
  )

  # Build tibble (unchanged)
  toast_tib <- tibble(
    model_name = model_name,
    estimate = res$ES,
    ci_low = res$LL_CI_TOST,
    ci_high = res$UL_CI_TOST,
    lower_bound = res$low_eqbound_d,
    upper_bound = res$high_eqbound_d,
    p_tost1 = res$TOST_p1,
    p_tost2 = res$TOST_p2,
    p_nhst = p_val,
    NHST_significant = p_nhst < 0.05,
    TOST_equivalent = (res$TOST_p1 < 0.05 & res$TOST_p2 < 0.05),
    scenario_code = dplyr::case_when(
      TOST_equivalent & !NHST_significant ~ "A",
      !TOST_equivalent & NHST_significant ~ "B",
      TOST_equivalent & NHST_significant ~ "C",
      TRUE ~ "D"
    ),
    scenario_label = dplyr::case_when(
      scenario_code == "A" ~ "Equivalent, not different from zero",
      scenario_code == "B" ~ "Different from zero, not equivalent",
      scenario_code == "C" ~ "Different from zero and equivalent",
      scenario_code == "D" ~ "Undetermined (neither different nor equivalent)"
    )
  )

  library(crayon)

  colour_fun <- switch(toast_tib$scenario_code,
    "A" = bgGreen$black$bold,
    "B" = bgRed$white$bold,
    "C" = bgBlue$white$bold,
    "D" = bgYellow$black$bold
  )

  msg <- paste0(
    "The ", model_name, " analysis",
    " was found to be: ", toast_tib$scenario_label
  )

  message(colour_fun(paste("📌 ", msg)))


  return(list(TOST_res = res, tibble = toast_tib))
}


####### for multi reg
calc_toast_for_multi_reg <- function(level_name, model, mdes_res, model_name, factor_var = "aiType") {
  # Use enriched summary table (contains ALL levels)
  sum_tab <- model$model_without_intercept[["summaryTable"]]

  # Build coefficient name
  coef_name <- paste0(factor_var, ":", level_name)

  # Locate row
  row_idx <- match(coef_name, sum_tab$Term)
  if (is.na(row_idx)) stop("Level not found in summaryTable: ", coef_name)

  # Extract values
  beta_val <- sum_tab$estimate[row_idx]
  se_val <- sum_tab$se[row_idx]
  p_val <- sum_tab$pval[row_idx]

  # MDES value
  mdes_val <- mdes_res[["mdes"]]

  # Run TOST
  res <- TOSTmeta(
    beta_val,
    se_val,
    low_eqbound_d = -mdes_val,
    high_eqbound_d = mdes_val,
    alpha = 0.05
  )

  # Build tibble
  toast_tib <- tibble::tibble(
    model_name = model_name,
    estimate = res$ES,
    ci_low = res$LL_CI_TOST,
    ci_high = res$UL_CI_TOST,
    lower_bound = res$low_eqbound_d,
    upper_bound = res$high_eqbound_d,
    p_tost1 = res$TOST_p1,
    p_tost2 = res$TOST_p2,
    p_nhst = p_val,
    NHST_significant = p_nhst < 0.05,
    TOST_equivalent = (res$TOST_p1 < 0.05 & res$TOST_p2 < 0.05),
    scenario_code = dplyr::case_when(
      TOST_equivalent & !NHST_significant ~ "A",
      !TOST_equivalent & NHST_significant ~ "B",
      TOST_equivalent & NHST_significant ~ "C",
      TRUE ~ "D"
    ),
    scenario_label = dplyr::case_when(
      scenario_code == "A" ~ "Equivalent, not different from zero",
      scenario_code == "B" ~ "Different from zero, not equivalent",
      scenario_code == "C" ~ "Different from zero and equivalent",
      scenario_code == "D" ~ "Undetermined (neither different nor equivalent)"
    )
  )

  # Colour message
  colour_fun <- switch(toast_tib$scenario_code,
    "A" = crayon::bgGreen$black$bold,
    "B" = crayon::bgRed$white$bold,
    "C" = crayon::bgBlue$white$bold,
    "D" = crayon::bgYellow$black$bold
  )

  msg <- paste0(
    "The ", model_name, " analysis for ", level_name,
    " was found to be: ", toast_tib$scenario_label
  )

  message(colour_fun(paste("📌 ", msg)))

  list(TOST_res = res, tibble = toast_tib)
}
