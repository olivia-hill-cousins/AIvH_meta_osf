calc_main_toast <- function(extract_val, model, mdes_res, model_name) {
  mdes_val <- mdes_res[["final"]][["mdes"]]
  # Run TOST using the specified bound
  # res <- TOSTmeta(
  #   model[["beta"]][[extract_val]],
  #   model[["se"]][[extract_val]],
  #   low_eqbound_d = -mdes_val,
  #   high_eqbound_d = mdes_val,
  #   alpha = 0.05
  # )
  
  res <- equivalence_test(model, 
                          rule = "classic", 
                          range = c(-mdes_val,mdes_val))
  res <- as.data.frame(res)
  # Build tibble
  toast_tib <- tibble(
    model_name = model_name,
    estimate = model$beta[[1]],
    ci_low = res$CI_low,
    ci_high = res$CI_high,
    lower_bound = res$ROPE_low,
    upper_bound = res$ROPE_high,
    p_tost = res$p,
    p_nhst = model[["pval"]][[extract_val]],
    NHST_significant = p_nhst < 0.05,
    TOST_equivalent = (p_tost < 0.05),
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
  # run Toast
  res <- equivalence_test(fit, 
                          rule = "classic", 
                          range = c(-mdes_val,mdes_val))
  res <- as.data.frame(res)
  res <- res %>% 
    dplyr::filter(Parameter == coef_name)
  # Build tibble (unchanged)
  toast_tib <- tibble(
    model_name = model_name,
    estimate = beta_val,
    ci_low = res$CI_low,
    ci_high = res$CI_high,
    lower_bound = res$ROPE_low,
    upper_bound = res$ROPE_high,
    p_tost = res$p,
    p_nhst = p_val,
    NHST_significant = p_nhst < 0.05,
    TOST_equivalent = res$p < 0.05,
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


calc_toast_cont <- function(df,mod_col,model, mdes_res, model_name) {
  # Dynamic extraction by level name (replaces [[extract_val]])
  # Extract from rma.mv (NOT lme4!)
  formula_with_intercept <- as.formula(paste("~ as.numeric(",mod_col,")"))
  fit <- metafor::rma.mv(
    yi, vi,
    random = ~ 1 | participant_id / efN_id,
    mods = formula_with_intercept,
    test = "t", dfs = "contain", method = "REML",
    data = df
  )
  # Build coef name dynamically
  # level_name <- paste0(factor_var, level_name)
  beta_val <- fit[["beta"]][[2]]
  se_val <- fit[["se"]][[2]]
  p_val <- fit[["pval"]][[2]]


  mdes_val <- mdes_res[["final"]][["mdes"]]
  coef_name <- paste0("as.numeric(", mod_col, ")")
  # Run TOST
  res <- equivalence_test(fit, 
                          rule = "classic", 
                          range = c(-mdes_val,mdes_val))
  res <- as.data.frame(res)
  res <- res %>% 
    filter(Parameter == coef_name)

  # Build tibble (unchanged)
  # Build tibble
  toast_tib <- tibble(
    model_name = model_name,
    estimate = beta_val,
    ci_low = res$CI_low,
    ci_high = res$CI_high,
    lower_bound = res$ROPE_low,
    upper_bound = res$ROPE_high,
    p_tost = res$p,
    p_nhst = p_val,
    NHST_significant = p_nhst < 0.05,
    TOST_equivalent = (p_tost < 0.05),
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
# calc_toast_for_multi_reg <- function(level_name, model, mdes_res, model_name, factor_var = "aiType") {
#   # Use enriched summary table (contains ALL levels)
#   sum_tab <- model$model_without_intercept[["fit"]]
# 
#   # Build coefficient name
#   coef_name <- paste0(factor_var, ":", level_name)
# 
#   # Locate row
#   row_idx <- match(coef_name, sum_tab$Term)
#   if (is.na(row_idx)) stop("Level not found in summaryTable: ", coef_name)
# 
#   # Extract values
#   beta_val <- sum_tab$estimate[row_idx]
#   se_val <- sum_tab$se[row_idx]
#   p_val <- sum_tab$pval[row_idx]
# 
#   # MDES value
#   mdes_val <- mdes_res[["mdes"]]
#   
#   
#   
#   res <- equivalence_test(sum_tab, 
#                           rule = "classic", 
#                           range = c(-mdes_val,mdes_val))
#   res <- as.data.frame(res)
#   res <- res %>% 
#     dplyr::filter(Parameter == coef_name)
#   # Build tibble (unchanged)
#   toast_tib <- tibble(
#     model_name = model_name,
#     estimate = beta_val,
#     ci_low = res$CI_low,
#     ci_high = res$CI_high,
#     lower_bound = res$ROPE_low,
#     upper_bound = res$ROPE_high,
#     p_tost = res$p,
#     p_nhst = p_val,
#     NHST_significant = p_nhst < 0.05,
#     TOST_equivalent = res$p < 0.05,
#     scenario_code = dplyr::case_when(
#       TOST_equivalent & !NHST_significant ~ "A",
#       !TOST_equivalent & NHST_significant ~ "B",
#       TOST_equivalent & NHST_significant ~ "C",
#       TRUE ~ "D"
#     ),
#     scenario_label = dplyr::case_when(
#       scenario_code == "A" ~ "Equivalent, not different from zero",
#       scenario_code == "B" ~ "Different from zero, not equivalent",
#       scenario_code == "C" ~ "Different from zero and equivalent",
#       scenario_code == "D" ~ "Undetermined (neither different nor equivalent)"
#     )
#   )
# 
#   # Colour message
#   colour_fun <- switch(toast_tib$scenario_code,
#     "A" = crayon::bgGreen$black$bold,
#     "B" = crayon::bgRed$white$bold,
#     "C" = crayon::bgBlue$white$bold,
#     "D" = crayon::bgYellow$black$bold
#   )
# 
#   msg <- paste0(
#     "The ", model_name, " analysis for ", level_name,
#     " was found to be: ", toast_tib$scenario_label
#   )
# 
#   message(colour_fun(paste("📌 ", msg)))
# 
#   list(TOST_res = res, tibble = toast_tib)
# }

calc_toast_for_multi_reg <- function(df, mod, mdes_tost_row, mod_select,mod_col, data_type, data_type_select) {
  
  L <- mod
  # MDES value
  mdes_val <- mdes_tost_row[["mdes"]]
  # --- Moderator cleaning (unchanged) ---
  for (i in seq_along(mod_col)) {
    col <- rlang::sym(mod_col[i])
    
    if (data_type[i] == "cat") {
      df[[col]] <- as.factor(df[[col]])
      df[[col]] <- droplevels(df[[col]])
      df[[col]] <- trimws(df[[col]])
      df[[col]][df[[col]] %in% c("", "NA", "N/A", ".", "na")] <- NA
      df <- df |> dplyr::filter(!is.na(.data[[col]]))
      df[[col]] <- factor(df[[col]])
      df[[col]] <- droplevels(df[[col]])
      
      if (nlevels(df[[col]]) < 2) {
        stop("Categorical moderator '", col, "' has only one level.")
      }
    } else if (data_type[i] == "cont") {
      converted <- suppressWarnings(as.numeric(df[[col]]))
      df[[col]] <- converted
      df <- df |> dplyr::filter(!is.na(.data[[col]]))
      df[[col]][df[[col]] %in% c("", "NA", "N/A", ".", "na")] <- NA
    }
  }
  
  
  # --- Build formulas ---
  terms <- purrr::map2_chr(mod_col, data_type, ~ {
    if (.y == "cat") {
      paste0("as.factor(", .x, ")")
    } else {
      paste0("as.numeric(", .x, ")")
    }
  })
  
  formula_without_intercept <- as.formula(paste("~ -1 +", paste(terms, collapse = " + ")))
  # Ensure the target level L is NOT the reference level
  levs_current <- levels(df[[mod_select]])
  
  if (levs_current[1] == L) {
    # Move L to the end so it is not the reference
    new_order <- c(levs_current[-1], L)
    df[[mod_select]] <- factor(df[[mod_select]], levels = new_order)
  }
  
  
    fit <- metafor::rma.mv(
      yi, vi,
      random = ~ 1 | participant_id / efN_id,
      mods = formula_without_intercept,
      test = "t", dfs = "contain", method = "REML",
      data = df
    )
    
    tost <- equivalence_test(
      fit,
      rule = "classic",
      range = c(-mdes_val, mdes_val)
    ) |> 
      as.data.frame()
    
    term <- purrr::map2_chr(mod_select, data_type_select, ~ {
      if (.y == "cat") {
        paste0("as.factor(", .x, ")")
      } else {
        paste0("as.numeric(", .x, ")")
      }
    })
    
    # Extract intercept row
    parameter <- paste0(term,L)
    idx <- which(rownames(fit$beta) == parameter)
    tost_row <- tost %>% 
      dplyr::filter(Parameter == parameter)
    beta_val <- fit$beta[parameter, 1]
    p_val <- fit$pval[idx]
    out <- tibble(
      Level = L,
      estimate = beta_val,
      ci_low = tost_row$CI_low,
      ci_high = tost_row$CI_high,
      lower_bound = tost_row$ROPE_low,
      upper_bound = tost_row$ROPE_high,
      p_tost = tost_row$p,
      p_nhst = p_val,
      NHST_significant = p_nhst < 0.05,
      TOST_equivalent = tost_row$p < 0.05,
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
    colour_fun <- switch(out$scenario_code,
                         "A" = crayon::bgGreen$black$bold,
                         "B" = crayon::bgRed$white$bold,
                         "C" = crayon::bgBlue$white$bold,
                         "D" = crayon::bgYellow$black$bold
    )
    
    msg <- paste0(
      "The ", mod_col, " multi-reg analysis for ", mod_select, " mod level: ", mod,
      " was found to be: ", out$scenario_label
    )
    
    message(colour_fun(paste("📌 ", msg)))
    
  
  
  return(out)
}



######### Main Toast w. fixed hedge's g val (as per pre-reg)
calc_main_toast_fixed_bounds <- function(extract_val, model, mdes_res, model_name) {
  mdes_val <- mdes_res
  # Run TOST using the specified bound
  # res <- TOSTmeta(
  #   model[["beta"]][[extract_val]],
  #   model[["se"]][[extract_val]],
  #   low_eqbound_d = -mdes_val,
  #   high_eqbound_d = mdes_val,
  #   alpha = 0.05
  # )
  res <- equivalence_test(model, 
                          rule = "classic", 
                          range = c(-mdes_val,mdes_val))
  res <- as.data.frame(res)
  # Build tibble
  toast_tib <- tibble(
    model_name = model_name,
    estimate = model$beta[[1]],
    ci_low = res$CI_low,
    ci_high = res$CI_high,
    lower_bound = res$ROPE_low,
    upper_bound = res$ROPE_high,
    p_tost = res$p,
    p_nhst = model[["pval"]][[extract_val]],
    NHST_significant = p_nhst < 0.05,
    TOST_equivalent = (p_tost < 0.05),
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
