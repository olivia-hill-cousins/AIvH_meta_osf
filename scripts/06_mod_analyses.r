run_meta_analysis <- function(df, mod_col, data_type) {
  # mod_col: vector of moderator names, e.g. c("age_mean", "harm")
  # data_type: vector of same length, e.g. c("cont", "cat")

  # Ensure equal lengths
  if (length(mod_col) != length(data_type)) {
    stop("mod_col and data_type must have the same length.")
  }

  # Convert moderator columns appropriately
  for (i in seq_along(mod_col)) {
    col <- rlang::sym(mod_col[i])

    if (data_type[i] == "cat") {
      df[[col]] <- as.factor(df[[col]])
      df[[col]] <- droplevels(df[[col]])

      df[[col]] <- trimws(df[[col]]) # remove stray spaces
      df[[col]][df[[col]] %in% c("", "NA", "N/A", ".", "na")] <- NA

      df <- df |> filter(!is.na(.data[[col]])) # drop NA rows

      df[[col]] <- factor(df[[col]]) # recreate factor cleanly
      df[[col]] <- droplevels(df[[col]]) # drop unused levels


      if (all(is.na(df[[col]]))) {
        stop("Categorical moderator '", col, "' contains only NA values.")
      }

      if (nlevels(df[[col]]) < 2) {
        stop(
          "Categorical moderator '", col, "' has only one level after cleaning: '",
          levels(df[[col]])[1], "'."
        )
      }

      if (any(is.na(df[[col]]))) {
        message("⚠️ Dropping rows with NA in categorical moderator '", col, "'")
        df <- df |> dplyr::filter(!is.na(.data[[col]]))
      }
    } else if (data_type[i] == "cont") {
      converted <- suppressWarnings(as.numeric(df[[col]]))
      bad_idx <- which(is.na(converted) & !is.na(df[[col]]))

      if (length(bad_idx) > 0) {
        bad_vals <- unique(df[[col]][bad_idx])
        message(
          "⚠️ Dropping non-numeric values in continuous moderator '", col, "': ",
          paste(bad_vals, collapse = ", ")
        )
      }

      df[[col]] <- converted
      df <- df |> dplyr::filter(!is.na(.data[[col]]))
    } else {
      stop("data_type must be 'cat' or 'cont'.")
    }
  }


  # Build moderator terms
  terms <- purrr::map2_chr(mod_col, data_type, ~ {
    if (.y == "cat") paste0("factor(", .x, ")") else .x
  })

  # Build formulas
  formula_with_intercept <- as.formula(paste("~", paste(terms, collapse = " + ")))
  formula_without_intercept <- as.formula(paste("~ -1 +", paste(terms, collapse = " + ")))

  # --- AFTER moderator conversion loop ---

  # Columns to check for NA
  all_vars <- c("yi", "vi", mod_col)

  # Report how many rows will be dropped
  na_rows <- sum(!complete.cases(df[, all_vars]))

  if (na_rows > 0) {
    message("⚠️ Dropping ", na_rows, " rows with NA in model variables")
  }

  # Drop all NAs at once
  df <- df[complete.cases(df[, all_vars]), ]

  # Fit models
  mod_with_intercept <- metafor::rma.mv(
    yi, vi,
    random = ~ 1 | participant_id / efN_id,
    mods = formula_with_intercept,
    test = "t", dfs = "contain", method = "REML",
    data = df
  )

  mod_without_intercept <- metafor::rma.mv(
    yi, vi,
    random = ~ 1 | participant_id / efN_id,
    mods = formula_without_intercept,
    test = "t", dfs = "contain", method = "REML",
    data = df
  )

  # Summaries
  summary_with_intercept <- summary(mod_with_intercept)
  summary_without_intercept <- summary(mod_without_intercept)

  # Build participant summary for each moderator
  participants_summary <- purrr::map2_df(mod_col, data_type, function(mc, dt) {
    sym <- rlang::sym(mc)

    df %>%
      dplyr::group_by(!!sym) %>%
      dplyr::summarise(
        nexp     = sum(aiN, na.rm = TRUE),
        ctrl     = sum(humanN, na.rm = TRUE),
        kcomp    = dplyr::n(),
        kstudies = dplyr::n_distinct(participant_id),
        .groups  = "drop"
      ) %>%
      dplyr::mutate(
        Term = if (dt == "cat") paste0("factor(", mc, ")", !!sym) else mc
      )
  })

  # Join participant summary to coef table
  summary_table_without_intercept <- as.data.frame(coef(summary_without_intercept))
  summary_table_without_intercept$Term <- rownames(summary_table_without_intercept)

  summary_table_without_intercept <- summary_table_without_intercept %>%
    dplyr::left_join(participants_summary, by = "Term") %>%
    dplyr::select(Term, nexp, ctrl, kcomp, kstudies, everything())

  # Return results
  list(
    model_with_intercept = list(
      fit = mod_with_intercept,
      summary = summary_with_intercept,
      summaryTable = coef(summary_with_intercept)
    ),
    model_without_intercept = list(
      fit = mod_without_intercept,
      summary = summary_without_intercept,
      summaryTable = summary_table_without_intercept
    )
  )
}

###############################################
# Z-SCORING RQ
###############################################

make_cont_mods_z_score <- function(df, mod) {
  df[[mod]] <- as.numeric(df[[mod]])
  df[[mod]] <- scale(df[[mod]])[, 1]
  df
}

#### for RQ (distribution)
create_distribution_plot_for_rq <- function(trimmed_df, RQ) {
  pink_red <- "#FB9A99"
  font_add("Georgia",
           regular = "Georgia.ttf",
           bold = "Georgia Bold.ttf",
           italic = "Georgia Italic.ttf",
           bolditalic = "Georgia Bold Italic.ttf"
  )
  
  showtext_auto()
  dis <- ggplot(trimmed_df, aes(x = RQ)) +
    geom_histogram(binwidth = 5, colour = "#33A02C", fill = "#B2DF8A") +
    geom_rug(alpha = 0.3) +
    geom_vline(aes(xintercept = mean(RQ)), colour = pink_red, linetype = "dashed") +
    labs(
      x = "Raw Research Quality Score",
      y = "Count",
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

  fig_save_name <- "figures/rq_distribution_plot.png"

  png(fig_save_name, width = 2280, height = 2280, res = 300) # A4 landscape in inches (height reduced from 2480 to leave room for note)
  par(
    par(mar = c(4, 4, 5, 2)),
    par(oma = c(4, 0, 4, 0)),
    par(family = "Georgia")
  )

  # if using showtext

  print(dis)

  dev.off()

  return(fig_save_name)
}

###############################################
# ORCHARD PLOTS for Mods
###############################################
####### orchard plots for cont mods
make_orchard_plot_for_cont <- function(df, model, mod, name) {
  model <- model$model_with_intercept$fit
  df[[mod]] <- as.numeric(df[[mod]])
  orch <- orchaRd::bubble_plot(model,
                               mod = mod, group = "participant_id", xlab = mod,
                               ylab = "Effect Size (AI vs Human Moral Judgements)", legend.pos = "top.left"
  ) +
    geom_point(data = df, aes(x = .data[[mod]], y = yi))
  
  fig_save_name <- paste0("figures/", name, ".png")
  
  png(fig_save_name, width = 3508, height = 2280, res = 300) # A4 landscape in inches (height reduced from 2480 to leave room for note)
  par(
    par(mar = c(4, 4, 5, 2)),
    par(oma = c(4, 0, 4, 0)),
    par(family = "Georgia")
  )
  
  # if using showtext
  
  print(orch)
  
  dev.off()
  
  return(fig_save_name)
}

######## orchard plots for cat mods
create_orchard_plot_for_cat_mods <- function(model, mod, name) {
  model <- model$model_with_intercept$fit
  f <- orchaRd::mod_results(model, mod = mod, group = "participant_id")

  p3 <- orchaRd::orchard_plot(model,
    mod = mod, group = "participant_id", xlab = expression("Hedge's " * italic(g)),
    transfm = "none"
  )

  fig_save_name <- paste0("figures/", name, ".png")

  png(fig_save_name, width = 3508, height = 2280, res = 300) # A4 landscape in inches (height reduced from 2480 to leave room for note)
  par(
    par(mar = c(4, 4, 5, 2)),
    par(oma = c(4, 0, 4, 0)),
    par(family = "Georgia")
  )

  # if using showtext

  print(p3)

  dev.off()
}

###############################################
# Multiple Mods
###############################################
extract_all_levels_no_intercept <- function(df, mod_col, data_type, formula_without_intercept) {
  rows <- list()
  
  for (i in seq_along(mod_col)) {
    col <- mod_col[i]
    
    # Continuous moderators: only one coefficient
    if (data_type[i] == "cont") {
      fit <- metafor::rma.mv(
        yi, vi,
        random = ~ 1 | participant_id / efN_id,
        mods = formula_without_intercept,
        test = "t", dfs = "contain", method = "REML",
        data = df
      )
      
      coef_tab <- coef(summary(fit))
      
      if (col %in% rownames(coef_tab)) {
        row <- coef_tab[col, , drop = FALSE]
        row$Term <- col
        rows[[length(rows) + 1]] <- row
      }
      
      next
    }
    
    
    # Categorical moderators
    df[[col]] <- as.factor(df[[col]])
    levs <- levels(df[[col]])
    
    for (L in levs) {
      df2 <- df
      df2[[col]] <- stats::relevel(df2[[col]], ref = L)
      
      fit <- metafor::rma.mv(
        yi, vi,
        random = ~ 1 | participant_id / efN_id,
        mods = formula_without_intercept,
        test = "t", dfs = "contain", method = "REML",
        data = df2
      )
      
      coef_tab <- coef(summary(fit))
      
      # 🔥 Take ONLY the first matching coefficient
      match_rows <- grep(paste0("^as\\.factor\\(", col, "\\)"), rownames(coef_tab), value = TRUE)
      
      if (length(match_rows) > 0) {
        target <- match_rows[1] # ← CRUCIAL: only one row per fit
        
        row <- coef_tab[target, , drop = FALSE]
        
        # Flip to get estimate for reference level
        row$estimate <- -row$estimate
        row$ci.lb <- -coef_tab[target, "ci.ub"]
        row$ci.ub <- -coef_tab[target, "ci.lb"]
        
        row$Term <- paste0("as.factor(", col, ")", L)
        row$Label <- paste0(col, ":", L)
        
        rows[[length(rows) + 1]] <- row
      }
    }
  }
  
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}


run_multi_mod_meta_analysis <- function(df, mod_col, data_type) {
  if (length(mod_col) != length(data_type)) {
    stop("mod_col and data_type must have the same length.")
  }
  
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
  
  formula_with_intercept <- as.formula(paste("~", paste(terms, collapse = " + ")))
  formula_without_intercept <- as.formula(paste("~ -1 +", paste(terms, collapse = " + ")))
  
  # --- Drop NA rows ---
  all_vars <- c("yi", "vi", mod_col)
  df <- df[complete.cases(df[, all_vars]), ]
  
  # --- Fit models ---
  mod_with_intercept <- metafor::rma.mv(
    yi, vi,
    random = ~ 1 | participant_id / efN_id,
    mods = formula_with_intercept,
    test = "t", dfs = "contain", method = "REML",
    data = df
  )
  
  mod_without_intercept <- metafor::rma.mv(
    yi, vi,
    random = ~ 1 | participant_id / efN_id,
    mods = formula_without_intercept,
    test = "t", dfs = "contain", method = "REML",
    data = df
  )
  
  summary_with_intercept <- summary(mod_with_intercept)
  
  # --- Participant summary (unchanged) ---
  participants_summary <- purrr::map2_df(mod_col, data_type, function(mc, dt) {
    sym <- rlang::sym(mc)
    
    df %>%
      dplyr::group_by(!!sym) %>%
      dplyr::summarise(
        nexp     = sum(aiN, na.rm = TRUE),
        ctrl     = sum(humanN, na.rm = TRUE),
        kcomp    = dplyr::n(),
        kstudies = dplyr::n_distinct(participant_id),
        .groups  = "drop"
      ) %>%
      dplyr::mutate(
        Term = if (dt == "cat") paste0("as.factor(", mc, ")", !!sym) else mc
      )
  })
  
  # --- ⭐ NEW: Extract all levels for intercept-free model ---
  summary_table_without_intercept <- extract_all_levels_no_intercept(
    df,
    mod_col,
    data_type,
    formula_without_intercept
  )
  
  
  summary_table_without_intercept <- summary_table_without_intercept %>%
    dplyr::left_join(participants_summary, by = "Term") %>%
    dplyr::mutate(Term = Label) %>% # ← replace AFTER join
    dplyr::select(Term, nexp, ctrl, kcomp, kstudies, everything())
  
  # --- Return identical structure ---
  list(
    model_with_intercept = list(
      fit = mod_with_intercept,
      summary = summary_with_intercept,
      summaryTable = coef(summary_with_intercept)
    ),
    model_without_intercept = list(
      fit = mod_without_intercept,
      summary = summary(mod_without_intercept),
      summaryTable = summary_table_without_intercept
    )
  )
}