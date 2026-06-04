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
