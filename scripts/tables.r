create_definition_table <- function() {
  concepts <- tribble(
    ~Term, ~Definition,
    "moral agent", "an entity that makes a decision, or carries out an action, that influences the outcome in a morally-salient scenario.",
    "moral victim/patient", "an entity that is the recipient or target of a moral decision/act/inaction in a morally-salient scenario",
    "moral decision-making", "the process where a moral agent makes a decision that influences the outcome in a morally-salient scenario, such as deciding whether to carry out a certain action or not.",
    "moral agency", "the capacity for an entity to be a moral agent; to discern right from wrong, make moral decisions, and be held morally judged for these decisions/(in)actions.",
    "moral judgement", "an evaluation or assessment, typically by a third-party, of a moral agent's decision or (in)action and its impact on the moral victim(s) in a morally-salient scenario.",
    "moral permissibility", "a specific type of moral judgement that assesses whether a moral agent's decision or (in)action in a morally-salient scenario can be considered permissible (or not) within a given moral framework.",
  )

  kable(concepts, format = "latex", booktabs = TRUE, longtable = TRUE) %>%
    kable_styling(latex_options = c("hold_position"))
}
create_influence_table <- function(influence_stats_df) {
  influence_stats_df <- influence_stats_df %>%
    dplyr::mutate(
      authors = paste0(
        str_to_title(str_extract(ref, "^[a-z]+")),
        " et al., (",
        str_extract(ref, "\\d{4}"),
        ")"
      ),
      study = paste0("Study ", str_extract(ref, "(?<=_s)\\d+"))
    )
  # influence df w. only outliers
  count.outliers <- influence_stats_df %>%
    dplyr::filter(outlier == TRUE)
  # filter w. only influential outliers
  count.influentialOutliers <- count.outliers %>%
    dplyr::filter(hat_flag == TRUE)
  message("Influential outliers *N*:", nrow(count.influentialOutliers))
  influ.Out <- table(count.influentialOutliers$authors, count.influentialOutliers$study, count.influentialOutliers$hat_flag)
  influ.Out <- as.data.frame(influ.Out)

  influ.Out <- influ.Out %>%
    filter(Freq != 0) %>%
    arrange(Var1, Var2)
  write.csv(count.influentialOutliers, "tables/influence_outliers_table.csv")
  kable(influ.Out[, c("Var1", "Var2", "Freq")],
    col.names = c("First Author", "Study", "*n*"),
    caption = NULL, digits = 1
  )
}


###############################################################################
# DESCRIPTIVES TABLE (TRIMMED)
###############################################################################
construct_desc_tbl <- function(trimmed_df) {
  data <- trimmed_df
  ###### hidalgo 2021
  # --- Hidalgo 2021 S1 override ---
  # NA for trimmed df
  # hidalgo_rows <- which(grepl("hidalgo2021_s1", data$ref, ignore.case = TRUE))
  #
  # if (length(hidalgo_rows) > 0) {
  #   data$pct_female[hidalgo_rows] <- "–"
  #   data$age_mean[hidalgo_rows] <- "–"
  #   data$age_sd[hidalgo_rows] <- "–"
  # }
  #
  ###### meder 2019
  # --- meder 2019 S1 override ---
  # NA for trimmed df
  # meder_rows <- which(grepl("meder2019_s1", data$ref, ignore.case = TRUE))
  #
  # if (length(meder_rows) > 0) {
  #   data$pct_female[meder_rows] <- "–"
  #   data$age_mean[meder_rows] <- "–"
  #   data$age_sd[meder_rows] <- "–"
  # }


  ## create descriptives table
  descriptives <- data %>%
    dplyr::select(ref, article_id, study_id, participant_id, efN_id, pct_female, age_mean, age_sd, dv_var, aiN, aiMean, aiSD, humanN, humanMean, humanSD, aiN.0, aiN.1, humanN.0, humanN.1)

  descriptives <- descriptives %>%
    dplyr::mutate(
      apa_ref = paste0(
        str_to_title(str_extract(ref, "^[a-z]+")),
        " et al., (",
        str_extract(ref, "\\d{4}[a-z]?"), # FIXED: Capture year + optional letter
        ")"
      ),
      study_num = paste0(
        "Study ",
        str_extract(ref, "(?<=_s)\\d+(?:\\.\\d+)?")
      ),
      dv_var_cap = str_replace_all(dv_var, "\\.", " (") %>%
        str_to_title() %>%
        str_replace("\\)$", ")") %>%
        str_replace("([A-Za-z]+) \\(", "\\1 (") %>%
        paste0(if_else(str_detect(dv_var, "\\."), ")", "")),
      participant_id = participant_id
    )

  descriptives <- descriptives %>%
    dplyr::select(apa_ref, study_num, participant_id, efN_id, dv_var_cap, pct_female, age_mean, age_sd, aiN, aiMean, aiSD, humanN, humanMean, humanSD, aiN.0, aiN.1, humanN.0, humanN.1)
  descriptives
}

format_col_desc_tbl <- function(descriptives) {
  descriptives$pct_female <- as.numeric(descriptives$pct_female)
  descriptives$age_mean <- as.numeric(descriptives$age_mean)
  descriptives$age_sd <- as.numeric(descriptives$age_sd)
  # Prepare data (same as before)
  df_excel <- descriptives %>%
    mutate(
      pct_female = round(pct_female, 2),
      age_mean = round(age_mean, 2),
      age_sd = round(age_sd, 2),
      aiMean_rnd = round(aiMean, 2),
      aiSD_rnd = round(aiSD, 2),
      humanMean_rnd = round(humanMean, 2),
      humanSD_rnd = round(humanSD, 2),
      dv_short = substr(dv_var_cap, 1, 1),
      blank_base = NA_real_,
      blank = NA_real_,
      blank_one = NA_real_,
      blank_two = NA_real_
    ) %>%
    # Reorder EXPLICITLY - no column name conflicts
    dplyr::select(
      Article = apa_ref,
      Study = study_num,
      Ps = participant_id,
      ES = efN_id,
      DV = dv_short,
      "% Female" = pct_female,
      "Age M" = age_mean,
      "Age SD" = age_sd,
      blank_base,
      "AI N" = aiN, # Spaced names
      "AI M" = aiMean_rnd,
      "AI SD" = aiSD_rnd,
      blank,
      "Human N" = humanN,
      "Human M" = humanMean_rnd,
      "Human SD" = humanSD_rnd,
      blank_one,
      "AI N=0" = aiN.0,
      "AI N=1" = aiN.1,
      blank_two,
      "Human N=0" = humanN.0,
      "Human N=1" = humanN.1
    ) %>%
    mutate(article_group = cumsum(duplicated(Article, fromLast = TRUE) == FALSE)) # Add AFTER select
  # === FIXED collapse logic ===
  # Only collapse rows when M and SD match — NOT N
  cont_cols <- c("AI M", "AI SD", "Human M", "Human SD")
  bin_cols <- c("AI N=0", "AI N=1", "Human N=0", "Human N=1")

  keep_row <- rep(TRUE, nrow(df_excel))

  for (i in 2:nrow(df_excel)) {
    # Only compare rows within the same participant group
    if (df_excel$Ps[i - 1] != df_excel$Ps[i]) next

    # Continuous DVs: collapse only if M and SD match
    cont_same <- all(df_excel[i - 1, cont_cols] == df_excel[i, cont_cols], na.rm = TRUE)

    # Binary DVs: collapse only if binary counts match
    bin_same <- if (!cont_same) {
      all(df_excel[i - 1, bin_cols] == df_excel[i, bin_cols], na.rm = TRUE)
    } else {
      FALSE
    }

    # Collapse only when appropriate
    if (cont_same | bin_same) {
      keep_row[i] <- FALSE
    }
  }

  df_simplified <- df_excel[keep_row, ]
  row_mapping <- cumsum(keep_row)[keep_row]


  # 2. Create comma-separated ES lists
  for (i in 1:nrow(df_simplified)) {
    orig_indices <- which(cumsum(keep_row) == row_mapping[i])
    if (length(orig_indices) > 1) {
      es_values <- sort(unique(df_excel$ES[orig_indices]))
      df_simplified$ES[i] <- paste(es_values, collapse = ",")
    }
  }

  # 3. Convert consecutive sequences to ranges (e.g., "1,2,3,5" → "1-3,5")
  convert_to_ranges <- function(es_str) {
    if (!grepl(",", es_str)) {
      return(es_str)
    } # Single value

    nums <- as.numeric(strsplit(es_str, ",")[[1]])
    if (length(nums) == 1) {
      return(es_str)
    }

    ranges <- c()
    start <- nums[1]
    prev <- nums[1]

    for (i in 2:length(nums)) {
      if (nums[i] == prev + 1) {
        # Continue range
        prev <- nums[i]
      } else {
        # End range
        if (start == prev) {
          ranges <- c(ranges, as.character(start))
        } else {
          ranges <- c(ranges, paste0(start, "-", prev))
        }
        start <- nums[i]
        prev <- nums[i]
      }
    }

    # Add final range
    if (start == prev) {
      ranges <- c(ranges, as.character(start))
    } else {
      ranges <- c(ranges, paste0(start, "-", prev))
    }

    paste(ranges, collapse = ",")
  }

  # Apply range conversion
  df_final <- df_simplified
  df_final$ES <- sapply(df_final$ES, convert_to_ranges)

  print(paste("Simplified from", nrow(df_excel), "to", nrow(df_final), "rows"))
  print("ES examples: '1-3,5' or '2,4,6' or '7'")

  df_excel <- df_final
  # 1. Remove the literal word "Study" (case-sensitive) and surrounding space
  df_excel$Study <- gsub("\\b[Ss]tudy\\b\\s*", "", df_excel$Study)
  df_excel$Study[df_excel$Study == ""] <- NA

  df_excel$article_group <- cumsum(duplicated(df_excel$Article, fromLast = TRUE) == FALSE)

  df_excel
}

create_descriptives_workbook <- function(df_excel) {
  wb <- createWorkbook()
  addWorksheet(wb, "Descriptives")

  # Write headers


  # Row 1: TOP-LEVEL (Continuous DVs / Binary DVs)
  writeData(wb, "Descriptives", rep("", 7), startCol = 1, startRow = 1) # Empty Article-Study-Ps-ES-DV
  writeData(wb, "Descriptives", "Table 8.", startCol = 1, startRow = 2)
  writeData(wb, "Descriptives", "Descriptive Statistics for Effect Sizes with Both Continuous and Binary DVs Included in the Meta-Analysis.", startCol = 1, startRow = 3)
  # writeData(wb, "Descriptives", rep("", 7), startCol = 1, startRow = 3)  # Empty Article-Study-Ps-ES-DV
  writeData(wb, "Descriptives", rep("", 7), startCol = 1, startRow = 4) # Empty Article-Study-Ps-ES-DV
  writeData(wb, "Descriptives", "Continuous DVs", xy = c(10, 4))
  writeData(wb, "Descriptives", "Binary DVs", xy = c(18, 4))
  # # Row 2: AI/Human level
  labels <- c("Article", "Study", "Ps", "ES", "DV", "% Female")

  for (col in 1:6) {
    writeData(wb, "Descriptives", rep(labels[col], 3), startCol = col, startRow = 4)
    mergeCells(wb, "Descriptives", cols = col, rows = 4:6)
  }

  writeData(wb, "Descriptives", rep("Age", 2), startCol = 7, startRow = 4)
  writeData(wb, "Descriptives", "AI", xy = c(10, 5))
  writeData(wb, "Descriptives", "Human", xy = c(14, 5))
  writeData(wb, "Descriptives", "AI", xy = c(18, 5))
  writeData(wb, "Descriptives", "Human", xy = c(21, 5))
  # # Row 3: N/M/SD level
  writeData(wb, "Descriptives", c("M"), xy = c(7, 6))
  writeData(wb, "Descriptives", c("SD"), xy = c(8, 6))
  writeData(wb, "Descriptives", c("N"), xy = c(10, 6))
  writeData(wb, "Descriptives", c("M"), xy = c(11, 6))
  writeData(wb, "Descriptives", c("SD"), xy = c(12, 6))
  writeData(wb, "Descriptives", c("N"), xy = c(14, 6))
  writeData(wb, "Descriptives", c("M"), xy = c(15, 6))
  writeData(wb, "Descriptives", c("SD"), xy = c(16, 6))
  # # Row 4: Binary level (N DV=0, N DV=1)
  writeData(wb, "Descriptives", c("N DV=0"), xy = c(18, 6))
  writeData(wb, "Descriptives", c("N DV=1"), xy = c(19, 6))
  writeData(wb, "Descriptives", c("N DV=0"), xy = c(21, 6))
  writeData(wb, "Descriptives", c("N DV=1"), xy = c(22, 6))


  # MERGE all spanning cells
  mergeCells(wb, "Descriptives", cols = 10:16, rows = 4) # Continuous DVs (cols F-K, row 1)
  mergeCells(wb, "Descriptives", cols = 18:21, rows = 4) # Binary DVs (cols L-Q, row 1)
  mergeCells(wb, "Descriptives", cols = 7:8, rows = 4:5)
  mergeCells(wb, "Descriptives", cols = 10:12, rows = 5) # AI (cols G-I, row 2)
  mergeCells(wb, "Descriptives", cols = 14:16, rows = 5) # Human (cols J-L, row 2)
  mergeCells(wb, "Descriptives", cols = 18:19, rows = 5) # AI (binary, row 2)
  mergeCells(wb, "Descriptives", cols = 21:22, rows = 5) # Human (binary, row 2)


  # Data starts Row 6
  writeData(wb, "Descriptives", df_excel, startRow = 7, colNames = FALSE)

  ##############
  # Formatting
  ##############
  center_style <- createStyle(
    fontName = "Georgia",
    fontSize = 11,
    halign = "center",
    valign = "center"
  )
  addStyle(
    wb, "Descriptives",
    style = center_style,
    rows = 4:6,
    cols = 1:6,
    gridExpand = TRUE,
    stack = TRUE
  )
  age_style <- createStyle(
    halign = "center",
    valign = "center",
    fontName = "Georgia",
    fontSize = 11
  )

  addStyle(
    wb, "Descriptives",
    style = age_style,
    rows = 4:5,
    cols = 7,
    gridExpand = TRUE,
    stack = TRUE
  )

  # italic + center
  header_italic_style <- createStyle(
    textDecoration = "italic",
    halign = "center"
  )
  addStyle(wb, "Descriptives",
    style = header_italic_style,
    rows = 6, cols = 7:15
  )


  # # Georgia font everywhere
  # georgia_style <- createStyle(fontName = "Georgia", fontSize = 11)
  addStyle(wb, "Descriptives", createStyle(fontName = "Georgia", fontSize = 11), cols = 1:30, rows = 1:30, gridExpand = TRUE, stack = TRUE)

  # # Alignments: Article left, others center
  georgia_left_style <- createStyle(
    fontName = "Georgia",
    fontSize = 11,
    halign = "left"
  )
  georgia_center_style <- createStyle(
    fontName = "Georgia",
    fontSize = 11,
    halign = "center"
  )

  # Article column (left-aligned)
  addStyle(wb, "Descriptives",
    style = georgia_left_style,
    rows = 1:(410), cols = 1, gridExpand = TRUE
  )

  # All other columns (center-aligned)
  addStyle(wb, "Descriptives",
    style = georgia_center_style,
    rows = 1:410, cols = 2:25, gridExpand = TRUE
  )


  addStyle(wb, "Descriptives", createStyle(fontName = "Georgia", fontSize = 11, textDecoration = "bold"), rows = 2, cols = 1, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, "Descriptives", createStyle(fontName = "Georgia", fontSize = 11, textDecoration = "italic"), rows = 3, cols = 1, gridExpand = TRUE, stack = TRUE)

  startRow <- 8 # whatever row your df_excel data start on in Excel

  df_helper <- df_excel |>
    mutate(
      Article = trimws(Article),
      Ps      = trimws(Ps),
      Study   = trimws(Study),
      Study   = as.integer(Study),
      row_idx = row_number() + startRow - 1
    )

  # Counts per Article+Study
  df_helper <- df_helper |>
    group_by(Article, Study) |>
    mutate(
      study_n      = n(),
      first_row_st = min(row_idx) # first Excel row where THIS Study appears in THIS article
    ) |>
    ungroup()

  # First row per Article (for Ps logic, if needed)
  first_article_rows <- df_helper |>
    filter(!is.na(Article)) |>
    group_by(Article) |>
    summarise(first_row = min(row_idx), .groups = "drop")

  first_row_lookup <- setNames(
    first_article_rows$first_row,
    first_article_rows$Article
  )

  study_col <- which(colnames(df_excel) == "Study")
  ps_col <- which(colnames(df_excel) == "Ps")

  for (i in seq_len(nrow(df_helper))) {
    excel_row <- df_helper$row_idx[i]
    art <- df_helper$Article[i]
    ps_val <- df_helper$Ps[i]
    study_val <- df_helper$Study[i]

    if (is.na(art)) next

    is_first_row_for_article <- excel_row == first_row_lookup[[art]]

    ## ----- Study -----
    # Keep Study when:
    # - it's the first row of the article, OR
    # - that Study appears only once in the article, OR
    # - this row is the first occurrence of this Study within the article
    if (!is_first_row_for_article &&
      !is.na(study_val) &&
      df_helper$study_n[i] > 1 &&
      excel_row != df_helper$first_row_st[i]) {
      deleteData(
        wb, "Descriptives",
        cols = study_col,
        rows = excel_row,
        gridExpand = TRUE
      )
    }
  }


  # APA table lines

  line_style <- createStyle(
    border = "bottom",
    borderStyle = "thin",
    borderColour = "#000000"
  )
  addStyle(wb, "Descriptives",
    style = line_style,
    rows = 3, cols = 1:22, gridExpand = TRUE, stack = TRUE
  ) # Row 2 bottom = above row 3

  note_row <- nrow(df_excel) + 7

  # "Note." in italics
  writeData(wb, "Descriptives", "Note.", startCol = 1, startRow = note_row)


  # Rest of the note
  writeData(
    wb, "Descriptives",
    "Continuous DVs include age, reaction times, and other continuous measures. Binary DVs include accuracy, correct/incorrect responses, and categorical outcomes.",
    startCol = 2, startRow = note_row
  )

  bottom_line <- createStyle(
    border = "top",
    borderStyle = "thin",
    borderColour = "#000000"
  )


  addStyle(wb, "Descriptives",
    style = bottom_line,
    rows = 5, cols = 10:16, stack = TRUE
  )
  addStyle(wb, "Descriptives",
    style = bottom_line,
    rows = 5, cols = 18:22, stack = TRUE
  )

  addStyle(wb, "Descriptives",
    style = bottom_line,
    rows = 6, cols = 7:8, stack = TRUE
  )


  addStyle(wb, "Descriptives",
    style = bottom_line,
    rows = 6, cols = 10:12, stack = TRUE
  )
  addStyle(wb, "Descriptives",
    style = bottom_line,
    rows = 6, cols = 14:16, stack = TRUE
  )
  addStyle(wb, "Descriptives",
    style = bottom_line,
    rows = 6, cols = 17:19, stack = TRUE
  )
  addStyle(wb, "Descriptives",
    style = bottom_line,
    rows = 6, cols = 21:22, stack = TRUE
  )
  addStyle(wb, "Descriptives",
    style = bottom_line,
    rows = 6, cols = 10:12, stack = TRUE
  )

  addStyle(wb, "Descriptives",
    style = bottom_line,
    rows = 7, cols = 1:22, stack = TRUE
  )
  addStyle(wb, "Descriptives",
    style = bottom_line,
    rows = note_row, cols = 1:22
  ) # Row 99 bottom
  addStyle(
    wb, "Descriptives",
    createStyle(
      fontName = "Georgia",
      fontSize = 10,
      textDecoration = "italic",
      halign = "left"
    ),
    rows = note_row, cols = 1,
    stack = TRUE
  )

  addStyle(
    wb, "Descriptives",
    createStyle(
      fontName = "Georgia",
      fontSize = 10,
      halign = "left"
    ),
    rows = note_row, cols = 2,
    stack = TRUE
  )


  saveWorkbook(wb, "tables/descriptives_apafinal.xlsx", overwrite = TRUE)
}

################################################################################
# INFLUENTIAL STATS TABLE
################################################################################
format_influential_stats_table_apa <- function(influence_stats_df) {
  # influence df w. only outliers
  count.outliers <- influence_stats_df %>%
    dplyr::filter(outlier == TRUE)
  # filter w. only influential outliers
  count.influentialOutliers <- count.outliers %>%
    dplyr::filter(hat_flag == TRUE)
  count.influentialOutliers <- count.influentialOutliers %>%
    dplyr::mutate(
      apa_ref = paste0(
        str_to_title(str_extract(ref, "^[a-z]+")),
        " et al., (",
        str_extract(ref, "\\d{4}[a-z]?"), # FIXED: Capture year + optional letter
        ")"
      ),
      Study = paste0(
        "Study ",
        str_extract(ref, "(?<=_s)\\d+(?:\\.\\d+)?")
      ),
      participant_id = participant_id
    ) %>%
    dplyr::select(apa_ref, Study, participant_id, effect_size, cooks, cooks_flag, intrcpt, intrcpt.1, hatvalues, hat_flag)

  count.influentialOutliers$Study <- gsub("\\b[Ss]tudy\\b\\s*", "", count.influentialOutliers$Study)
  count.influentialOutliers$Study[count.influentialOutliers$Study == ""] <- NA

  count.influentialOutliers

  count.influentialOutliers <- count.influentialOutliers %>%
    mutate(
      effect_size = round(effect_size, 2),
      cooks = round(cooks, 2),
      intrcpt = round(intrcpt, 2),
      hatvalues = round(hatvalues, 2),
      blank_one = NA_real_,
      blank_two = NA_real_
    ) %>%
    # Reorder EXPLICITLY - no column name conflicts
    dplyr::select(
      Article = apa_ref,
      Study = Study,
      "Ps ID" = participant_id,
      g = effect_size,
      "Cooks Value" = cooks, # Spaced names
      "Cooks Flag" = cooks_flag,
      blank_one,
      "DFBETAS Value" = intrcpt,
      "DFBETAS Flag" = intrcpt.1,
      blank_two,
      "Hat Values" = hatvalues,
      "Hat Flag" = hat_flag
    )
  count.influentialOutliers
}

format_full_outlier_stats_table_apa <- function(count.influentialOutliers) {
  # influence df w. only outliers
  count.influentialOutliers <- count.influentialOutliers %>%
    dplyr::mutate(
      apa_ref = paste0(
        str_to_title(str_extract(ref, "^[a-z]+")),
        " et al., (",
        str_extract(ref, "\\d{4}[a-z]?"), # FIXED: Capture year + optional letter
        ")"
      ),
      Study = paste0(
        "Study ",
        str_extract(ref, "(?<=_s)\\d+(?:\\.\\d+)?")
      ),
      participant_id = participant_id
    ) %>%
    dplyr::select(apa_ref, Study, participant_id, effect_size, cooks, cooks_flag, intrcpt, intrcpt.1, hatvalues, hat_flag)

  count.influentialOutliers$Study <- gsub("\\b[Ss]tudy\\b\\s*", "", count.influentialOutliers$Study)
  count.influentialOutliers$Study[count.influentialOutliers$Study == ""] <- NA

  count.influentialOutliers

  count.influentialOutliers <- count.influentialOutliers %>%
    mutate(
      effect_size = round(effect_size, 2),
      cooks = round(cooks, 2),
      intrcpt = round(intrcpt, 2),
      hatvalues = round(hatvalues, 2),
      blank_one = NA_real_,
      blank_two = NA_real_
    ) %>%
    # Reorder EXPLICITLY - no column name conflicts
    dplyr::select(
      Article = apa_ref,
      Study = Study,
      PsID = participant_id,
      g = effect_size,
      "Cooks Value" = cooks, # Spaced names
      "Cooks Flag" = cooks_flag,
      blank_one,
      "DFBETAS Value" = intrcpt,
      "DFBETAS Flag" = intrcpt.1,
      blank_two,
      "Hat Values" = hatvalues,
      "Hat Flag" = hat_flag
    )

  df2 <- count.influentialOutliers |>
    mutate(
      Article = trimws(Article),
      PsID = trimws(PsID),
      Study = as.integer(trimws(Study))
    ) |>
    arrange(Article) |>
    # First row within each Article
    group_by(Article) |>
    mutate(
      is_first_row_for_article = row_number() == 1
    ) |>
    ungroup() |>
    # First row within each Article × Study
    group_by(Article, Study) |>
    mutate(
      study_n = n(),
      is_first_row_st = row_number() == 1
    ) |>
    ungroup() |>
    # Apply your Study deleteData() logic
    mutate(
      Study_display = if_else(
        !is_first_row_for_article &
          !is.na(Study) &
          study_n > 1 &
          !is_first_row_st,
        "",
        as.character(Study)
      ),

      # Apply Article deleteData() logic
      Article_display = if_else(
        is_first_row_for_article,
        Article,
        ""
      )
    )


  #  2. Drop the original Study column and rename Study_display

  df2 <- df2 |>
    select(-Study, -Article) |>
    rename(
      Study   = Study_display,
      Article = Article_display,
      "Ps ID" = PsID
    ) %>%
    select(Article, Study, "Ps ID", g, "Cooks Value", "Cooks Flag", "blank_one", "DFBETAS Value", "DFBETAS Flag", "blank_two", "Hat Values", "Hat Flag")
  df2
}


create_influential_workbook <- function(df_excel) {
  wb <- createWorkbook()
  addWorksheet(wb, "Influential")

  # Write headers


  # Row 1: TOP-LEVEL (Continuous DVs / Binary DVs)
  # writeData(wb, "Influential", "Table 5.", startCol = 1, startRow = 1)
  # writeData(wb, "Influential", "Table of the influential outliers and their respective statistics for the influential diagnostics.", startCol = 1, startRow = 2)

  # # Row 2: AI/Human level
  labels <- c("Article", "Study", "Ps", "g")

  for (col in 1:5) {
    writeData(wb, "Influential", rep(labels[col], 3), startCol = col, startRow = 3)
    mergeCells(wb, "Influential", cols = col, rows = 5:6)
  }

  writeData(wb, "Influential", rep("Cooks", 2), startCol = 5, startRow = 3)
  writeData(wb, "Influential", "DFBETA", xy = c(8, 3))
  writeData(wb, "Influential", "Hat", xy = c(11, 3))
  # # Row 3: N/M/SD level
  writeData(wb, "Influential", c("Value"), xy = c(5, 4))
  writeData(wb, "Influential", c("Flag"), xy = c(6, 4))
  writeData(wb, "Influential", c("Value"), xy = c(8, 4))
  writeData(wb, "Influential", c("Flag"), xy = c(9, 4))
  writeData(wb, "Influential", c("Value"), xy = c(11, 4))
  writeData(wb, "Influential", c("Flag"), xy = c(12, 4))


  # # MERGE all spanning cells
  mergeCells(wb, "Influential", cols = 5:6, rows = 3)
  mergeCells(wb, "Influential", cols = 8:9, rows = 3)
  mergeCells(wb, "Influential", cols = 11:12, rows = 3)

  # # Data starts Row 6
  writeData(wb, "Influential", df_excel, startRow = 5, colNames = FALSE)

  # ##############
  # # Formatting
  # ##############
  center_style <- createStyle(
    fontName = "Georgia",
    fontSize = 11,
    halign = "center",
    valign = "center"
  )
  addStyle(
    wb, "Influential",
    style = center_style,
    rows = 3:5,
    cols = 1:6,
    gridExpand = TRUE,
    stack = TRUE
  )
  age_style <- createStyle(
    halign = "center",
    valign = "center",
    fontName = "Georgia",
    fontSize = 11
  )

  addStyle(
    wb, "Influential",
    style = age_style,
    rows = 3:5,
    cols = 7,
    gridExpand = TRUE,
    stack = TRUE
  )

  # italic + center
  header_italic_style <- createStyle(
    textDecoration = "italic",
    halign = "center"
  )
  addStyle(wb, "Influential",
    style = header_italic_style,
    rows = 4, cols = 7:15
  )


  # # Georgia font everywhere
  # georgia_style <- createStyle(fontName = "Georgia", fontSize = 11)
  addStyle(wb, "Influential", createStyle(fontName = "Georgia", fontSize = 11), cols = 1:30, rows = 1:30, gridExpand = TRUE, stack = TRUE)

  # # Alignments: Article left, others center
  georgia_left_style <- createStyle(
    fontName = "Georgia",
    fontSize = 11,
    halign = "left"
  )
  georgia_center_style <- createStyle(
    fontName = "Georgia",
    fontSize = 11,
    halign = "center"
  )

  # Article column (left-aligned)
  addStyle(wb, "Influential",
    style = georgia_left_style,
    rows = 1:(410), cols = 1, gridExpand = TRUE
  )

  # All other columns (center-aligned)
  addStyle(wb, "Influential",
    style = georgia_center_style,
    rows = 1:410, cols = 2:25, gridExpand = TRUE
  )


  addStyle(wb, "Influential", createStyle(fontName = "Georgia", fontSize = 11, textDecoration = "bold"), rows = 1, cols = 1, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, "Influential", createStyle(fontName = "Georgia", fontSize = 11, textDecoration = "italic"), rows = 2, cols = 1, gridExpand = TRUE, stack = TRUE)

  startRow <- 7 # whatever row your df_excel data start on in Excel
  #
  # df_helper <- df_excel |>
  #   mutate(
  #     Article = trimws(Article),
  #     Ps      = trimws(Ps),
  #     Study   = trimws(Study),
  #     Study   = as.integer(Study),
  #     row_idx = row_number() + startRow - 1
  #   )
  #
  # # Counts per Article+Study
  # df_helper <- df_helper |>
  #   group_by(Article, Study) |>
  #   mutate(
  #     study_n      = n(),
  #     first_row_st = min(row_idx)   # first Excel row where THIS Study appears in THIS article
  #   ) |>
  #   ungroup()
  #
  # # First row per Article (for Ps logic, if needed)
  # first_article_rows <- df_helper |>
  #   filter(!is.na(Article)) |>
  #   group_by(Article) |>
  #   summarise(first_row = min(row_idx), .groups = "drop")
  #
  # first_row_lookup <- setNames(first_article_rows$first_row,
  #                              first_article_rows$Article)
  #
  # study_col <- which(colnames(df_excel) == "Study")
  # ps_col    <- which(colnames(df_excel) == "Ps")
  #
  # for (i in seq_len(nrow(df_helper))) {
  #   excel_row <- df_helper$row_idx[i]
  #   art       <- df_helper$Article[i]
  #   ps_val    <- df_helper$Ps[i]
  #   study_val <- df_helper$Study[i]
  #
  #   if (is.na(art)) next
  #
  #   is_first_row_for_article <- excel_row == first_row_lookup[[art]]
  #
  #   ## ----- Study -----
  #   # Keep Study when:
  #   # - it's the first row of the article, OR
  #   # - that Study appears only once in the article, OR
  #   # - this row is the first occurrence of this Study within the article
  #   if (!is_first_row_for_article &&
  #       !is.na(study_val) &&
  #       df_helper$study_n[i] > 1 &&
  #       excel_row != df_helper$first_row_st[i]) {
  #
  #     deleteData(
  #       wb, "Descriptives",
  #       cols = study_col,
  #       rows = excel_row,
  #       gridExpand = TRUE
  #     )
  #   }
  # }
  #
  #
  # APA table lines
  #
  line_style <- createStyle(
    border = "bottom",
    borderStyle = "thin",
    borderColour = "#000000"
  )
  addStyle(wb, "Influential",
    style = line_style,
    rows = 4, cols = 1:12, gridExpand = TRUE, stack = TRUE
  ) # Row 2 bottom = above row 3

  # note_row <- nrow(df_excel) + 7
  #
  # # "Note." in italics
  # writeData(wb, "Descriptives", "Note.", startCol = 1, startRow = note_row)
  #
  #
  # # Rest of the note
  # writeData(
  #   wb, "Descriptives",
  #   "Continuous DVs include age, reaction times, and other continuous measures. Binary DVs include accuracy, correct/incorrect responses, and categorical outcomes.",
  #   startCol = 2, startRow = note_row
  # )
  #
  bottom_line <- createStyle(
    border = "top",
    borderStyle = "thin",
    borderColour = "#000000"
  )
  #
  #
  addStyle(wb, "Influential",
    style = bottom_line,
    rows = 6, cols = 5:6, stack = TRUE
  )
  addStyle(wb, "Influential",
    style = bottom_line,
    rows = 6, cols = 8:9, stack = TRUE
  )

  addStyle(wb, "Influential",
    style = bottom_line,
    rows = 6, cols = 11:12, stack = TRUE
  )
  #
  #
  addStyle(wb, "Influential",
    style = bottom_line,
    rows = 7, cols = 1:12, stack = TRUE
  )
  # addStyle(wb, "Descriptives", style = bottom_line,
  #          rows = 6, cols = 14:16, stack=TRUE)
  # addStyle(wb, "Descriptives", style = bottom_line,
  #          rows = 6, cols = 17:19, stack=TRUE)
  # addStyle(wb, "Descriptives", style = bottom_line,
  #          rows = 6, cols = 21:22, stack=TRUE)
  # addStyle(wb, "Descriptives", style = bottom_line,
  #          rows = 6, cols = 10:12, stack=TRUE)
  #
  # addStyle(wb, "Descriptives", style = bottom_line,
  #          rows = 7, cols = 1:22, stack=TRUE)
  addStyle(wb, "Influential",
    style = bottom_line,
    rows = 14, cols = 1:12
  ) # Row 99 bottom
  addStyle(
    wb, "Influential",
    createStyle(
      fontName = "Georgia",
      fontSize = 11,
      textDecoration = "italic",
      halign = "center"
    ),
    rows = 5, cols = 4,
    stack = TRUE
  )
  #
  # addStyle(
  #   wb, "Descriptives",
  #   createStyle(
  #     fontName = "Georgia",
  #     fontSize = 10,
  #     halign = "left"
  #   ),
  #   rows = note_row, cols = 2,
  #   stack=TRUE
  # )
  #
  #


  saveWorkbook(wb, "tables/influential_apafinal.xlsx", overwrite = TRUE)
}


###########################
# DV SYNONYM MOD TABLES
###########################

# summarise number of studies for each level of dv mod ("acceptable", "appropriate", "good", "justifiable", "moral", "permissible", "right", "unethical",  "wrong")
create_k_dv_syn_sum_table <- function(dv_synonym_model) {
  df <- data.frame(
    Level = dv_synonym_model[["model_without_intercept"]][["summaryTable"]][["Term"]],
    k = dv_synonym_model[["model_without_intercept"]][["summaryTable"]][["kstudies"]],
    g = dv_synonym_model[["model_without_intercept"]][["summaryTable"]][["estimate"]],
    CI_LB = dv_synonym_model[["model_without_intercept"]][["summaryTable"]][["ci.lb"]],
    CI_UB = dv_synonym_model[["model_without_intercept"]][["summaryTable"]][["ci.ub"]],
    p = dv_synonym_model[["model_without_intercept"]][["summaryTable"]][["pval"]]
  )

  df$Level <- gsub("^factor\\(dv_synonym\\)", "", df$Level)
  df$Level <- factor(str_to_title(as.character(df$Level)))

  df <- df %>%
    mutate(
      g = round(g, 2),
      CI_LB = round(CI_LB, 2),
      CI_UB = round(CI_UB, 2),
      p = round(p, 3)
    )

  df
}


### TOST ("acceptable", "appropriate", "good", "justifiable", "moral", "permissible", "right", "unethical",  "wrong")

create_TOST_dv_syn_sum_table <- function(trimmedToast_dv_synonym_acceptable,
                                         trimmedToast_dv_synonym_appropriate,
                                         trimmedToast_dv_synonym_good,
                                         trimmedToast_dv_synonym_justifiable,
                                         trimmedToast_dv_synonym_moral,
                                         trimmedToast_dv_synonym_permissible,
                                         trimmedToast_dv_synonym_right,
                                         trimmedToast_dv_synonym_unethical,
                                         trimmedToast_dv_synonym_wrong) {
  df <- rbind(
    trimmedToast_dv_synonym_acceptable[["tibble"]],
    trimmedToast_dv_synonym_appropriate[["tibble"]],
    trimmedToast_dv_synonym_good[["tibble"]],
    trimmedToast_dv_synonym_justifiable[["tibble"]],
    trimmedToast_dv_synonym_moral[["tibble"]],
    trimmedToast_dv_synonym_permissible[["tibble"]],
    trimmedToast_dv_synonym_right[["tibble"]],
    trimmedToast_dv_synonym_unethical[["tibble"]],
    trimmedToast_dv_synonym_wrong[["tibble"]]
  )

  df$model_name <- gsub("Trimmed DV Synonym ", "", df$model_name)

  df
}


###########################
# RQ MOD TABLE
###########################
create_rq_table <- function(df) {
  df <- read_csv(df)
  df
}

###########################
# LOWEST CAT LEVEL TABLE
###########################
create_low_cat_lvl_tbl <- function(combined_df, trimmed_df) {
  get_min_rows <- function(df, var, source_label) {
    var_sym <- rlang::sym(var)


    summary <- df %>%
      filter(
        !is.na(!!var_sym),
        !!var_sym != "NULL",
        !!var_sym != "NA",
        !!var_sym != ""
      ) %>%
      distinct(study_id, !!var_sym) %>%
      count(!!var_sym, name = "n_studies") %>%
      left_join(
        df %>%
          filter(
            !is.na(!!var_sym),
            !!var_sym != "NULL",
            !!var_sym != ""
          ) %>%
          count(!!var_sym, name = "n_effect_sizes"),
        by = var
      )

    min_studies <- summary %>%
      ungroup() %>%
      slice_min(n_studies)
    min_es <- summary %>%
      ungroup() %>%
      slice_min(n_effect_sizes)

    bind_rows(min_studies, min_es) %>%
      distinct() %>%
      rename(level = !!var_sym) %>%
      mutate(
        variable = var,
        source = source_label,
        .before = level
      )
  }

  merge_combined_trimmed <- function(var) {
    combined <- get_min_rows(combined_df, var, "combined")
    trimmed <- get_min_rows(trimmed_df, var, "trimmed")

    # If identical rows → return one row with source = "both"
    if (nrow(combined) == nrow(trimmed) &&
      all(combined$level == trimmed$level) &&
      all(combined$n_studies == trimmed$n_studies) &&
      all(combined$n_effect_sizes == trimmed$n_effect_sizes)) {
      combined %>%
        mutate(source = "both")
    } else {
      # Otherwise return both
      bind_rows(combined, trimmed)
    }
  }

  df <- bind_rows(
    merge_combined_trimmed("harm"),
    merge_combined_trimmed("in_action"),
    merge_combined_trimmed("dv_synonym"),
    merge_combined_trimmed("intent"),
    merge_combined_trimmed("aiType_a"),
    merge_combined_trimmed("aiType_b"),
    merge_combined_trimmed("agent_intel")
  )


  df
}

streamline_min_k_cat_level_tbl <- function(min_k_cat_level) {
  # Pivot combined/trimmed into wide format
  wide <- min_k_cat_level %>%
    pivot_wider(
      id_cols = c(variable, level),
      names_from = source,
      values_from = c(n_studies, n_effect_sizes),
      names_sep = "_"
    )

  make_superscript <- function(combined, trimmed, both) {
    # Collect all non-missing comparison values
    candidates <- c(trimmed, both)
    candidates <- candidates[!is.na(candidates)]

    # Case 1: combined is NA and no other values exist
    if (is.na(combined) && length(candidates) == 0) {
      return("")
    }

    # Case 2: combined is NA but trimmed/both exist
    if (is.na(combined) && length(candidates) > 0) {
      smallest <- min(candidates)
      return(as.character(smallest))
    }

    # Case 3: combined exists but no trimmed/both exist
    if (!is.na(combined) && length(candidates) == 0) {
      return(as.character(combined))
    }

    # Case 4: combined exists and trimmed/both exist
    smallest <- min(candidates)

    if (combined == smallest) {
      return(as.character(combined))
    }

    paste0(combined, "^{", smallest, "}")
  }
  final_table <- wide %>%
    mutate(
      Studies = mapply(
        make_superscript,
        n_studies_combined,
        n_studies_trimmed,
        n_studies_both
      ),
      ES = mapply(
        make_superscript,
        n_effect_sizes_combined,
        n_effect_sizes_trimmed,
        n_effect_sizes_both
      )
    ) %>%
    select(variable, level, Studies, ES)

  final_table
}


create_mod_df_table <- function(mod.df) {
  # Ensure numeric sorting if possible
  mod.df <- mod.df[order(as.numeric(as.character(mod.df$article_id))), ]

  # Create consecutive numbering with preserved grouping
  mod.df$article_id <- match(
    as.character(mod.df$article_id),
    unique(as.character(mod.df$article_id))
  )

  # Check results
  sort(unique(mod.df$article_id)) # should be 1, 2, 3, ...
  setdiff(seq_len(max(mod.df$article_id)), unique(mod.df$article_id)) # should be integer(0)


  # Ensure numeric sorting if possible
  mod.df <- mod.df[order(as.numeric(as.character(mod.df$study_id))), ]

  # Create consecutive numbering with preserved grouping
  mod.df$study_id <- match(
    as.character(mod.df$study_id),
    unique(as.character(mod.df$study_id))
  )

  # Check results
  sort(unique(mod.df$study_id)) # should be 1, 2, 3, ...
  setdiff(seq_len(max(mod.df$study_id)), unique(mod.df$study_id)) # should be integer(0)


  # Ensure numeric sorting if possible
  mod.df <- mod.df[order(as.numeric(as.character(mod.df$participant_id))), ]

  # Create consecutive numbering with preserved grouping
  mod.df$participant_id <- match(
    as.character(mod.df$participant_id),
    unique(as.character(mod.df$participant_id))
  )

  # Check results
  sort(unique(mod.df$participant_id)) # should be 1, 2, 3, ...
  setdiff(seq_len(max(mod.df$participant_id)), unique(mod.df$participant_id)) # should be integer(0)


  # note - Malle et al. (2019) uses the word "cancel" which could imply action to Ps, despite there rest of the scenario being consistent with this being an "inaction" condition. We, therefore, categorised this condition as inaction, and then tested for any obvious diff. with this categorised as action. This made negligible difference.


  mod.df <- mod.df %>%
    dplyr::select(
      ref, article_id, study_id, participant_id, efN_id, aiN, humanN, dv_var, dv_synonym, yi, vi, intent, in_action, harm, agent_intel, aiType_a, aiType_b, PMA, PMC, responsible, RQ
    )


  mod.df <- mod.df %>%
    mutate(
      dv_synonym = as.character(dv_synonym),
      dv_var_cap = case_when(
        dv_synonym == "acceptable" ~ "Ac",
        dv_synonym == "appropriate" ~ "Ap",
        TRUE ~ str_to_upper(str_sub(dv_synonym, 1, 1))
      )
    )


  mod.df <- mod.df %>%
    mutate(
      PMA = ifelse(is.na(PMA), "", sprintf("%.2f", PMA)),
      PMC = ifelse(is.na(PMC), "", sprintf("%.2f", PMC)),
      responsible = ifelse(is.na(responsible), "", sprintf("%.2f", responsible)),
      RQ = ifelse(is.na(RQ), "", sprintf("%.2f", RQ))
    )


  collapse_ranges <- function(x) {
    x <- sort(unique(as.numeric(x)))
    runs <- split(x, cumsum(c(1, diff(x)) != 1))

    formatted <- vapply(runs, function(run) {
      if (length(run) >= 3) {
        paste0(run[1], "-", run[length(run)])
      } else {
        paste(run, collapse = ", ")
      }
    }, character(1))

    paste(formatted, collapse = ", ")
  }

  collapse_cols <- c("efN_id", "PMA", "PMC", "responsible", "RQ")
  group_cols <- setdiff(names(mod.df), collapse_cols)

  mod.df <- mod.df |>
    group_by(across(all_of(group_cols))) |>
    summarise(
      across(
        all_of(collapse_cols),
        ~ if (cur_column() == "efN_id") {
          collapse_ranges(.x)
        } else {
          paste(unique(.x), collapse = ", ")
        }
      ),
      .groups = "drop"
    )


  extract_start <- function(x) as.numeric(sub("([0-9]+).*", "\\1", x))

  mod.df <- mod.df |>
    mutate(efN_start = extract_start(efN_id)) |>
    arrange(efN_start) |>
    select(-efN_start)

  mod.df <- mod.df %>%
    mutate(
      apa_ref = paste0(
        str_to_title(str_extract(ref, "^[a-z]+")),
        " et al., (",
        str_extract(ref, "\\d{4}"),
        ")"
      ),
      study_num = paste0(
        "Study ",
        str_extract(ref, "(?<=_s)\\d+(?:[._]\\d+)?") |>
          str_replace("_", ".")
      )
    ) %>%
    # 2️⃣ Order rows correctly
    arrange(
      apa_ref,
      as.numeric(str_replace(study_num, "Study ", ""))
    ) %>%
    # 3️⃣ Collapse APA reference (only first per article)
    group_by(apa_ref) %>%
    mutate(
      apa_ref_display = if_else(row_number() == 1, apa_ref, " ")
    ) %>%
    # 4️⃣ Collapse study number (only first per study within article)
    group_by(apa_ref, study_num) %>%
    mutate(
      study_num = if_else(row_number() == 1, study_num, " ")
    ) %>%
    ungroup() %>%
    mutate(
      slab_labels = if_else(
        apa_ref == " ",
        paste0("   ", study_num), # indent studies
        paste0(apa_ref, " ", study_num)
      )
    )

  ## ensure numeric variables are numeric
  mod.df$yi <- as.numeric(mod.df$yi)
  mod.df$vi <- as.numeric(mod.df$vi)

  mod.df <- mod.df %>%
    mutate(
      yi = suppressWarnings(as.numeric(gsub("[^0-9\\.\\-]", "", gsub(",", ".", as.character(yi))))),
      vi = suppressWarnings(as.numeric(gsub("[^0-9\\.\\-]", "", gsub(",", ".", as.character(vi)))))
    )

  mod.df <- mod.df %>%
    mutate(
      yi = round(yi, 2),
      vi = round(vi, 2)
    )

  mod.df <- mod.df %>%
    mutate(
      yi = sprintf("%.2f", yi),
      vi = sprintf("%.2f", vi)
    )

  mod.df <- mod.df %>%
    dplyr::select(
      apa_ref_display, study_num, participant_id, efN_id, aiN, humanN, dv_var_cap, yi, vi, intent, in_action, harm, agent_intel, aiType_a, aiType_b, PMA, PMC, responsible, RQ
    )

  mods <- c("intent", "in_action", "harm", "agent_intel", "aiType_a", "aiType_b")

  mod.df <- mod.df |>
    dplyr::mutate(across(all_of(mods), stringr::str_to_title))

  mods <- c("intent", "in_action", "harm", "agent_intel", "aiType_a", "aiType_b")

  recode_maps <- list(
    intent = c(
      "M-E" = "Meane",
      "S-E" = "Sidee"
    ),
    in_action = c(
      "I" = "Inaction",
      "A" = "Action"
    ),
    harm = c(
      "NH" = "Notharm",
      "H" = "Harm"
    ),
    agent_intel = c(
      "NI" = "Notimplied",
      "I" = "Implied"
    ),
    aiType_a = c(
      "AI" = "Ai",
      "H" = "Human",
      "R" = "Robot"
    ),
    aiType_b = c(
      "AI" = "Ai",
      "H" = "Human",
      "Mech." = "Mechanical",
      "Hum." = "Humanoid"
    )
  )

  mod.df[mods] <- Map(
    function(col, map) forcats::fct_recode(col, !!!map),
    mod.df[mods],
    recode_maps[mods]
  )


  mod.df <- mod.df %>%
    dplyr::rename(
      "First Author, Year" = apa_ref_display,
      "Study" = study_num,
      "PsID" = participant_id,
      "EFIDs" = efN_id,
      "AI \\textit{N}" = aiN,
      "Human \\textit{N}" = humanN,
      "DV* (synonym)" = dv_var_cap,
      "\\textit{g}" = yi,
      "\\textit{Var}" = vi,
      "Intent" = intent,
      "Decision Type" = in_action,
      "Moral Domain" = harm,
      "Agent Intelligence" = agent_intel,
      "2-Category AI" = aiType_a,
      "3-Category AI" = aiType_b,
      PMA = PMA,
      PMC = PMC,
      "Responsibility" = responsible,
      RQ = RQ
    )


  return(mod.df)
}
