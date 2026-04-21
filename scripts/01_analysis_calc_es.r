####### calculate effect sizes for studies where data is available
calc_es_data_avail <- function(master_df) {
  ## calculate es for data studies
  safe_escalc <- function(row) {
    tryCatch(
      {
        if (row$dv_type == "continuous") {
          escalc(
            measure = "SMD",
            m1i = row$aiMean,
            sd1i = row$aiSD,
            n1i = row$aiN,
            m2i = row$humanMean,
            sd2i = row$humanSD,
            n2i = row$humanN,
            var.names = c("yi", "vi")
          )
        } else {
          escalc(
            measure = "OR2DL",
            ai = row$aiN.1,
            bi = row$aiN.0,
            n1i = row$aiN,
            ci = row$humanN.1,
            di = row$humanN.0,
            n2i = row$humanN,
            var.names = c("yi", "vi")
          )
        }
      },
      error = function(e) {
        message("Error in study: ", row$ref, " / study_id: ", row$study_id)
        message("  dv_type: ", row$dv_type)
        message("  Error: ", e$message)
        return(NULL)
      }
    )
  }

  full_data_avail_df <- master_df
  full_data_avail_df <- full_data_avail_df %>%
    rowwise() %>%
    dplyr::mutate(escalc_result = list(safe_escalc(cur_data())))

  full_data_avail_df <- full_data_avail_df %>%
    rowwise() %>%
    dplyr::mutate(
      yi = escalc_result$yi[1],
      vi = escalc_result$vi[1]
    )
  full_data_avail_df
}

################################################################################
# MANUAL STUDIES EFFECT SIZES
################################################################################
calc_es_manual <- function(manual_data_df, full_data_avail_df) {
  manual_desc <- tibble() # initialize empty

  for (i in seq_len(nrow(manual_data_df))) {
    row <- manual_data_df[i, ]

    # -------------------------
    # Extract manual_stats safely
    # -------------------------
    stats <- row$manual_stats
    if (is.null(stats)) next # skip if no manual stats

    df_out <- NULL

    # Continuous descriptive
    if (!is.null(stats[[1]]$humanMean) && !is.null(stats[[1]]$aiMean)) {
      message("Processing continuous descriptive: ", row$ref[[1]], " / study_id: ", row$study_id[[1]])
      df_out <- tryCatch(
        {
          tib <- metafor::escalc(
            measure = "SMD",
            m1i = stats[[1]]$aiMean, sd1i = stats[[1]]$aiSD, n1i = stats[[1]]$aiN,
            m2i = stats[[1]]$humanMean, sd2i = stats[[1]]$humanSD, n2i = stats[[1]]$humanN,
            data = tibble(ref = row$ref[[1]], study_id = row$study_id[[1]])
          )
          tib %>%
            dplyr::mutate(
              article_id = row$article_id[[1]],
              efN_id = row$efN_id[[1]],
              dv_type = "continuous",
              dv_var = row$dv_var[[1]],
              pct_female = row$pct_female[[1]],
              age_mean = row$age_mean[[1]],
              age_sd = row$age_sd[[1]],
              aiMean = stats[[1]]$aiMean,
              aiSD = stats[[1]]$aiSD,
              aiN = stats[[1]]$aiN,
              humanMean = stats[[1]]$humanMean,
              humanSD = stats[[1]]$humanSD,
              humanN = stats[[1]]$humanN,
              RQ = row$RQ[[1]],
              harm = row$harm[[1]],
              aiType_a = row$aiType_a[[1]],
              aiType_b = row$aiType_b[[1]],
              responsible = row$responsible[[1]],
              responsibleCat = row$responsibleCat[[1]],
              dv_synonym = row$dv_synonym[[1]],
              agent_intel = row$agent_intel[[1]],
              PMA = row$PMA[[1]],
              PMC = row$PMC[[1]],
              in_action = row$in_action[[1]],
              intent = row$intent[[1]]
            )
        },
        error = function(e) {
          message("Error in continuous study ", row$ref[[1]], " / study_id: ", row$study_id[[1]])
          message("  ", e$message)
          return(NULL)
        }
      )
    }

    # Binary descriptive
    if (!is.null(stats[[1]]$aiN.1) && !is.null(stats[[1]]$humanN.1)) {
      message("Processing binary descriptive: ", row$ref[[1]], " / study_id: ", row$study_id[[1]])
      df_out <- tryCatch(
        {
          tib <- metafor::escalc(
            measure = "OR2DL",
            ai = stats[[1]]$aiN.1, bi = stats[[1]]$aiN.0, n1i = stats[[1]]$aiN,
            ci = stats[[1]]$humanN.1, di = stats[[1]]$humanN.0, n2i = stats[[1]]$humanN,
            data = tibble(ref = row$ref[[1]], study_id = row$study_id[[1]])
          )
          tib %>%
            dplyr::mutate(
              article_id = row$article_id[[1]],
              efN_id = row$efN_id[[1]],
              dv_type = "binary",
              pct_female = row$pct_female[[1]],
              age_mean = row$age_mean[[1]],
              age_sd = row$age_sd[[1]],
              aiN = stats[[1]]$aiN,
              aiN.1 = stats[[1]]$aiN.1,
              aiN.0 = stats[[1]]$aiN.0,
              humanN = stats[[1]]$humanN,
              humanN.1 = stats[[1]]$humanN.1,
              humanN.0 = stats[[1]]$humanN.0,
              RQ = row$RQ[[1]],
              harm = row$harm[[1]],
              aiType_a = row$aiType_a[[1]],
              aiType_b = row$aiType_b[[1]],
              responsible = row$responsible[[1]],
              responsibleCat = row$responsibleCat[[1]],
              dv_synonym = row$dv_synonym[[1]],
              agent_intel = row$agent_intel[[1]],
              PMA = row$PMA[[1]],
              PMC = row$PMC[[1]],
              in_action = row$in_action[[1]],
              intent = row$intent[[1]]
            )
        },
        error = function(e) {
          message("Error in binary study ", row$ref[[1]], " / study_id: ", row$study_id[[1]])
          message("  ", e$message)
          return(NULL)
        }
      )
    }

    # -------------------------
    # Bind results to manual_desc
    # -------------------------
    if (!is.null(df_out)) {
      manual_desc <- bind_rows(manual_desc, df_out)
    } else {
      message("No df_out created for study ", row$ref[[1]], " / study_id: ", row$study_id[[1]])
    }
  }

  manual_inf <- tibble() # initialize empty

  for (i in seq_len(nrow(manual_data_df))) {
    row <- manual_data_df[i, ]
    stats <- row$manual_stats
    if (is.null(stats)) next # skip if no manual stats

    df_out <- NULL

    # F-ratio inferential studies
    if (!is.null(stats[[1]]$F_value)) {
      es <- esc_f(
        f = stats[[1]]$F_value,
        totaln = stats[[1]]$totaln,
        grp1n = stats[[1]]$N_ai,
        grp2n = stats[[1]]$N_ai,
        es.type = "g"
      )
      df_out <- tibble(
        ref = row$ref[[1]],
        study_id = row$study_id[[1]],
        article_id = row$article_id[[1]],
        efN_id = row$efN_id[[1]],
        yi = es$es,
        vi = es$var,
        dv_type = "continuous_inferential",
        pct_female = row$pct_female[[1]],
        age_mean = row$age_mean[[1]],
        age_sd = row$age_sd[[1]],
        aiN = stats[[1]]$N_ai,
        humanN = stats[[1]]$N_human,
        RQ = row$RQ[[1]],
        harm = row$harm[[1]],
        aiType_a = row$aiType_a[[1]],
        aiType_b = row$aiType_b[[1]],
        responsible = row$responsible[[1]],
        responsibleCat = row$responsibleCat[[1]],
        dv_synonym = row$dv_synonym[[1]],
        agent_intel = row$agent_intel[[1]],
        PMA = row$PMA[[1]],
        PMC = row$PMC[[1]],
        in_action = row$in_action[[1]],
        intent = row$intent[[1]]
      )
    }

    # SMD inferential studies
    if (!is.null(stats[[1]]$SMD) && !is.null(stats[[1]]$var_SMD)) {
      df_out <- tibble(
        ref = row$ref[[1]],
        study_id = row$study_id[[1]],
        article_id = row$article_id[[1]],
        efN_id = row$efN_id[[1]],
        pct_female = row$pct_female[[1]],
        age_mean = row$age_mean[[1]],
        age_sd = row$age_sd[[1]],
        yi = stats[[1]]$SMD,
        vi = stats[[1]]$var_SMD,
        dv_type = "continuous_inferential",
        aiN = stats[[1]]$aiN,
        humanN = stats[[1]]$humanN,
        RQ = row$RQ[[1]],
        harm = row$harm[[1]],
        aiType_a = row$aiType_a[[1]],
        aiType_b = row$aiType_b[[1]],
        responsible = row$responsible[[1]],
        responsibleCat = row$responsibleCat[[1]],
        dv_synonym = row$dv_synonym[[1]],
        agent_intel = row$agent_intel[[1]],
        PMA = row$PMA[[1]],
        PMC = row$PMC[[1]],
        in_action = row$in_action[[1]],
        intent = row$intent[[1]]
      )
    }

    if (!is.null(df_out)) {
      manual_inf <- bind_rows(manual_inf, df_out)
    }
  }

  manual_data_df <- bind_rows(manual_desc, manual_inf)
  # order columns
  manual_data_df <- manual_data_df %>%
    dplyr::select(
      ref, article_id, study_id, efN_id, pct_female, age_mean, age_sd, dv_type, dv_var, aiN,
      aiMean, aiSD, humanN, humanMean, humanSD, aiN.1, aiN.0,
      humanN.1, humanN.0, RQ, harm, aiType_a, aiType_b, responsible, responsibleCat, dv_synonym,
      agent_intel, PMA, PMC, in_action, intent, yi, vi
    )


  ### add in manual direction of effect

  # cassioli
  #  Humans judged as more moral than AI
  manual_data_df <- manual_data_df %>%
    dplyr::mutate(
      yi_orig = yi,
      yi = case_when(
        ref == "cassioli2023_s1" ~ -yi_orig,
        TRUE ~ yi_orig
      )
    )

  # malle 2016
  # using study 1 interpretations, we can assume that for this effect, Humans were judged as more wrong than robots. Therefore, here with AI as experimental condition, the direction of this effect is correct.

  # meder 2019
  # was coded 0 = AV, 1 = Human driver (& one aspect - staying more pronounced for AV than human) ∴ positive effect size here is correct.
  # 1. Keep original IDs for the auto studies
  full_data_avail_df$og_participant_id <- full_data_avail_df$participant_id

  # 2. Define refs that should share a participant_id
  shared_refs <- c("wilson2022_s1", "zhang2023_s1", "zhang2023_s2", "zhang2023_s3")

  # 3. Assign unique IDs to all manual rows first
  max_auto_id <- max(full_data_avail_df$participant_id, na.rm = TRUE)

  manual_data_df <- manual_data_df %>%
    arrange(ref) %>%
    mutate(
      participant_id = max_auto_id + row_number() # unique per row
    )

  # 4. Collapse IDs for shared refs: one ID per ref
  manual_data_df <- manual_data_df %>%
    group_by(ref) %>%
    mutate(
      participant_id = if (ref[1] %in% shared_refs) {
        # all rows in this ref get the same ID
        min(participant_id)
      } else {
        participant_id
      }
    ) %>%
    ungroup()
  manual_data_df
}

################################################################################
# COMBINE DATA AVAIL DF AND MANUAL DF
################################################################################
combine_manual_and_data_avail_df <- function(full_data_avail_df, full_manual_df) {
  full_data_avail_df$escalc_result <- NULL
  full_data_avail_df$pct_female <- as.numeric(full_data_avail_df$pct_female)
  full_manual_df$pct_female <- as.numeric(full_manual_df$pct_female)
  # make age_mean columns numeric in both df
  full_data_avail_df$age_mean <- as.numeric(full_data_avail_df$age_mean)
  full_manual_df$age_mean <- as.numeric(full_manual_df$age_mean)
  # make age_sd columns numeric in both df
  full_data_avail_df$age_sd <- as.numeric(full_data_avail_df$age_sd)
  full_manual_df$age_sd <- as.numeric(full_manual_df$age_sd)
  # 5. Combine
  overall_df <- bind_rows(full_data_avail_df, full_manual_df)

  # 6. Order by ref
  overall_df <- overall_df[order(overall_df$ref), ]

  # 7. Compress participant IDs to 1..N
  overall_df$participant_id <- match(overall_df$participant_id, unique(overall_df$participant_id))

  overall_df$og_participant_id <- overall_df$participant_id


  ## make efN_id unique for every row
  overall_df$og_efN_id <- overall_df$efN_id

  overall_df$efN_id <- 1:nrow(overall_df)
  overall_df
}

correct_participant_id_in_data_avail_df <- function(overall_df, full_data_avail_df) {
  ## add corrected Ps ids to full_data_avail_df (i.e. df w. only studies w. data)
  id_lookup <- overall_df %>%
    dplyr::select(og_participant_id, participant_id) %>%
    distinct()

  full_data_avail_df <- full_data_avail_df %>%
    left_join(id_lookup,
      by = c("participant_id" = "og_participant_id"),
      suffix = c("", "_new")
    ) %>%
    mutate(participant_id = participant_id_new) %>%
    select(-participant_id_new)
}
