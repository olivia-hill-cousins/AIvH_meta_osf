##### number of studies calculated manually & in what way
summary_manual_calc_methods <- function(full_registry) {
  study_reg.df <- as.data.frame(full_registry)

  study_reg.df <- study_reg.df %>%
    dplyr::select(-manual_stats) %>%
    unnest_wider(moderator_extractors)

  # Count studies by each method/field
  method_counts <- study_reg.df %>%
    filter(!map_lgl(manual_stats, is.null)) %>%
    transmute(
      row = row_number(),
      manual_stats
    ) %>%
    rowwise() %>%
    dplyr::mutate(
      method = case_when(
        !is.null(manual_stats$F_value) ~ "F_value",
        !is.null(manual_stats$aiMean) ~ "Descriptives (Cont.)",
        !is.null(manual_stats$aiN.0) ~ "Descriptives (Binary)",
        !is.null(manual_stats$SMD) ~ "SMD",
        TRUE ~ "other"
      )
    ) %>%
    ungroup() %>%
    count(method, name = "n_studies") %>%
    arrange(desc(n_studies))
}

#### functions below are used to calculate total & group Ns, as well as N ranges, used in the manuscript
# full
calculate_total_and_group_Ns <- function(data_studies_df, inf_manual_data_df) {
  
  data_available_df <- data_studies_df
  
  data_available_df$ID_agent_map <- lapply(seq_len(nrow(data_available_df)), function(i) {
    
    data <- data_available_df$data_path[[i]]
    
    ids <- data$ID
    agents <- data$agent
    
    if (is.null(ids) || is.null(agents)) return(NULL)
    
    tibble::tibble(
      ID = ids,
      agent = agents,
      ref = data_available_df$ref[i],
      study = data_available_df$study_id[i]
    ) %>%
      dplyr::filter(!is.na(ID)) %>%
      dplyr::distinct() %>%
      dplyr::group_by(ID) %>%
      dplyr::summarise(
        ref = dplyr::first(ref),
        study = dplyr::first(study),
        agent_levels = list(unique(as.character(agent))),
        n_agent_levels = dplyr::n_distinct(agent),
        .groups = "drop"
      )
  })
  
  
  
  clean_maps <- lapply(data_available_df$ID_agent_map, function(df) {
    
    if (is.null(df)) return(NULL)
    
    df %>%
      dplyr::mutate(
        ref = as.character(ref),
        study = as.character(study),
        ID = as.character(ID),
        agent_levels = purrr::map_chr(agent_levels, ~ paste(unique(.x), collapse = "_"))
      )
  })
  
  all_ids <- dplyr::bind_rows(clean_maps) %>%
    mutate(
      ID = as.character(ID),
      agent_levels = purrr::map_chr(agent_levels, ~ paste(unique(.x), collapse = "_")),
      ID_global = paste(study, ID, sep = "_")
    ) %>%
    tidyr::separate_rows(agent_levels, sep = "_")
  
  inf_manual_data_df <- inf_manual_data_df %>%
    dplyr::filter(!(ref == "hidalgo2021_s1")) %>%
    dplyr::bind_rows(
      inf_manual_data_df %>%
        dplyr::filter(ref == "hidalgo2021_s1") %>%
        dplyr::slice(1)
    )
  
  
  manual_ns <- inf_manual_data_df %>%
    dplyr::mutate(
      ref = as.character(ref),
      study_id = as.character(study_id)
    ) %>%
    dplyr::group_by(ref, study_id) %>%
    dplyr::summarise(
      human_total_manual = sum(humanN, na.rm = TRUE),
      ai_total_manual = sum(aiN, na.rm = TRUE),
      .groups = "drop"
    )
  extracted_total <- length(unique(all_ids$ID_global))
  extracted_human <- length(unique(all_ids$ID_global[all_ids$agent_levels == "human"]))
  extracted_ai    <- length(unique(all_ids$ID_global[all_ids$agent_levels == "AI"]))
  
  manual_total <- sum(manual_ns$human_total_manual + manual_ns$ai_total_manual, na.rm = TRUE)
  manual_human <- sum(manual_ns$human_total_manual, na.rm = TRUE)
  manual_ai <- sum(manual_ns$ai_total_manual, na.rm = TRUE)
  
  out <- list(
    
    extracted = list(
      total = extracted_total,
      human_total = extracted_human,
      ai_total = extracted_ai
    ),
    
    manual = list(
      total = manual_total,
      human_total = manual_human,
      ai_total = manual_ai
    ),
    
    combined = list(
      total = extracted_total + manual_total,
      human_total = extracted_human + manual_human,
      ai_total = extracted_ai + manual_ai
    )
  )
  label <- "total_group_Ns_full"
  # build the filename using the input
  file_name <- paste0("outputs/", label, ".rds")
  
  # save the object
  saveRDS(out, file_name)
  out
}
# trimmed
calculate_total_and_group_Ns_for_trimmed <-  function(data_studies_df, inf_manual_data_df) {
    data_available_df <- data_studies_df
    
    data_available_df$ID_agent_map <- lapply(seq_len(nrow(data_available_df)), function(i) {
      
      data <- data_available_df$data_path[[i]]
      
      ids <- data$ID
      agents <- data$agent
      
      if (is.null(ids) || is.null(agents)) return(NULL)
      
      tibble::tibble(
        ID = ids,
        agent = agents,
        ref = data_available_df$ref[i],
        study = data_available_df$study_id[i]
      ) %>%
        dplyr::filter(!is.na(ID)) %>%
        dplyr::distinct() %>%
        dplyr::group_by(ID) %>%
        dplyr::summarise(
          ref = dplyr::first(ref),
          study = dplyr::first(study),
          agent_levels = list(unique(as.character(agent))),
          n_agent_levels = dplyr::n_distinct(agent),
          .groups = "drop"
        )
    })
    
    
    
    clean_maps <- lapply(data_available_df$ID_agent_map, function(df) {
      
      if (is.null(df)) return(NULL)
      
      df %>%
        dplyr::mutate(
          ref = as.character(ref),
          study = as.character(study),
          ID = as.character(ID),
          agent_levels = purrr::map_chr(agent_levels, ~ paste(unique(.x), collapse = "_"))
        )
    })
    
    all_ids <- dplyr::bind_rows(clean_maps) %>%
      mutate(
        ID = as.character(ID),
        agent_levels = purrr::map_chr(agent_levels, ~ paste(unique(.x), collapse = "_")),
        ID_global = paste(study, ID, sep = "_")
      ) %>%
      tidyr::separate_rows(agent_levels, sep = "_")
    
    
    
    
    manual_ns <- inf_manual_data_df %>%
      dplyr::mutate(
        ref = as.character(ref),
        study_id = as.character(study_id)
      ) %>%
      dplyr::group_by(ref, study_id) %>%
      dplyr::summarise(
        human_total_manual = sum(humanN, na.rm = TRUE),
        ai_total_manual = sum(aiN, na.rm = TRUE),
        .groups = "drop"
      )  %>%
      filter(ref != "meder2019_s1") %>%
      filter(ref != "malle2016_s2") %>%
      filter(ref != "hidalgo2021_s1")
    extracted_total <- length(unique(all_ids$ID_global))
    extracted_human <- length(unique(all_ids$ID_global[all_ids$agent_levels == "human"]))
    extracted_ai    <- length(unique(all_ids$ID_global[all_ids$agent_levels == "AI"]))
    
    manual_total <- sum(manual_ns$human_total_manual + manual_ns$ai_total_manual, na.rm = TRUE)
    manual_human <- sum(manual_ns$human_total_manual, na.rm = TRUE)
    manual_ai <- sum(manual_ns$ai_total_manual, na.rm = TRUE)
    
    out <- list(
      
      extracted = list(
        total = extracted_total,
        human_total = extracted_human,
        ai_total = extracted_ai
      ),
      
      manual = list(
        total = manual_total,
        human_total = manual_human,
        ai_total = manual_ai
      ),
      
      combined = list(
        total = extracted_total + manual_total,
        human_total = extracted_human + manual_human,
        ai_total = extracted_ai + manual_ai
      )
    )
    label <- "total_group_Ns_trimmed"
    # build the filename using the input
    file_name <- paste0("outputs/", label, ".rds")
    
    # save the object
    saveRDS(out, file_name)
    out
}

# full
calculate_per_study_N_ranges_trimmed <- function(data_studies_df, inf_manual_data_df) {
  
  # -----------------------------
  # 1. Build ID → agent map (extracted)
  # -----------------------------
  data_available_df <- data_studies_df
  
  data_available_df$ID_agent_map <- lapply(seq_len(nrow(data_available_df)), function(i) {
    
    data <- data_available_df$data_path[[i]]
    ids <- data$ID
    agents <- data$agent
    
    if (is.null(ids) || is.null(agents)) return(NULL)
    
    tibble::tibble(
      ID    = ids,
      agent = agents,
      ref   = data_available_df$ref[i],
      study = data_available_df$study_id[i]
    ) %>%
      dplyr::filter(!is.na(ID)) %>%
      dplyr::distinct() %>%
      dplyr::group_by(ID) %>%
      dplyr::summarise(
        ref          = dplyr::first(ref),
        study        = dplyr::first(study),
        agent_levels = list(unique(as.character(agent))),
        .groups      = "drop"
      )
  })
  
  
  # -----------------------------
  # 2. Clean maps (extracted)
  # -----------------------------
  clean_maps <- lapply(data_available_df$ID_agent_map, function(df) {
    if (is.null(df)) return(NULL)
    
    df %>%
      dplyr::mutate(
        ref          = as.character(ref),
        study        = as.character(study),
        ID           = as.character(ID),
        agent_levels = purrr::map_chr(agent_levels, ~ paste(unique(.x), collapse = "_"))
      )
  })
  
  
  # -----------------------------
  # 3. Flatten into all_ids (extracted)
  # -----------------------------
  all_ids <- dplyr::bind_rows(clean_maps) %>%
    dplyr::mutate(
      ID           = as.character(ID),
      agent_levels = purrr::map_chr(agent_levels, ~ paste(unique(.x), collapse = "_")),
      ID_global    = paste(study, ID, sep = "_")
    ) %>%
    tidyr::separate_rows(agent_levels, sep = "_")
  
  
  # -----------------------------
  # 4. Extracted per‑study Ns
  # -----------------------------
  extracted_per_study <- all_ids %>%
    dplyr::group_by(study) %>%
    dplyr::summarise(
      extracted_total_N = dplyr::n_distinct(ID),
      extracted_human_N = dplyr::n_distinct(ID[agent_levels == "human"]),
      extracted_ai_N    = dplyr::n_distinct(ID[agent_levels == "AI"]),
      .groups = "drop"
    )
  
  
  # -----------------------------
  # 5. Manual per‑study Ns
  # -----------------------------
  manual_per_study <- inf_manual_data_df %>%
    dplyr::mutate(
      ref   = as.character(ref),
      study = as.character(study_id)
    ) %>%
    dplyr::group_by(study) %>%   # collapse across refs
    dplyr::summarise(
      manual_human_N = sum(humanN, na.rm = TRUE),
      manual_ai_N    = sum(aiN,    na.rm = TRUE),
      manual_total_N = manual_human_N + manual_ai_N,
      .groups = "drop"
    ) %>%
    dplyr::filter(!study %in% c("meder2019_s1", "malle2016_s2", "hidalgo2021_s1"))
  
  
  # -----------------------------
  # 6. Combine extracted + manual per study
  # -----------------------------
  combined_per_study <- extracted_per_study %>%
    dplyr::left_join(manual_per_study, by = "study") %>%
    dplyr::mutate(
      combined_total_N = extracted_total_N + dplyr::coalesce(manual_total_N, 0),
      combined_human_N = extracted_human_N + dplyr::coalesce(manual_human_N, 0),
      combined_ai_N    = extracted_ai_N    + dplyr::coalesce(manual_ai_N, 0)
    )
  
  
  # -----------------------------
  # 7. Ranges for extracted, manual, combined
  # -----------------------------
  extracted_ranges <- extracted_per_study %>%
    dplyr::summarise(
      total_min = min(extracted_total_N),
      total_max = max(extracted_total_N),
      human_min = min(extracted_human_N),
      human_max = max(extracted_human_N),
      ai_min    = min(extracted_ai_N),
      ai_max    = max(extracted_ai_N)
    )
  
  manual_ranges <- manual_per_study %>%
    dplyr::summarise(
      total_min = min(manual_total_N),
      total_max = max(manual_total_N),
      human_min = min(manual_human_N),
      human_max = max(manual_human_N),
      ai_min    = min(manual_ai_N),
      ai_max    = max(manual_ai_N)
    )
  
  combined_ranges <- combined_per_study %>%
    dplyr::summarise(
      total_min = min(combined_total_N),
      total_max = max(combined_total_N),
      human_min = min(combined_human_N),
      human_max = max(combined_human_N),
      ai_min    = min(combined_ai_N),
      ai_max    = max(combined_ai_N)
    )
  
  
  # -----------------------------
  # 8. Return everything cleanly
  # -----------------------------
  out <- list(
    extracted_per_study = extracted_per_study,
    manual_per_study    = manual_per_study,
    combined_per_study  = combined_per_study,
    
    extracted_ranges = extracted_ranges,
    manual_ranges    = manual_ranges,
    combined_ranges  = combined_ranges
  )
  
  label <- "min_max_Ns_trimmed"
  # build the filename using the input
  file_name <- paste0("outputs/", label, ".rds")
  
  # save the object
  saveRDS(out, file_name)
  out
}
# trimmed
calculate_per_study_N_ranges_full <- function(data_studies_df, inf_manual_data_df) {
  
  # -----------------------------
  # 1. Build ID → agent map (extracted)
  # -----------------------------
  data_available_df <- data_studies_df
  
  data_available_df$ID_agent_map <- lapply(seq_len(nrow(data_available_df)), function(i) {
    
    data <- data_available_df$data_path[[i]]
    ids <- data$ID
    agents <- data$agent
    
    if (is.null(ids) || is.null(agents)) return(NULL)
    
    tibble::tibble(
      ID    = ids,
      agent = agents,
      ref   = data_available_df$ref[i],
      study = data_available_df$study_id[i]
    ) %>%
      dplyr::filter(!is.na(ID)) %>%
      dplyr::distinct() %>%
      dplyr::group_by(ID) %>%
      dplyr::summarise(
        ref          = dplyr::first(ref),
        study        = dplyr::first(study),
        agent_levels = list(unique(as.character(agent))),
        .groups      = "drop"
      )
  })
  
  
  # -----------------------------
  # 2. Clean maps (extracted)
  # -----------------------------
  clean_maps <- lapply(data_available_df$ID_agent_map, function(df) {
    if (is.null(df)) return(NULL)
    
    df %>%
      dplyr::mutate(
        ref          = as.character(ref),
        study        = as.character(study),
        ID           = as.character(ID),
        agent_levels = purrr::map_chr(agent_levels, ~ paste(unique(.x), collapse = "_"))
      )
  })
  
  
  # -----------------------------
  # 3. Flatten into all_ids (extracted)
  # -----------------------------
  all_ids <- dplyr::bind_rows(clean_maps) %>%
    dplyr::mutate(
      ID           = as.character(ID),
      agent_levels = purrr::map_chr(agent_levels, ~ paste(unique(.x), collapse = "_")),
      ID_global    = paste(study, ID, sep = "_")
    ) %>%
    tidyr::separate_rows(agent_levels, sep = "_")
  
  
  # -----------------------------
  # 4. Extracted per‑study Ns
  # -----------------------------
  extracted_per_study <- all_ids %>%
    dplyr::group_by(study) %>%
    dplyr::summarise(
      extracted_total_N = dplyr::n_distinct(ID),
      extracted_human_N = dplyr::n_distinct(ID[agent_levels == "human"]),
      extracted_ai_N    = dplyr::n_distinct(ID[agent_levels == "AI"]),
      .groups = "drop"
    )
  
  
  # -----------------------------
  # 5. Manual per‑study Ns (NO EXCLUSIONS)
  # -----------------------------
  manual_per_study <- inf_manual_data_df %>%
    dplyr::mutate(
      ref   = as.character(ref),
      study = as.character(study_id)
    ) %>%
    dplyr::group_by(study) %>%
    dplyr::summarise(
      manual_human_N = sum(humanN, na.rm = TRUE),
      manual_ai_N    = sum(aiN,    na.rm = TRUE),
      manual_total_N = manual_human_N + manual_ai_N,
      .groups = "drop"
    )
  
  
  # -----------------------------
  # 6. Combine extracted + manual per study
  # -----------------------------
  combined_per_study <- extracted_per_study %>%
    dplyr::left_join(manual_per_study, by = "study") %>%
    dplyr::mutate(
      manual_total_N = dplyr::coalesce(manual_total_N, 0),
      manual_human_N = dplyr::coalesce(manual_human_N, 0),
      manual_ai_N    = dplyr::coalesce(manual_ai_N, 0),
      
      combined_total_N = extracted_total_N + manual_total_N,
      combined_human_N = extracted_human_N + manual_human_N,
      combined_ai_N    = extracted_ai_N    + manual_ai_N
    )
  
  
  # -----------------------------
  # 7. Ranges for extracted, manual, combined
  # -----------------------------
  extracted_ranges <- extracted_per_study %>%
    dplyr::summarise(
      total_min = min(extracted_total_N),
      total_max = max(extracted_total_N),
      human_min = min(extracted_human_N),
      human_max = max(extracted_human_N),
      ai_min    = min(extracted_ai_N),
      ai_max    = max(extracted_ai_N)
    )
  
  manual_ranges <- manual_per_study %>%
    dplyr::summarise(
      total_min = min(manual_total_N),
      total_max = max(manual_total_N),
      human_min = min(manual_human_N),
      human_max = max(manual_human_N),
      ai_min    = min(manual_ai_N),
      ai_max    = max(manual_ai_N)
    )
  
  combined_ranges <- combined_per_study %>%
    dplyr::summarise(
      total_min = min(combined_total_N),
      total_max = max(combined_total_N),
      human_min = min(combined_human_N),
      human_max = max(combined_human_N),
      ai_min    = min(combined_ai_N),
      ai_max    = max(combined_ai_N)
    )
  
  
  # -----------------------------
  # 8. Return everything cleanly
  # -----------------------------
  out <- list(
    extracted_per_study = extracted_per_study,
    manual_per_study    = manual_per_study,
    combined_per_study  = combined_per_study,
    
    extracted_ranges = extracted_ranges,
    manual_ranges    = manual_ranges,
    combined_ranges  = combined_ranges
  )
  
  label <- "min_max_Ns_full"
  # build the filename using the input
  file_name <- paste0("outputs/", label, ".rds")
  
  # save the object
  saveRDS(out, file_name)
  out
  
}
