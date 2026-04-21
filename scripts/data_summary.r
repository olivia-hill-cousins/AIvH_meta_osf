##### number of studies calculated manually & in what way
summary_manual_calc_methods <- function(full_registry) {
  study_reg.df <- as.data.frame(full_registry)

  study_reg.df <- study_reg.df %>%
    dplyr::select(-manual_stats) %>%
    unnest_wider(moderator_extractors)

  # Unnest all manual_stats to see what fields exist
  # manual_fields <- study_reg.df %>%
  # filter(!map_lgl(manual_stats, is.null)) %>%
  # pull(manual_stats) %>%
  # map(~ names(.x)) %>%
  # unlist() %>%
  # unique() %>%
  # sort()

  ## print("All manual_stats fields found:")
  ## print(manual_fields)

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


###############################################
# EXTRACTING VALUES FROM MAIN MODELS
###############################################
