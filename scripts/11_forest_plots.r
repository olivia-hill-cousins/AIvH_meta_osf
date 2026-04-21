create_forest_subsets <- function(forest_df) {
  forest_df <- forest_df %>%
    # 1️⃣ Build APA reference + study number + dv_var_cap
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
      ),
      dv_var_cap = dv_var |>
        str_replace_all("\\.", " (") |>
        str_to_title() |>
        str_replace("\\)$", ")") |>
        str_replace("([A-Za-z]+) \\(", "\\1 (") |>
        paste0(if_else(str_detect(dv_var, "\\."), ")", ""))
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
      study_display = if_else(row_number() == 1, study_num, " ")
    ) %>%
    ungroup() %>%
    # 5️⃣ Build final slab label
    mutate(
      slab_labels = if_else(
        apa_ref_display == " ",
        paste0("   ", study_display), # indent studies
        paste0(apa_ref_display, " ", study_display)
      )
    )


  forest_df <- forest_df %>%
    arrange(efN_id)


  forest_df$efN_id <- as.numeric(forest_df$efN_id)
  write.csv(forest_df, "tables/forest_df.csv")

  # we have 96 unique participant_ids
  participant_ids <- sort(unique(forest_df$participant_id))

  # Split into three groups
  first_ids <- participant_ids[1:25]
  second_ids <- participant_ids[26:51]
  third_ids <- participant_ids[52:77]


  first.studies <- subset(forest_df, participant_id %in% first_ids)
  second.studies <- subset(forest_df, participant_id %in% second_ids)
  third.studies <- subset(forest_df, participant_id %in% third_ids)

  forest_subsets <- list(
    first_studies = first.studies,
    second_studies = second.studies,
    third_studies = third.studies
  )
  forest_subsets
}


create_forest_plot_for_subset <- function(
  forest_subset, name, forest_df,
  plot_title = "Forest Plot", return_summary = FALSE,
  model_version = NULL, full_model, trimmed_model
) {
  # ❗ forest_df is already a data frame — do NOT read CSV again
  # forest_df <- read.csv(forest_df)  # REMOVE THIS LINE

  # 1️⃣ Compute participant-level summary
  first_summary <- forest_subset %>%
    group_by(participant_id) %>%
    summarise(
      yi = sum(yi / vi) / sum(1 / vi),
      vi = 1 / sum(1 / vi),
      k = n(),
      apa_ref = first(apa_ref),
      study_num = first(study_num),
      dv_var_cap = first(dv_var_cap),
      slab_labels = first(slab_labels), # <-- already computed upstream
      .groups = "drop"
    )

  if (return_summary) {
    return(first_summary)
  }

  # 1️⃣ Extract labels for forest()
  slab_labels <- first_summary$slab_labels
  pIDs <- first_summary$participant_id
  n_samples <- first_summary$k

  # 2️⃣ Plot
  fig_save_name <- paste0("figures/", name, ".pdf")

  font_add("Georgia",
    regular = "Georgia.ttf",
    bold = "Georgia Bold.ttf",
    italic = "Georgia Italic.ttf",
    bolditalic = "Georgia Bold Italic.ttf"
  )

  showtext_auto()


  if (model_version == "last") {
    pdf(fig_save_name, width = 11, height = 8.27) # A4 landscape in inches
    par(mar = c(4, 4, 5, 2)) # bottom, left, top, right margins
    par(oma = c(0, 0, 0, 0)) # outer margins
    par(family = "Georgia") # if using showtext

    # extra vertical space for polygons
    with(first_summary, forest(
      yi, vi,
      slab = slab_labels,
      header = "Author (s) and Year",
      ilab = cbind(pIDs, n_samples),
      ilab.lab = c("Ps Sample", "Number of ESs"),
      ilab.xpos = c(-3, -2),
      cex = 0.8,
      pch = 22,
      col = "black",
      cex.lab = 0.8,
      cex.axis = 0.8,
      showweights = TRUE,
      ylim = c(-6, nrow(first_summary) + 3)
    ))
    abline(h = 0)

    pred <- predict(full_model)
    pred_trimmed <- predict(trimmed_model)

    addpoly(pred, row = -2.2, col = "black", addpred = TRUE)
    addpoly(pred_trimmed, row = -4.2, col = "black", addpred = TRUE)

    text(-1.45, -2.3, "Overall Model Estimate", pos = 2, cex = 0.8, font = 4)
    text(-1.45, -4.3, "Trimmed Model Estimate", pos = 2, cex = 0.8, font = 4)
    dev.off()
  } else {
    pdf(fig_save_name, width = 11, height = 8.27) # A4 landscape in inches
    par(mar = c(4, 4, 5, 2)) # bottom, left, top, right margins
    par(oma = c(0, 0, 0, 0)) # outer margins
    par(family = "Georgia") # if using showtext

    with(first_summary, forest(
      yi, vi,
      slab = slab_labels,
      header = "Author (s) and Year",
      ilab = cbind(pIDs, n_samples),
      ilab.lab = c("Ps Sample", "Number of ESs"),
      ilab.xpos = c(-3.5, -2.4),
      cex = 0.8,
      pch = 22,
      col = "black",
      cex.lab = 0.8,
      cex.axis = 0.8,
      showweights = TRUE,
      ylim = c(-1, nrow(first_summary) + 3)
    ))
    abline(h = 0)
    dev.off()
  }
}
