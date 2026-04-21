run_basic_outlier_analysis <- function(df, model) {
  n_before <- nrow(df)
  df_original <- df
  df_removed <- df_original %>%
    filter(yi <= -3 | yi >= 3)
  df <- df %>%
    dplyr::filter(!(yi <= -3 | yi >= 3))
  n_after <- nrow(df)
  removed <- n_before - n_after


  message("Filtered out ", removed, " rows (", removed / n_before * 100, "%).")

  message("Removed")
  df_removed
}

run_conservative_outlier_analysis <- function(df, model) {
  df$upperci <- df$yi + 1.96 * sqrt(df$vi)
  df$lowerci <- df$yi - 1.96 * sqrt(df$vi)
  df$outlier <- df$upperci < model$ci.lb | df$lowerci > model$ci.ub
  outliers <- df %>%
    filter(outlier == TRUE)
  message(nrow(outliers), " marked as outliers with more conservative check")
  df
}

create_outlier_df_to_inspect <- function(df) {
  outliers <- df %>%
    filter(outlier == TRUE)
  outliers
  saveRDS(outliers, "outputs/outliers.rds")
}


#### influential stats
calculate_influential_stats <- function(df, model) {
  # Calculate Cook's distance
  cooks_inftlma <- cooks.distance(model)

  # Calculate dfbetas
  dfbetas_inftlma <- dfbetas(model)

  # Calculate hat values
  hatvalues_inftlma <- hatvalues(model)

  # Calculate p/k
  p_inftlma <- length(coef(model))
  k_inftlma <- nrow(df)
  if (length(coef(model)) > 1) {
    p_inftlma <- p_inftlma - 1
  }
  p_over_k_inftlma <- 3 * (p_inftlma / k_inftlma)

  # Calculate hat_flag
  hat_flag_inftlma <- ifelse(hatvalues_inftlma > p_over_k_inftlma, "TRUE", "")

  # Combine influence metrics with correct column names
  influence_stats <- data.frame(
    ref = df$ref,
    article_id = df$article_id,
    study_id = df$study_id,
    participant_id = df$participant_id,
    effect_id = df$efN_id,
    effect_size = df$yi,
    outlier = ifelse(df$outlier == FALSE, "", "TRUE"),
    cooks = cooks_inftlma,
    cooks_flag = ifelse(cooks_inftlma > 0.5, "TRUE", ""),
    dfbetas = dfbetas_inftlma,
    dfbetas_flag = ifelse(abs(dfbetas_inftlma) > 1, "TRUE", ""),
    hatvalues = hatvalues_inftlma,
    hat_flag = hat_flag_inftlma
  )

  influence_stats
}


####### distribution of outlier classification
plot_outlier_classification_distribution <- function(df, model) {
  ggplot(data = df, aes(x = yi, colour = outlier, fill = outlier)) +
    geom_histogram(alpha = 0.2, bins = 30, position = "identity") +
    geom_vline(xintercept = model$b[1], linetype = "dashed", linewidth = 1) +
    labs(
      title = "", # Distribution of Effect Sizes by Outlier Classification
      x = bquote("Effect Size (Hedge's " * italic("g") * ")"),
      y = "Study Count",
      colour = "Outlier",
      fill = "Outlier"
    ) +
    scale_colour_manual(values = c("FALSE" = "#CC6677", "TRUE" = "#009E73")) +
    scale_fill_manual(values = c("FALSE" = "#CC6677", "TRUE" = "#009E73")) +
    theme_bw() +
    theme(
      plot.title = element_text(
        family = "Georgia", # set font
        size = 11, # title size
        face = "italic",
        lineheight = 1.5 # double spacing
      ),
      axis.title = element_text(
        family = "Georgia",
        size = 10,
        lineheight = 1.5 # double spacing
      ),
      axis.text = element_text(
        family = "Georgia",
        size = 10
      ),
      legend.title = element_text(
        family = "Georgia",
        size = 10
      ),
      legend.text = element_text(
        family = "Georgia",
        size = 9
      )
    )
}

remove_verified_outliers <- function(df) {
  # hard cut off
  trimmed_df <- df %>%
    dplyr::filter(!(yi <= -3 | yi >= 3))
  # create trimmed
  trimmed_df <- trimmed_df %>%
    filter(ref != "meder2019_s1") %>%
    filter(ref != "malle2016_s2") %>%
    filter(ref != "hidalgo2021_s1")

  write.csv(trimmed_df, "data_clean/trimmed_df.csv")
  trimmed_df
}
