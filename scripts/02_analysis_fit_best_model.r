fit_fourlvl <- function(df, label) {
  model <- rma.mv(yi, vi,
    random = ~ 1 | study_id / participant_id / efN_id,
    method = "REML", test = "t", dfs = "contain",
    data = df
  )
  # build the filename using the input
  file_name <- paste0("outputs/", label, ".rds")

  # save the object
  saveRDS(model, file_name)

  model
}

fit_fourlvl_article <- function(df, label) {
  model <- rma.mv(yi, vi,
    random = ~ 1 | article_id / study_id / participant_id,
    method = "REML", test = "t", dfs = "contain",
    data = df
  )
  # build the filename using the input
  file_name <- paste0("outputs/", label, ".rds")

  # save the object
  saveRDS(model, file_name)
  model
}

fit_threelvl_article_pID <- function(df, label) {
  model <- rma.mv(yi,
    vi,
    random = ~ 1 | article_id / participant_id,
    method = "REML",
    test = "t",
    dfs = "contain",
    data = df
  )
  # build the filename using the input
  file_name <- paste0("outputs/", label, ".rds")

  # save the object
  saveRDS(model, file_name)
  model
}

fit_threelvl_pID <- function(df, label) {
  model <- rma.mv(yi, vi,
    random = ~ 1 | participant_id / efN_id,
    method = "REML", test = "t", dfs = "contain",
    data = df
  )
  # build the filename using the input
  file_name <- paste0("outputs/", label, ".rds")

  # save the object
  saveRDS(model, file_name)
  model
}

fit_threelvl_study <- function(df, label) {
  model <- rma.mv(yi, vi,
    random = ~ 1 | study_id / efN_id,
    method = "REML", test = "t", dfs = "contain",
    data = df
  )
  # build the filename using the input
  file_name <- paste0("outputs/", label, ".rds")

  # save the object
  saveRDS(model, file_name)
  model
}

fit_threelvl_article <- function(df, label) {
  model <- rma.mv(yi, vi,
    random = ~ 1 | article_id / efN_id,
    method = "REML", test = "t", dfs = "contain",
    data = df
  )
  # build the filename using the input
  file_name <- paste0("outputs/", label, ".rds")

  # save the object
  saveRDS(model, file_name)
  model
}

fit_threelvl <- function(df, label) {
  model <- rma.mv(yi, vi,
    random = ~ 1 | study_id / participant_id,
    method = "REML", test = "t", dfs = "contain",
    data = df
  )
  # build the filename using the input
  file_name <- paste0("outputs/", label, ".rds")

  # save the object
  saveRDS(model, file_name)
  model
}

fit_twolvl <- function(df, label) {
  model <- rma.mv(yi, vi,
    random = ~ 1 | efN_id,
    method = "REML", test = "t", dfs = "contain",
    data = df
  )
  # build the filename using the input
  file_name <- paste0("outputs/", label, ".rds")

  # save the object
  saveRDS(model, file_name)
  model
}

fit_twolvl_pID <- function(df, label) {
  model <- rma.mv(yi, vi,
    random = ~ 1 | participant_id,
    method = "REML", test = "t", dfs = "contain",
    data = df
  )
  # build the filename using the input
  file_name <- paste0("outputs/", label, ".rds")

  # save the object
  saveRDS(model, file_name)
  model
}

fit_twolvl_study <- function(df, label) {
  model <- rma.mv(yi, vi,
    random = ~ 1 | study_id,
    method = "REML", test = "t", dfs = "contain",
    data = df
  )
  # build the filename using the input
  file_name <- paste0("outputs/", label, ".rds")

  # save the object
  saveRDS(model, file_name)
  model
}

fit_onelvl <- function(df, label) {
  model <- rma.mv(yi, vi,
    method = "REML", test = "t", dfs = "contain",
    data = df
  )
  # build the filename using the input
  file_name <- paste0("outputs/", label, ".rds")

  # save the object
  saveRDS(model, file_name)
  model
}

### manually compute values that compare_performance() would give
compare_all_models <- function(
    fourlvl,
    fourlvl_article,
    threelvl_article_pID,
    threelvl_pID,
    threelvl_study,
    threelvl_article,
    threelvl,
    twolvl,
    twolvl_pID,
    twolvl_study,
    onelvl
) {
  
  message(
    "\nIMPORTANT: these values are calculated manually. If this message shows, please re-run the compare_performance() outside of the workflow in the 02_analysis_fit_best_model.r script instead.\n"
  )
  
  # ---- file paths ----
  fourlvl <- paste0("outputs/", fourlvl, ".rds")
  fourlvl_article <- paste0("outputs/", fourlvl_article, ".rds")
  threelvl_article_pID <- paste0("outputs/", threelvl_article_pID, ".rds")
  threelvl_pID <- paste0("outputs/", threelvl_pID, ".rds")
  threelvl_study <- paste0("outputs/", threelvl_study, ".rds")
  threelvl_article <- paste0("outputs/", threelvl_article, ".rds")
  threelvl <- paste0("outputs/", threelvl, ".rds")
  twolvl <- paste0("outputs/", twolvl, ".rds")
  twolvl_pID <- paste0("outputs/", twolvl_pID, ".rds")
  twolvl_study <- paste0("outputs/", twolvl_study, ".rds")
  onelvl <- paste0("outputs/", onelvl, ".rds")
  
  model_list <- list(
    fourlvl = fourlvl,
    fourlvl_article = fourlvl_article,
    threelvl_article_pID = threelvl_article_pID,
    threelvl_pID = threelvl_pID,
    threelvl_study = threelvl_study,
    threelvl_article = threelvl_article,
    threelvl = threelvl,
    twolvl = twolvl,
    twolvl_pID = twolvl_pID,
    twolvl_study = twolvl_study,
    onelvl = onelvl
  )
  
  # ---- load models ----
  models <- lapply(model_list, readRDS)
  
  # ---- performance ----
  performance_list <- lapply(models, model_performance)
  performance_df <- dplyr::bind_rows(performance_list, .id = "Name")
  
  # ---- logLik ----
  logLik_vec <- purrr::map_dbl(models, ~ as.numeric(logLik(.x)))
  names(logLik_vec) <- names(model_list)
  
  # ---- Hedges g (model intercept) ----
  g_vec <- purrr::map_dbl(models, function(m) {
    as.numeric(m$b[1, 1])
  })
  names(g_vec) <- names(model_list)
  
  performance_df <- performance_df %>%
    dplyr::mutate(
      logLik = logLik_vec[Name],
      g = g_vec[Name]
    )
  
  # ---- variance components (WIDE FORMAT) ----
  variance_components_long <- purrr::map_dfr(
    models,
    function(m) {
      
      sigma <- m$sigma2
      
      if (is.null(sigma)) {
        return(data.frame(
          Name = NA_character_,
          var = NA_character_,
          sigma2 = NA_real_
        ))
      }
      
      names_vec <- names(sigma)
      
      if (is.null(names_vec) || any(names_vec == "")) {
        names_vec <- paste0("sigma2_", seq_along(sigma))
      }
      
      data.frame(
        var = names_vec,
        sigma2 = as.numeric(sigma)
      )
    },
    .id = "Name"
  )
  
  # ---- pivot to wide ----
  variance_components_wide <- variance_components_long %>%
    tidyr::pivot_wider(
      names_from = var,
      values_from = sigma2,
      names_prefix = ""
    )
  
  # ---- merge ----
  performance_df <- performance_df %>%
    dplyr::left_join(variance_components_wide, by = "Name") %>%
    dplyr::select(Name, everything())
  
  performance_df <- performance_df %>%
    select(
      Name, AIC, BIC, logLik, g, sigma2_1, sigma2_2, sigma2_3         
    ) %>% 
    rename(
      `Model Fit Index_AIC` = AIC,
      `Model Fit Index_BIC` = BIC,
      `Model Fit Index_logLik` = logLik,
      "\\sigma²_1" = sigma2_1, 
      "\\sigma²_2" = sigma2_2,
      "\\sigma²_3" = sigma2_3
    ) %>% 
    mutate(
      `Model Fit Index_AIC` = round(`Model Fit Index_AIC`, 2),
      g = round(g, 2),
      `Model Fit Index_BIC` = round(`Model Fit Index_BIC`, 2),
      `Model Fit Index_logLik` = round(`Model Fit Index_logLik`, 2),
      `\\sigma²_1` = round(`\\sigma²_1`, 3),
      `\\sigma²_2` = round(`\\sigma²_2`, 3),
      `\\sigma²_3` = round(`\\sigma²_3`, 3),
      blank1 = "",
      blank2 = "",
      blank3 = ""
    ) %>% 
    select(Name, blank1, 
           `Model Fit Index_AIC`, `Model Fit Index_BIC`, `Model Fit Index_logLik`, blank2, 
           g, blank3, 
           `\\sigma²_1`, `\\sigma²_2`, `\\sigma²_3`
      
    )
  
  performance_df
  
  
  
}

######### compare winners
compare_model_comparison_winners_anova <- function(winner1, winner2) {
  anova.rma(winner1, winner2)
}

## create function for fitting model (and moderators)
fit_selected_model_structure <- function(mod = NULL, dat = df) {
  if (is.null(mod)) {
    model <- rma.mv(yi, vi, data = dat, method = "REML", test = "t", dfs = "contain", random = ~ 1 | participant_id / efN_id)
  } else {
    model <- rma.mv(yi, vi, data = dat, method = "REML", test = "t", dfs = "contain", random = ~ 1 | participant_id / efN_id, mods = mod)
  }
  return(model)
}
################################################################################
# MODEL COMPARISON OUTSIDE OF WORKFLOW
################################################################################
# tar_load(combined_df)
# df <- combined_df
# fourlvl <-  rma.mv(yi,
#                    vi,
#                    random=~1|study_id/participant_id/efN_id,
#                    method="REML",
#                    test="t",
#                    dfs="contain",
#                    data=df)
# # ##print(summary(fourlvl,digits=3))
# #
# fourlvl_article <-  rma.mv(yi,
#                            vi,
#                            random=~1|article_id/study_id/participant_id,
#                            method="REML",
#                            test="t",
#                            dfs="contain",
#                            data=df)
# # ##print(summary(fourlvl.article,digits=3))
# #
# threelvl_pID <- rma.mv(yi,
#                        vi,
#                        random=~1|participant_id/efN_id,
#                        method="REML",
#                        test="t",
#                        dfs="contain",
#                        data=df)
# # ##print(summary(threelvl_pID,digits=3))
#
# ## best model twolvl_pID (threelvl_pID actually a bit less, and fourlvl even less than that)
#
# ## test w. article ID level
# threelvl_article_pID <- rma.mv(yi,
#                                vi,
#                                random=~1|article_id/participant_id,
#                                method="REML",
#                                test="t",
#                                dfs="contain",
#                                data=df)
# # #
# threelvl_study <- rma.mv(yi,
#                          vi,
#                          random=~1|study_id/efN_id,
#                          method="REML",
#                          test="t",
#                          dfs="contain",
#                          data=df)
# #
# #
# threelvl_article <- rma.mv(yi,
#                            vi,
#                            random=~1|article_id/efN_id,
#                            method="REML",
#                            test="t",
#                            dfs="contain",
#                            data=df)
# #
# #
# threelvl <- rma.mv(yi,
#                    vi,
#                    random=~1|study_id/participant_id,
#                    method="REML",
#                    test="t",
#                    dfs="contain",
#                    data=df)
#
# twolvl <- rma.mv(yi,
#                  vi,
#                  random=~1|efN_id,
#                  method="REML",
#                  test="t",
#                  dfs="contain",
#                  data=df)
#
#
# twolvl_pID <- rma.mv(yi,
#                      vi,
#                      random=~1|participant_id,
#                      method="REML",
#                      test="t",
#                      dfs="contain",
#                      data=df)
#
#
#
# twolvl_study <- rma.mv(yi,
#                        vi,
#                        random=~1|study_id,
#                        method="REML",
#                        test="t",
#                        dfs="contain",
#                        data=df)
#
#
# onelvl <- rma.mv(yi,
#                  vi,
#                  method="REML",
#                  test="t",
#                  dfs="contain",
#                  data=df)
# #
# #
#
# model_comp <- compare_performance(onelvl, twolvl_study, twolvl_pID, twolvl, threelvl_article, threelvl_study, threelvl_pID, threelvl, fourlvl_article, fourlvl)
# print(model_comp)
#
# saveRDS(model_comp, "outputs/combined_diff_model_comparison.rds")


################################################################################
# MODEL COMPARISON FOR TRIMMED DATA OUTSIDE OF WORKFLOW
################################################################################
# tar_load(trimmed_df)
# fourlvl <-  rma.mv(yi,
#                    vi,
#                    random=~1|study_id/participant_id/efN_id,
#                    method="REML",
#                    test="t",
#                    dfs="contain",
#                    data=trimmed_df)
#
# fourlvl_article <-  rma.mv(yi,
#                            vi,
#                            random=~1|article_id/study_id/participant_id,
#                            method="REML",
#                            test="t",
#                            dfs="contain",
#                            data=trimmed_df)
#
# threelvl_pID <- rma.mv(yi,
#                        vi,
#                        random=~1|participant_id/efN_id,
#                        method="REML",
#                        test="t",
#                        dfs="contain",
#                        data=trimmed_df)
#
#
# ## best model twolvl_pID (threelvl_pID actually a bit less, and fourlvl even less than that)
#
# ## test w. article ID level
# threelvl_article_pID <- rma.mv(yi,
#                                vi,
#                                random=~1|article_id/participant_id,
#                                method="REML",
#                                test="t",
#                                dfs="contain",
#                                data=trimmed_df)
# # #
# threelvl_study <- rma.mv(yi,
#                          vi,
#                          random=~1|study_id/efN_id,
#                          method="REML",
#                          test="t",
#                          dfs="contain",
#                          data=trimmed_df)
# #
# #
# threelvl_article <- rma.mv(yi,
#                            vi,
#                            random=~1|article_id/efN_id,
#                            method="REML",
#                            test="t",
#                            dfs="contain",
#                            data=trimmed_df)
# #
# #
# threelvl <- rma.mv(yi,
#                    vi,
#                    random=~1|study_id/participant_id,
#                    method="REML",
#                    test="t",
#                    dfs="contain",
#                    data=trimmed_df)
#
# twolvl <- rma.mv(yi,
#                  vi,
#                  random=~1|efN_id,
#                  method="REML",
#                  test="t",
#                  dfs="contain",
#                  data=trimmed_df)
#
#
# twolvl_pID <- rma.mv(yi,
#                      vi,
#                      random=~1|participant_id,
#                      method="REML",
#                      test="t",
#                      dfs="contain",
#                      data=trimmed_df)
#
#
#
# twolvl_study <- rma.mv(yi,
#                        vi,
#                        random=~1|study_id,
#                        method="REML",
#                        test="t",
#                        dfs="contain",
#                        data=trimmed_df)
#
#
# onelvl <- rma.mv(yi,
#                  vi,
#                  method="REML",
#                  test="t",
#                  dfs="contain",
#                  data=trimmed_df)
# #
# #
#
# model_comp <- compare_performance(onelvl, twolvl_study, twolvl_pID, twolvl, threelvl_article, threelvl_study, threelvl_pID, threelvl, fourlvl_article, fourlvl)
# print(model_comp)
#
# saveRDS(model_comp, "outputs/trimmed_diff_model_comparison.rds")

####### Prep table
create_model_fit_table <- function(threelvl_article_pID,
                                   threelvl_pID,
                                   threelvl,
                                   twolvl_pID,
                                   onelvl) {
  message(
    "\nIMPORTANT: these values are calculated manually. If this message shows, please re-run the compare_performance() outside of the workflow in the 02_analysis_fit_best_model.r script instead.\n"
  )
  threelvl_article_pID <- paste0("outputs/", threelvl_article_pID, ".rds")
  threelvl_pID <- paste0("outputs/", threelvl_pID, ".rds")
  threelvl <- paste0("outputs/", threelvl, ".rds")
  twolvl_pID <- paste0("outputs/", twolvl_pID, ".rds")
  onelvl <- paste0("outputs/", onelvl, ".rds")


  models <- list(
    onelvl = onelvl,
    twolvl_pID = twolvl_pID,
    threelvl_article_pID = threelvl_article_pID,
    threelvl = threelvl,
    threelvl_pID = threelvl_pID
  )

  models <- lapply(models, readRDS) # now each element is an rma.mv object
  # 2. Compute model performance
  performance_list <- lapply(models, model_performance)

  # 3. Combine into one data frame with Name column
  compThree <- bind_rows(performance_list, .id = "Name")

  # 4. Optionally reorder columns
  compThree <- compThree %>% dplyr::select(Name, everything())

  # reassign properly
  onelvl <- models$onelvl
  twolvl_pID <- models$twolvl_pID
  threelvl_article_pID <- models$threelvl_article_pID
  threelvl <- models$threelvl
  threelvl_pID <- models$threelvl_pID

  comp.onextwo <- anova.rma(onelvl, twolvl_pID)
  comp.twoxthree_es <- anova.rma(twolvl_pID, threelvl_pID)
  comp.twoxthree_s <- anova.rma(twolvl_pID, threelvl)
  comp.twoxthree_a <- anova.rma(twolvl_pID, threelvl_article_pID)

  # rounding function
  round2 <- function(x) ifelse(is.na(x), NA, sprintf("%.2f", round(x, 2)))
  round3 <- function(x) ifelse(is.na(x), NA, sprintf("%.3f", round(x, 3)))
  format_p <- function(p) {
    ifelse(is.na(p), NA, ifelse(p < .001, "< .001", sprintf("%.3f", round(p, 3))))
  }

  # summary
  objects_toTidy <- list(one = list(model = onelvl, level_added = "none"), two = list(model = twolvl_pID, level_added = "Participants"), three_es = list(model = threelvl_pID, level_added = "Effect Size"), three_s = list(model = threelvl, level_added = "Study"), three_a = list(model = threelvl_article_pID, level_added = "Article"))

  # --- Prepare fit_indices safely (keep numeric for calculations) ---
  fit_indices <- compThree %>%
    dplyr::mutate(
      name = as.character(Name),
      AIC = as.numeric(AIC),
      level_added = case_when(
        name == "onelvl" ~ "None",
        name == "twolvl_pID" ~ "Participants",
        name == "threelvl_pID" ~ "Effect Size",
        name == "threelvl" ~ "Study",
        name == "threelvl_article_pID" ~ "Article",
        TRUE ~ NA_character_
      ),
      Model = case_when(
        name == "onelvl" ~ "One",
        name == "twolvl_pID" ~ "Two",
        name == "threelvl_pID" ~ "Three (Opt. 1)",
        name == "threelvl" ~ "Three (Opt. 2)",
        name == "threelvl_article_pID" ~ "Three (Opt. 3)",
        TRUE ~ NA_character_
      )
    )

  fits <- fit_indices %>%
    dplyr::mutate(logLik = case_when(
      Model == "One" ~ comp.onextwo[["fit.stats.r"]][["ll"]],
      Model == "Two" ~ comp.onextwo[["fit.stats.f"]][["ll"]],
      Model == "Three (Opt. 1)" ~ comp.twoxthree_es[["fit.stats.f"]][["ll"]],
      Model == "Three (Opt. 2)" ~ comp.twoxthree_s[["fit.stats.f"]][["ll"]],
      Model == "Three (Opt. 3)" ~ comp.twoxthree_a[["fit.stats.f"]][["ll"]]
    ))

  fits <- fits %>%
    dplyr::mutate(
      logLik = round2(logLik),
      AIC = round2(AIC)
    )

  ## create variance components df
  variance_components <- lapply(objects_toTidy, function(x) {
    m <- x$model
    if (is.null(m$sigma2) || length(m$sigma2) == 0) {
      return(data.frame(variance_component = NA, sigma2 = NA))
    }
    # Fix naming logic here:
    names_vec <- if (is.null(names(m$sigma2)) || any(nzchar(names(m$sigma2)) == FALSE)) paste0("sigma", seq_along(m$sigma2)) else names(m$sigma2)
    data.frame(variance_component = names_vec, sigma2 = m$sigma2, stringsAsFactors = FALSE)
  }) |> bind_rows(.id = "Model")


  variance_components <- variance_components %>%
    dplyr::mutate(
      sigma2 = round3(sigma2),
      Model = str_to_title(Model)
    )


  # Merge
  table_core <- fits %>%
    left_join(variance_components, by = "Model") %>%
    dplyr::mutate(g = NA) # placeholder for your effect size

  model_comparisons <- bind_rows(data.frame(comparison = "1 vs 2", LRT = comp.onextwo$LRT, p = comp.onextwo$pval), data.frame(comparison = "2 vs 3 (Opt. 1)", LRT = comp.twoxthree_es$LRT, p = comp.twoxthree_es$pval), data.frame(comparison = "2 vs 3 (Opt. 2)", LRT = comp.twoxthree_s$LRT, p = comp.twoxthree_s$pval), data.frame(comparison = "2 vs 3 (Opt. 3)", LRT = comp.twoxthree_a$LRT, p = comp.twoxthree_a$pval))


  fits <- as.data.frame(fits)
  fits <- fits %>%
    dplyr::select(Model, level_added, AIC, logLik)
  model_comparisons <- as.data.frame(model_comparisons)
  Model <- c("Two", "Three (Opt. 1)", "Three (Opt. 2)", "Three (Opt. 3)")
  Model <- as.data.frame(Model)
  model_comparisons <- cbind(Model, model_comparisons)
  one_row <- data.frame(
    Model = "One",
    comparison = NA,
    LRT = NA,
    p = NA
  )
  model_comparisons <- rbind(one_row, model_comparisons)

  variance_components <- as.data.frame(variance_components)

  variance_components <- variance_components %>% pivot_wider(names_from = variance_component, values_from = sigma2)
  model <- c("One", "Two", "Three (Opt. 1)", "Three (Opt. 2)", "Three (Opt. 3)")
  variance_components <- cbind(model, variance_components)
  variance_components$Model <- variance_components$model
  variance_components <- variance_components %>%
    dplyr::select(Model, sigma1, sigma2)
  table_comp <- left_join(fits, model_comparisons, by = join_by(Model))
  # add effect size for each model in df
  effect_sizes_vector <- c(onelvl[["beta"]], twolvl_pID[["beta"]], threelvl_pID[["beta"]], threelvl[["beta"]], threelvl_article_pID[["beta"]])
  # add to df
  table_comp$g <- effect_sizes_vector

  table_comp <- table_comp %>%
    left_join(variance_components, by = "Model")
  ###### prep table
  table_comp$Model <- factor(
    table_comp$Model,
    levels = c("One", "Two", "Three (Opt. 1)", "Three (Opt. 2)", "Three (Opt. 3)")
  )

  table_comp <- table_comp[order(table_comp$Model), ]

  ###### sort columns - formatting
  table_comp <- table_comp %>%
    mutate(
      AddedLevel_Higher = if_else(level_added == "Effect Size", NA_character_, level_added),
      AddedLevel_Lower = if_else(level_added == "Effect Size", "ESIDs", NA_character_)
    )

  table_comp <- table_comp %>%
    separate(
      col = Model,
      into = c("ModelLevel", "ModelOption"),
      sep = " \\(",
      fill = "right",
      remove = FALSE
    ) %>%
    mutate(
      ModelOption = gsub("\\)", "", ModelOption) # remove trailing ")"
    )

  table_comp <- table_comp %>%
    mutate(ModelComparisonIndex_Model = case_when(
      Model == "Two" ~ comparison, # keep the original value
      Model == "Three (Opt. 1)" ~ "2 vs 3", # add your new value
      TRUE ~ NA_character_ # everything else becomes NA
    )) %>%
    rename(
      ModelComparisonIndex_LRT = LRT,
      ModelComparisonIndex_p = p,
      ModelFitIndex_AIC = AIC,
      ModelFitIndex_logLik = logLik,
      σ_sigma1 = sigma1,
      σ_sigma2 = sigma2
    )
  ## round to 2 dp (or 3 for p val and sigmas)
  table_comp <- table_comp %>%
    mutate(
      ModelComparisonIndex_LRT = as.numeric(ModelComparisonIndex_LRT),
      ModelComparisonIndex_p = as.numeric(ModelComparisonIndex_p),
      σ_sigma1 = as.numeric(σ_sigma1),
      σ_sigma2 = as.numeric(σ_sigma2),
      g = as.numeric(g)
    )

  table_comp <- table_comp %>%
    mutate(
      ModelComparisonIndex_LRT = round(ModelComparisonIndex_LRT, 2),
      g = round(g, 2),
      ModelComparisonIndex_p = round(ModelComparisonIndex_p, 3),
      σ_sigma1 = round(σ_sigma1, 3),
      σ_sigma2 = round(σ_sigma2, 3),
      blank1 = "",
      blank2 = ""
    )


  ## select necessary columns
  table_comp <- table_comp %>%
    dplyr::select(
      ModelLevel, ModelOption, AddedLevel_Higher, AddedLevel_Lower, blank1, ModelFitIndex_AIC, ModelFitIndex_logLik, blank2, ModelComparisonIndex_Model, ModelComparisonIndex_LRT, ModelComparisonIndex_p, g, σ_sigma1, σ_sigma2
    )

  ## add spaces in shared words
  table_comp <- table_comp %>%
    rename(
      "Model Level" = ModelLevel,
      "Model Option" = ModelOption,
      "Added Level_Higher" = AddedLevel_Higher,
      "Added Level_Lower" = AddedLevel_Lower,
      "Model Fit Index_AIC" = ModelFitIndex_AIC,
      "Model Fit Index_logLik" = ModelFitIndex_logLik,
      "Model Comparison Index_Model" = ModelComparisonIndex_Model,
      "Model Comparison Index_LRT" = ModelComparisonIndex_LRT,
      "Model Comparison Index_p" = ModelComparisonIndex_p,
      "\\sigma²_1" = σ_sigma1,
      "\\sigma²_2" = σ_sigma2
    )
  table_comp
}
