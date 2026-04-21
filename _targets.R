# Load packages required to define the pipeline:
library(targets)

# Set target options:
tar_option_set(
  packages = c(
    "tidyr", "haven", "dplyr", "psych", "effsize", "metafor", "readxl",
    "purrr", "stringr", "rlang", "ggplot2", "tidyverse", "performance",
    "broom", "Hmisc", "flextable", "officer", "papaja", "labelled", "TOSTER",
    "forcats", "RColorBrewer", "jtools", "viridis", "ggrepel", "furrr", "grateful", "esc", "openxlsx", "ggplot2",
    "kableExtra", "car", "caret", "emmeans", "lme4", "lmerTest", "lsr", "patchwork", "sjstats", "showtext", "orchaRd", "ggrepel", "tibble"
  )
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source("scripts")
# tar_source("other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own:
list(
  tar_target(full_df, read_clean_data("data_clean/full_data.csv")),
  tar_target(
    overall_studies_csv,
    {
      # ensure folder exists
      dir.create("data_clean", showWarnings = FALSE, recursive = TRUE)

      # build the cleaned dataset
      overall_studies <- full_df %>%
        dplyr::select(
          ref, article_id, study_id, participant_id, efN_id, pct_female,
          age_mean, age_sd,
          aiN, aiMean, aiSD, aiN.1, aiN.0,
          humanN, humanMean, humanSD, humanN.1, humanN.0,
          dv_var, dv_type, dv_synonym, intent, in_action,
          RQ, harm, agent_intel, aiType_a, aiType_b,
          PMA, PMC, responsible, responsibleCat,
          yi, vi
        ) %>%
        dplyr::mutate(
          responsibleCat = sub("[0-9]+$", "", responsibleCat)
        )

      # write the CSV
      write.csv(overall_studies, "data_clean/overallDF.csv", row.names = FALSE)

      # return the file path for targets tracking
      "data_clean/overallDF.csv"
    },
    format = "file"
  ),
  tar_target(desc_inf_data_avail_df, read_clean_data("data_clean/desc_inf_data_avail_df.csv")),
  tar_target(final_data_avail_df, correct_participant_id_in_data_avail_df(full_df, desc_inf_data_avail_df)),
  ###########################
  # MAIN MODEL W. ALL DATA
  ###########################
  tar_target(fourlvl, fit_fourlvl(full_df, "fourlvl")),
  tar_target(fourlvl_article, fit_fourlvl_article(full_df, "fourlvl_article")),
  tar_target(threelvl_pID, fit_threelvl_pID(full_df, "threelvl_pID")),
  tar_target(threelvl_study, fit_threelvl_study(full_df, "threelvl_study")),
  tar_target(threelvl_article_pID, fit_threelvl_article_pID(full_df, "threelvl_article_pID")),
  tar_target(threelvl_article, fit_threelvl_article(full_df, "threelvl_article")),
  tar_target(threelvl, fit_threelvl(full_df, "threelvl")),
  tar_target(twolvl, fit_twolvl(full_df, "twolvl")),
  tar_target(twolvl_pID, fit_twolvl_pID(full_df, "twolvl_pID")),
  tar_target(twolvl_study, fit_twolvl_study(full_df, "twolvl_study")),
  tar_target(onelvl, fit_onelvl(full_df, "onelvl")),
  tar_target(
    model_comparison_full,
    compare_all_models(
      "fourlvl", "fourlvl_article", "threelvl_article_pID", "threelvl_pID", "threelvl_study", "threelvl_article", "threelvl", "twolvl", "twolvl_pID", "twolvl_study", "onelvl"
    )
  ),
  tar_target(twolvl_pID_vs_threelvl, compare_model_comparison_winners_anova(twolvl_pID, threelvl)),
  tar_target(twolvl_pID_vs_threelvl_article_pID, compare_model_comparison_winners_anova(threelvl_article_pID, twolvl_pID)),
  tar_target(m_multi, fit_selected_model_structure(mod = NULL, full_df)),
  tar_target(basic_outlier_trimmed_df, run_basic_outlier_analysis(full_df, m_multi)),
  tar_target(conservative_check_df, run_conservative_outlier_analysis(full_df, m_multi)),
  tar_target(outliers, create_outlier_df_to_inspect(conservative_check_df)),
  tar_target(influence_stats_df, calculate_influential_stats(conservative_check_df, m_multi)),
  tar_target(
    influence_stats_df_csv,
    {
      # ensure folder exists
      dir.create("outputs", showWarnings = FALSE, recursive = TRUE)

      # write the CSV
      write.csv(influence_stats_df, "outputs/influence_stats_df.csv", row.names = FALSE)

      # return the file path so targets can track it
      "outputs/influence_stats_df.csv"
    },
    format = "file"
  ),
  tar_target(influence_outliers_table, create_influence_table(influence_stats_df)),
  ##### data summary
  tar_target(
    full_registry_file,
    "outputs/full_registry.rds",
    format = "file"
  ),
  tar_target(
    full_registry,
    read_rds_data(full_registry_file)
  ),
  tar_target(method_counts, summary_manual_calc_methods(full_registry)),
  tar_target(outlier_class_distrib, plot_outlier_classification_distribution(conservative_check_df, m_multi)),
  tar_target(outlier_class_distrib_plot,
    {
      dir.create("figures/", showWarnings = FALSE, recursive = TRUE)
      file <- "figures/outlier_class_distrib.png"
      ggplot2::ggsave(
        filename = file,
        plot = outlier_class_distrib,
        width = 8,
        height = 5,
        dpi = 300
      )
      file
    },
    format = "file"
  ),
  tar_target(trimmed_df, remove_verified_outliers(full_df)),
  ###########################
  # MAIN MODEL W. TRIMMED DATA
  ###########################
  tar_target(fourlvl_trimmed, fit_fourlvl(trimmed_df, "fourlvl_trimmed")),
  tar_target(fourlvl_trimmed_article, fit_fourlvl_article(trimmed_df, "fourlvl_trimmed_article")),
  tar_target(threelvl_trimmed_pID, fit_threelvl_pID(trimmed_df, "threelvl_trimmed_pID")),
  tar_target(threelvl_trimmed_study, fit_threelvl_study(trimmed_df, "threelvl_trimmed_study")),
  tar_target(threelvl_trimmed_article_pID, fit_threelvl_article_pID(trimmed_df, "threelvl_trimmed_article_pID")),
  tar_target(threelvl_trimmed_article, fit_threelvl_article(trimmed_df, "threelvl_trimmed_article")),
  tar_target(threelvl_trimmed, fit_threelvl(trimmed_df, "threelvl_trimmed")),
  tar_target(twolvl_trimmed, fit_twolvl(trimmed_df, "twolvl_trimmed")),
  tar_target(twolvl_trimmed_pID, fit_twolvl_pID(trimmed_df, "twolvl_trimmed_pID")),
  tar_target(twolvl_trimmed_study, fit_twolvl_study(trimmed_df, "twolvl_trimmed_study")),
  tar_target(onelvl_trimmed, fit_onelvl(trimmed_df, "onelvl_trimmed")),
  tar_target(
    model_comparison_trimmed,
    compare_all_models(
      "fourlvl_trimmed", "fourlvl_trimmed_article", "threelvl_trimmed_article_pID", "threelvl_trimmed_pID", "threelvl_trimmed_study", "threelvl_trimmed_article", "threelvl_trimmed", "twolvl_trimmed", "twolvl_trimmed_pID", "twolvl_trimmed_study", "onelvl_trimmed"
    )
  ),
  tar_target(twolvl_trimmed_pID_vs_threelvl_trimmed, compare_model_comparison_winners_anova(twolvl_trimmed_pID, threelvl_trimmed)),
  tar_target(twolvl_trimmed_pID_vs_threelvl_trimmed_article_pID, compare_model_comparison_winners_anova(threelvl_trimmed_article_pID, twolvl_trimmed_pID)),
  tar_target(m_multi_trimmed, fit_selected_model_structure(mod = NULL, trimmed_df)),
  ###########################
  # PUBLICATION BIAS
  ###########################
  ############### full model
  ######## pet test
  tar_target(pet_full_model, fit_selected_model_structure(mod = ~ sqrt(vi), dat = full_df)),
  ######## peese test
  tar_target(peese_full_model, fit_selected_model_structure(mod = ~vi, dat = full_df)),

  ############### trimmed model
  ######## pet test
  tar_target(pet_trimmed_model, fit_selected_model_structure(mod = ~ sqrt(vi), dat = trimmed_df)),
  ######## peese test
  tar_target(peese_trimmed_model, fit_selected_model_structure(mod = ~vi, dat = trimmed_df)),
  ###########################
  # MODEL FIT
  ###########################
  tar_target(full_data_profile_plot, make_profile_plot(
    mod_article_pid = threelvl_article_pID,
    mod_study_pid = threelvl,
    mod_pid_efid = threelvl_pID,
    file = "figures/profilePlot_full.png",
    fig_title = "Profile Likelihood Plots of the Variance Components in the Full 3-level Models"
  )),
  tar_target(trimmed_data_profile_plot, make_profile_plot(
    mod_article_pid = threelvl_trimmed_article_pID,
    mod_study_pid = threelvl_trimmed,
    mod_pid_efid = threelvl_trimmed_pID,
    file = "figures/profilePlot_trimmed.png",
    fig_title = "Profile Likelihood Plots of the Variance Components in the Trimmed 3-level Models"
  )),
  ###########################
  # I2
  ###########################
  # full data
  tar_target(full_data_i2, run_i2_test(m_multi, "full_data_i2_table.html", "full_data_i2_plot.png")),
  # trimmed data
  tar_target(trimmed_data_i2, run_i2_test(m_multi_trimmed, "trimmed_data_i2_table.html", "trimmed_data_i2_plot.png")),

  ###########################
  # CATEGORICAL MODS
  ###########################

  ### Full data
  ## Harm
  tar_target(harm_mv, run_meta_analysis(full_df, "harm", "cat")),
  ## Inaction
  tar_target(in_action_mv, run_meta_analysis(full_df, "in_action", "cat")),
  ## Intent
  tar_target(intent_mv, run_meta_analysis(full_df, "intent", "cat")),
  ## Agent Intelligence
  tar_target(agent_intel_mv, run_meta_analysis(full_df, "agent_intel", "cat")),
  ## AI Type A (AI vs Robot)
  tar_target(aiType_a_mv, run_meta_analysis(full_df, "aiType_a", "cat")),
  ## AI Type B (AI vs. Mechanical Robot vs. Humanoid Robot)
  tar_target(aiType_b_mv, run_meta_analysis(full_df, "aiType_b", "cat")),
  ## DV Synonym
  tar_target(dv_synonym_mv, run_meta_analysis(full_df, "dv_synonym", "cat")),

  ### Trimmed data
  ## Harm
  tar_target(harm_trimmed_mv, run_meta_analysis(trimmed_df, "harm", "cat")),
  ## Inaction
  tar_target(in_action_trimmed_mv, run_meta_analysis(trimmed_df, "in_action", "cat")),
  ## Intent
  tar_target(intent_trimmed_mv, run_meta_analysis(trimmed_df, "intent", "cat")),
  ## Agent Intelligence
  tar_target(agent_intel_trimmed_mv, run_meta_analysis(trimmed_df, "agent_intel", "cat")),
  ## AI Type A (AI vs Robot)
  tar_target(aiType_a_trimmed_mv, run_meta_analysis(trimmed_df, "aiType_a", "cat")),
  ## AI Type B (AI vs. Mechanical Robot vs. Humanoid Robot)
  tar_target(aiType_b_trimmed_mv, run_meta_analysis(trimmed_df, "aiType_b", "cat")),
  ## DV Synonym
  tar_target(dv_synonym_trimmed_mv, run_meta_analysis(trimmed_df, "dv_synonym", "cat")),

  ###########################
  # CONTINUOUS MODS
  ###########################

  ### Full Data
  ## Responsible
  tar_target(responsible_mv, run_meta_analysis(full_df, "responsible", "cont")),
  ## PMA
  tar_target(pma_mv, run_meta_analysis(full_df, "PMA", "cont")),
  ## PMC
  tar_target(pmc_mv, run_meta_analysis(full_df, "PMC", "cont")),
  ## RQ
  tar_target(rqz_df, make_cont_mods_z_score(full_df, "RQ")),
  tar_target(rq_mv, run_meta_analysis(rqz_df, "RQ", "cont")),

  ### Trimmed Data
  ## Responsible
  tar_target(responsible_trimmed_mv, run_meta_analysis(trimmed_df, "responsible", "cont")),
  ## PMA
  tar_target(pma_trimmed_mv, run_meta_analysis(trimmed_df, "PMA", "cont")),
  ## PMC
  tar_target(pmc_trimmed_mv, run_meta_analysis(trimmed_df, "PMC", "cont")),
  ## RQ
  tar_target(rqz_trimmed_df, make_cont_mods_z_score(trimmed_df, "RQ")),
  tar_target(rq_trimmed_mv, run_meta_analysis(rqz_trimmed_df, "RQ", "cont")),


  ###########################
  # MULTIPLE REGRESSION MODS
  ###########################

  ### Full Data
  ## PMA + AI Type A
  tar_target(pma_aiTypeA_mv, run_meta_analysis(full_df, mod_col = c("PMA", "aiType_a"), data_type = c("cont", "cat"))),
  ## PMA + AI Type B
  tar_target(pma_aiTypeB_mv, run_meta_analysis(full_df, mod_col = c("PMA", "aiType_b"), data_type = c("cont", "cat"))),
  ## PMA + Intent
  tar_target(pma_intent_mv, run_meta_analysis(full_df, mod_col = c("PMA", "intent"), data_type = c("cont", "cat"))),
  ## PMA + In_action
  tar_target(pma_in_action_mv, run_meta_analysis(full_df, mod_col = c("PMA", "in_action"), data_type = c("cont", "cat"))),
  ## PMA + responsible
  tar_target(pma_responsible_mv, run_meta_analysis(full_df, mod_col = c("PMA", "responsible"), data_type = c("cont", "cont"))),
  ## PMC + AI Type A
  tar_target(pmc_aiTypeA_mv, run_meta_analysis(full_df, mod_col = c("PMC", "aiType_a"), data_type = c("cont", "cat"))),
  ## PMC + AI Type B
  tar_target(pmc_aiTypeB_mv, run_meta_analysis(full_df, mod_col = c("PMC", "aiType_b"), data_type = c("cont", "cat"))),
  ## PMC + Intent
  tar_target(pmc_intent_mv, run_meta_analysis(full_df, mod_col = c("PMC", "intent"), data_type = c("cont", "cat"))),
  ## PMC + In_action
  tar_target(pmc_in_action_mv, run_meta_analysis(full_df, mod_col = c("PMC", "in_action"), data_type = c("cont", "cat"))),
  ## PMA + PMC
  tar_target(pma_pmc_mv, run_meta_analysis(full_df, mod_col = c("PMA", "PMC"), data_type = c("cont", "cont"))),
  ## PMC + responsible
  tar_target(pmc_responsible_mv, run_meta_analysis(full_df, mod_col = c("PMC", "responsible"), data_type = c("cont", "cont"))),
  ## Intent + Inaction
  tar_target(intent_in_action_mv, run_multi_mod_meta_analysis(full_df, mod_col = c("intent", "in_action"), data_type = c("cat", "cat"))),
  ## Intent + Responsible
  tar_target(intent_responsible_mv, run_meta_analysis(full_df, mod_col = c("intent", "responsible"), data_type = c("cat", "cont"))),
  ## Responsible Categorical Levels + Responsible
  tar_target(responsibleCat_responsible_mv, run_multi_mod_meta_analysis(full_df, mod_col = c("responsibleCat", "responsible"), data_type = c("cat", "cont"))),


  ### Trimmed Data
  ## PMA + AI Type A
  tar_target(pma_aiTypeA_trimmed_mv, run_meta_analysis(trimmed_df, mod_col = c("PMA", "aiType_a"), data_type = c("cont", "cat"))),
  ## PMA + AI Type B
  tar_target(pma_aiTypeB_trimmed_mv, run_meta_analysis(trimmed_df, mod_col = c("PMA", "aiType_b"), data_type = c("cont", "cat"))),
  ## PMA + Intent
  tar_target(pma_intent_trimmed_mv, run_meta_analysis(trimmed_df, mod_col = c("PMA", "intent"), data_type = c("cont", "cat"))),
  ## PMA + In_action
  tar_target(pma_in_action_trimmed_mv, run_meta_analysis(trimmed_df, mod_col = c("PMA", "in_action"), data_type = c("cont", "cat"))),
  ## PMA + responsible
  tar_target(pma_responsible_trimmed_mv, run_meta_analysis(trimmed_df, mod_col = c("PMA", "responsible"), data_type = c("cont", "cont"))),
  ## PMC + AI Type A
  tar_target(pmc_aiTypeA_trimmed_mv, run_meta_analysis(trimmed_df, mod_col = c("PMC", "aiType_a"), data_type = c("cont", "cat"))),
  ## PMC + AI Type B
  tar_target(pmc_aiTypeB_trimmed_mv, run_meta_analysis(trimmed_df, mod_col = c("PMC", "aiType_b"), data_type = c("cont", "cat"))),
  ## PMC + Intent
  tar_target(pmc_intent_trimmed_mv, run_meta_analysis(trimmed_df, mod_col = c("PMC", "intent"), data_type = c("cont", "cat"))),
  ## PMC + In_action
  tar_target(pmc_in_action_trimmed_mv, run_meta_analysis(trimmed_df, mod_col = c("PMC", "in_action"), data_type = c("cont", "cat"))),
  ## PMA + PMC
  tar_target(pma_pmc_trimmed_mv, run_meta_analysis(trimmed_df, mod_col = c("PMA", "PMC"), data_type = c("cont", "cont"))),
  ## PMC + responsible
  tar_target(pmc_responsible_trimmed_mv, run_meta_analysis(trimmed_df, mod_col = c("PMC", "responsible"), data_type = c("cont", "cont"))),
  ## Intent + Inaction
  tar_target(intent_in_action_trimmed_mv, run_multi_mod_meta_analysis(trimmed_df, mod_col = c("intent", "in_action"), data_type = c("cat", "cat"))),
  ## Intent + Responsible
  tar_target(intent_responsible_trimmed_mv, run_multi_mod_meta_analysis(trimmed_df, mod_col = c("intent", "responsible"), data_type = c("cat", "cont"))),
  ## Responsible Categorical Levels + Responsible
  tar_target(responsibleCat_responsible_trimmed_mv, run_multi_mod_meta_analysis(trimmed_df, mod_col = c("responsibleCat", "responsible"), data_type = c("cat", "cont"))),

  ###########################
  # PWR SENS ANALYSES
  ###########################
  tar_target(mdes_main_full, find_mdes_main(full_df, m_multi, nsim = 500, seed = 123, tol = 0.001)),
  tar_target(mdes_main_trimmed, find_mdes_main(trimmed_df, m_multi_trimmed, nsim = 500, seed = 123, tol = 0.001)),

  ###########################
  # CATEGORICAL MODS
  ###########################

  ### Full data
  ## Harm
  tar_target(mdes_harm_harm, find_mdes_level(full_df, harm_mv, "harm", "harm", nsim = 500)),
  tar_target(mdes_harm_notharm, find_mdes_level(full_df, harm_mv, "harm", "notharm", nsim = 500)),
  #  ## In_action
  tar_target(mdes_in_action_action, find_mdes_level(full_df, in_action_mv, "in_action", "action", nsim = 500, tol = 0.001)),
  tar_target(mdes_in_action_inaction, find_mdes_level(full_df, in_action_mv, "in_action", "inaction", nsim = 500, tol = 0.001)),
  #  ## AI Type A (AI vs. Robot)
  tar_target(mdes_aiTypeA_ai, find_mdes_level(full_df, aiType_a_mv, "aiType_a", "AI", nsim = 500, tol = 0.001)),
  tar_target(mdes_aiTypeA_robot, find_mdes_level(full_df, aiType_a_mv, "aiType_a", "robot", nsim = 500, tol = 0.001)),
  #  ## AI Type B (AI vs. Mechanical Robot vs. Humanoid Robot)
  tar_target(mdes_aiTypeB_ai, find_mdes_level(full_df, aiType_b_mv, "aiType_b", "AI", nsim = 500, tol = 0.001)),
  tar_target(mdes_aiTypeB_mechanical, find_mdes_level(full_df, aiType_b_mv, "aiType_b", "mechanical", nsim = 500, tol = 0.001)),
  tar_target(mdes_aiTypeB_humanoid, find_mdes_level(full_df, aiType_b_mv, "aiType_b", "humanoid", nsim = 500, tol = 0.001)),
  #  ## Agent Intelligence
  tar_target(mdes_agent_intel_implied, find_mdes_level(full_df, agent_intel_mv, "agent_intel", "implied", nsim = 500, tol = 0.001)),
  tar_target(mdes_agent_intel_notImplied, find_mdes_level(full_df, agent_intel_mv, "agent_intel", "notImplied", nsim = 500, tol = 0.001)),
  # ## Intent
  tar_target(mdes_intent_meane, find_mdes_level(full_df, intent_mv, "intent", "meane", nsim = 500, tol = 0.001)),
  tar_target(mdes_intent_sidee, find_mdes_level(full_df, intent_mv, "intent", "sidee", nsim = 500, tol = 0.001)),

  # ## DV Synonym ("acceptable", "appropriate", "good", "justifiable", "moral", "permissible", "right", "unethical",  "wrong")
  tar_target(mdes_dv_synonym_acceptable, find_mdes_level(full_df, dv_synonym_mv, "dv_synonym", "acceptable", nsim = 500, tol = 0.001)),
  tar_target(mdes_dv_synonym_appropriate, find_mdes_level(full_df, dv_synonym_mv, "dv_synonym", "appropriate", nsim = 500, tol = 0.001)),
  tar_target(mdes_dv_synonym_good, find_mdes_level(full_df, dv_synonym_mv, "dv_synonym", "good", nsim = 500, tol = 0.001)),
  tar_target(mdes_dv_synonym_justifiable, find_mdes_level(full_df, dv_synonym_mv, "dv_synonym", "justifiable", nsim = 500, tol = 0.001)),
  tar_target(mdes_dv_synonym_moral, find_mdes_level(full_df, dv_synonym_mv, "dv_synonym", "moral")),
  tar_target(mdes_dv_synonym_permissible, find_mdes_level(full_df, dv_synonym_mv, "dv_synonym", "permissible", nsim = 500, tol = 0.001)),
  tar_target(mdes_dv_synonym_right, find_mdes_level(full_df, dv_synonym_mv, "dv_synonym", "right", nsim = 500, tol = 0.001)),
  tar_target(mdes_dv_synonym_unethical, find_mdes_level(full_df, dv_synonym_mv, "dv_synonym", "unethical", nsim = 500, tol = 0.001)),
  tar_target(mdes_dv_synonym_wrong, find_mdes_level(full_df, dv_synonym_mv, "dv_synonym", "wrong", nsim = 500, tol = 0.001)),
  # Multiple Regression Models
  ## Intent x Decision Type
  tar_target(mdes_intent_in_action_ise, find_mdes_level_for_multi_reg(full_df, intent_in_action_mv, "intent", "sidee", nsim = 500, tol = 0.001)),
  tar_target(mdes_intent_in_action_ime, find_mdes_level_for_multi_reg(full_df, intent_in_action_mv, "intent", "meane", nsim = 500, tol = 0.001)),
  tar_target(mdes_intent_in_action_mda, find_mdes_level_for_multi_reg(full_df, intent_in_action_mv, "in_action", "action", nsim = 500, tol = 0.001)),
  tar_target(mdes_intent_in_action_mdi, find_mdes_level_for_multi_reg(full_df, intent_in_action_mv, "in_action", "inaction", nsim = 500, tol = 0.001)),

  ### Trimmed Data
  ## Harm
  tar_target(trimmed_mdes_harm_harm, find_mdes_level(trimmed_df, harm_trimmed_mv, "harm", "harm", nsim = 500)),
  tar_target(trimmed_mdes_harm_notharm, find_mdes_level(trimmed_df, harm_trimmed_mv, "harm", "notharm", nsim = 500)),
  #  ## In_action
  tar_target(trimmed_mdes_in_action_action, find_mdes_level(trimmed_df, in_action_trimmed_mv, "in_action", "action", nsim = 500, tol = 0.001)),
  tar_target(trimmed_mdes_in_action_inaction, find_mdes_level(trimmed_df, in_action_trimmed_mv, "in_action", "inaction", nsim = 500, tol = 0.001)),
  #  ## AI Type A (AI vs. Robot)
  tar_target(trimmed_mdes_aiTypeA_ai, find_mdes_level(trimmed_df, aiType_a_trimmed_mv, "aiType_a", "AI", nsim = 500, tol = 0.001)),
  tar_target(trimmed_mdes_aiTypeA_robot, find_mdes_level(trimmed_df, aiType_a_trimmed_mv, "aiType_a", "robot", nsim = 500, tol = 0.001)),
  #  ## AI Type B (AI vs. Mechanical Robot vs. Humanoid Robot)
  tar_target(trimmed_mdes_aiTypeB_ai, find_mdes_level(trimmed_df, aiType_b_trimmed_mv, "aiType_b", "AI", nsim = 500, tol = 0.001)),
  tar_target(trimmed_mdes_aiTypeB_mechanical, find_mdes_level(trimmed_df, aiType_b_trimmed_mv, "aiType_b", "mechanical", nsim = 500, tol = 0.001)),
  tar_target(trimmed_mdes_aiTypeB_humanoid, find_mdes_level(trimmed_df, aiType_b_trimmed_mv, "aiType_b", "humanoid", nsim = 500, tol = 0.001)),
  #  ## Agent Intelligence
  tar_target(trimmed_mdes_agent_intel_implied, find_mdes_level(trimmed_df, agent_intel_trimmed_mv, "agent_intel", "implied", nsim = 500, tol = 0.001)),
  tar_target(trimmed_mdes_agent_intel_notImplied, find_mdes_level(trimmed_df, agent_intel_trimmed_mv, "agent_intel", "notImplied", nsim = 500, tol = 0.001)),
  # ## Intent
  tar_target(trimmed_mdes_intent_meane, find_mdes_level(trimmed_df, intent_trimmed_mv, "intent", "meane", nsim = 500, tol = 0.001)),
  tar_target(trimmed_mdes_intent_sidee, find_mdes_level(trimmed_df, intent_trimmed_mv, "intent", "sidee", nsim = 500, tol = 0.001)),

  # ## DV Synonym ("acceptable", "appropriate", "good", "justifiable", "moral", "permissible", "right", "unethical",  "wrong")
  tar_target(trimmed_mdes_dv_synonym_acceptable, find_mdes_level(trimmed_df, dv_synonym_trimmed_mv, "dv_synonym", "acceptable", nsim = 500, tol = 0.001)),
  tar_target(trimmed_mdes_dv_synonym_appropriate, find_mdes_level(trimmed_df, dv_synonym_trimmed_mv, "dv_synonym", "appropriate", nsim = 500, tol = 0.001)),
  tar_target(trimmed_mdes_dv_synonym_good, find_mdes_level(trimmed_df, dv_synonym_trimmed_mv, "dv_synonym", "good", nsim = 500, tol = 0.001)),
  tar_target(trimmed_mdes_dv_synonym_justifiable, find_mdes_level(trimmed_df, dv_synonym_trimmed_mv, "dv_synonym", "justifiable", nsim = 500, tol = 0.001)),
  tar_target(trimmed_mdes_dv_synonym_moral, find_mdes_level(trimmed_df, dv_synonym_trimmed_mv, "dv_synonym", "moral")),
  tar_target(trimmed_mdes_dv_synonym_permissible, find_mdes_level(trimmed_df, dv_synonym_trimmed_mv, "dv_synonym", "permissible", nsim = 500, tol = 0.001)),
  tar_target(trimmed_mdes_dv_synonym_right, find_mdes_level(trimmed_df, dv_synonym_trimmed_mv, "dv_synonym", "right", nsim = 500, tol = 0.001)),
  tar_target(trimmed_mdes_dv_synonym_unethical, find_mdes_level(trimmed_df, dv_synonym_trimmed_mv, "dv_synonym", "unethical", nsim = 500, tol = 0.001)),
  tar_target(trimmed_mdes_dv_synonym_wrong, find_mdes_level(trimmed_df, dv_synonym_trimmed_mv, "dv_synonym", "wrong", nsim = 500, tol = 0.001)),
  # Multiple Regression Models
  ## Intent x Decision Type
  tar_target(trimmed_mdes_intent_in_action_ise, find_mdes_level_for_multi_reg(trimmed_df, intent_in_action_trimmed_mv, "intent", "sidee", nsim = 500, tol = 0.001)),
  tar_target(trimmed_mdes_intent_in_action_ime, find_mdes_level_for_multi_reg(trimmed_df, intent_in_action_trimmed_mv, "intent", "meane", nsim = 500, tol = 0.001)),
  tar_target(trimmed_mdes_intent_in_action_mda, find_mdes_level_for_multi_reg(trimmed_df, intent_in_action_trimmed_mv, "in_action", "action", nsim = 500, tol = 0.001)),
  tar_target(trimmed_mdes_intent_in_action_mdi, find_mdes_level_for_multi_reg(trimmed_df, intent_in_action_trimmed_mv, "in_action", "inaction", nsim = 500, tol = 0.001)),
  ###########################
  # CONTINUOUS MODS
  ###########################

  ### Full data
  # ## PMA
  tar_target(mdes_PMA, find_mdes_cont(full_df, pma_mv, mod = "PMA", nsim = 500, seed = 123, mdes_min = 0.07, mdes_max = 0.09, tol = 0.001)),
  # ## PMC
  tar_target(mdes_PMC, find_mdes_cont(full_df, pmc_mv, mod = "PMC", nsim = 500, seed = 123, tol = 0.001)),
  # ## Responsible
  # tar_target(mdes_responsible_init, find_mdes_cont(full_df, responsible_mv, mod = "responsible", nsim = 500, seed = 123, tol = 0.001)),
  tar_target(mdes_responsible, find_mdes_cont(full_df, responsible_mv, mod = "responsible", nsim = 500, seed = 123, mdes_min = 0.04, mdes_max = 0.08, tol = 0.001)),
  # ## RQ
  tar_target(mdes_RQ, find_mdes_cont(rqz_df, rq_mv, mod = "RQ", nsim = 500, seed = 123, mdes_min = 0.1, mdes_max = 0.2)),
  ## Responsible Cat Levels (e.g. blame, responsible, praise etc.) x Responsible
  tar_target(mdes_responsible_with_catLvls, find_mdes_cont(full_df, responsibleCat_responsible_mv, "responsible", nsim = 500, seed = 123, mdes_min = 0.04, mdes_max = 0.08, tol = 0.001)),


  ### Trimmed data
  # ## PMA
  tar_target(trimmed_mdes_PMA, find_mdes_cont(trimmed_df, pma_trimmed_mv, mod = "PMA", nsim = 500, seed = 123, mdes_min = 0.07, mdes_max = 0.09, tol = 0.001)),
  # tar_target(trimmed_mdes_PMA_init, find_mdes_cont(trimmed_df, pma_trimmed_mv, mod = "PMA", nsim = 500, seed = 123, tol = 0.001)),
  # ## PMC
  tar_target(trimmed_mdes_PMC, find_mdes_cont(trimmed_df, pmc_trimmed_mv, mod = "PMC", nsim = 500, seed = 123, tol = 0.001)),
  # ## Responsible
  tar_target(trimmed_mdes_responsible, find_mdes_cont(trimmed_df, responsible_trimmed_mv, mod = "responsible", nsim = 500, seed = 123, mdes_min = 0.04, mdes_max = 0.08, tol = 0.001)),
  # ## RQ
  tar_target(trimmed_mdes_RQ, find_mdes_cont(rqz_trimmed_df, rq_trimmed_mv, mod = "RQ", nsim = 500, seed = 123, mdes_min = 0.1, mdes_max = 0.2, tol = 0.001)),
  ## Responsible Cat Levels x Responsible
  tar_target(trimmed_mdes_responsible_with_catLvls, find_mdes_cont_multi_mods(df = trimmed_df, model = responsibleCat_responsible_trimmed_mv, mod = "responsible", nsim = 500, seed = 123, mdes_min = 0.02, mdes_max = 0.045, tol = 0.001)),


  ### Combined MDES Stuff
  tar_target(full_mdes_all, combine_mdes_results(
    mdes_main_full,
    # harm mod
    mdes_harm_harm, mdes_harm_notharm,
    # intent mod
    mdes_intent_meane, mdes_intent_sidee,
    # in_action mod
    mdes_in_action_action,
    mdes_in_action_inaction,
    # aiType a mod
    mdes_aiTypeA_ai,
    mdes_aiTypeA_robot,
    # ai type b mod
    mdes_aiTypeB_ai,
    mdes_aiTypeB_mechanical,
    mdes_aiTypeB_humanoid,
    # agent intel mod
    mdes_agent_intel_implied,
    mdes_agent_intel_notImplied,
    # dv synonym mod ("acceptable", "appropriate", "good", "justifiable", "moral", "permissible", "right", "unethical",  "wrong")
    mdes_dv_synonym_acceptable,
    mdes_dv_synonym_appropriate,
    mdes_dv_synonym_good,
    mdes_dv_synonym_justifiable,
    mdes_dv_synonym_moral,
    mdes_dv_synonym_permissible,
    mdes_dv_synonym_right,
    mdes_dv_synonym_unethical,
    mdes_dv_synonym_wrong,
    # continuous mods
    mdes_PMA,
    mdes_PMC,
    mdes_responsible,
    mdes_RQ
  )),
  tar_target(trimmed_mdes_all, combine_mdes_results(
    mdes_main_trimmed,
    # harm mod
    trimmed_mdes_harm_harm, trimmed_mdes_harm_notharm,
    # intent mod
    trimmed_mdes_intent_meane, trimmed_mdes_intent_sidee,
    # in_action mod
    trimmed_mdes_in_action_action,
    trimmed_mdes_in_action_inaction,
    # aiType a mod
    trimmed_mdes_aiTypeA_ai,
    trimmed_mdes_aiTypeA_robot,
    # ai type b mod
    trimmed_mdes_aiTypeB_ai,
    trimmed_mdes_aiTypeB_mechanical,
    trimmed_mdes_aiTypeB_humanoid,
    # agent intel mod
    trimmed_mdes_agent_intel_implied,
    trimmed_mdes_agent_intel_notImplied,
    # dv synonym mod ("acceptable", "appropriate", "good", "justifiable", "moral", "permissible", "right", "unethical",  "wrong")
    trimmed_mdes_dv_synonym_acceptable,
    trimmed_mdes_dv_synonym_appropriate,
    trimmed_mdes_dv_synonym_good,
    trimmed_mdes_dv_synonym_justifiable,
    trimmed_mdes_dv_synonym_moral,
    trimmed_mdes_dv_synonym_permissible,
    trimmed_mdes_dv_synonym_right,
    trimmed_mdes_dv_synonym_unethical,
    trimmed_mdes_dv_synonym_wrong,
    # continuous mods
    trimmed_mdes_PMA,
    trimmed_mdes_PMC,
    trimmed_mdes_responsible,
    trimmed_mdes_RQ
  )),
  tar_target(trimmed_mdes_mods, combine_mdes_results(
    trimmed_mdes_harm_harm, trimmed_mdes_harm_notharm,
    # in_action mod
    trimmed_mdes_in_action_action,
    trimmed_mdes_in_action_inaction,

    # agent intel mod
    trimmed_mdes_agent_intel_implied,
    trimmed_mdes_agent_intel_notImplied,
    # intent mod
    trimmed_mdes_intent_meane, trimmed_mdes_intent_sidee,
    # aiType a mod
    trimmed_mdes_aiTypeA_ai,
    trimmed_mdes_aiTypeA_robot,
    # ai type b mod
    trimmed_mdes_aiTypeB_ai,
    trimmed_mdes_aiTypeB_mechanical,
    trimmed_mdes_aiTypeB_humanoid,
    # dv synonym mod ("acceptable", "appropriate", "good", "justifiable", "moral", "permissible", "right", "unethical",  "wrong")
    trimmed_mdes_dv_synonym_acceptable,
    trimmed_mdes_dv_synonym_appropriate,
    trimmed_mdes_dv_synonym_good,
    trimmed_mdes_dv_synonym_justifiable,
    trimmed_mdes_dv_synonym_moral,
    trimmed_mdes_dv_synonym_permissible,
    trimmed_mdes_dv_synonym_right,
    trimmed_mdes_dv_synonym_unethical,
    trimmed_mdes_dv_synonym_wrong,
    # continuous mods
    trimmed_mdes_PMA,
    trimmed_mdes_PMC,
    trimmed_mdes_responsible,
    trimmed_mdes_RQ
  )),
  tar_target(main_mdes_all, combine_main_mdes_results(mdes_main_trimmed, mdes_main_full)),
  ## plot MDES values for overall model & moderator models for each data set
  tar_target(full_mdes_plot, plot_mdes_results(full_mdes_all)),
  tar_target(trimmed_mdes_plot, plot_mdes_results(trimmed_mdes_all)),
  tar_target(trimmed_mods_pwr_curve_plot, create_pwr_curve_for_all_mods(trimmed_mdes_mods)),
  tar_target(full_mdes_plot_file, ggsave("figures/full_mdes_plot.png", full_mdes_plot, width = 8, height = 6, dpi = 300), format = "file"),
  tar_target(trimmed_mdes_plot_file, ggsave("figures/trimmed_mdes_plot.png", trimmed_mdes_plot, width = 8, height = 6, dpi = 300), format = "file"),
  tar_target(
    trimmed_mods_pwr_curve,
    {
      trimmed_mods_pwr_curve_plot
      "figures/trimmed_mods_pwr_curve.pdf"
    },
    format = "file"
  ),


  # ###########################
  # CALCULATE TOAST
  ###########################
  tar_target(fullToast, calc_main_toast(1, m_multi, mdes_main_full, "Full")),
  tar_target(trimmedToast, calc_main_toast(1, m_multi_trimmed, mdes_main_trimmed, "Trimmed")),

  #### Full Data
  ### Categorical Moderators
  # Harm
  # harm level
  tar_target(fullToast_harm_harm, calc_toast("harm", harm_mv, mdes_harm_harm, "Full Harm", "harm")),
  # notharm level
  tar_target(fullToast_harm_notharm, calc_toast("notharm", harm_mv, mdes_harm_notharm, "Full Not Harm", "harm")),

  ## Intent
  # meane level
  tar_target(fullToast_intent_meane, calc_toast("meane", intent_mv, mdes_intent_meane, "Full Intent Mean-Effect", "intent")),
  # sidee level
  tar_target(fullToast_intent_sidee, calc_toast("sidee", intent_mv, mdes_intent_sidee, "Full Intent Side-Effect", "intent")),
  #
  #  ## In_action
  # action level
  tar_target(fullToast_in_action_action, calc_toast("action", in_action_mv, mdes_in_action_action, "Full In-Action Action", "in_action")),
  # inaction level
  tar_target(fullToast_in_action_inaction, calc_toast("inaction", in_action_mv, mdes_in_action_inaction, "Full In-Action Inaction", "in_action")),

  ## AI Type A (AI vs. Robot)
  ## AI level
  tar_target(fullToast_aiTypeA_ai, calc_toast("AI", aiType_a_mv, mdes_aiTypeA_ai, "Full AI Type A AI", "aiType_a")),
  # # Robot level
  tar_target(fullToast_aiTypeA_robot, calc_toast("robot", aiType_a_mv, mdes_aiTypeA_robot, "Full AI Type A Robot", "aiType_a")),
  #
  ## AI Type B (AI vs. Mechanical Robot vs. Humanoid Robot)
  # AI level
  tar_target(fullToast_aiTypeB_ai, calc_toast("AI", aiType_b_mv, mdes_aiTypeB_ai, "Full AI Type B AI", "aiType_b")),
  # Mechanical Robot level
  tar_target(fullToast_aiTypeB_mechanical, calc_toast("mechanical", aiType_b_mv, mdes_aiTypeB_mechanical, "Full AI Type B Mechanical Robot", "aiType_b")),
  # Humanoid Robot level
  tar_target(fullToast_aiTypeB_humanoid, calc_toast("humanoid", aiType_b_mv, mdes_aiTypeB_humanoid, "Full AI Type B Humanoid Robot", "aiType_b")),

  #  ## Agent Intelligence
  # # Implied level
  tar_target(fullToast_agent_intel_implied, calc_toast("implied", agent_intel_mv, mdes_agent_intel_implied, "Full Agent Intelligence Implied", "agent_intel")),
  # Not Implied level
  tar_target(fullToast_agent_intel_notImplied, calc_toast("notImplied", agent_intel_mv, mdes_agent_intel_notImplied, "Full Agent Intelligence Not Implied", "agent_intel")),

  # DV synonym ("acceptable", "appropriate", "good", "justifiable", "moral", "permissible", "right", "unethical",  "wrong")
  tar_target(fullToast_dv_synonym_acceptable, calc_toast("acceptable", dv_synonym_mv, mdes_dv_synonym_acceptable, "Full DV Synonym Acceptable", "dv_synonym")),
  tar_target(fullToast_dv_synonym_appropriate, calc_toast("appropriate", dv_synonym_mv, mdes_dv_synonym_appropriate, "Full DV Synonym Appropriate", "dv_synonym")),
  tar_target(fullToast_dv_synonym_good, calc_toast("good", dv_synonym_mv, mdes_dv_synonym_good, "Full DV Synonym Good", "dv_synonym")),
  tar_target(fullToast_dv_synonym_justifiable, calc_toast("justifiable", dv_synonym_mv, mdes_dv_synonym_justifiable, "Full DV Synonym Justifiable", "dv_synonym")),
  tar_target(fullToast_dv_synonym_moral, calc_toast("moral", dv_synonym_mv, mdes_dv_synonym_moral, "Full DV Synonym Moral", "dv_synonym")),
  tar_target(fullToast_dv_synonym_permissible, calc_toast("permissible", dv_synonym_mv, mdes_dv_synonym_permissible, "Full DV Synonym Permissible", "dv_synonym")),
  tar_target(fullToast_dv_synonym_right, calc_toast("right", dv_synonym_mv, mdes_dv_synonym_right, "Full DV Synonym Right", "dv_synonym")),
  tar_target(fullToast_dv_synonym_unethical, calc_toast("unethical", dv_synonym_mv, mdes_dv_synonym_unethical, "Full DV Synonym Unethical", "dv_synonym")),
  tar_target(fullToast_dv_synonym_wrong, calc_toast("wrong", dv_synonym_mv, mdes_dv_synonym_wrong, "Full DV Synonym Wrong", "dv_synonym")),

  ### continuous
  tar_target(fullToast_pma, calc_toast_cont(pma_mv, mdes_PMA, "Full PMA")),
  tar_target(fullToast_pmc, calc_toast_cont(pmc_mv, mdes_PMC, "Full PMC")),
  tar_target(fullToast_responsible, calc_toast_cont(responsible_mv, mdes_responsible, "Full Responsible")),
  tar_target(fullToast_rq, calc_toast_cont(rq_mv, mdes_RQ, "Full RQ")),

  ### Multiple Regression Models
  tar_target(fullToast_intent_in_action_ise, calc_toast_for_multi_reg("sidee", intent_in_action_mv, mdes_intent_in_action_ise, "Trimmed MD SE", "intent")),
  tar_target(fullToast_intent_in_action_ime, calc_toast_for_multi_reg("meane", intent_in_action_mv, mdes_intent_in_action_ise, "Trimmed MD ME", "intent")),
  tar_target(fullToast_intent_in_action_mdi, calc_toast_for_multi_reg("inaction", intent_in_action_mv, mdes_intent_in_action_mdi, "Trimmed I MDI", "in_action")),
  tar_target(fullToast_intent_in_action_mda, calc_toast_for_multi_reg("action", intent_in_action_mv, mdes_intent_in_action_mda, "Trimmed I MDA", "in_action")),
  # ## Responsible Cat Levels x Responsible
  tar_target(fullToast_responsible_with_catLvls, calc_toast_cont(responsibleCat_responsible_mv, mdes_responsible_with_catLvls, "Trimmed Responsible with Cat Levels")),
  #
  #### Trimmed Data
  ### Categorical Moderators
  # Harm
  # harm level
  tar_target(trimmedToast_harm_harm, calc_toast("harm", harm_trimmed_mv, trimmed_mdes_harm_harm, "Trimmed Harm", "harm")),
  # notharm level
  tar_target(trimmedToast_harm_notharm, calc_toast("notharm", harm_trimmed_mv, trimmed_mdes_harm_notharm, "Trimmed Not Harm", "harm")),

  ## Intent
  # meane level
  tar_target(trimmedToast_intent_meane, calc_toast("meane", intent_trimmed_mv, trimmed_mdes_intent_meane, "Trimmed Intent Mean-Effect", "intent")),
  # sidee level
  tar_target(trimmedToast_intent_sidee, calc_toast("sidee", intent_trimmed_mv, trimmed_mdes_intent_sidee, "Trimmed Intent Side-Effect", "intent")),
  #
  #  ## In_action
  # action level
  tar_target(trimmedToast_in_action_action, calc_toast("action", in_action_trimmed_mv, trimmed_mdes_in_action_action, "Trimmed In-Action Action", "in_action")),
  # inaction level
  tar_target(trimmedToast_in_action_inaction, calc_toast("inaction", in_action_trimmed_mv, trimmed_mdes_in_action_inaction, "Trimmed In-Action Inaction", "in_action")),

  ## AI Type A (AI vs. Robot)
  ## AI level
  tar_target(trimmedToast_aiTypeA_ai, calc_toast("AI", aiType_a_trimmed_mv, trimmed_mdes_aiTypeA_ai, "Trimmed AI Type A AI", "aiType_a")),
  # # Robot level
  tar_target(trimmedToast_aiTypeA_robot, calc_toast("robot", aiType_a_trimmed_mv, trimmed_mdes_aiTypeA_robot, "Trimmed AI Type A Robot", "aiType_a")),
  #
  ## AI Type B (AI vs. Mechanical Robot vs. Humanoid Robot)
  # AI level
  tar_target(trimmedToast_aiTypeB_ai, calc_toast("AI", aiType_b_trimmed_mv, trimmed_mdes_aiTypeB_ai, "Trimmed AI Type B AI", "aiType_b")),
  # Mechanical Robot level
  tar_target(trimmedToast_aiTypeB_mechanical, calc_toast("mechanical", aiType_b_trimmed_mv, trimmed_mdes_aiTypeB_mechanical, "Trimmed AI Type B Mechanical Robot", "aiType_b")),
  # Humanoid Robot level
  tar_target(trimmedToast_aiTypeB_humanoid, calc_toast("humanoid", aiType_b_trimmed_mv, trimmed_mdes_aiTypeB_humanoid, "Trimmed AI Type B Humanoid Robot", "aiType_b")),

  #  ## Agent Intelligence
  # # Implied level
  tar_target(trimmedToast_agent_intel_implied, calc_toast("implied", agent_intel_trimmed_mv, trimmed_mdes_agent_intel_implied, "Trimmed Agent Intelligence Implied", "agent_intel")),
  # Not Implied level
  tar_target(trimmedToast_agent_intel_notImplied, calc_toast("notImplied", agent_intel_trimmed_mv, trimmed_mdes_agent_intel_notImplied, "Trimmed Agent Intelligence Not Implied", "agent_intel")),

  # DV synonym ("acceptable", "appropriate", "good", "justifiable", "moral", "permissible", "right", "unethical",  "wrong")
  tar_target(trimmedToast_dv_synonym_acceptable, calc_toast("acceptable", dv_synonym_trimmed_mv, trimmed_mdes_dv_synonym_acceptable, "Trimmed DV Synonym Acceptable", "dv_synonym")),
  tar_target(trimmedToast_dv_synonym_appropriate, calc_toast("appropriate", dv_synonym_trimmed_mv, trimmed_mdes_dv_synonym_appropriate, "Trimmed DV Synonym Appropriate", "dv_synonym")),
  tar_target(trimmedToast_dv_synonym_good, calc_toast("good", dv_synonym_trimmed_mv, trimmed_mdes_dv_synonym_good, "Trimmed DV Synonym Good", "dv_synonym")),
  tar_target(trimmedToast_dv_synonym_justifiable, calc_toast("justifiable", dv_synonym_trimmed_mv, trimmed_mdes_dv_synonym_justifiable, "Trimmed DV Synonym Justifiable", "dv_synonym")),
  tar_target(trimmedToast_dv_synonym_moral, calc_toast("moral", dv_synonym_trimmed_mv, trimmed_mdes_dv_synonym_moral, "Trimmed DV Synonym Moral", "dv_synonym")),
  tar_target(trimmedToast_dv_synonym_permissible, calc_toast("permissible", dv_synonym_trimmed_mv, trimmed_mdes_dv_synonym_permissible, "Trimmed DV Synonym Permissible", "dv_synonym")),
  tar_target(trimmedToast_dv_synonym_right, calc_toast("right", dv_synonym_trimmed_mv, trimmed_mdes_dv_synonym_right, "Trimmed DV Synonym Right", "dv_synonym")),
  tar_target(trimmedToast_dv_synonym_unethical, calc_toast("unethical", dv_synonym_trimmed_mv, trimmed_mdes_dv_synonym_unethical, "Trimmed DV Synonym Unethical", "dv_synonym")),
  tar_target(trimmedToast_dv_synonym_wrong, calc_toast("wrong", dv_synonym_trimmed_mv, trimmed_mdes_dv_synonym_wrong, "Trimmed DV Synonym Wrong", "dv_synonym")),

  ### continuous
  tar_target(trimmedToast_pma, calc_toast_cont(pma_trimmed_mv, trimmed_mdes_PMA, "Trimmed PMA")),
  tar_target(trimmedToast_pmc, calc_toast_cont(pmc_trimmed_mv, trimmed_mdes_PMC, "Trimmed PMC")),
  tar_target(trimmedToast_responsible, calc_toast_cont(responsible_trimmed_mv, trimmed_mdes_responsible, "Trimmed Responsible")),
  tar_target(trimmedToast_rq, calc_toast_cont(rq_trimmed_mv, trimmed_mdes_RQ, "Trimmed RQ")),

  ### Multiple Regression Models
  tar_target(trimmedToast_intent_in_action_ise, calc_toast_for_multi_reg("sidee", intent_in_action_trimmed_mv, trimmed_mdes_intent_in_action_ise, "Trimmed MD SE", "intent")),
  tar_target(trimmedToast_intent_in_action_ime, calc_toast_for_multi_reg("meane", intent_in_action_trimmed_mv, trimmed_mdes_intent_in_action_ise, "Trimmed MD ME", "intent")),
  tar_target(trimmedToast_intent_in_action_mdi, calc_toast_for_multi_reg("inaction", intent_in_action_trimmed_mv, trimmed_mdes_intent_in_action_mdi, "Trimmed I MDI", "in_action")),
  tar_target(trimmedToast_intent_in_action_mda, calc_toast_for_multi_reg("action", intent_in_action_trimmed_mv, trimmed_mdes_intent_in_action_mda, "Trimmed I MDA", "in_action")),
  ## Responsible Cat Levels x Responsible
  tar_target(trimmedToast_responsible_with_catLvls, calc_toast_cont(responsibleCat_responsible_trimmed_mv, trimmed_mdes_responsible_with_catLvls, "Trimmed Responsible with Cat Levels")),

  ###########################
  # TABLES
  ###########################
  tar_target(descriptives, construct_desc_tbl(trimmed_df)),
  tar_target(df_excel, format_col_desc_tbl(descriptives)),
  tar_target(
    desc_excel_csv,
    {
      dir.create("outputs", showWarnings = FALSE, recursive = TRUE)
      write.csv(df_excel, "outputs/desc_excel.csv", row.names = FALSE)
      "outputs/desc_excel.csv"
    },
    format = "file"
  ),
  tar_target(demo_workbook, create_descriptives_workbook(df_excel)),
  tar_target(apa_col_influence_table, format_influential_stats_table_apa(influence_stats_df)),
  tar_target(apa_col_outlier_table, format_full_outlier_stats_table_apa(influence_stats_df)),
  tar_target(apa_influence_table, create_influential_workbook(apa_col_influence_table)),
  tar_target(model_fit_table_full, create_model_fit_table(
    "threelvl_article_pID",
    "threelvl_pID",
    "threelvl",
    "twolvl_pID",
    "onelvl"
  )),
  tar_target(model_fit_table, create_model_fit_table(
    "threelvl_trimmed_article_pID",
    "threelvl_trimmed_pID",
    "threelvl_trimmed",
    "twolvl_trimmed_pID",
    "onelvl_trimmed"
  )),
  tar_target(dv_syn_table, create_k_dv_syn_sum_table(dv_synonym_trimmed_mv)),
  tar_target(dv_syn_table_full, create_k_dv_syn_sum_table(dv_synonym_mv)),
  tar_target(TOST_dv_syn_table, create_TOST_dv_syn_sum_table(
    trimmedToast_dv_synonym_acceptable,
    trimmedToast_dv_synonym_appropriate,
    trimmedToast_dv_synonym_good,
    trimmedToast_dv_synonym_justifiable,
    trimmedToast_dv_synonym_moral,
    trimmedToast_dv_synonym_permissible,
    trimmedToast_dv_synonym_right,
    trimmedToast_dv_synonym_unethical,
    trimmedToast_dv_synonym_wrong
  )),
  tar_target(TOST_dv_syn_table_full, create_TOST_dv_syn_sum_table(
    fullToast_dv_synonym_acceptable,
    fullToast_dv_synonym_appropriate,
    fullToast_dv_synonym_good,
    fullToast_dv_synonym_justifiable,
    fullToast_dv_synonym_moral,
    fullToast_dv_synonym_permissible,
    fullToast_dv_synonym_right,
    fullToast_dv_synonym_unethical,
    fullToast_dv_synonym_wrong
  )),
  tar_target(rq_df, "docs/RQ_formattedTable.csv", format = "file"),
  tar_target(rq_table, create_rq_table(rq_df)),
  tar_target(min_k_cat_level, create_low_cat_lvl_tbl(full_df, trimmed_df)),
  tar_target(stream_min_k_cat_lvl_tbl, streamline_min_k_cat_level_tbl(min_k_cat_level)),
  tar_target(mod_table_apa, create_mod_df_table(trimmed_df)),
  ###########################
  # FIGURES
  ###########################
  tar_target(funnel_es_trimmed, three_funnel(trimmed_df, "funnel_es_trimmed")),
  tar_target(funnel_es_full, three_funnel(full_df, "funnel_es_full")),
  tar_target(funnel_study_trimmed, three_funnel_study(trimmed_df, "funnel_study_trimmed")),
  tar_target(funnel_study_full, three_funnel_study(full_df, "funnel_study_full")),
  tar_target(orchard_pub_bias_full, make_orchard_pub_bias_plot(m_multi, full_data_i2, "orchard_pub_bias_full")),
  tar_target(orchard_pub_bias_trimmed, make_orchard_pub_bias_plot(m_multi_trimmed, trimmed_data_i2, "orchard_pub_bias_trimmed")),
  tar_target(orchard_egger_reg_full, make_orchard_egger_reg_plot(m_multi, full_df, "orchard_egger_reg_full")),
  tar_target(orchard_egger_reg_trimmed, make_orchard_egger_reg_plot(m_multi_trimmed, trimmed_df, "orchard_egger_reg_trimmed")),
  tar_target(forest_subsets, create_forest_subsets(trimmed_df)),
  tar_target(forest_df, "tables/forest_df.csv", format = "file"),
  tar_target(first_forest_subset, create_forest_plot_for_subset(forest_subsets[["first_studies"]], "first_forest_subset_a4", forest_df,
    plot_title = "Forest Plot", return_summary = FALSE, "first"
  )),
  tar_target(second_forest_subset, create_forest_plot_for_subset(forest_subsets[["second_studies"]], "second_forest_subset_a4", forest_df,
    plot_title = "Forest Plot", return_summary = FALSE, "second"
  )),
  tar_target(third_forest_subset, create_forest_plot_for_subset(forest_subsets[["third_studies"]], "third_forest_subset_a4", forest_df,
    plot_title = "Forest Plot", return_summary = FALSE, "last", m_multi, m_multi_trimmed
  )),
  tar_target(pmc_orchard_plot_full, make_orchard_plot_for_cont(full_df, pmc_mv, "PMC", "pmc_orchard_plot_full")),
  tar_target(pma_orchard_plot_full, make_orchard_plot_for_cont(full_df, pma_mv, "PMA", "pma_orchard_plot_full")),
  tar_target(responsible_orchard_plot_full, make_orchard_plot_for_cont(full_df, responsible_mv, "responsible", "responsible_orchard_plot_full")),
  tar_target(rq_orchard_plot_full, make_orchard_plot_for_cont(rqz_df, rq_mv, "RQ", "rq_orchard_plot_full")),
  tar_target(rq_distribution_plot_full, create_distribution_plot_for_rq(full_df, "RQ")),
  tar_target(harm_orchard_plot_full, create_orchard_plot_for_cat_mods(harm_mv, "harm", "harm_orchard_plot_full")),
  tar_target(intent_orchard_plot_full, create_orchard_plot_for_cat_mods(intent_mv, "intent", "intent_orchard_plot_full")),
  tar_target(in_action_orchard_plot_full, create_orchard_plot_for_cat_mods(in_action_mv, "in_action", "in_action_orchard_plot_full")),
  tar_target(aiTypeA_orchard_plot_full, create_orchard_plot_for_cat_mods(aiType_a_mv, "aiType_a", "aiTypeA_orchard_plot_full")),
  tar_target(aiTypeB_orchard_plot_full, create_orchard_plot_for_cat_mods(aiType_b_mv, "aiType_b", "aiTypeB_orchard_plot_full")),
  tar_target(agent_intel_orchard_plot_full, create_orchard_plot_for_cat_mods(agent_intel_mv, "agent_intel", "agent_intel_orchard_plot_full")),
  tar_target(dv_synonym_orchard_plot_full, create_orchard_plot_for_cat_mods(dv_synonym_mv, "dv_synonym", "dv_synonym_orchard_plot_full")),
  tar_target(responsible_with_cat_orchard_plot_full, create_orchard_plot_for_cat_mods(responsibleCat_responsible_mv, "responsibleCat", "responsible_with_cat_orchard_plot_full")),
  tar_target(pmc_orchard_plot_trimmed, make_orchard_plot_for_cont(trimmed_df, pmc_trimmed_mv, "PMC", "pmc_orchard_plot_trimmed")),
  tar_target(pma_orchard_plot_trimmed, make_orchard_plot_for_cont(trimmed_df, pma_trimmed_mv, "PMA", "pma_orchard_plot_trimmed")),
  tar_target(responsible_orchard_plot_trimmed, make_orchard_plot_for_cont(trimmed_df, responsible_trimmed_mv, "responsible", "responsible_orchard_plot_trimmed")),
  tar_target(rq_orchard_plot_trimmed, make_orchard_plot_for_cont(rqz_trimmed_df, rq_trimmed_mv, "RQ", "rq_orchard_plot_trimmed")),
  tar_target(rq_distribution_plot_trimmed, create_distribution_plot_for_rq(trimmed_df, "RQ")),
  tar_target(harm_orchard_plot_trimmed, create_orchard_plot_for_cat_mods(harm_trimmed_mv, "harm", "harm_orchard_plot_trimmed")),
  tar_target(intent_orchard_plot_trimmed, create_orchard_plot_for_cat_mods(intent_trimmed_mv, "intent", "intent_orchard_plot_trimmed")),
  tar_target(in_action_orchard_plot_trimmed, create_orchard_plot_for_cat_mods(in_action_trimmed_mv, "in_action", "in_action_orchard_plot_trimmed")),
  tar_target(aiTypeA_orchard_plot_trimmed, create_orchard_plot_for_cat_mods(aiType_a_trimmed_mv, "aiType_a", "aiTypeA_orchard_plot_trimmed")),
  tar_target(aiTypeB_orchard_plot_trimmed, create_orchard_plot_for_cat_mods(aiType_b_trimmed_mv, "aiType_b", "aiTypeB_orchard_plot_trimmed")),
  tar_target(agent_intel_orchard_plot_trimmed, create_orchard_plot_for_cat_mods(agent_intel_trimmed_mv, "agent_intel", "agent_intel_orchard_plot_trimmed")),
  tar_target(dv_synonym_orchard_plot_trimmed, create_orchard_plot_for_cat_mods(dv_synonym_trimmed_mv, "dv_synonym", "dv_synonym_orchard_plot_trimmed")),
  tar_target(responsible_with_cat_orchard_plot_trimmed, create_orchard_plot_for_cat_mods(responsibleCat_responsible_trimmed_mv, "responsibleCat", "responsible_with_cat_orchard_plot_trimmed"))
)
