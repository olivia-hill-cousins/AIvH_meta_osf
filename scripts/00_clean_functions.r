## For transparency, the functions for cleaning the data are included below. However, the data themselves cannot be provided due to data privacy constraints. 
# Instead, we read in the data like this:
read_clean_data <- function(clean_data_csv){
  read.csv(clean_data_csv)
}

read_rds_data <- function(path){
  readRDS(path)
}

# ====================================================================== banks 2021 s1
# function for cleaning banks2021 s1
clean_banks2021_s1 <- function(raw_data) {
  data <- read.csv(raw_data)
  data$agent <- as.factor(data$ConditionAgent)
  data$agent <- recode(data$agent,
    `0` = 1,
    `1` = 2
  )
  data$agent <- factor(data$agent,
    levels = c(1, 2),
    labels = c("human", "AI")
  )

  # recalculate DV so only incl. moral scenarios (i.e. don't incl. nonmoral goodness)
  data <- data %>%
    dplyr::mutate(good = rowMeans(across(
      c("Care_Good", "Fair_Good", "Authority_Good", "Loyalty_Good", "Purity_Good", "Liberty_Good"),
      ~ as.numeric(.)
    ), na.rm = TRUE))

  # ========== Data Cleaning for Moderator Analyses ========#
  # Harm Domain Moderator
  data$harm.good <- data$Care_Good
  Hmisc::label(data$harm.good) <- "Harm Moderator - harm level"
  data <- data %>%
    dplyr::mutate(notharm.good = rowMeans(dplyr::select(., "Authority_Good", "Fair_Good", "Liberty_Good", "Loyalty_Good", "Purity_Good"), na.rm = TRUE))
  Hmisc::label(data$notharm.good) <- "Harm Moderator - notharm level"


  # Intent Moderator
  data <- data %>%
    dplyr::mutate(
      sidee = case_when(
        ConditionValence == "0" ~ rowMeans(dplyr::select(., Authority_Good, Care_Good, Fair_Good, Liberty_Good, Loyalty_Good, Purity_Good), na.rm = TRUE),
        ConditionValence == "1" ~ rowMeans(dplyr::select(., Authority_Good, Loyalty_Good, Purity_Good), na.rm = TRUE),
        TRUE ~ NA_real_
      ),
      meane = case_when(
        ConditionValence == "1" ~ rowMeans(dplyr::select(., Care_Good, Fair_Good, Liberty_Good), na.rm = TRUE),
        TRUE ~ NA_real_
      )
    )
  # PMA Moderator
  data <- data %>%
    dplyr::mutate(PMA = rowMeans(across(c("MEANMC_PMA_MoralCapacity", "MEANDep_PMA_Dependency"), ~ as.numeric(.)), na.rm = TRUE))

  data$PMA.bin <- data$MoralityBinary


  # AI Agent Type A Moderator
  data <- data %>%
    dplyr::mutate(aiType_a = recode_factor(as.character(agent),
      "human" = "human",
      "AI" = "robot"
    ))

  # AI Agent Type B Moderator
  data <- data %>%
    dplyr::mutate(aiType_b = recode_factor(as.character(agent),
      "human" = "human",
      "AI" = "humanoid"
    ))

  # Responsible Moderator
  data <- data %>%
    dplyr::mutate(responsible = rowMeans(across(
      c("Care_Responsible", "Fair_Responsible", "Authority_Responsible", "Loyalty_Responsible", "Purity_Responsible", "Liberty_Responsible"),
      ~ as.numeric(.)
    ), na.rm = TRUE))
  data$ID <- data$QualtricsID
  
  data$SexCat <- as.character(data$SexCat)
  data <- data %>%
    mutate(gender = recode(SexCat,
                          "0" = "male",
                          "1" = "female"
    ))
  data$age <- data$AgeNum
  

  banks2021_s1 <- data
  return(banks2021_s1)
}
# ====================================================================== banks 2021 s2
# function for cleaning banks2021 s2
clean_banks2021_s2 <- function(raw_data) {
  data <- read.csv(raw_data)

  data$agent <- as.factor(data$CondAgent)
  data$agent <- recode(data$agent,
    `0` = 1,
    `1` = 2
  )
  data$agent <- factor(data$agent,
    levels = c(1, 2),
    labels = c("human", "AI")
  )

  # recalculate DV so only incl. moral scenarios (i.e. don't incl. nonmoral goodness)
  data <- data %>% dplyr::mutate(good = rowMeans(across(c("Care_Goodness", "Fairness_Goodness", "Authority_Goodness", "Loyalty_Goodness", "Purity_Goodness", "Liberty_Goodness"), ~ as.numeric(.)), na.rm = TRUE))

  # ========== Data Cleaning for Moderator Analyses ========#
  data$harm.good <- data$Care_Goodness
  Hmisc::label(data$harm.good) <- "Harm Moderator - harm level"
  data <- data %>%
    dplyr::mutate(notharm.good = rowMeans(dplyr::select(., "Authority_Goodness", "Fairness_Goodness", "Liberty_Goodness", "Loyalty_Goodness", "Purity_Goodness"), na.rm = TRUE))
  Hmisc::label(data$notharm.good) <- "Harm Moderator - notharm level"

  # Intent Moderator
  data <- data %>%
    dplyr::mutate(
      sidee = case_when(
        CondValence == "0" ~ rowMeans(dplyr::select(., "Authority_Goodness", "Care_Goodness", "Fairness_Goodness", "Liberty_Goodness", "Loyalty_Goodness", "Purity_Goodness"), na.rm = TRUE),
        CondValence == "1" ~ rowMeans(dplyr::select(., "Authority_Goodness", "Loyalty_Goodness", "Purity_Goodness"), na.rm = TRUE),
        TRUE ~ NA_real_
      ),
      meane = case_when(
        CondValence == "1" ~ rowMeans(dplyr::select(., "Care_Goodness", "Fairness_Goodness", "Liberty_Goodness"), na.rm = TRUE),
        TRUE ~ NA_real_
      )
    )

  # Responsibility Moderator
  data <- data %>%
    dplyr::mutate(blame = rowMeans(across(c("CBlameCareRecode", "FBlameFairnessRecode", "ABlameAuthorityRecode", "LBlameLoyaltyRecode", "PBlamePurityRecode", "LiBlameLibertyRecode"), ~ as.numeric(.)), na.rm = TRUE))
  data$praise <- as.numeric(data$Mind_Moral_PraiseMoralAction)
  # PMA Moderator
  data <- data %>%
    dplyr::mutate(PMA = rowMeans(across(c("MEANMC_PMA_MoralCapacity", "MEAND_PMA_Dependency"), ~ as.numeric(.)), na.rm = TRUE))

  data$PMA.bin <- data$MoralityBinary
  # PMC Moderator
  data <- data %>%
    dplyr::mutate(PMC = rowMeans(across(c(
      "Mind_Moral_DisapprovMoral", "Mind_Moral_RightWrong", "Mind_Moral_UpholdMoralVal", "Mind_Moral_PraiseMoralAction",
      "Mind_Social_InferThinking", "Mind_Social_PlanFuture", "Mind_Social_UnderstandMinds", "Mind_Social_SetGoals",
      "Mind_Interact_CommVerbal", "Mind_Interact_SeeHearWorld", "Mind_Interact_LearnInstruct", "Mind_Interact_MoveOnOwn"
    ), ~ as.numeric(.)), na.rm = TRUE))

  # AI Agent Type A Moderator
  data <- data %>%
    dplyr::mutate(aiType_a = recode_factor(as.character(agent),
      "human" = "human",
      "AI" = "robot"
    ))

  # AI Agent Type B Moderator
  data <- data %>%
    dplyr::mutate(aiType_b = recode_factor(as.character(agent),
      "human" = "human",
      "AI" = "humanoid"
    ))

  data$ID <- data$QualtricsID
  data$age <- data$Age
  data <- data %>%
    mutate(gender = recode(Gender,
                          "Male" = "male",
                          "Female" = "female",
                          "Nonbinary" = "nonbinary"
    ))
  

  banks2021_s2 <- data
  return(banks2021_s2)
}

# ====================================================================== bigman 2018 s3
# function for cleaning bigman2018 s3
clean_bigman2018_s3 <- function(raw_data) {
  data <- read_sav(raw_data)

  # make sure agent is a factor variable
  data$agent <- as.factor(data$agent)
  data$AC_1 <- as.factor(data$AC_1)
  data$AC_2 <- as.factor(data$AC_2)

  # filter out Ps who failed attention check
  data <- data %>%
    filter(
      AC_2 == 2,
      (AC_1 == 1 & agent == -1) | (AC_1 == 2 & agent == 1)
    )

  # give agent consistent coding & labels
  data$agent <- recode(data$agent,
    `-1` = 1,
    `1` = 2
  )
  data$agent <- factor(data$agent,
    levels = c(1, 2),
    labels = c("human", "AI")
  )

  # ---- DVs
  # moral
  data$moral <- data$mj1

  # right
  data$right <- data$mj5

  # permissible
  data$permissible <- data$mj6

  # ========== Data Cleaning for Moderator Analyses ========#
  # Responsibility Moderator
  # blame / praise (highest value is praise, & lowest is blame - midpoint is neutral)
  data <- data %>%
    dplyr::mutate(
      praise = ifelse(mj2 > 5, mj2 - 5, NA_real_),
      blame = ifelse(mj2 < 5, mj2, NA_real_)
    )
  # punishment/reward (highest value is reward, & lowest is punishment - midpoint is neutral)
  data <- data %>%
    dplyr::mutate(
      reward = ifelse(mj4 > 5, mj4 - 5, NA_real_),
      punishment = ifelse(mj4 < 5, mj4, NA_real_)
    )
  # PMA Moderator
  # rev. "forbidden from"
  data$app_3 <- 6 - data$app_3
  data$PMA1 <- data$app_1
  data$PMA2 <- data$app_2
  data$PMA3 <- data$app_3

  # PMC Moderator
  data <- data %>%
    dplyr::mutate(PMC = rowMeans(across(
      c("mind_4", "mind_5", "mind_6", "mind_7", "mind_8", "mind_9"),
      ~ as.numeric(.)
    ), na.rm = TRUE))
  # AI Agent Type A & B Moderator
  data$aiType_a <- data$aiType_b <- data$agent

  data$ID <- 1:nrow(data)
  data$age <- data$Age
  data$gender <- as.character(haven::as_factor(data$Gender))
  
  data <- data %>%
    mutate(gender = recode(Gender,
                          "1" = "male",
                          "2" = "female"
    ))

  bigman2018_s3 <- data
  return(bigman2018_s3)
}

# ====================================================================== bigman 2018 s4
# function for cleaning bigman2018 s4
clean_bigman2018_s4 <- function(raw_data) {
  data <- read_sav(raw_data)

  # make sure agent is a factor variable
  data$agent <- as.factor(data$agent)
  data$AC_1 <- as.factor(data$AC_1)
  data$AC_2 <- as.factor(data$AC_2)

  # filter out Ps who failed attention check
  data <- data %>%
    filter(
      AC_2 == 2,
      (AC_1 == 1 & agent == -1) | (AC_1 == 2 & agent == 1)
    )

  # give agent consistent coding & labels
  data$agent <- recode(data$agent,
    `-1` = 1,
    `1` = 2
  )
  data$agent <- factor(data$agent,
    levels = c(1, 2),
    labels = c("human", "AI")
  )
  # data$agent

  # ---- DVs
  # moral
  data$moral <- data$mj1

  # right
  data$right <- data$mj5

  # permissible
  data$permissible <- data$mj6

  # ========== Data Cleaning for Moderator Analyses ========#
  # Responsibility Moderator
  # blame / praise (highest value is praise, & lowest is blame - midpoint is neutral)
  data <- data %>%
    dplyr::mutate(
      praise = ifelse(mj2 > 5, mj2 - 5, NA_real_),
      blame = ifelse(mj2 < 5, mj2, NA_real_)
    )
  # punishment/reward (highest value is reward, & lowest is punishment - midpoint is neutral)
  data <- data %>%
    dplyr::mutate(
      reward = ifelse(mj4 > 5, mj4 - 5, NA_real_),
      punishment = ifelse(mj4 < 5, mj4, NA_real_)
    )
  # PMA Moderator
  # rev. "forbidden from"
  data$app3 <- 6 - data$app3
  data$PMA1 <- data$app1
  data$PMA2 <- data$app2
  data$PMA3 <- data$app3
  # PMC Moderator
  data <- data %>%
    dplyr::mutate(PMC = rowMeans(across(
      c("agency", "exper"),
      ~ as.numeric(.)
    ), na.rm = TRUE))

  # AI Agent Type A & B Moderator
  data$aiType_a <- data$aiType_b <- data$agent

  data$ID <- 1:nrow(data)
  data$age <- data$Age
  data$gender <- as.character(haven::as_factor(data$Gender))
  
  data <- data %>%
    mutate(gender = recode(Gender,
                           "1" = "male",
                           "2" = "female"
    ))
  
  bigman2018_s4 <- data
  bigman2018_s4
}
# ====================================================================== bigman 2018 s5
# function for cleaning bigman2018 s5
clean_bigman2018_s5 <- function(raw_data) {
  data <- read_sav(raw_data)

  # make sure agent is a factor variable
  data$agent <- as.factor(data$agent)
  data$AC_1 <- as.factor(data$AC_1)
  data$AC_2 <- as.factor(data$AC_2)

  # filter out Ps who failed attention check
  data <- data %>%
    filter(
      (AC_2 == 1 & outcome == 1) | (AC_2 == 2 & outcome == -1),
      (AC_1 == 1 & agent == -1) | (AC_1 == 2 & agent == 1)
    )


  # give agent consistent coding & labels
  data$agent <- recode(data$agent,
    `-1` = 1,
    `1` = 2
  )
  data$agent <- factor(data$agent,
    levels = c(1, 2),
    labels = c("human", "AI")
  )
  # data$agent

  # ---- DVs
  # moral
  data$moral <- data$mj1

  # right
  data$right <- data$mj5

  # permissible
  data$permissible <- data$mj6

  # ========== Data Cleaning for Moderator Analyses ========#
  # Responsibility Moderator
  # blame / praise (highest value is praise, & lowest is blame - midpoint is neutral)
  data <- data %>%
    dplyr::mutate(
      praise = ifelse(mj2 > 5, mj2 - 5, NA_real_),
      blame = ifelse(mj2 < 5, mj2, NA_real_)
    )
  # punishment/reward (highest value is reward, & lowest is punishment - midpoint is neutral)
  data <- data %>%
    dplyr::mutate(
      reward = ifelse(mj4 > 5, mj4 - 5, NA_real_),
      punishment = ifelse(mj4 < 5, mj4, NA_real_)
    )
  # PMA Moderator
  # rev. "forbidden from"
  data$app_app_3 <- 6 - data$app_app_3
  data$PMA1 <- data$app_app_1
  data$PMA2 <- data$app_app_2
  data$PMA3 <- data$app_app_3

  # PMC Moderator
  data <- data %>%
    dplyr::mutate(PMC = rowMeans(across(
      c("mind_agency_1", "mind_agency_2", "mind_agency_3", "mind_exper_1", "mind_exper_2", "mind_exper_3"),
      ~ as.numeric(.)
    ), na.rm = TRUE))

  # AI Agent Type A & B Moderator
  data$aiType_a <- data$aiType_b <- data$agent


  data$ID <- 1:nrow(data)
  data$age <- data$Age
  data$gender <- as.character(haven::as_factor(data$Gender))
  
  data <- data %>%
    mutate(gender = recode(Gender,
                           "1" = "male",
                           "2" = "female"
    ))
  
  bigman2018_s5 <- data
  bigman2018_s5
}

# ====================================================================== chu 2023 s2
# function for cleaning chu2023_s2
clean_chu2023_s2 <- function(raw_data) {
  data <- read.csv(raw_data)


  # make sure agent is a factor variable
  data$agent <- data$Agent
  data$agent <- as.factor(data$agent)


  # give agent consistent coding & labels
  data$agent <- recode(data$agent,
    `0` = 1,
    `1` = 2
  )
  data$agent <- factor(data$agent,
    levels = c(1, 2),
    labels = c("human", "AI")
  )
  # data$agent

  #---- DVs
  # permissibility
  data$permissible <- data$Permitted

  # wrongness (reversed)
  data$wrong <- 11 - data$Wrong

  # acceptability
  data$acceptable <- data$Acceptability

  # ========== Data Cleaning for Moderator Analyses ========#
  # Responsibility Moderator
  data$blame <- as.numeric(data$Blameworthy)
  # AI Agent Type A Moderator
  data <- data %>%
    dplyr::mutate(aiType_a = recode_factor(as.character(agent),
      "human" = "human",
      "AI" = "robot"
    ))

  # AI Agent Type B Moderator
  data <- data %>%
    dplyr::mutate(aiType_b = recode_factor(as.character(agent),
      "human" = "human",
      "AI" = "mechanical"
    ))


  
  # to avoid unnesting inaccurate combinations e.g. inaction & mean E, we subset the data here
  data$Decision <- as.factor(data$Decision)
  
  data$age <- data$Age
  data <- data %>%
    mutate(gender = recode(Gender,
                          "0" = "male",
                          "1" = "female"
    ))
  chu2023_s2 <- data
  chu2023_s2
}

make_se_action_subset_chu2023_s2 <- function(clean_data) {
  data <- clean_data
  # Subset by dilemmas Moderator
  chu2023_trol <- data %>%
    filter(Dilemma == "0")

  # subset by decision made
  chu2023_trol.a <- chu2023_trol %>%
    filter(Decision == "1")

  chu2023_trol.a
}

make_me_action_subset_chu2023_s2 <- function(clean_data) {
  data <- clean_data
  # Subset by dilemmas Moderator
  chu2023_trol <- data %>%
    filter(Dilemma == "0")

  chu2023_foot <- data %>%
    filter(Dilemma == "1")

  # subset by decision made
  chu2023_foot.a <- chu2023_foot %>%
    filter(Decision == "1")

  chu2023_foot.a
}

make_inaction_subset_chu2023_s2 <- function(clean_data) {
  data <- clean_data
  # Subset by dilemmas Moderator
  chu2023_trol <- data %>%
    filter(Dilemma == "0")

  chu2023_foot <- data %>%
    filter(Dilemma == "1")

  # subset by decision made
  chu2023_trol.ia <- chu2023_trol %>%
    filter(Decision == "0")

  chu2023_foot.ia <- chu2023_foot %>%
    filter(Decision == "0")

  chu2023.ia <- bind_rows(chu2023_trol.ia, chu2023_foot.ia)
  chu2023.ia
}

# ====================================================================== he 2024 s1
# function for cleaning he2024_s1
clean_he2024_s1 <- function(raw_data) {
  data <- read.csv(raw_data)
  data$ID <- data$Sub

  # avg the morality DV scale for loyalty & authority (omitting nonmoral)
  ## print(names(data))
  data <- data %>%
    dplyr::mutate(morality.AI = rowMeans(dplyr::select(., "moral.scale_loyalty_AI", "moral.scale_authority_AI"), na.rm = TRUE))

  data <- data %>%
    dplyr::mutate(morality.human = rowMeans(dplyr::select(., "moral.scale_loyalty_human", "moral.scale_authority_human"), na.rm = TRUE))

  # convert from wide to long format
  data <- gather(data, agent, moral, "morality.AI":"morality.human", factor_key = TRUE)

  # give agent consistent coding & labels
  data$agent <- recode(data$agent,
    "morality.human" = 1,
    "morality.AI" = 2
  )
  data$agent <- factor(data$agent,
    levels = c(1, 2),
    labels = c("human", "AI")
  )

  # AI Type A & B Moderators
  data$aiType_a <- data$aiType_b <- data$agent
  

  he2024_s1 <- data
  he2024_s1
}

# ====================================================================== komatsu 2021 s1
# function for cleaning komatsu2021_s1
clean_komatsu2021_s1 <- function(raw_data) {
  data <- read.csv(raw_data)
  data$ID <- data$SubjID

  data$agent <- factor(data$Agent)
  data$agent <- factor(data$agent,
    levels = c(1, 2),
    labels = c("human", "AI")
  )
  data$permissible <- data$Permissible.

  # ========== Data Cleaning for Moderator Analyses ========#
  # In.action Moderator
  data <- data %>%
    dplyr::mutate(
      Decision = as.factor(Decision),
      in_action = case_when(
        Decision == "1" ~ "action",
        Decision %in% c("0") ~ "inaction",
        TRUE ~ NA_character_
      )
    )


  # Responsible Moderator
  data$blame <- as.numeric(data$Blame_Rating)
  # PMC Moderator
  data <- data %>%
    dplyr::mutate(PMC = rowMeans(across(
      c(
        "Pain", "Afraid", "Pleasure",
        "SelfControl", "Deliberate", "Remember"
      ),
      ~ as.numeric(.)
    ), na.rm = TRUE))
  # AI Agent Type A Moderator
  data <- data %>%
    dplyr::mutate(aiType_a = recode_factor(as.character(agent),
      "human" = "human",
      "AI" = "robot"
    ))

  # AI Agent Type B Moderator
  data <- data %>%
    dplyr::mutate(aiType_b = recode_factor(as.character(agent),
      "human" = "human",
      "AI" = "mechanical"
    ))
  data$age <- data$Age
  data <- data %>%
    mutate(gender = recode(Gender,
                           "1" = "male",
                           "0" = "female",
                           .default = "other"
    ))
  komatsu2021_s1 <- data
  komatsu2021_s1
}

#-------------------------- study 2 + 3
read_combined_komatsu2021_s2_3_data <- function(combined_data) {
  dataDouble <- read.csv(combined_data)
  dataDouble
}

clean_komatsu2021_s2 <- function(dataDouble) {
  # separate data from 2 + 3
  data <- filter(dataDouble, Study == "3.2")

  data$ID <- 1:nrow(data)

  data$agent <- factor(data$Agent)
  data$agent <- factor(data$agent,
    levels = c(1, 2),
    labels = c("human", "AI")
  )
  data$permissible <- as.factor(data$Permissible)

  # ========== Data Cleaning for Moderator Analyses ========#
  # In.action Moderator
  data <- data %>%
    dplyr::mutate(
      Decision = as.factor(Decision),
      in_action = case_when(
        Decision == "1" ~ "action",
        Decision %in% c("0") ~ "inaction",
        TRUE ~ NA_character_
      )
    )


  # Responsible Moderator
  data$blame <- as.numeric(data$Blame)
  # PMC Moderator
  data <- data %>%
    dplyr::mutate(PMC = rowMeans(across(
      c(
        "Pain", "Afraid", "Pleasure",
        "SelfControl", "Deliberate", "Remember"
      ),
      ~ as.numeric(.)
    ), na.rm = TRUE))
  # AI Agent Type A Moderator
  data <- data %>%
    dplyr::mutate(aiType_a = recode_factor(as.character(agent),
      "human" = "human",
      "AI" = "robot"
    ))

  # AI Agent Type B Moderator
  data <- data %>%
    dplyr::mutate(aiType_b = recode_factor(as.character(agent),
      "human" = "human",
      "AI" = "mechanical"
    ))
  data$age <- data$Age
  data <- data %>%
    mutate(gender = recode(Gender,
                           "1" = "male",
                           "0" = "female",
                           .default = "other"
    ))

  komatsu2021_s2 <- data
  komatsu2021_s2
}

clean_komatsu2021_s3 <- function(dataDouble) {
  # separate data from 2 + 3
  data <- filter(dataDouble, Study == "3.3")

  data$ID <- 1:nrow(data)

  data$agent <- factor(data$Agent)
  data$agent <- factor(data$agent,
    levels = c(1, 2),
    labels = c("human", "AI")
  )
  data$permissible <- as.factor(data$Permissible)

  # ========== Data Cleaning for Moderator Analyses ========#
  # In.action Moderator
  data <- data %>%
    dplyr::mutate(
      Decision = as.factor(Decision),
      in_action = case_when(
        Decision == "1" ~ "action",
        Decision %in% c("0") ~ "inaction",
        TRUE ~ NA_character_
      )
    )


  # Responsible Moderator
  data$blame <- as.numeric(data$Blame)
  # PMC Moderator
  data <- data %>%
    dplyr::mutate(PMC = rowMeans(across(
      c(
        "Pain", "Afraid", "Pleasure",
        "SelfControl", "Deliberate", "Remember"
      ),
      ~ as.numeric(.)
    ), na.rm = TRUE))
  # AI Agent Type A Moderator
  data <- data %>%
    dplyr::mutate(aiType_a = recode_factor(as.character(agent),
      "human" = "human",
      "AI" = "robot"
    ))

  # AI Agent Type B Moderator
  data <- data %>%
    dplyr::mutate(aiType_b = recode_factor(as.character(agent),
      "human" = "human",
      "AI" = "mechanical"
    ))
  data$age <- data$Age
  data <- data %>%
    mutate(gender = recode(Gender,
                           "1" = "male",
                           "0" = "female",
                           .default = "other"
    ))

  komatsu2021_s3 <- data
  komatsu2021_s3
}

# ====================================================================== laakasuo 2021 s1
# function for cleaning laakasuo2021_s1
clean_laakasuo2021_s1 <- function(raw_data) {
  data <- read_dta(raw_data)

  data$ID <- 1:nrow(data)
  # collapse AI agents into 1
  data <- data %>%
    dplyr::mutate(agent = recode(as.factor(Agent2),
      `1` = "1",
      .default = "2"
    ))

  data$agent <- factor(data$agent,
    levels = c(1, 2),
    labels = c("human", "AI")
  )


  # avg the morality DV across all scenarios
  data <- data %>%
    dplyr::mutate(moral = rowMeans(dplyr::select(., "TerroristSon_Moral":"Mountain_Moral"), na.rm = TRUE))

  # ========== Data Cleaning for Moderator Analyses ========#

  # PMA Moderator
  data$PMA <- as.numeric(data$mc_1_q_1)
  # PMC Moderator
  data <- data %>%
    dplyr::mutate(PMC = rowMeans(across(
      c("mindperc1":"mindperc17"),
      ~ as.numeric(.)
    ), na.rm = TRUE))
  # AI Agent Type A Moderator
  data <- data %>%
    dplyr::mutate(aiType_a = recode_factor(as.character(agent),
      "human" = "human",
      "AI" = "robot"
    ))

  # AI Agent Type B Moderator
  data$Agent2 <- as.factor(data$Agent2)
  data <- data %>%
    dplyr::mutate(Agent2 = recode_factor(Agent2,
      `1` = "1",
      `2` = "2", .default = "3"
    ))
  data <- data %>%
    dplyr::mutate(aiType_b = recode_factor(as.character(Agent2),
      `1` = "human",
      `2` = "mechanical",
      `3` = "humanoid"
    ))
  
  data$age <- data$age
  data <- data %>%
    mutate(gender = recode(gender,
                           "0" = "male",
                           "1" = "female"
    ))
  laakasuo2021_s1 <- data
  laakasuo2021_s1
}

# to avoid unnesting inaccurate combinations e.g. inaction & mean E, we subset the data here
# Subset by action/inaction (and meane/sidee)
make_action_subset_laakasuo2021_s1 <- function(clean_data) {
  data <- clean_data
  data$Deont2 <- as.factor(data$Deont2)
  laak2021_s1.a <- data %>%
    filter(Deont2 == "1")
  laak2021_s1.a
}

make_inaction_subset_laakasuo2021_s1 <- function(clean_data) {
  data <- clean_data
  data$Deont2 <- as.factor(data$Deont2)
  laak2021_s1.ia <- data %>%
    filter(Deont2 == "0")
  laak2021_s1.ia
}

# ====================================================================== laakasuo 2021 s2
# function for cleaning laakasuo2021_s2
clean_laakasuo2021_s2 <- function(raw_data) {
  data <- read_dta(raw_data)

  data$ID <- 1:nrow(data)

  # collapse AI agents into 1
  data <- data %>%
    dplyr::mutate(agent = recode(as.factor(decider),
      `1` = "1",
      .default = "2"
    ))

  data$agent <- factor(data$agent,
    levels = c(1, 2),
    labels = c("human", "AI")
  )

  data$moral <- data$DV4i

  # ========== Data Cleaning for Moderator Analyses ========#
  # PMA Moderator
  data$PMA <- data$ma
  # PMC Moderator
  data <- data %>%
    dplyr::mutate(PMC = rowMeans(across(
      c("manmindper_4":"manmindper_20"),
      ~ as.numeric(.)
    ), na.rm = TRUE))
  # AI Agent Type A Moderator
  data <- data %>%
    dplyr::mutate(aiType_a = recode_factor(as.character(agent),
      "human" = "human",
      "AI" = "robot"
    ))

  # AI Agent Type B Moderator
  data$decider <- as.factor(data$decider)
  data <- data %>%
    dplyr::mutate(decider = recode_factor(decider,
      `1` = "1",
      `2` = "2", .default = "3"
    ))
  data <- data %>%
    dplyr::mutate(aiType_b = recode_factor(as.character(decider),
      `1` = "human",
      `2` = "mechanical",
      `3` = "humanoid"
    ))

  data$ID <- 1:nrow(data)
  data <- data %>%
    mutate(gender = recode(gender,
                           "1" = "male",
                           "2" = "female",
    ))
  laakasuo2021_s2 <- data
  laakasuo2021_s2
}
# to avoid unnesting inaccurate combinations e.g. inaction & mean E, we subset the data here
# Subset by action/inaction (and meane/sidee)
make_action_subset_laakasuo2021_s2 <- function(clean_data) {
  data <- clean_data
  laak2021_s2.a <- data %>%
    filter(decisionquality == "2")
  laak2021_s2.a
}

make_inaction_subset_laakasuo2021_s2 <- function(clean_data) {
  data <- clean_data
  laak2021_s2.ia <- data %>%
    filter(decisionquality == "1")
  laak2021_s2.ia
}

# ====================================================================== laakasuo 2023a s1
# function for cleaning laakasuo2023a_s1
clean_laakasuo2023a_s1 <- function(raw_data) {
  data <- read_dta(raw_data)

  # rm NAs
  data <- data %>%
    filter(!is.na(agent))

  # save agent variable for moderation analyses
  data$agentMod <- data$agent
  # collapse AI agents into 1
  data$agent <- as.factor(data$agent)
  data <- data %>%
    dplyr::mutate(agent = recode(agent,
      "Man" = "1",
      .default = "2"
    ))

  data$agent <- factor(data$agent,
    levels = c(1, 2),
    labels = c("human", "AI")
  )

  # filter data based on researchers' manipulation/attention checks
  data <- filter(data, eng_prof == 4, attcheck == 2, GenderAnswered < 3)

  data$moral <- data$DV1_5items

  # ========== Data Cleaning for Moderator Analyses ========#
  # PMA Moderator
  data$PMA <- data$MC_HowMoral
  # PMC Moderator
  data$PMC <- data$MindPerception
  # AI Agent Type A Moderator
  data <- data %>%
    dplyr::mutate(aiType_a = recode_factor(as.character(agent),
      "human" = "human",
      "AI" = "robot"
    ))

  # AI Agent Type B Moderator
  # collapse AI agents into 1
  data <- data %>%
    dplyr::mutate(aiType_b = recode_factor(Agent2,
      "5" = "1",
      "4" = "3", .default = "2"
    ))

  data$aiType_b <- factor(data$aiType_b,
    levels = c(1, 2, 3),
    labels = c("human", "mechanical", "humanoid")
  )
  
  data <- data %>%
    mutate(gender = recode(gender,
                           "Man" = "male",
                           "Woman" = "female"
    ))

  data$ID <- 1:nrow(data)
  laakasuo2023a_s1 <- data
  laakasuo2023a_s1
}

# to avoid unnesting inaccurate combinations e.g. inaction & mean E, we subset the data here
# Subset by action/inaction (and meane/sidee)
make_action_subset_laakasuo2023a_s1 <- function(clean_data) {
  data <- clean_data
  data$Decision2 <- as.factor(data$Decision2)
  laak2023a_s1.a <- data %>%
    filter(Decision2 == "2")
  laak2023a_s1.a
}
make_inaction_subset_laakasuo2023a_s1 <- function(clean_data) {
  data <- clean_data
  data$Decision2 <- as.factor(data$Decision2)
  laak2023a_s1.ia <- data %>%
    filter(Decision2 == "1")
  laak2023a_s1.ia
}

# ====================================================================== laakasuo 2023b s2
# function for cleaning laakasuo2023b_s2
clean_laakasuo2023b_s2 <- function(raw_data) {
  data <- read_dta(raw_data)

  data$agent <- as.factor(data$Human)
  data$agent <- as.factor(data$agent)

  data$agent <- factor(data$agent,
    levels = c(1, 2),
    labels = c("human", "AI")
  )

  #---- DVs
  # appropriate
  data$appropriate <- as.numeric(data$dv_item_1)
  # right
  data$right <- as.numeric(data$dv_item_2)

  # ========== Data Cleaning for Moderator Analyses ========#
  # Harm Domain Moderator
  data$harm <- factor(data$Forced,
    levels = c(1, 2),
    labels = c("notharm", "harm")
  )

  # In.action Moderator
  data$in_action <- factor(data$Forced,
    levels = c(1, 2),
    labels = c("inaction", "action")
  )
  # AI Agent Type A Moderator
  data <- data %>%
    dplyr::mutate(aiType_a = recode_factor(as.character(agent),
      "human" = "human",
      "AI" = "robot"
    ))

  # AI Agent Type B Moderator
  data <- data %>%
    dplyr::mutate(aiType_b = recode_factor(as.character(agent),
      "human" = "human",
      "AI" = "mechanical"
    ))
  data$ID <- 1:nrow(data)

  laakasuo2023b_s2 <- data
  laakasuo2023b_s2
}

# ====================================================================== laakasuo 2023b s3
# function for cleaning laakasuo2023b_s3
clean_laakasuo2023b_s3 <- function(raw_data) {
  data <- read_dta(raw_data)
  # data exclusions (below native english fluency)
  data <- data %>%
    filter(english == "Very Good")

  data$agent <- as.factor(data$nurse)
  data$agent <- as.factor(data$agent)
  data <- data %>%
    dplyr::mutate(agent = recode(agent,
      "Human nurse" = "1",
      .default = "2"
    ))

  data$agent <- factor(data$agent,
    levels = c(1, 2),
    labels = c("human", "AI")
  )

  #---- DVs
  # appropriate
  data$appropriate <- data$dv_item_1
  # right
  data$right <- data$dv_item_2

  # ========== Data Cleaning for Moderator Analyses ========#
  # Harm Domain Moderator
  data$harm <- recode(data$fm,
    `Forced` = "harm",
    `Not forced` = "notharm"
  )

  # Inaction Domain Moderator
  data$in_action <- recode(data$fm,
    `Forced` = "action",
    `Not forced` = "inaction"
  )

  # Implied intel
  data$agentIntel <- recode(data$mc_rep,
    `Incompetent` = "notImplied",
    `Competent` = "implied"
  )
  # AI Agent Type A Moderator
  data <- data %>%
    dplyr::mutate(aiType_a = recode_factor(as.character(agent),
      "human" = "human",
      "AI" = "robot"
    ))

  # AI Agent Type B Moderator
  data <- data %>%
    dplyr::mutate(aiType_b = recode_factor(as.character(agent),
      "human" = "human",
      "AI" = "mechanical"
    ))
  
  data <- data %>%
    mutate(gender = recode(gender,
                           "Man" = "male",
                           "Woman" = "female",
                           "Non-binary" = "nonbinary",
                           "Not sure" = "other",
                           "Agender" = "agender",
                           "Rather not say" = "other"
    ))
  
  data$ID <- 1:nrow(data)
  laakasuo2023b_s3 <- data
  laakasuo2023b_s3
}

# ====================================================================== laakasuo 2023b s4
# function for cleaning laakasuo2023b_s4
clean_laakasuo2023b_s4 <- function(raw_data) {
  data <- read_dta(raw_data)
  data$agent <- as.factor(data$Human)
  data$agent <- as.factor(data$agent)

  data$agent <- factor(data$agent,
    levels = c(1, 2),
    labels = c("human", "AI")
  )

  #---- DVs
  # appropriate
  data$appropriate <- data$dv_item_1
  # right
  data$right <- data$dv_item_2

  # ========== Data Cleaning for Moderator Analyses ========#
  # Harm Domain Moderator
  data <- data %>%
    dplyr::mutate(
      Dies = as.factor(Dies),
      Forced = as.factor(Forced),
      harm = case_when(
        Dies == "2" ~ "harm", # Dies == 2 → harm
        Dies == "1" & Forced == "2" ~ "harm", # Dies == 1 & Forced == 2 → harm
        Dies == "1" & Forced == "1" ~ "notharm", # Dies == 1 & Forced == 1 → notharm
        TRUE ~ NA_character_
      )
    )


  # AI Agent Type A Moderator
  data <- data %>%
    dplyr::mutate(aiType_a = recode_factor(as.character(agent),
      "human" = "human",
      "AI" = "robot"
    ))

  # AI Agent Type B Moderator
  data <- data %>%
    dplyr::mutate(aiType_b = recode_factor(as.character(agent),
      "human" = "human",
      "AI" = "mechanical"
    ))
  data <- data %>%
    mutate(gender = recode(gender,
                           "Mies" = "male",
                           "Nainen" = "female"
    ))
  data$ID <- 1:nrow(data)
  laakasuo2023b_s4 <- data
  laakasuo2023b_s4
}

# Subset by dilemmas Moderator
## Dies = 2 (patient dies), Dies = 1 (patient doesn't die)
make_se_action_subset_laakasuo2023b_s4 <- function(clean_data) {
  laak23b_s4.se <- clean_data %>%
    filter(Dies == "2")
  laak23b_s4.se.a <- laak23b_s4.se %>%
    filter(Forced == "2")
  laak23b_s4.se.a
}

make_se_inaction_subset_laakasuo2023b_s4 <- function(clean_data) {
  laak23b_s4.se <- clean_data %>%
    filter(Dies == "2")
  laak23b_s4.se.ia <- laak23b_s4.se %>%
    filter(Forced == "1")
  laak23b_s4.se.ia
}

make_action_subset_laakasuo2023b_s4 <- function(clean_data) {
  laak23b_s4 <- clean_data %>%
    filter(Dies == "1")
  laak23b_s4.a <- laak23b_s4 %>%
    filter(Forced == "2")
  laak23b_s4.a
}

# ====================================================================== laakasuo 2023b s5
# function for cleaning laakasuo2023b_s5
clean_laakasuo2023b_s5 <- function(raw_data) {
  data <- read_dta(raw_data)
  # give agent consistent coding & labels
  data$agent <- recode(data$nurse,
    "Human" = 1,
    "Robot" = 2
  )
  data$agent <- factor(data$agent,
    levels = c(1, 2),
    labels = c("human", "AI")
  )

  #---- DVs
  # appropriate
  data$appropriate <- data$dv1_1
  # right
  data$right <- data$dv1_2

  # ========== Data Cleaning for Moderator Analyses ========#
  # Harm Domain Moderator
  data$harm <- factor(data$Forced,
    levels = c(0, 1),
    labels = c("notharm", "harm")
  )

  # In.action Moderator
  data$in_action <- factor(data$Forced,
    levels = c(0, 1),
    labels = c("inaction", "action")
  )

  # AI Agent Type A Moderator
  data <- data %>%
    dplyr::mutate(aiType_a = recode_factor(as.character(agent),
      "human" = "human",
      "AI" = "robot"
    ))

  # AI Agent Type B Moderator
  data <- data %>%
    dplyr::mutate(aiType_b = recode_factor(as.character(agent),
      "human" = "human",
      "AI" = "mechanical"
    ))
  data <- data %>%
    mutate(gender = recode(gender,
                           "Man" = "male",
                           "Woman" = "female"
    ))
  data$ID <- 1:nrow(data)
  laakasuo2023b_s5 <- data
  laakasuo2023b_s5
}


# ====================================================================== malle 2015 s1
# function for cleaning malle2015_s1
clean_malle2015_s1 <- function(raw_data) {
  data <- read.csv(raw_data)
  data$ID <- data$Subject
  data$permissible <- data$Permissible.
  data$agent <- as.factor(data$Agent)
  data$agent <- factor(data$agent,
    levels = c(1, 2),
    labels = c("human", "AI")
  )

  # data exclusions (no resp.)
  data <- data %>%
    filter((permissible == 1) | (permissible == 0))

  # ========== Data Cleaning for Moderator Analyses ========#
  # In.action Moderator
  data$in_action <- factor(data$Decision,
    levels = c(0, 1),
    labels = c("inaction", "action")
  )

  # Responsible Moderator
  data$blame <- as.numeric(data$Blame)
  # PMC Moderator
  data$PMC <- as.numeric(data$Capacity)
  # AI Agent Type A Moderator
  data <- data %>%
    dplyr::mutate(aiType_a = recode_factor(as.character(agent),
      "human" = "human",
      "AI" = "robot"
    ))

  # AI Agent Type B Moderator
  data <- data %>%
    dplyr::mutate(aiType_b = recode_factor(as.character(agent),
      "human" = "human",
      "AI" = "mechanical"
    ))
  data$age <- data$Age
  data <- data %>%
    mutate(gender = recode(Gender,
                           "1" = "male",
                           "0" = "female"
    ))
  malle2015_s1 <- data
  malle2015_s1
}

# ====================================================================== malle 2015 s2
# function for cleaning malle2015_s2
clean_malle2015_s2 <- function(raw_data) {
  data <- read.csv(raw_data)
  data$ID <- data$Subject

  data$wrong <- data$Wrong
  data$agent <- as.factor(data$Agent)
  data$agent <- factor(data$agent,
    levels = c(1, 2),
    labels = c("human", "AI")
  )

  # data exclusions (no resp.)
  data <- data %>%
    filter((wrong == 1) | (wrong == 0))

  # recode so that in correct direction i.e. 1 = Not Wrong
  data$wrong <- recode(data$wrong,
    `1` = 0,
    `0` = 1
  )
  data$wrong <- factor(data$wrong,
    levels = c(0, 1),
    labels = c("wrong", "not wrong")
  )

  # ========== Data Cleaning for Moderator Analyses ========#
  # In.action Moderator
  data$in_action <- factor(data$Decision,
    levels = c(0, 1),
    labels = c("inaction", "action")
  )

  # Responsible Moderator
  data$blame <- as.numeric(data$Blame)
  # PMC Moderator
  data$PMC <- as.numeric(data$Capacity)
  # AI Agent Type A Moderator
  data <- data %>%
    dplyr::mutate(aiType_a = recode_factor(as.character(agent),
      "human" = "human",
      "AI" = "robot"
    ))

  # AI Agent Type B Moderator
  data <- data %>%
    dplyr::mutate(aiType_b = recode_factor(as.character(agent),
      "human" = "human",
      "AI" = "mechanical"
    ))
  
  data$age <- data$Age
  data <- data %>%
    mutate(gender = recode(Gender,
                           "1" = "male",
                           "0" = "female"
    ))

  malle2015_s2 <- data
  malle2015_s2
}


# ====================================================================== malle 2019 s1
# function for cleaning malle2019_s1
clean_malle2019_s1 <- function(raw_data) {
  data <- read.csv(raw_data)
  data$ID <- data$Number

  data <- data %>%
    dplyr::mutate(agent = recode(Agent,
      "1" = "1",
      .default = "2"
    ))

  data$agent <- factor(data$agent,
    levels = c(1, 2),
    labels = c("human", "AI")
  )

  # recode so that in correct direction i.e. 1 = Not Wrong
  data$wrong <- recode(data$Wrong,
    `1` = 0,
    `0` = 1
  )
  data$wrong <- factor(data$wrong,
    levels = c(0, 1),
    labels = c("wrong", "not wrong")
  )

  # ========== Data Cleaning for Moderator Analyses ========#

  # AI Agent Type A& B Moderators
  data$aiType_a <- data$agent
  data$aiType_b <- data$agent

  data$age <- data$Age
  data <- data %>%
    mutate(gender = recode(Sex,
                           "1" = "male",
                           "0" = "female",
                           .default = "other"
    ))
  malle2019_s1 <- data
  malle2019_s1
}

# to avoid unnesting inaccurate combinations e.g. inaction & mean E, we subset the data here
# Subset by action/inaction (and meane/sidee)
make_action_subset_malle2019_s1 <- function(clean_data) {
  malle19_s1.a <- clean_data %>%
    filter(Decision == "1")
  malle19_s1.a
}

make_inaction_subset_malle2019_s1 <- function(clean_data) {
  malle19_s1.ia <- clean_data %>%
    filter(Decision == "0")
  malle19_s1.ia
}

# ====================================================================== malle 2014 s2.1
# function for cleaning malle2024_s2.1
clean_malle2024_s2.1 <- function(raw_data) {
  data <- read.csv(raw_data)
  data$ID <- data$Subject

  data$agent <- as.factor(data$Agent)
  data$agent <- factor(data$agent,
    levels = c(1, 2),
    labels = c("human", "AI")
  )

  # reverse morally wrong so that 1 = Not wrong
  data <- data %>%
    dplyr::mutate(wrong = recode(Wrong,
      "1" = "0",
      .default = "1"
    ))

  data$wrong <- as.factor(data$wrong)

  # ========== Data Cleaning for Moderator Analyses ========#
  # In.action Moderator
  data$in_action <- factor(data$Decision,
    levels = c(0, 1),
    labels = c("inaction", "action")
  )

  # Responsible Moderator
  data$blame <- as.numeric(data$Blame)
  # AI Agent Type A Moderator
  data <- data %>%
    dplyr::mutate(aiType_a = recode_factor(as.character(agent),
      "human" = "human",
      "AI" = "robot"
    ))

  # AI Agent Type B Moderator
  data <- data %>%
    dplyr::mutate(aiType_b = recode_factor(as.character(agent),
      "human" = "human",
      "AI" = "mechanical"
    ))
  data$age <- data$Age
  data <- data %>%
    mutate(gender = recode(Gender,
                           "1" = "male",
                           "0" = "female",
                           .default = "other"
    ))
  malle2024_s2.1 <- data
  malle2024_s2.1
}

# ====================================================================== malle 2014 s2.2
# function for cleaning malle2024_s2.2
clean_malle2024_s2.2 <- function(raw_data) {
  data <- read.csv(raw_data)
  data$ID <- data$SubjID

  data$agent <- as.factor(data$Agent)
  data$agent <- factor(data$agent,
    levels = c(1, 2),
    labels = c("human", "AI")
  )

  # reverse morally wrong so that 1 = Not wrong
  data <- data %>%
    dplyr::mutate(wrong = recode(Wrongness,
      "1" = "0",
      .default = "1"
    ))

  data$wrong <- as.factor(data$wrong)

  # ========== Data Cleaning for Moderator Analyses ========#

  # Responsible Moderator
  data$blame <- as.numeric(data$Blame)
  # AI Agent Type A Moderator
  data <- data %>%
    dplyr::mutate(aiType_a = recode_factor(as.character(agent),
      "human" = "human",
      "AI" = "robot"
    ))

  # AI Agent Type B Moderator
  data <- data %>%
    dplyr::mutate(aiType_b = recode_factor(as.character(agent),
      "human" = "human",
      "AI" = "mechanical"
    ))
  data$age <- data$Age
  data <- data %>%
    mutate(gender = recode(Gender,
                           "2" = "male",
                           "1" = "female"
    ))

  malle2024_s2.2 <- data
  malle2024_s2.2
}

# to avoid unnesting inaccurate combinations e.g. inaction & mean E, we subset the data here
# Subset by dilemmas Moderator - unknown level (side effect wasn't known)
make_inaction_subset_malle2024_s2.2 <- function(clean_data) {
  data <- clean_data
  data$Decision <- as.factor(data$Decision)
  malle24_s2.2.me <- data %>%
    filter(EventStructure == "MeansEnd")

  malle24_s2.2.se <- data %>%
    filter(EventStructure == "SideEffect")
  # subset by decision made
  malle24_s2.2.me.ia <- malle24_s2.2.me %>%
    filter(Decision == "0")

  malle24_s2.2.se.ia <- malle24_s2.2.se %>%
    filter(Decision == "0")

  malle24_s2.2.ia <- bind_rows(malle24_s2.2.me.ia, malle24_s2.2.se.ia)
  malle24_s2.2.ia
}

make_me_action_subset_malle2024_s2.2 <- function(clean_data) {
  data <- clean_data
  malle24_s2.2.me <- data %>%
    filter(EventStructure == "MeansEnd")

  malle24 <- data %>%
    filter(EventStructure == "Unknown")
  # subset by decision made
  malle24_s2.2.me.a <- malle24_s2.2.me %>%
    filter(Decision == "1")
  malle24_s2.2.me.a
}

make_se_action_subset_malle2024_s2.2 <- function(clean_data) {
  data <- clean_data

  malle24_s2.2.se <- data %>%
    filter(EventStructure == "SideEffect")
  # subset by decision made
  malle24_s2.2.se.a <- malle24_s2.2.se %>%
    filter(Decision == "1")
  malle24_s2.2.se.a
}

make_action_subset_malle2024_s2.2 <- function(clean_data) {
  data <- clean_data
  malle24 <- data %>%
    filter(EventStructure == "Unknown")
  malle24_s2.2.a <- malle24 %>%
    filter(Decision == "1")
  malle24_s2.2.a
}

# ====================================================================== malle 2014 s2.3
# function for cleaning malle2024_s2.3
clean_malle2024_s2.3 <- function(raw_data) {
  data <- read.csv(raw_data)
  data$ID <- data$SubjID
  data$agent <- as.factor(data$Agent)
  data$agent <- factor(data$agent,
    levels = c(1, 2),
    labels = c("human", "AI")
  )

  # reverse morally wrong so that 1 = Not wrong
  data <- data %>%
    dplyr::mutate(wrong = recode(Wrong,
      "1" = "0",
      .default = "1"
    ))

  data$wrong <- as.factor(data$wrong)

  # ========== Data Cleaning for Moderator Analyses ========#

  # Responsible Moderator
  data$blame <- as.numeric(data$Blame)
  # AI Agent Type A Moderator
  data <- data %>%
    dplyr::mutate(aiType_a = recode_factor(as.character(agent),
      "human" = "human",
      "AI" = "robot"
    ))

  # AI Agent Type B Moderator
  data <- data %>%
    dplyr::mutate(aiType_b = recode_factor(as.character(agent),
      "human" = "human",
      "AI" = "mechanical"
    ))

  data$age <- data$Age
  data <- data %>%
    mutate(gender = recode(Gender,
                           "1" = "male",
                           "0" = "female",
                           "2" = "nonbinary",
                           "3" = "other"
    ))
  malle2024_s2.3 <- data
  malle2024_s2.3
}

make_inaction_subset_malle2024_s2.3 <- function(clean_data) {
  data <- clean_data
  # Subset by dilemmas Moderator - unknown level (side effect wasn't known)
  malle24_s2.3.me <- data %>%
    filter(Event._structure == "2")

  malle24_s2.3.se <- data %>%
    filter(Event._structure == "1")

  # subset by decision made

  malle24_s2.3.me.ia <- malle24_s2.3.me %>%
    filter(Decision == "0")

  malle24_s2.3.se.ia <- malle24_s2.3.se %>%
    filter(Decision == "0")

  malle24_s2.3.ia <- bind_rows(malle24_s2.3.me.ia, malle24_s2.3.se.ia)
  malle24_s2.3.ia
}


make_me_action_subset_malle2024_s2.3 <- function(clean_data) {
  data <- clean_data
  # Subset by dilemmas Moderator - unknown level (side effect wasn't known)
  malle24_s2.3.me <- data %>%
    filter(Event._structure == "2")

  # subset by decision made
  malle24_s2.3.me.a <- malle24_s2.3.me %>%
    filter(Decision == "1")
  malle24_s2.3.me.a
}

make_se_action_subset_malle2024_s2.3 <- function(clean_data) {
  data <- clean_data
  # Subset by dilemmas Moderator - unknown level (side effect wasn't known)

  malle24_s2.3.se <- data %>%
    filter(Event._structure == "1")


  # subset by decision made
  malle24_s2.3.se.a <- malle24_s2.3.se %>%
    filter(Decision == "1")
  malle24_s2.3.se.a
}

# ====================================================================== malle 2014 s2.4
# function for cleaning malle2024_s2.4
clean_malle2024_s2.4 <- function(raw_data) {
  data <- read.csv(raw_data)
  data$ID <- data$SubjID

  data$agent <- as.factor(data$Agent)
  data$agent <- factor(data$agent,
    levels = c(1, 2),
    labels = c("human", "AI")
  )

  # reverse morally wrong so that 1 = Not wrong
  data <- data %>%
    dplyr::mutate(wrong = recode(Wrong,
      "1" = "0",
      .default = "1"
    ))

  data$wrong <- as.factor(data$wrong)

  # ========== Data Cleaning for Moderator Analyses ========#

  # Responsible Moderator
  data$blame <- as.numeric(data$Blame)
  # AI Agent Type A Moderator
  data <- data %>%
    dplyr::mutate(aiType_a = recode_factor(as.character(agent),
      "human" = "human",
      "AI" = "robot"
    ))

  # AI Agent Type B Moderator
  data <- data %>%
    dplyr::mutate(aiType_b = recode_factor(as.character(agent),
      "human" = "human",
      "AI" = "mechanical"
    ))

  data$age <- data$Age
  data <- data %>%
    mutate(gender = recode(Gender,
                           "1" = "male",
                           "0" = "female",
                           "2" = "nonbinary",
                           "3" = "other"
    ))
  malle2024_s2.4 <- data
  malle2024_s2.4
}

# Subset by dilemmas Moderator - unknown level (side effect wasn't known)
# subset by decision made
make_inaction_subset_malle2024_s2.4 <- function(clean_data) {
  data <- clean_data
  malle24_s2.4.me <- data %>%
    filter(Event._structure == "2")

  malle24_s2.4.se <- data %>%
    filter(Event._structure == "1")

  malle24_s2.4.me.ia <- malle24_s2.4.me %>%
    filter(Decision == "0")

  malle24_s2.4.se.ia <- malle24_s2.4.se %>%
    filter(Decision == "0")

  malle24_s2.4.ia <- bind_rows(malle24_s2.4.me.ia, malle24_s2.4.se.ia)
  malle24_s2.4.ia
}

make_inaction_unknown_subset_malle2024_s2.4 <- function(clean_data) {
  data <- clean_data
  malle24_s2.4 <- data %>%
    filter(Event._structure == "Unknown")
  malle24_s2.4.ia_unknown <- malle24_s2.4 %>%
    filter(Decision == "0")
  malle24_s2.4.ia_unknown
}

make_me_action_subset_malle2024_s2.4 <- function(clean_data) {
  data <- clean_data
  malle24_s2.4.me <- data %>%
    filter(Event._structure == "2")

  # subset by decision made
  malle24_s2.4.me.a <- malle24_s2.4.me %>%
    filter(Decision == "1")
  malle24_s2.4.me.a
}

make_se_action_subset_malle2024_s2.4 <- function(clean_data) {
  data <- clean_data
  malle24_s2.4.se <- data %>%
    filter(Event._structure == "1")
  malle24_s2.4.se.a <- malle24_s2.4.se %>%
    filter(Decision == "1")
  malle24_s2.4.se.a
}


# ====================================================================== malle 2014 s4.1
# function for cleaning malle2024_s4.1
clean_malle2024_s4.1 <- function(raw_data) {
  data <- read.csv(raw_data)

  data$ID <- data$Subject

  data$agent <- as.factor(data$Agent)
  data$agent <- factor(data$agent,
    levels = c(1, 2),
    labels = c("human", "AI")
  )

  # reverse morally wrong so that 1 = Not wrong
  data <- data %>%
    dplyr::mutate(wrong = recode(Wrong,
      "1" = "0",
      .default = "1"
    ))

  data$wrong <- as.factor(data$wrong)

  # ========== Data Cleaning for Moderator Analyses ========#
  # In.action Moderator
  data$in_action <- factor(data$Decision,
    levels = c(0, 1),
    labels = c("inaction", "action")
  )

  # Responsible Moderator
  data$blame <- as.numeric(data$Blame)
  # AI Agent Type A Moderator
  data <- data %>%
    dplyr::mutate(aiType_a = recode_factor(as.character(agent),
      "human" = "human",
      "AI" = "robot"
    ))

  # AI Agent Type B Moderator
  data <- data %>%
    dplyr::mutate(aiType_b = recode_factor(as.character(agent),
      "human" = "human",
      "AI" = "mechanical"
    ))
  data$age <- data$Age
  data <- data %>%
    mutate(gender = recode(Gender,
                           "1" = "male",
                           "0" = "female",
                           "2" = "nonbinary",
                           "3" = "other"
    ))
  malle2024_s4.1 <- data
  malle2024_s4.1
}

# ====================================================================== malle 2014 s4_3
# function for cleaning malle2024_s4_3
clean_malle2024_s4_3 <- function(raw_data) {
  data <- read.csv(raw_data)
  data$ID <- data$SubjID

  data$agent <- as.factor(data$Agent)
  data$agent <- factor(data$agent,
    levels = c(1, 2),
    labels = c("human", "AI")
  )
  # reverse morally wrong so that 1 = Not wrong
  data <- data %>%
    dplyr::mutate(wrong = recode(Wrong,
      "1" = "0",
      .default = "1"
    ))

  data$wrong <- as.factor(data$wrong)

  # Responsible Moderator
  data$blame <- as.numeric(data$Blame)
  # AI Agent Type A Moderator
  data <- data %>%
    dplyr::mutate(aiType_a = recode_factor(as.character(agent),
      "human" = "human",
      "AI" = "robot"
    ))

  # AI Agent Type B Moderator
  data <- data %>%
    dplyr::mutate(aiType_b = recode_factor(as.character(agent),
      "human" = "human",
      "AI" = "mechanical"
    ))
  data$age <- data$Age
  data <- data %>%
    mutate(gender = recode(Gender,
                           "1" = "male",
                           "0" = "female",
                           "2" = "nonbinary",
                           "3" = "other"
    ))
  malle2024_s4_3 <- data
  malle2024_s4_3
}

# ====================================================================== maninger 2022 s2
# function for cleaning maninger2022_s2
clean_maninger2022_s2 <- function(raw_data) {
  data <- read.csv(raw_data)
  data <- data %>%
    # 1) normalize inputs / types
    dplyr::mutate(
      # make isHuman -> agent robustly
      agent = case_when(
        is.logical(isHuman) ~ if_else(isHuman, "human", "AI"),
        is.character(isHuman) ~ if_else(tolower(trimws(isHuman)) %in% c("true", "t", "yes", "y", "1", "human", "participant", "person"),
          "human", "AI"
        ),
        TRUE ~ as.character(isHuman) # fallback (shouldn't usually happen)
      ),
      agent = factor(agent, levels = c("human", "AI")), # canonical factor order

      # force Foundation/Scenario to character so "1" matches "1"
      Foundation = as.character(Foundation),
      Scenario = as.character(Scenario)
    ) %>%
    # 2) create derived vars
    dplyr::mutate(
      harm = case_when(
        Foundation == "1" ~ "harm",
        Foundation %in% c("2", "3", "4", "5", "6") ~ "notharm",
        TRUE ~ NA_character_
      ),
      intent = case_when(
        Foundation == "1" ~ "meane",
        TRUE ~ NA_character_
      ),
      blame = rowMeans(across(starts_with("Actor_Blame")), na.rm = TRUE)
    ) %>%
    # 3) explicit AI/robot mapping (no | precedence ambiguity)
    dplyr::mutate(
      aiType_a = case_when(
        # AI -> "AI" cases
        agent == "AI" & Foundation == "1" & Scenario %in% c("1", "2") ~ "AI",
        agent == "AI" & Foundation == "2" & Scenario %in% c("1", "2") ~ "AI",
        agent == "AI" & Foundation == "4" & Scenario == "2" ~ "AI",
        agent == "AI" & Foundation == "5" & Scenario %in% c("1", "2", "3") ~ "AI",
        agent == "AI" & Foundation == "6" & Scenario %in% c("1", "2", "3") ~ "AI",

        # AI -> "robot" cases
        agent == "AI" & Foundation == "1" & Scenario == "3" ~ "robot",
        agent == "AI" & Foundation == "2" & Scenario == "3" ~ "robot",
        agent == "AI" & Foundation == "3" ~ "robot",
        agent == "AI" & Foundation == "4" & Scenario == "1" ~ "robot",
        agent == "AI" & Foundation == "5" & Scenario %in% c("2", "3") ~ "robot",

        # humans
        agent == "human" ~ "human",

        # fallback
        TRUE ~ NA_character_
      )
    )


  # rename moral wrongness variable
  data$wrong <- as.numeric(data$Moral_Wrongness)

  data <- data %>%
    filter(!is.na(aiType_a))

  # Agent Type B Moderator
  data <- data %>%
    dplyr::mutate(aiType_b = recode(aiType_a,
      "AI" = "AI",
      "robot" = "mechanical",
      "human" = "human"
    ))
  #### Pivot the DV into wide format with columns like mf1, mf2, ..., mf6

  # first try returned only 280 Ps. This check looks for duplicate data
  check <- data %>%
    count(ID) %>%
    filter(n != 6)
  # ID 9109 has two sets. Data is different so renaming on of the IDs. This code changes second group.
  data <- data %>%
    group_by(ID) %>%
    dplyr::mutate(block = (row_number() - 1) %/% 6 + 1) %>%
    ungroup() %>%
    dplyr::mutate(ID = ifelse(ID == 9109 & block == 2, 99999, ID)) %>%
    dplyr::select(-block)


  data$wrong <- 6 - data$wrong


  maninger2022_s2 <- data
  maninger2022_s2
}

## subset by harm (and in-turn intent)
## subset by AI type
make_harm_ai_subset_maninger2022_s2 <- function(clean_data) {
  data <- clean_data
  ## subset by harm (and in-turn intent)
  data.harm <- subset(data, data$harm %in% c("harm"))

  ## subset by AI type
  m22.harm.ai <- subset(data.harm, data.harm$aiType_b %in% c("AI", "human"))
  m22.harm.ai
}

make_harm_mechanical_subset_maninger2022_s2 <- function(clean_data) {
  data <- clean_data
  ## subset by harm (and in-turn intent)
  data.harm <- subset(data, data$harm %in% c("harm"))
  # subset by AI type
  m22.harm.mechanical <- subset(data.harm, data.harm$aiType_b %in% c("mechanical", "human"))
  m22.harm.mechanical
}

make_notharm_mechanical_subset_maninger2022_s2 <- function(clean_data) {
  data <- clean_data
  ## subset by harm (and in-turn intent)
  data.notharm <- subset(data, data$harm %in% c("notharm"))
  # subset by AI type
  m22.notharm.mechanical <- subset(data.notharm, data.notharm$aiType_b %in% c("mechanical", "human"))
  m22.notharm.mechanical
}


make_notharm_ai_subset_maninger2022_s2 <- function(clean_data) {
  data <- clean_data
  ## subset by harm (and in-turn intent)
  data.notharm <- subset(data, data$harm %in% c("notharm"))
  # subset by AI type
  m22.notharm.ai <- subset(data.notharm, data.notharm$aiType_b %in% c("AI", "human"))
  m22.notharm.ai
}


# ====================================================================== mayer 2023 s1
# function for cleaning mayer2023_s1
clean_mayer2023_s1 <- function(raw_data) {
  data <- read.csv(raw_data)
  data$ID <- 1:nrow(data)

  data$agent <- as.factor(data$Group)
  data$agent <- factor(data$agent,
    levels = c(1, 2),
    labels = c("human", "AI")
  )

  # avg the DV across all scenarios

  ## print(names(data))
  data <- data %>%
    dplyr::mutate(justifiable = rowMeans(across(
      c("EV01":"EV24"),
      ~ as.numeric(.)
    ), na.rm = TRUE))
  # AI Agent Type A & B Moderator
  data$aiType_a <- data$aiType_b <- data$agent

  mayer2023_s1 <- data
  mayer2023_s1
}

# ====================================================================== mayer 2023 s2
# function for cleaning mayer2023_s2
clean_mayer2023_s2 <- function(raw_data) {
  data <- read.csv(raw_data)

  data$agent <- as.factor(data$Group)

  # collapse AI agents into 1
  data <- data %>%
    dplyr::mutate(agent = recode(agent,
      `1` = "1",
      .default = "2"
    ))

  data$agent <- factor(data$agent,
    levels = c(1, 2),
    labels = c("human", "AI")
  )

  # avg the DV across all scenarios

  ## print(names(data))
  data <- data %>%
    dplyr::mutate(justifiable = rowMeans(across(
      c("EV01":"EV24"),
      ~ as.numeric(.)
    ), na.rm = TRUE))
  # AI Agent Type A & B Moderator
  data$aiType_a <- data$aiType_b <- data$agent

  mayer2023_s2 <- data
  mayer2023_s2
}

# ====================================================================== soares 2023 s1
# function for cleaning soares2023_s1
clean_soares2023_s1 <- function(raw_data) {
  data <- read.csv(raw_data)
  data$ID <- 1:nrow(data)

  data$agent <- as.factor(data$Agent)

  # give agent consistent coding & labels
  data$agent <- recode(data$agent,
    `-1` = 1,
    `1` = 2
  )
  data$agent <- factor(data$agent,
    levels = c(1, 2),
    labels = c("human", "AI")
  )

  #---- DVs
  # acceptable
  data$acceptable <- data$AM_1
  # right
  data$right <- data$AM_2

  # ========== Data Cleaning for Moderator Analyses ========#
  # In.action Moderator
  data$in_action <- factor(data$Ethic,
    levels = c(-1, 1),
    labels = c("inaction", "action")
  )

  # Responsibility Moderator
  data$blame <- as.numeric(data$RM_3)
  data <- data %>%
    dplyr::mutate(responsible = rowMeans(across(
      c("AM_1", "AM_2"),
      ~ as.numeric(.)
    ), na.rm = TRUE))
  # PMC Moderator
  data <- data %>%
    dplyr::mutate(PMC = rowMeans(across(
      c("Warmth", "Competence"),
      ~ as.numeric(.)
    ), na.rm = TRUE))

  # AI Agent Type A Moderator
  data <- data %>%
    dplyr::mutate(aiType_a = recode_factor(as.character(agent),
      "human" = "human",
      "AI" = "robot"
    ))

  # AI Agent Type B Moderator
  data <- data %>%
    dplyr::mutate(aiType_b = recode_factor(as.character(agent),
      "human" = "human",
      "AI" = "mechanical"
    ))
  
  data$age <- data$Age
  data <- data %>%
    mutate(gender = recode(Sx,
                           "2" = "male",
                           "1" = "female",
                           "3" = "other"
    ))
  
  soares2023_s1 <- data
  soares2023_s1
}

# ====================================================================== sundvall 2023 s1
# function for cleaning sundvall2023_s1
clean_sundvall2023_s1 <- function(raw_data) {
  data <- read.csv(raw_data)

  # give agent consisdata#give agent consistent coding & labels
  data$agent <- recode(data$agent,
    `Human` = 1,
    `Robot` = 2
  )
  data$agent <- factor(data$agent,
    levels = c(1, 2),
    labels = c("human", "AI")
  )


  # Remove rows where one of the DVs or 'agent' is NA
  data <- data %>% drop_na(DVacceptable, agent)

  #---- DVs
  # acceptable
  data$acceptable <- data$DVacceptable

  # wrong (reversed)
  data$wrong <- 8 - data$DVwrong

  # not morally right (reversed)
  data$notOk <- 8 - data$DVnot_OK

  # unethical (reversed)
  data$unethical <- 8 - data$DVunethical


  # AI Agent Type A Moderator
  data <- data %>%
    dplyr::mutate(aiType_a = recode_factor(as.character(agent),
      "human" = "human",
      "AI" = "robot"
    ))

  # AI Agent Type B Moderator
  data <- data %>%
    dplyr::mutate(aiType_b = recode_factor(as.character(agent),
      "human" = "human",
      "AI" = "mechanical"
    ))

  # Responsible Moderator
  # irresponsible reversed
  data$responsible <- 8 - data$DVirresponsible
  
  data <- data %>%
    mutate(gender = recode(gender,
                           "Male" = "male",
                           "Female" = "female",
                           .default = "other"
    ))
  
  data$ID <- 1:nrow(data)

  sundvall2023_s1 <- data
  sundvall2023_s1
}

# ====================================================================== sundvall 2023 s2.1
# function for cleaning sundvall2023_s2.1
clean_sundvall2023_s2.1 <- function(raw_data) {
  lb2a_raw <- read.csv(raw_data)

  #-------------------------script from sundvall 2023 ---------------------------#

  ### Data Exclusions ---------------------------------------------------------


  ## Manipulation checks

  # Check who answered comprehension checks correctly
  lb2a_raw$includeMC1 <- ifelse(lb2a_raw$LsavingDichMC11 == "Robot" & lb2a_raw$agent != "Human" |
    lb2a_raw$LsavingDichMC11 == "Human" & lb2a_raw$agent == "Human", 1, 0)
  lb2a_raw$includeMC2 <- ifelse(lb2a_raw$LsavingDichMC21 == "Motorboaters" & lb2a_raw$rescued == "Motorboaters" |
    lb2a_raw$LsavingDichMC21 == "Fisherman" & lb2a_raw$rescued == "Fisherman", 1, 0)

  # Check which participants responded both comprehension checks correctly
  includeMCtotal <- ifelse(lb2a_raw$includeMC1 == 1 & lb2a_raw$includeMC2 == 1, 1, 0)

  # Filter data by comprehension checks and an attention check
  lb2a <- lb2a_raw %>%
    dplyr::filter(
      includeMCtotal == 1,
      AC == 4
    )


  # dim(lb2a_raw)  # 213 participants recruited
  # dim(lb2a)      # 186 left

  #------------------end of script from sundvall 2023 ---------------------------#

  data <- lb2a

  # save agent variable for moderator analyses
  data$agentMod <- data$agent

  # recode agent variable for main analyses
  data <- data %>%
    dplyr::mutate(agent = recode(agent,
      `Human` = "1",
      .default = "2"
    ))
  data$agent <- factor(data$agent,
    levels = c(1, 2),
    labels = c("human", "AI")
  )

  #---- DVs
  # acceptable
  data$acceptable <- data$DV1_lsMoralAcc1

  # wrong (reversed)
  data$wrong <- 8 - data$DVorig1

  # not morally right (reversed)
  data$notOk <- 8 - data$DVorig5

  # unethical (reversed)
  data$unethical <- 8 - data$DVorig8

  # ========== Data Cleaning for Moderator Analyses ========#
  # AI Agent Type A Moderator
  data <- data %>%
    dplyr::mutate(aiType_a = recode_factor(as.character(agent),
      "human" = "human",
      "AI" = "robot"
    ))


  # Agent Type B Moderator
  data <- data %>%
    dplyr::mutate(
      agentMod = as.factor(agentMod),
      aiType_b = case_when(
        agentMod == "Human" ~ "human",
        agentMod == "Floater robot" ~ "mechanical",
        agentMod == "Android robot" ~ "humanoid",
        TRUE ~ NA_character_
      )
    )
  # Responsible Moderator
  data$responsible <- as.numeric(data$DVorig3)
  data$blame <- as.numeric(data$DVorig2)
  # PMA Moderator
  data$PMA <- data$DVorig13

  # Responsible Moderator
  data$responsible <- data$DVorig3
  data$blame <- data$DVorig2
  # "should be convicted in court"
  data$punishment <- data$DVorig6
  # irresponsible reversed
  data$responsible1 <- 8 - data$DVorig4
  
  data <- data %>%
    mutate(gender = recode(gender,
                           "Male" = "male",
                           "Female" = "female"
    ))

  data$ID <- 1:nrow(data)

  sundvall2023_s2.1 <- data
  sundvall2023_s2.1
}

# ====================================================================== sundvall 2023 s2.2
# function for cleaning sundvall2023_s2.2
clean_sundvall2023_s2.2 <- function(raw_data) {
  data <- read.csv(raw_data)
  # save agent variable for moderator analyses
  data$agentMod <- data$agent

  # recode agent variable for main analyses
  data <- data %>%
    dplyr::mutate(agent = recode(agent,
      `Human` = "1",
      .default = "2"
    ))
  data$agent <- factor(data$agent,
    levels = c(1, 2),
    labels = c("human", "AI")
  )


  #---- DVs
  # acceptable
  data$acceptable <- data$DV1_morality

  # wrong (reversed)
  data$wrong <- 8 - data$DVorig1

  # not morally right (reversed)
  data$notOk <- 8 - data$DVorig5

  # unethical (reversed)
  data$unethical <- 8 - data$DVorig8

  # ========== Data Cleaning for Moderator Analyses ========#
  # AI Agent Type A Moderator
  data <- data %>%
    dplyr::mutate(aiType_a = recode_factor(as.character(agent),
      "human" = "human",
      "AI" = "robot"
    ))


  # Agent Type B Moderator
  data <- data %>%
    dplyr::mutate(
      agentMod = as.factor(agentMod),
      aiType_b = case_when(
        agentMod == "Human" ~ "human",
        agentMod == "Floater" ~ "mechanical",
        agentMod == "Android" ~ "humanoid",
        TRUE ~ NA_character_
      )
    )


  # PMA Moderator
  data$PMA <- data$DVorig13

  # Responsible Moderator
  data$responsible <- data$DVorig3
  data$blame <- data$DVorig2
  # "should be convicted in court"
  data$punishment <- data$DVorig6
  # irresponsible reversed
  data$responsible1 <- 8 - data$DVorig4
  
  data <- data %>%
    mutate(gender = recode(gender,
                           "Male" = "male",
                           "Female" = "female",
                           .default = "other"
    ))

  data$ID <- 1:nrow(data)

  sundvall2023_s2.2 <- data
  sundvall2023_s2.2
}

# ====================================================================== sundvall 2023 s2.3
# function for cleaning sundvall2023_s2.3
clean_sundvall2023_s2.3 <- function(raw_data) {
  lb2c_raw <- read.csv(raw_data)

  #------------------------ script from sundvall 2023 ---------------------------#

  ### DATA EXCLUSIONS ---------------------------------------------------------

  # Check who answered comprehension checks correctly
  CCagent_correct <- lb2c_raw$CC2 == "A robot" & lb2c_raw$agent != "Human" |
    lb2c_raw$CC2 == "A human" & lb2c_raw$agent == "Human"

  CCsaved_correct <- lb2c_raw$CC3 == "The fisherman" & lb2c_raw$rescued == "Fisherman" |
    lb2c_raw$CC3 == "The motorboater" & lb2c_raw$rescued == "Motorboater"

  # Excluding participants who fail two or more attention checks
  lb2c_raw$AC_check <- (lb2c_raw$AC1 != 1) + (lb2c_raw$AC5 != 5) + (lb2c_raw$AC6 != 6)


  # Exclusion criteria: two or more failed attention checks out of three, indicating being both a tailor and a shoemaker, and failing
  # either one or two of the comprehension check questions

  lb2c <- lb2c_raw %>%
    dplyr::filter(
      AC_check < 2,
      !(shoes == "Yes" & clothes == "Yes"),
      CCagent_correct,
      CCsaved_correct,
      (english_skills == "Very Good" | english_skills == "Good"),
      health1 == "No"
    )

  # dim(lb2c_raw)  # 503 participants collected
  # dim(lb2c)      # 33 participants excluced, 470 remain

  #------------------end of script from sundvall 2023 ---------------------------#
  data <- lb2c

  # save agent variable for moderator analyses
  data$agentMod <- data$agent

  # recode agent variable for main analyses
  data <- data %>%
    dplyr::mutate(agent = recode(agent,
      `Human` = "1",
      .default = "2"
    ))
  data$agent <- as.factor(data$agent)
  data$agent <- factor(data$agent,
    levels = c(1, 2),
    labels = c("human", "AI")
  )


  #---- DVs
  # acceptable
  data$acceptable <- data$dv1

  # wrong (reversed)
  data$wrong <- 8 - data$DVorig_1

  # not morally right (reversed)
  data$notOk <- 8 - data$DVorig_5

  # unethical (reversed)
  data$unethical <- 8 - data$DVorig_8

  # ========== Data Cleaning for Moderator Analyses ========#
  # AI Agent Type A Moderator
  data <- data %>%
    dplyr::mutate(aiType_a = recode_factor(as.character(agent),
      "human" = "human",
      "AI" = "robot"
    ))

  # Agent Type B Moderator
  data <- data %>%
    dplyr::mutate(
      agentMod = as.factor(agentMod),
      aiType_b = case_when(
        agentMod == "Human" ~ "human",
        agentMod == "Drone" ~ "mechanical",
        agentMod == "Android" ~ "humanoid",
        TRUE ~ NA_character_
      )
    )

  # PMA Moderator
  data$PMA <- data$DVorig_13

  # Responsible Moderator
  data$responsible <- data$DVorig_3
  data$blame <- data$DVorig_2
  # "should be convicted in court"
  data$punishment <- data$DVorig_6
  # irresponsible reversed
  data$responsible1 <- 8 - data$DVorig_4
  
  data <- data %>%
    mutate(gender = recode(gender,
                           "Male" = "male",
                           "Female" = "female",
                           .default = "other"
    ))
  

  data$ID <- 1:nrow(data)

  sundvall2023_s2.3 <- data
  sundvall2023_s2.3
}

# ====================================================================== sundvall 2023 s4.1
# function for cleaning sundvall2023_s4.1
clean_sundvall2023_s4.1 <- function(raw_data) {
  lb4a_raw <- read.csv(raw_data)
  #-------------------------script from sundvall 2023 ---------------------------#

  ### DATA EXCLUSIONS ---------------------------------------------------------


  # ATTENTION CHECKS
  lb4a_raw$AC1_fail <- !lb4a_raw$AC1_likert == 2
  lb4a_raw$AC1_fail2 <- !(is.na(lb4a_raw$AC1_slider_1) | (!is.na(lb4a_raw$AC1_slider_1) & lb4a_raw$AC1_slider_1 == 50))

  lb4a_raw$AC2_fail <- !lb4a_raw$AC2_likert == 6
  lb4a_raw$AC2_fail2 <- !(is.na(lb4a_raw$AC2_slider) | (!is.na(lb4a_raw$AC2_slider) & lb4a_raw$AC2_slider == 50))

  lb4a_raw$AC3_fail <- !lb4a_raw$AC3 == "No"
  lb4a_raw$AC3_fail[is.na(lb4a_raw$AC3_fail)] <- NA

  # Total AC failed
  lb4a_raw$AC_num <- lb4a_raw$AC1_fail + lb4a_raw$AC1_fail2 + lb4a_raw$AC2_fail + lb4a_raw$AC2_fail2 + lb4a_raw$AC3_fail

  # COMPREHENSION CHECKS:
  lb4a_raw$CC1_late_fail <- !lb4a_raw$CC1_later == lb4a_raw$CC1
  lb4a_raw$CC2_late_fail <- !lb4a_raw$CC2_later == lb4a_raw$CC2

  # Total later CC failed
  lb4a_raw$MC_total <- lb4a_raw$CC1_late_fail + lb4a_raw$CC2_late_fail

  # Filter data by attention checks, comprehension checks, "troll" questions,
  # and full responses
  lb4a <- lb4a_raw %>%
    dplyr::filter(
      AC_num < 2,
      MC_total == 0,
      shoes == "No" | clothes == "No",
      english_skills == "Good" | english_skills == "Very Good",
      Finished == TRUE,
      country_7_TEXT != "Netherlands",
      health1 == "No"
    )
  #------------------end of script from sundvall 2023 ---------------------------#

  data <- lb4a


  # collapse AI agents into 1 & relabel
  data$agent <- as.factor(data$CC1)
  data <- data %>%
    dplyr::mutate(agent = recode(agent,
      `Human` = "1",
      .default = "2"
    ))
  data$agent <- as.factor(data$agent)
  data$agent <- factor(data$agent,
    levels = c(1, 2),
    labels = c("human", "AI")
  )

  # Reverse items so highest val. is acceptable/permissible synontm
  colsToReverse <- c("DV1_wrong_motor", "DV1_wrong_fisher")
  data[colsToReverse] <- 8 - data[colsToReverse]

  colsToReverse.slider <- c("DV1_slider_1", "DV1_slider_1.1")
  data[colsToReverse.slider] <- 100 - data[colsToReverse.slider]

  #---- DVs
  # acceptable
  data <- data %>%
    dplyr::mutate(acceptable = rowMeans(across(
      c("DV2_acceptable_motor", "DV2_acceptable_fisher"),
      ~ as.numeric(.)
    ), na.rm = TRUE))

  data <- data %>%
    dplyr::mutate(acceptable.slider = rowMeans(across(
      c("DV2_slider_1", "DV2_slider_1.1"),
      ~ as.numeric(.)
    ), na.rm = TRUE))

  # wrong (reversed)
  data <- data %>%
    dplyr::mutate(wrong = rowMeans(across(
      c("DV1_wrong_motor", "DV1_wrong_fisher"),
      ~ as.numeric(.)
    ), na.rm = TRUE))

  data <- data %>%
    dplyr::mutate(wrong.slider = rowMeans(across(
      c("DV1_slider_1", "DV1_slider_1.1"),
      ~ as.numeric(.)
    ), na.rm = TRUE))

  # permissible
  data <- data %>%
    dplyr::mutate(permissible = rowMeans(across(
      c("DV3_permissible_motor", "DV3_permissible_fisher"),
      ~ as.numeric(.)
    ), na.rm = TRUE))

  data <- data %>%
    dplyr::mutate(permissible.slider = rowMeans(across(
      c("DV3_slider_1", "DV3_slider_1.1"),
      ~ as.numeric(.)
    ), na.rm = TRUE))

  # Responsible Moderator
  data <- data %>%
    dplyr::mutate(blame = rowMeans(across(
      c("DV4_blame_motor", "DV4_blame_fisher"),
      ~ as.numeric(.)
    ), na.rm = TRUE))
  # slider
  data <- data %>%
    dplyr::mutate(blame.slider = rowMeans(across(
      c("DV4_slider_1", "DV4_slider_1.1"),
      ~ as.numeric(.)
    ), na.rm = TRUE))
  data <- data %>%
    dplyr::mutate(punishment = rowMeans(across(
      c("DV5_punishment_motor", "DV5_punishment_fisher"),
      ~ as.numeric(.)
    ), na.rm = TRUE))
  # slider
  data <- data %>%
    dplyr::mutate(punishment.slider = rowMeans(across(
      c("DV5_slider_1", "DV5_slider_1.1"),
      ~ as.numeric(.)
    ), na.rm = TRUE))

  data <- data %>%
    dplyr::mutate(praise = rowMeans(across(
      c("DV6_praise_motor", "DV6_praise_fisher"),
      ~ as.numeric(.)
    ), na.rm = TRUE))

  # slider
  data <- data %>%
    dplyr::mutate(praise.slider = rowMeans(across(
      c("DV6_slider_1", "DV6_slider_1.1"),
      ~ as.numeric(.)
    ), na.rm = TRUE))

  # PMA Moderator
  data$PMA1 <- data$DV13_human_likert_1
  data$PMA2 <- data$DV13_human_slider_1
  # AI Agent Type A Moderator
  data <- data %>%
    dplyr::mutate(aiType_a = recode_factor(as.character(agent),
      "human" = "human",
      "AI" = "robot"
    ))

  # AI Agent Type B Moderator
  data <- data %>%
    dplyr::mutate(aiType_b = recode_factor(as.character(agent),
      "human" = "human",
      "AI" = "humanoid"
    ))
  
  data <- data %>%
    mutate(gender = recode(gender,
                           "Male" = "male",
                           "Female" = "female",
                           .default = "other"
    ))
  

  data$ID <- 1:nrow(data)

  sundvall2023_s4.1 <- data
  sundvall2023_s4.1
}

# ====================================================================== sundvall 2023 s4.2
# function for cleaning sundvall2023_s4.2
clean_sundvall2023_s4.2 <- function(raw_data) {
  lb4b_raw <- read.csv(raw_data)
  #-------------------------script from sundvall 2023 ---------------------------#

  ## ATTENTION CHECKS

  # Attention check 1: Count a failure in either the likert, the slider, or both as ONE failed attention check

  lb4b_raw$AC1_fail <- !lb4b_raw$AC1_likert == 2 | (is.na(lb4b_raw$AC1_likert))
  lb4b_raw$AC1_fail2 <- !(is.na(lb4b_raw$AC1_slider) | (!is.na(lb4b_raw$AC1_slider) & lb4b_raw$AC1_slider == 50))

  lb4b_raw <- lb4b_raw %>% dplyr::mutate(AC1_failsum = ifelse(AC1_fail | AC1_fail2, 1, 0))

  # Attention check 2: Count a failure in either the likert, the slider, or both as ONE failed attention check

  lb4b_raw$AC2_fail <- !lb4b_raw$AC2_likert == 6 | (is.na(lb4b_raw$AC2_likert))
  lb4b_raw$AC2_fail2 <- !(is.na(lb4b_raw$AC2_slider) | (!is.na(lb4b_raw$AC2_slider) & lb4b_raw$AC2_slider == 50))

  lb4b_raw <- lb4b_raw %>% dplyr::mutate(AC2_failsum = ifelse(AC2_fail | AC2_fail2, 1, 0))

  # Attention check 3


  lb4b_raw$AC3_fail <- !lb4b_raw$AC3 == "No"
  lb4b_raw$AC3_fail[is.na(lb4b_raw$AC3_fail)] <- NA

  # Total AC failed
  lb4b_raw$AC_num <- lb4b_raw$AC1_failsum + lb4b_raw$AC2_failsum + lb4b_raw$AC3_fail

  # MANIPULATION CHECKS:

  lb4b_raw$CC1_late_fail <- !(lb4b_raw$CC1_later == lb4b_raw$CC1)
  lb4b_raw$CC2_late_fail <- !(lb4b_raw$CC2_later == lb4b_raw$CC2)

  # Total later AC failed

  lb4b_raw$MC_total <- lb4b_raw$CC1_late_fail + lb4b_raw$CC2_late_fail


  ## DATA EXCLUSIONS
  lb4b <- lb4b_raw %>%
    dplyr::filter(
      AC_num < 2,
      MC_total == 0,
      shoes == "No" | clothes == "No",
      health1 == "No"
    )


  lb4b$ID <- 1:dim(lb4b)[1]
  #------------------end of script from sundvall 2023 ---------------------------#

  data <- lb4b


  # collapse AI agents into 1 & relabel
  data$agent <- as.factor(data$CC1)
  data <- data %>%
    dplyr::mutate(agent = recode(agent,
      `Human` = "1",
      .default = "2"
    ))
  data$agent <- factor(data$agent,
    levels = c(1, 2),
    labels = c("human", "AI")
  )

  # Reverse items so highest val. is acceptable/permissible synontm
  colsToReverse <- c("DV1_wrong_motor", "DV1_wrong_fisher")
  data[colsToReverse] <- 8 - data[colsToReverse]

  colsToReverse.slider <- c("DV1_slider_1", "DV1_slider_1.1")
  data[colsToReverse.slider] <- 100 - data[colsToReverse.slider]

  # ---- DVs
  # acceptable
  data <- data %>%
    dplyr::mutate(acceptable = rowMeans(across(
      c("DV2_acceptable_motor", "DV2_acceptable_fisher"),
      ~ as.numeric(.)
    ), na.rm = TRUE))

  data <- data %>%
    dplyr::mutate(acceptable.slider = rowMeans(across(
      c("DV2_slider_1", "DV2_slider_1.1"),
      ~ as.numeric(.)
    ), na.rm = TRUE))

  # wrong (reversed)
  data <- data %>%
    dplyr::mutate(wrong = rowMeans(across(
      c("DV1_wrong_motor", "DV1_wrong_fisher"),
      ~ as.numeric(.)
    ), na.rm = TRUE))

  data <- data %>%
    dplyr::mutate(wrong.slider = rowMeans(across(
      c("DV1_slider_1", "DV1_slider_1.1"),
      ~ as.numeric(.)
    ), na.rm = TRUE))

  # permissible
  data <- data %>%
    dplyr::mutate(permissible = rowMeans(across(
      c("DV3_permissible_motor", "DV3_permissible_fisher"),
      ~ as.numeric(.)
    ), na.rm = TRUE))

  data <- data %>%
    dplyr::mutate(permissible.slider = rowMeans(across(
      c("DV3_slider_1", "DV3_slider_1.1"),
      ~ as.numeric(.)
    ), na.rm = TRUE))


  # Responsible Moderator
  data <- data %>%
    dplyr::mutate(blame = rowMeans(across(
      c("DV4_blame_motor", "DV4_blame_fisher"),
      ~ as.numeric(.)
    ), na.rm = TRUE))
  # slider
  data <- data %>%
    dplyr::mutate(blame.slider = rowMeans(across(
      c("DV4_slider_1", "DV4_slider_1.1"),
      ~ as.numeric(.)
    ), na.rm = TRUE))
  data <- data %>%
    dplyr::mutate(punishment = rowMeans(across(
      c("DV5_punishment_motor", "DV5_punishment_fisher"),
      ~ as.numeric(.)
    ), na.rm = TRUE))
  # slider
  data <- data %>%
    dplyr::mutate(punishment.slider = rowMeans(across(
      c("DV5_slider_1", "DV5_slider_1.1"),
      ~ as.numeric(.)
    ), na.rm = TRUE))

  data <- data %>%
    dplyr::mutate(praise = rowMeans(across(
      c("DV6_praise_motor", "DV6_praise_fisher"),
      ~ as.numeric(.)
    ), na.rm = TRUE))

  # slider
  data <- data %>%
    dplyr::mutate(praise.slider = rowMeans(across(
      c("DV6_slider_1", "DV6_slider_1.1"),
      ~ as.numeric(.)
    ), na.rm = TRUE))

  # PMA Moderator
  data$PMA1 <- data$DV13_human_likert_1
  data$PMA2 <- data$DV13_human_slider_1

  # AI Agent Type A Moderator
  data <- data %>%
    dplyr::mutate(aiType_a = recode_factor(as.character(agent),
      "human" = "human",
      "AI" = "robot"
    ))

  # AI Agent Type B Moderator
  data <- data %>%
    dplyr::mutate(aiType_b = recode_factor(as.character(agent),
      "human" = "human",
      "AI" = "humanoid"
    ))
  
  data <- data %>%
    mutate(gender = recode(gender,
                           "Male" = "male",
                           "Female" = "female",
                           .default = "other"
    ))
  

  data$ID <- 1:nrow(data)

  sundvall2023_s4.2 <- data
  sundvall2023_s4.2
}


# ====================================================================== sundvall 2023 s5
# function for cleaning sundvall2023_s5
clean_sundvall2023_s5 <- function(raw_data) {
  lb5_raw <- read.csv(raw_data)

  #-------------------------script from sundvall 2023 ---------------------------#

  ### DATA EXCLUSIONS ---------------------------------------------------------

  ## ATTENTION CHECKS

  # note: slider considered failed if the response is anything other than NA or 50
  # Participants were instructed to ignore the slider, the default value of the slider being 50.
  # Participants who never click the slider will have been recorded as NA on the slider.
  # Participants who have clicked on the slider but not moved it or moved it but then rectified
  # this error will have been recorded as 50.

  # Attention check #1
  lb5_raw$AC1_fail <- !lb5_raw$AC1_likert == 2 | (is.na(lb5_raw$AC1_likert))
  lb5_raw$AC1_fail2 <- !(is.na(lb5_raw$AC1_slider_1) | (!is.na(lb5_raw$AC1_slider_1) & lb5_raw$AC1_slider_1 == 50))

  # Count a failure in either the likert, the slider, or both as ONE failed attention check
  lb5_raw <- lb5_raw %>% dplyr::mutate(AC1_failsum = ifelse(AC1_fail | AC1_fail2, 1, 0))

  # Attention check #2
  lb5_raw$AC2_fail <- !lb5_raw$AC2_likert == 6
  lb5_raw$AC2_fail2 <- !(is.na(lb5_raw$AC2_slider) | (!is.na(lb5_raw$AC2_slider) & lb5_raw$AC2_slider == 50))

  # Count a failure in either the likert, the slider, or both as ONE failed attention check
  lb5_raw <- lb5_raw %>% dplyr::mutate(AC2_failsum = ifelse(AC2_fail | AC2_fail2, 1, 0))

  # Attention check #3
  lb5_raw$AC3_fail <- !lb5_raw$AC_3 == "No" | (is.na(lb5_raw$AC_3))
  lb5_raw$AC3_fail[is.na(lb5_raw$AC3_fail)] <- NA

  # Total AC failed
  lb5_raw$AC_num <- lb5_raw$AC1_failsum + lb5_raw$AC2_failsum + lb5_raw$AC3_fail


  ## COMPREHENSION CHECKS:

  # Check that comprehension checks at the end of experiment (not forced correct)
  # match the comprehension checks after vignette (forced correct)
  lb5_raw$CC1_late_fail <- !lb5_raw$CC1_later == lb5_raw$CC1
  lb5_raw$CC2_late_fail <- !as.character(lb5_raw$CC2_later) == as.character(lb5_raw$CC2)

  # Total later CC failed
  lb5_raw$CC_total <- lb5_raw$CC1_late_fail + lb5_raw$CC2_late_fail

  # "Do you make your own shoes" / "Do you make your own clothes"
  lb5_raw <- lb5_raw %>% dplyr::mutate(
    shoesclothestroll =
      ifelse(shoes == "Yes" & clothes == "Yes", 1, 0)
  )

  ## DATA EXCLUSIONS: exclude participants who have:
  # * at least two failed attention checks
  # * any failed manipulation checks
  # * Reported their english skills less than good (good english skills were
  #   stated as inclusion criterion)
  # * answered "yes" to both questions regarding shoemaking and clothes-making
  #   (see https://osf.io/5bpxq)
  # * Missing values in any of the DV variables

  lb5 <- lb5_raw %>%
    dplyr::filter(
      AC_num < 2,
      CC_total == 0,
      english_skills == "Good" | english_skills == "Very Good",
      Finished == "True",
      shoes == "No" | clothes == "No",
      health1 == "No",
      !is.na(DV1_wrong_motor),
      !is.na(DV1_wrong_fisher),
      !is.na(DV2_acceptable_motor),
      !is.na(DV2_acceptable_fisher),
      !is.na(DV3_permissible_motor),
      !is.na(DV3_permissible_fisher),
      !is.na(DV4_blame_motor),
      !is.na(DV4_blame_fisher),
      !is.na(DV5_punishment_motor),
      !is.na(DV5_punishment_fisher),
      !is.na(DV6_praise_motor),
      !is.na(DV6_praise_fisher),
      !is.na(DV7_harm_motor),
      !is.na(DV7_harm_motor)
    )
  #------------------end of script from sundvall 2023 ---------------------------#

  data <- lb5

  # collapse AI agents into 1 & relabel
  data$agent <- as.factor(data$CC1)

  data <- data %>%
    dplyr::mutate(agent = recode(agent,
      `Human` = "1",
      .default = "2"
    ))
  data$agent <- as.factor(data$agent)
  data$agent <- factor(data$agent,
    levels = c(1, 2),
    labels = c("human", "AI")
  )

  # Reverse items so highest val. is acceptable/permissible synontm
  colsToReverse <- c("DV1_wrong_motor", "DV1_wrong_fisher")
  data[colsToReverse] <- 8 - data[colsToReverse]

  colsToReverse.slider <- c("DV1_slider_1", "DV1_slider_1.1")
  data[colsToReverse.slider] <- 100 - data[colsToReverse.slider]

  # ---- DVs
  # acceptable
  data <- data %>%
    dplyr::mutate(acceptable = rowMeans(across(
      c("DV2_acceptable_motor", "DV2_acceptable_fisher"),
      ~ as.numeric(.)
    ), na.rm = TRUE))

  data <- data %>%
    dplyr::mutate(acceptable.slider = rowMeans(across(
      c("DV2_slider_1", "DV2_slider_1.1"),
      ~ as.numeric(.)
    ), na.rm = TRUE))

  # wrong (reversed)
  data <- data %>%
    dplyr::mutate(wrong = rowMeans(across(
      c("DV1_wrong_motor", "DV1_wrong_fisher"),
      ~ as.numeric(.)
    ), na.rm = TRUE))

  data <- data %>%
    dplyr::mutate(wrong.slider = rowMeans(across(
      c("DV1_slider_1", "DV1_slider_1.1"),
      ~ as.numeric(.)
    ), na.rm = TRUE))

  # permissible
  data <- data %>%
    dplyr::mutate(permissible = rowMeans(across(
      c("DV3_permissible_motor", "DV3_permissible_fisher"),
      ~ as.numeric(.)
    ), na.rm = TRUE))

  data <- data %>%
    dplyr::mutate(permissible.slider = rowMeans(across(
      c("DV3_slider_1", "DV3_slider_1.1"),
      ~ as.numeric(.)
    ), na.rm = TRUE))

  # Responsible Moderator
  data <- data %>%
    dplyr::mutate(blame = rowMeans(across(
      c("DV4_blame_motor", "DV4_blame_fisher"),
      ~ as.numeric(.)
    ), na.rm = TRUE))
  # slider
  data <- data %>%
    dplyr::mutate(blame.slider = rowMeans(across(
      c("DV4_slider_1", "DV4_slider_1.1"),
      ~ as.numeric(.)
    ), na.rm = TRUE))
  data <- data %>%
    dplyr::mutate(punishment = rowMeans(across(
      c("DV5_punishment_motor", "DV5_punishment_fisher"),
      ~ as.numeric(.)
    ), na.rm = TRUE))
  # slider
  data <- data %>%
    dplyr::mutate(punishment.slider = rowMeans(across(
      c("DV5_slider_1", "DV5_slider_1.1"),
      ~ as.numeric(.)
    ), na.rm = TRUE))

  data <- data %>%
    dplyr::mutate(praise = rowMeans(across(
      c("DV6_praise_motor", "DV6_praise_fisher"),
      ~ as.numeric(.)
    ), na.rm = TRUE))

  # slider
  data <- data %>%
    dplyr::mutate(praise.slider = rowMeans(across(
      c("DV6_slider_1", "DV6_slider_1.1"),
      ~ as.numeric(.)
    ), na.rm = TRUE))

  # PMA Moderator
  data$PMA1 <- data$DV13_human_likert_1
  data$PMA2 <- data$DV13_human_slider_1
  # AI Agent Type A Moderator
  data <- data %>%
    dplyr::mutate(aiType_a = recode_factor(as.character(agent),
      "human" = "human",
      "AI" = "robot"
    ))

  # AI Agent Type B Moderator
  data <- data %>%
    dplyr::mutate(aiType_b = recode_factor(as.character(agent),
      "human" = "human",
      "AI" = "humanoid"
    ))
   
  data <- data %>%
    mutate(gender = recode(gender,
                           "Male" = "male",
                           "Female" = "female",
                           .default = "other"
    ))
  

  data$ID <- 1:nrow(data)

  sundvall2023_s5 <- data
  sundvall2023_s5
}

# ====================================================================== young 2019 s1
# function for cleaning young2019_s1
clean_young2019_s1 <- function(raw_data) {
  data <- read.csv(raw_data)
  data <- data %>%
    dplyr::mutate(agent = recode_factor(as.character(Agent),
      "Human" = "human",
      "AI" = "AI"
    ))

  data$permissible <- as.factor(data$m_permit)


  data <- data %>%
    dplyr::mutate(permissible = recode(permissible,
      "2" = "0",
      "1" = "1"
    ))
  # relevel permissible so that .desc comes out w. permissible=0 as 1st and 3rd rows
  data$permissible <- factor(data$permissible, levels = c(0, 1))

  # ========== Data Cleaning for Moderator Analyses ========#
  # In.action Moderator
  data$in_action <- factor(data$action_inaction,
    levels = c(2, 1),
    labels = c("inaction", "action")
  )

  # Responsible Moderator
  data$blame <- as.numeric(data$blame)
  # PMA Moderator
  data$PMA <- data$moral
  # PMC Moderator
  data$PMC <- data$mindedness
  # AI Agent Type A Moderator
  data$aiType_a <- as.factor(data$agent)
  data$aiType_b <- as.factor(data$agent)
  
  data <- data %>%
    mutate(gender = recode(sex,
                           "1" = "male",
                           "2" = "female",
                           .default = "other"
    ))
  

  data$ID <- 1:nrow(data)

  young2019_s1 <- data
  young2019_s1
}

# ====================================================================== young 2019 s2
# function for cleaning young2019_s2
clean_young2019_s2 <- function(raw_data) {
  data <- read.csv(raw_data)
  data$agent <- as.factor(data$AI_condition_name)
  data$permissible <- as.factor(data$m_permit)

  # code agent variable w. numbers
  data <- data %>%
    dplyr::mutate(agent = recode(agent,
      "human" = "1",
      .default = "2"
    ))

  data$agent <- factor(data$agent,
    levels = c(1, 2),
    labels = c("human", "AI")
  )

  data <- data %>%
    dplyr::mutate(permissible = recode(permissible,
      "2" = "0",
      "1" = "1"
    ))
  # relevel permissible so that .desc comes out w. permissible=0 as 1st and 3rd rows
  data$permissible <- factor(data$permissible, levels = c(0, 1))

  # ========== Data Cleaning for Moderator Analyses ========#
  # In.action Moderator
  data$in_action <- factor(data$Action_condition,
    levels = c(2, 1),
    labels = c("inaction", "action")
  )


  # Responsible Moderator
  data$blame <- as.numeric(data$blame)
  # PMA Moderator
  data$PMA <- data$moral
  # PMC Moderator
  data$PMC <- data$mindedness
  # AI Agent Type A Moderator
  data$aiType_a <- as.factor(data$agent)
  data$aiType_b <- as.factor(data$agent)
  
  data <- data %>%
    mutate(gender = recode(sex,
                           "1" = "male",
                           "2" = "female",
                           .default = "other"
    ))
  

  data$ID <- 1:nrow(data)

  young2019_s2 <- data
  young2019_s2
}

# ====================================================================== young 2019 s3
# function for cleaning young2019_s3
clean_young2019_s3 <- function(raw_data) {
  data <- read.csv(raw_data)
  data$agent <- as.factor(data$AGENT_CONDITION)
  data$permissible <- as.factor(data$m_permit)

  # code agent variable w. numbers
  data <- data %>%
    dplyr::mutate(agent = recode(agent,
      "1" = "1",
      .default = "2"
    ))
  data$agent <- factor(data$agent,
    levels = c(1, 2),
    labels = c("human", "AI")
  )

  data <- data %>%
    dplyr::mutate(permissible = recode(permissible,
      "2" = "0",
      "1" = "1"
    ))
  # relevel permissible so that .desc comes out w. permissible=0 as 1st and 3rd rows
  data$permissible <- factor(data$permissible, levels = c(0, 1))

  # ========== Data Cleaning for Moderator Analyses ========#
  # In.action Moderator
  data$in_action <- factor(data$ACTION_CONDITION,
    levels = c(2, 1),
    labels = c("inaction", "action")
  )

  # Responsible Moderator
  data$blame <- as.numeric(data$blame)
  # PMA Moderator
  data$PMA <- data$moral
  # PMC Moderator
  data$PMC <- data$mindedness
  # AI Agent Type A Moderator
  data$aiType_a <- as.factor(data$agent)
  data$aiType_b <- as.factor(data$agent)
  
  data <- data %>%
    mutate(gender = recode(sex,
                           "Male" = "male",
                           "Female" = "female",
                           .default = "other"
    ))
  

  data$ID <- 1:nrow(data)

  young2019_s3 <- data
  young2019_s3
}

# ====================================================================== komatsu 2017 s1
# function for cleaning komatsu2017_s1
clean_komatsu2017_s1 <- function(raw_data) {
  data <- read.csv(raw_data)
  data$ID <- 1:nrow(data)
  data$agent <- as.factor(data$agent)

  data$agent <- factor(data$agent,
    levels = c(1, 2),
    labels = c("human", "AI")
  )
  data$wrong <- as.factor(data$wrong)

  data <- data %>%
    dplyr::mutate(wrong = recode(wrong,
      "1" = 0,
      "0" = 1
    ))
  data$wrong <- as.factor(data$wrong)

  # ========== Data Cleaning for Moderator Analyses ========#
  # In.action Moderator
  data$in_action <- factor(data$in_action,
    levels = c(2, 1),
    labels = c("inaction", "action")
  )
  # AI Agent Type A Moderator
  data <- data %>%
    dplyr::mutate(aiType_a = recode_factor(as.character(agent),
      "human" = "human",
      "AI" = "robot"
    ))

  # AI Agent Type B Moderator
  data <- data %>%
    dplyr::mutate(aiType_b = recode_factor(as.character(agent),
      "human" = "human",
      "AI" = "mechanical"
    ))

  komatsu2017_s1 <- data
  komatsu2017_s1
}

################################################################################
# MANUAL STUDIES
################################################################################

# ====================================================================== wilson 2022 s1
# function for computing the manual data from wilson2022_s1
compute_manual_wilson2022_s1 <- function() {
  # wrong (reversed)
  w22_wrong <- list(
    n1  = 50,
    m1  = 6 - 3.87,
    sd1 = 1.26,
    n2  = 51,
    m2  = 6 - 3.60,
    sd2 = 1.44
  )

  # justified
  w22_justif <- list(
    n1  = 50,
    m1  = 2.25,
    sd1 = 1.20,
    n2  = 51,
    m2  = 2.38,
    sd2 = 1.26
  )

  # permissible
  w22_perm <- list(
    n1  = 50,
    m1  = 2.27,
    sd1 = 1.33,
    n2  = 51,
    m2  = 2.42,
    sd2 = 1.33
  )

  # return all three as one object
  list(
    wrong       = w22_wrong,
    justified   = w22_justif,
    permissible = w22_perm
  )
}


# ====================================================================== wilson 2022 s2
# function for computing the manual data from wilson2022_s2
compute_manual_wilson2022_s2 <- function() {
  w22_s2_wrong <- list(
    n1  = 154,
    m1  = 8 - 3.15,
    sd1 = 0.989,
    n2  = 159,
    m2  = 8 - 3.05,
    sd2 = 1.211
  )
  w22_s2_wrong
}

# ====================================================================== zhang 2022 s1
# function for computing the manual data from zhang2022_s1
compute_manual_zhang2022_s1 <- function() {
  # coefficients
  bH <- 0.177
  bHI <- 0.249
  bHA <- 0.079
  bHIA <- 0.000

  # AI coefficients
  bH_ai <- 0.256
  bHI_ai <- 0.169

  # SEs
  se_bH <- 0.047
  se_bHI <- 0.070
  se_bH_ai <- 0.020
  se_bHI_ai <- 0.041

  # residual SDs + sample sizes
  resid_h <- 0.241
  resid_ai <- 0.264
  n_h <- 1062
  n_ai <- 4563

  # evaluation point
  I0 <- 0.35

  # assumed correlation
  rho <- 0

  # slopes
  slope_h <- bH + bHI * I0
  slope_ai <- bH_ai + bHI_ai * I0

  # variances of slopes
  cov_h <- rho * se_bH * se_bHI
  var_h <- se_bH^2 + (I0^2) * se_bHI^2 + 2 * I0 * cov_h

  cov_ai <- rho * se_bH_ai * se_bHI_ai
  var_ai <- se_bH_ai^2 + (I0^2) * se_bHI_ai^2 + 2 * I0 * cov_ai

  # difference
  delta <- slope_ai - slope_h
  var_delta <- var_ai + var_h

  # pooled SD
  sd_pooled <- sqrt(((n_h - 1) * resid_h^2 + (n_ai - 1) * resid_ai^2) / (n_h + n_ai - 2))

  # SMD
  SMD <- delta / sd_pooled
  var_SMD <- var_delta / (sd_pooled^2)
  se_SMD <- sqrt(var_SMD)

  # Hedges correction
  df <- n_h + n_ai - 2
  J <- 1 - (3 / (4 * df - 1))
  g <- J * SMD
  g <- -g
  var_g <- J * se_SMD

  # return exactly what you need
  list(
    humanN = 312, # round(625/2)
    aiN = 312, # round(625/2)
    SMD = SMD,
    var_SMD = var_SMD,
    g = g,
    var_g = var_g
  )
}


# ====================================================================== zhang 2023
# function for computing the manual data from zhang2023 (studies 1–3)
compute_manual_zhang2023 <- function() {
  # ==========================================================================
  # Study 1
  # ==========================================================================
  n1 <- n2 <- round(195 / 4)

  #------------------------------ Morality: Action
  z_s1_a_m <- list(
    humanN = n1,
    aiN = n2,
    humanMean = 60.88,
    aiMean = 52.53,
    humanSD = 3.97,
    aiSD = 3.90
  )

  #------------------------------ Morality: Inaction
  z_s1_ia_m <- list(
    humanN = n1,
    aiN = n2,
    humanMean = 62.49,
    aiMean = 53.74,
    humanSD = 3.97,
    aiSD = 4.10
  )

  #------------------------------ Permissibility: Action
  z_s1_a_p <- list(
    humanN = n1,
    aiN = n2,
    humanMean = 61.14,
    aiMean = 58.04,
    humanSD = 3.97,
    aiSD = 3.89
  )

  #------------------------------ Permissibility: Inaction
  z_s1_ia_p <- list(
    humanN = n1,
    aiN = n2,
    humanMean = 60.00,
    aiMean = 57.93,
    humanSD = 3.97,
    aiSD = 4.10
  )

  #------------------------------ Wrongness: Action (reversed)
  z_s1_a_w <- list(
    humanN = n1,
    aiN = n2,
    humanMean = 101 - 44.63,
    aiMean = 101 - 51.31,
    humanSD = 4.06,
    aiSD = 3.98
  )

  #------------------------------ Wrongness: Inaction (reversed)
  z_s1_ia_w <- list(
    humanN = n1,
    aiN = n2,
    humanMean = 101 - 44.04,
    aiMean = 101 - 46.41,
    humanSD = 4.06,
    aiSD = 4.19
  )


  # ==========================================================================
  # Study 2
  # ==========================================================================
  n1 <- n2 <- round(194 / 4)

  #------------------------------ Morality: Action
  z_s2_a_m <- list(
    humanN = n1,
    aiN = n2,
    humanMean = 35.96,
    aiMean = 39.09,
    humanSD = 4.27,
    aiSD = 4.32
  )

  #------------------------------ Morality: Inaction
  z_s2_ia_m <- list(
    humanN = n1,
    aiN = n2,
    humanMean = 63.43,
    aiMean = 64.12,
    humanSD = 4.23,
    aiSD = 4.19
  )

  #------------------------------ Permissibility: Action
  z_s2_a_p <- list(
    humanN = n1,
    aiN = n2,
    humanMean = 40.90,
    aiMean = 34.43,
    humanSD = 3.99,
    aiSD = 4.03
  )

  #------------------------------ Permissibility: Inaction
  z_s2_ia_p <- list(
    humanN = n1,
    aiN = n2,
    humanMean = 59.39,
    aiMean = 67.38,
    humanSD = 3.95,
    aiSD = 3.91
  )

  #------------------------------ Wrongness: Action (reversed)
  z_s2_a_w <- list(
    humanN = n1,
    aiN = n2,
    humanMean = 101 - 68.10,
    aiMean = 101 - 61.21,
    humanSD = 4.02,
    aiSD = 4.06
  )

  #------------------------------ Wrongness: Inaction (reversed)
  z_s2_ia_w <- list(
    humanN = n1,
    aiN = n2,
    humanMean = 101 - 41.55,
    aiMean = 101 - 37.14,
    humanSD = 4.02,
    aiSD = 3.93
  )


  # ==========================================================================
  # Study 3
  # ==========================================================================
  n1 <- n2 <- round(236 / 4)

  #------------------------------ Morality: Action (Side-effect)
  z_s3_a_m_se <- list(
    humanN = n1,
    aiN = n2,
    humanMean = 48.74,
    aiMean = 45.66,
    humanSD = 3.42,
    aiSD = 3.50
  )

  #------------------------------ Morality: Inaction (Side-effect)
  z_s3_ia_m_se <- list(
    humanN = n1,
    aiN = n2,
    humanMean = 60.42,
    aiMean = 53.17,
    humanSD = 3.47,
    aiSD = 3.50
  )

  #------------------------------ Permissibility: Action (Side-effect)
  z_s3_a_p_se <- list(
    humanN = n1,
    aiN = n2,
    humanMean = 54.46,
    aiMean = 51.16,
    humanSD = 3.25,
    aiSD = 3.34
  )

  #------------------------------ Permissibility: Inaction (Side-effect)
  z_s3_ia_p_se <- list(
    humanN = n1,
    aiN = n2,
    humanMean = 62.81,
    aiMean = 48.90,
    humanSD = 3.31,
    aiSD = 3.34
  )

  #------------------------------ Wrongness: Action (Side-effect, reversed)
  z_s3_a_w_se <- list(
    humanN = n1,
    aiN = n2,
    humanMean = 101 - 54.03,
    aiMean = 101 - 48.31,
    humanSD = 3.46,
    aiSD = 3.55
  )

  #------------------------------ Wrongness: Inaction (Side-effect, reversed)
  z_s3_ia_w_se <- list(
    humanN = n1,
    aiN = n2,
    humanMean = 101 - 40.36,
    aiMean = 101 - 47.47,
    humanSD = 3.52,
    aiSD = 3.55
  )

  #------------------------------ Morality: Action (Means-to-end)
  z_s3_a_m_me <- list(
    humanN = n1,
    aiN = n2,
    humanMean = 33.87,
    aiMean = 37.36,
    humanSD = 3.62,
    aiSD = 3.71
  )

  #------------------------------ Morality: Inaction (Means-to-end)
  z_s3_ia_m_me <- list(
    humanN = n1,
    aiN = n2,
    humanMean = 59.78,
    aiMean = 56.19,
    humanSD = 3.68,
    aiSD = 3.71
  )

  #------------------------------ Permissibility: Action (Means-to-end)
  z_s3_a_p_me <- list(
    humanN = n1,
    aiN = n2,
    humanMean = 38.18,
    aiMean = 40.90,
    humanSD = 3.47,
    aiSD = 3.56
  )

  #------------------------------ Permissibility: Inaction (Means-to-end)
  z_s3_ia_p_me <- list(
    humanN = n1,
    aiN = n2,
    humanMean = 60.25,
    aiMean = 53.00,
    humanSD = 3.53,
    aiSD = 3.56
  )

  #------------------------------ Wrongness: Action (Means-to-end, reversed)
  z_s3_a_w_me <- list(
    humanN = n1,
    aiN = n2,
    humanMean = 101 - 65.36,
    aiMean = 101 - 69.79,
    humanSD = 3.53,
    aiSD = 3.62
  )

  #------------------------------ Wrongness: Inaction (Means-to-end, reversed)
  z_s3_ia_w_me <- list(
    humanN = n1,
    aiN = n2,
    humanMean = 101 - 46.71,
    aiMean = 101 - 44.69,
    humanSD = 3.53,
    aiSD = 3.62
  )


  # ==========================================================================
  # Return everything
  # ==========================================================================
  list(
    study1 = list(
      morality_action      = z_s1_a_m,
      morality_inaction    = z_s1_ia_m,
      permiss_action       = z_s1_a_p,
      permiss_inaction     = z_s1_ia_p,
      wrong_action         = z_s1_a_w,
      wrong_inaction       = z_s1_ia_w
    ),
    study2 = list(
      morality_action      = z_s2_a_m,
      morality_inaction    = z_s2_ia_m,
      permiss_action       = z_s2_a_p,
      permiss_inaction     = z_s2_ia_p,
      wrong_action         = z_s2_a_w,
      wrong_inaction       = z_s2_ia_w
    ),
    study3 = list(
      morality_action_se   = z_s3_a_m_se,
      morality_inaction_se = z_s3_ia_m_se,
      permiss_action_se    = z_s3_a_p_se,
      permiss_inaction_se  = z_s3_ia_p_se,
      wrong_action_se      = z_s3_a_w_se,
      wrong_inaction_se    = z_s3_ia_w_se,
      morality_action_me   = z_s3_a_m_me,
      morality_inaction_me = z_s3_ia_m_me,
      permiss_action_me    = z_s3_a_p_me,
      permiss_inaction_me  = z_s3_ia_p_me,
      wrong_action_me      = z_s3_a_w_me,
      wrong_inaction_me    = z_s3_ia_w_me
    )
  )
}
