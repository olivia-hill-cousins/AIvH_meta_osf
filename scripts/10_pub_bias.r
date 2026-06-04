three_funnel <- function(mydf, name) {
  mydf <- mydf %>%
    dplyr::mutate(
      N = humanN + aiN,
      sd = sqrt(vi),
      sqrtN = sqrt(N),
      se = sd / sqrtN
    )

  mydf$author <- mydf$ref
  article_id <- mydf$article_id
  study <- mydf$participant_id
  out <- mydf$efN_id
  ES <- mydf$yi
  var <- mydf$vi
  se <- mydf$se
  author <- mydf$author

  dataset <- data.frame(study, ES, out, var, se)
  contour.points <- 200
  meta_abu <- summary(rma.mv(
    yi = ES, V = var,
    random = ~ 1 | study / out,
    method = "REML",
    test = "t",
    dfs = "contain",
    data = dataset
  ))
  estimate <- meta_abu$beta[1] # pooled effect
  tau <- meta_abu$sigma2[1] # between-study variance
  out <- meta_abu$sigma2[2] # residual variance / extra random effect variance

  maxse <- max(dataset$se)
  ylim <- c(0, maxse)
  csize <- seq(ylim[1], ylim[2], length.out = contour.points)
  csize[csize <= 0] <- 1e-07 * min(dataset$se)
  csize

  CI_Lim <- matrix(0, nrow = length(csize), ncol = 2)
  colnames(CI_Lim) <- c("lb_total", "ub_total")

  for (i in 1:length(csize)) {
    CI_Lim[i, 1] <- estimate - 1.96 * sqrt((csize[i]^2) + tau + out) # add 1.96*
    CI_Lim[i, 2] <- estimate + 1.96 * sqrt((csize[i]^2) + tau + out)
  }
  CI_Lim <- as.data.frame(CI_Lim)

  dataset$study <- as.character(dataset$study)
  dataset$study <- factor(dataset$study)
  geom.text.size <- 3
  max_SE <- max(dataset$se)
  le <- length(CI_Lim[, 1])

  if ((CI_Lim[le, 1]) < 0) {
    minimum <- min(CI_Lim[, 1])
  } else {
    minimum <- max(CI_Lim[, 1])
  }

  if ((CI_Lim[le, 2]) > 0) {
    maximum <- max(CI_Lim[, 2])
  } else {
    maximum <- min(CI_Lim[, 2])
  }


  lim_minimum <- floor(minimum - 0.10)
  lim_maximum <- ceiling(maximum + 0.10)
  Axis_ES <- seq(lim_minimum, lim_maximum, by = 1)

  d <- ggplot(data = dataset, aes(x = se, y = ES, ylim(0, max_SE))) +
    geom_point() +
    labs(
      title = "", # "Funnel Plot for All Effect Sizes",

      x = ("Standard Error"),
      y = (bquote("Effect Size (Hedge's " * italic("g") * ")"))
    ) +
    geom_hline(yintercept = estimate) +
    geom_hline(yintercept = 0, color = "grey") +
    scale_x_reverse() +
    scale_y_continuous(breaks = Axis_ES, limits = c(lim_minimum, lim_maximum)) +
    coord_flip() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      # plot.title = element_text(. #I comment this out so that I can combine the plots more seamlessly later
      # hjust = 0.5,        # centre the title
      # family = "Georgia", # set font
      # size = 14,          # title size
      # face = "bold",
      # lineheight = 1.5    # double spacing
      # ),
      axis.title = element_text(
        family = "Georgia",
        size = 10,
        lineheight = 1.5 # double spacing
      ),
      axis.text = element_text(
        family = "Georgia",
        size = 10
      ),
      axis.line = element_line()
    )

  d <- d + geom_line(data = CI_Lim, aes(y = lb_total, x = csize), colour = "black") +
    geom_line(data = CI_Lim, aes(y = ub_total, x = csize), colour = "black")

  fig_save_name <- paste0("figures/", name, ".png")
  png(fig_save_name, width = 3508, height = 2480, res = 300) # A4 landscape in inches
  par(
    par(mar = c(4, 4, 5, 2)),
    par(oma = c(0, 0, 0, 0)),
    par(family = "Georgia")
  )

  # if using showtext

  print(d)

  dev.off()

  return(fig_save_name)
}


three_funnel_study <- function(mydf, name) {
  font_add("Georgia",
    regular = "Georgia.ttf",
    bold = "Georgia Bold.ttf",
    italic = "Georgia Italic.ttf",
    bolditalic = "Georgia Bold Italic.ttf"
  )

  showtext_auto()
  mydf <- mydf %>%
    dplyr::mutate(
      N = humanN + aiN,
      sd = sqrt(vi),
      sqrtN = sqrt(N),
      se = sd / sqrtN
    )
  size_dots <- 1
  numbers <- 1
  mydf$author <- mydf$ref
  article_id <- mydf$article_id
  study <- mydf$participant_id
  out <- mydf$efN_id
  ES <- mydf$yi
  var <- mydf$vi
  se <- mydf$se
  author <- mydf$author
  numbers <- numbers
  size_dots <- size_dots
  dataset <- data.frame(study, ES, out, var, se)
  contour.points <- 200

  meta_abu <- rma.mv(
    yi = ES,
    V = var,
    random = ~ 1 | study / out,
    data = dataset,
    method = "REML"
  )
  estimate <- meta_abu$beta[1] # pooled effect
  tau <- meta_abu$sigma2[1] # between-study variance
  out <- meta_abu$sigma2[2] # residual variance / extra random effect variance

  row <- 1
  nrow <- max(dataset$study)
  studyinfo <- data.frame(
    Study = numeric(nrow),
    id = numeric(nrow),
    ES = numeric(nrow),
    SE = numeric(nrow),
    k = numeric(nrow),
    median_SE = numeric(nrow)
  )
  Study1 <- c()
  geom.text.size <- 3

  for (i in unique(dataset$study)) {
    data <- subset(dataset, study == i)
    uni <- nrow(data)

    if (uni == 1) {
      studyinfo$ES[row] <- data$ES
      studyinfo$SE[row] <- data$se
      studyinfo$median_SE[row] <- data$se
    } else {
      a <- rma(y = data$ES, vi = data$var, data = data, method = "REML")
      studyinfo$ES[row] <- a$b
      studyinfo$SE[row] <- a$se
      studyinfo$median_SE[row] <- median(data$se)
    }

    studyinfo$id[row] <- i
    studyinfo$k[row] <- nrow(data)
    studyinfo$Study[row] <- c(Study1, paste("Study", i))
    row <- row + 1
  }

  median_k <- median(studyinfo$k)
  maxse <- max(studyinfo$SE)
  ylim <- c(0, maxse)
  csize <- seq(ylim[1], ylim[2], length.out = contour.points)
  csize[csize <= 0] <- 1e-07 * min(studyinfo$SE)
  CI_Lim <- matrix(0, nrow = length(csize), ncol = 2)
  colnames(CI_Lim) <- c("lb_total", "ub_total")

  for (i in 1:length(csize)) {
    CI_Lim[i, 1] <- estimate - 1.96 * sqrt((((csize[i]^2) + out) / median_k) + tau) # add 1.96*
    CI_Lim[i, 2] <- estimate + 1.96 * sqrt((((csize[i]^2) + out) / median_k) + tau)
  }
  CI_Lim <- as.data.frame(CI_Lim)

  le <- length(CI_Lim[, 1])


  if ((CI_Lim[le, 1]) < 0) {
    minimum <- min(CI_Lim[, 1])
  } else {
    minimum <- max(CI_Lim[, 1])
  }

  if ((CI_Lim[le, 2]) > 0) {
    maximum <- max(CI_Lim[, 2])
  } else {
    maximum <- min(CI_Lim[, 2])
  }


  lim_minimum <- floor(minimum - 0.10)
  lim_maximum <- ceiling(maximum + 0.10)
  Axis_ES <- seq(lim_minimum, lim_maximum, by = 1)

  if (size_dots == 1) {
    if (numbers == 1) {
      e <- ggplot(data = studyinfo, aes(x = SE, y = ES, ylim(0, maxse))) +
        geom_point(data = studyinfo, aes(size = k)) +
        geom_text_repel(aes(label = factor(studyinfo$k)), hjust = 0, vjust = -0.40, size = geom.text.size, family = "Georgia", direction = "x", segment.size = 0.2, segment.color = "grey50") +
        labs(
          y = bquote("Participant Sample Mean Effect (Hedge's " * italic(g) * ")"),
          x = bquote("Meta-Analytic " * italic(SE))
        ) +
        geom_hline(yintercept = estimate) +
        geom_hline(yintercept = 0, color = "grey") +
        scale_x_reverse() +
        scale_y_continuous(breaks = Axis_ES, limits = c(lim_minimum, lim_maximum)) +
        coord_flip() +
        theme_bw() +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(),
          text = element_text(family = "Georgia"),
          axis.text = element_text(
            family = "Georgia",
            size = 10
          ),
          legend.position = "none"
        )
    } else {
      e <- ggplot(data = studyinfo, aes(x = SE, y = ES, ylim(0, maxse))) +
        geom_point(data = studyinfo, aes(size = k)) +
        labs(
          title = "Funnel Plot for Participant Samples",
          y = bquote("Participant Sample Mean Effect (Hedge's " * italic(g) * ")"),
          x = bquote("Meta-Analytic " * italic(SE))
        ) +
        geom_hline(yintercept = estimate) +
        geom_hline(yintercept = 0, color = "grey") +
        scale_x_reverse() +
        scale_y_continuous(breaks = Axis_ES, limits = c(lim_minimum, lim_maximum)) +
        coord_flip() +
        theme_bw() +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          text = element_text(family = "Georgia"),
          legend.position = "none",
          plot.title = element_text(
            hjust = 0.5, # centre the title
            family = "Georgia", # set font
            size = 10, # title size
            face = "bold",
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
          axis.line = element_line()
        )
    }
  } else {
    if (numbers == 1) {
      e <- ggplot(data = studyinfo, aes(x = SE, y = ES, ylim(0, maxse))) +
        geom_point() +
        geom_text_repel(aes(label = factor(studyinfo$k)), hjust = 0, vjust = -0.40, size = geom.text.size, family = "Georgia", direction = "x", segment.size = 0.2, segment.color = "grey50") +
        labs(
          title = "Funnel Plot for Participant Samples",
          y = bquote("Participant Sample Mean Effect (Hedge's " * italic(g) * ")"),
          x = bquote("Meta-Analytic " * italic(SE))
        ) +
        geom_hline(yintercept = estimate) +
        geom_hline(yintercept = 0, color = "grey") +
        scale_x_reverse() +
        scale_y_continuous(breaks = Axis_ES, limits = c(lim_minimum, lim_maximum)) +
        coord_flip() +
        theme_bw() +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          text = element_text(family = "Georgia"),
          legend.position = "none",
          plot.title = element_text(
            hjust = 0.5, # centre the title
            family = "Georgia", # set font
            size = 10, # title size
            face = "bold",
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
          axis.line = element_line()
        )
    } else {
      e <- ggplot(data = studyinfo, aes(x = SE, y = ES, ylim(0, maxse))) +
        geom_point() +
        labs(
          title = "Funnel Plot for Participant Samples",
          y = bquote("Participant Sample Mean Effect (Hedge's " * italic(g) * ")"),
          x = bquote("Meta-Analytic " * italic(SE))
        )
      +
        geom_hline(yintercept = estimate) +
        geom_hline(yintercept = 0, color = "grey") +
        scale_x_reverse() +
        scale_y_continuous(breaks = Axis_ES, limits = c(lim_minimum, lim_maximum)) +
        coord_flip() +
        theme_bw() +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          text = element_text(family = "Georgia"),
          legend.position = "none",
          plot.title = element_text(
            hjust = 0.5, # centre the title
            family = "Georgia", # set font
            size = 10, # title size
            face = "bold",
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
          axis.line = element_line()
        )
    }
  }

  e <- e + geom_line(data = CI_Lim, aes(y = lb_total, x = csize), colour = "black") +
    geom_line(data = CI_Lim, aes(y = ub_total, x = csize), colour = "black")
  # print(e)

  fig_save_name <- paste0("figures/", name, ".png")

  png(fig_save_name, width = 3508, height = 2280, res = 300) # A4 landscape in inches (height reduced from 2480 to leave room for note)
  par(
    par(mar = c(4, 4, 5, 2)),
    par(oma = c(4, 0, 4, 0)),
    par(family = "Georgia")
  )

  # if using showtext

  print(e)

  dev.off()

  return(fig_save_name)
}
# print(d)

make_orchard_egger_reg_plot <- function(model, df, name) {
  colours <- brewer.pal(12, "Paired")
  light_green <- "#B2DF8A"
  dark_green <- "#33A02C"
  pink_red <- "#FB9A99"
  red <- "#E31A1C"
  light_blue <- "#A6CEE3"
  dark_blue <- "#1F78B4"
  font_add("Georgia",
    regular = "Georgia.ttf", bold = "Georgia Bold.ttf",
    italic = "Georgia Italic.ttf", bolditalic = "Georgia Bold Italic.ttf"
  )

  showtext_auto() # Enable for all plots

  georgia_theme <- theme_minimal(base_family = "Georgia") +
    theme(
      text = element_text(family = "Georgia"),
      axis.text = element_text(family = "Georgia"),
      axis.title = element_text(family = "Georgia"),
      legend.text = element_text(family = "Georgia"),
      legend.title = element_text(family = "Georgia"),
      # Add axis lines back in
      axis.line = element_line(colour = "black", linewidth = 0.1)
    )

  pub_bias_plot <- function(plot, fe_model, v_model = NULL, col = c(pink_red, dark_blue), plotadj = -0.05, textadj = 0.05, branch.size = 1.2, trunk.size = 3) {
    # Add check to make sure it's an intercept ONLY model being added. Message to user if not.
    if (length(fe_model$b) > 1) {
      stop("The model you are trying to add to the plot is not an intercept only model. Please ensure you have fit an intercept only meta-analysis. See vignette for details: https://daniel1noble.github.io/orchaRd/")
    }

    # Get the predictions from the final model and create a label for the plot
    pub_bias_data <- get_ints_dat(fe_model, type = "br")

    if (is.null(v_model)) {
      # Add to Existing Orchard Plot
      plot + geom_pub_stats_yang(pub_bias_data, plotadj = plotadj, textadj = textadj, branch.size = branch.size, trunk.size = trunk.size)
    } else {
      # Extract the corrected meta-analytic mean and CI
      pub_bias_data2 <- get_ints_dat(v_model, type = "bc")

      plot + geom_pub_stats_yang(pub_bias_data, plotadj = plotadj, textadj = textadj, branch.size = branch.size, trunk.size = trunk.size) + geom_pub_stats_naka(pub_bias_data2, plotadj = plotadj, textadj = textadj, branch.size = branch.size, trunk.size = trunk.size)
    }
  }


  #####################
  ## Helper functioons
  #####################


  #' @title geom_pub_stats_yang
  #' @description This function adds a corrected meta-analytic mean, sensu Yang et al. 2023, confidence interval and text annotation to an intercept only orchard plot.
  #' @param data The data frame containing the corrected meta-analytic mean and confidence intervals.
  #' @param col The colour of the mean and confidence intervals.
  #' @param plotadj The adjustment to the x-axis position of the mean and confidence intervals.
  #' @param textadj The adjustment to the y-axis position of the mean and confidence intervals for the text displaying the type of correction.
  #' @param branch.size Size of the confidence intervals.
  #' @param trunk.size Size of the mean, or central point.
  #' @return A list of ggplot2 objects to be added to the orchard plot.
  #' @author Daniel Noble - daniel.noble@anu.edu.au

  geom_pub_stats_yang <- function(data, col = pink_red, plotadj = -0.05, textadj = 0.3, branch.size = 1.2, trunk.size = 3) {
    list(
      ggplot2::geom_point(data = data[[1]], ggplot2::aes(x = name, y = pred), color = col, alpha = 0.6, shape = "diamond", position = position_nudge(plotadj), size = trunk.size),
      ggplot2::geom_linerange(data = data[[1]], ggplot2::aes(x = name, ymin = ci.lb, ymax = ci.ub), color = col, position = position_nudge(plotadj), size = branch.size),
      ggplot2::annotate("text", x = 1 + plotadj - (textadj + 0.2), y = data[[1]]$pred - 4, label = data[[2]], color = col, size = 4, hjust = data[[1]]$ci.ub - 0.2)
    )
  }

  #' @title geom_pub_stats_naka
  #' @description This function adds a corrected meta-analytic mean, sensu Nakagawa et al. 2022, confidence interval and text annotation to an intercept only orchard plot.
  #' @param data The data frame containing the corrected meta-analytic mean and confidence intervals.
  #' @param col The colour of the mean and confidence intervals.
  #' @param plotadj The adjustment to the x-axis position of the mean and confidence intervals.
  #' @param textadj The adjustment to the y-axis position of the mean and confidence intervals for the text displaying the type of correction.
  #' @param branch.size Size of the confidence intervals.
  #' @param trunk.size Size of the mean, or central point.
  #' @return A list of ggplot2 objects to be added to the orchard plot.
  #' @author Daniel Noble - daniel.noble@anu.edu.au

  geom_pub_stats_naka <- function(data, col = dark_blue, plotadj = -0.05, textadj = 0.05, branch.size = 1.2, trunk.size = 3) {
    list(
      ggplot2::geom_point(data = data[[1]], ggplot2::aes(x = name, y = pred), color = col, alpha = 0.6, shape = "diamond", position = position_nudge(abs(plotadj)), size = trunk.size),
      ggplot2::geom_linerange(data = data[[1]], ggplot2::aes(x = name, ymin = ci.lb, ymax = ci.ub), color = col, position = position_nudge(abs(plotadj)), size = branch.size),
      ggplot2::annotate("text", x = 1 + abs(plotadj) + textadj, y = data[[1]]$pred - textadj, label = data[[2]], color = col, size = 4, hjust = data[[1]]$ci.ub + 0.2)
    )
  }

  #' @title get_ints_dat
  #' @description This function extracts the corrected meta-analytic mean and confidence intervals from a model object.
  #' @param model The rma model object containing the corrected meta-analytic mean and confidence intervals.
  #' @param type The type of correction to extract the corrected meta-analytic mean and confidence intervals from. "br" (i.e., Bias Robust) for Yang et al. 2023, "bc" (i.e., Bias-Corrected) for Nakagawa et al. 2023.
  #' @return A list containing the corrected meta-analytic mean and confidence intervals, and a label for the plot.
  #' @author Daniel Noble - daniel.noble@anu.edu.au

  get_ints_dat <- function(model, type = c("bc", "br")) {
    # Extract the corrected meta-analytic mean and CI
    type <- match.arg(type)

    dat <- data.frame(
      name = "Intrcpt",
      pred = model$b["intrcpt", ],
      ci.lb = model$ci.lb[1],
      ci.ub = model$ci.ub[1]
    )
    if (type == "bc") {
      lab <- paste0(
        "Bias Corrected Estimate: ", round(dat$pred, 2),
        ", 95% CI [", round(dat$ci.lb, 2), ",", round(dat$ci.ub, 2), "]"
      )
    }

    if (type == "br") {
      lab <- paste0(
        "Bias Robust Estimate: ", round(dat$pred, 2),
        ", 95% CI [", round(dat$ci.lb, 2), ",", round(dat$ci.ub, 2), "]"
      )
    }

    return(list(dat, lab))
  }
  # MLMA model
  MA1 <- model
  # Now apply the approach of Yang et al. 2024
  # Step 1: Fit the fixed effect model
  MA2 <- metafor::rma(yi, vi, data = df, test = "t", method = "FE")

  # # Step 2: Correct for dependency
  MA2_1 <- metafor::robust(MA2, cluster = participant_id, clubSandwich = TRUE)

  # # Step 3: Testing modified eggers. Need intercept following Nakagawa et al. 2022
  MA4 <- metafor::rma.mv(yi, vi, mod = ~vi, random = ~ 1 | participant_id / efN_id, test = "t", data = df, dfs = "contain")
  #
  plot <- orchard_plot(MA1, group = "participant_id", xlab = expression("Hedge's " * italic(g)), cb = FALSE, alpha = 0.15) +
    georgia_theme +
    scale_fill_manual(values = light_green) +
    scale_colour_manual(values = dark_green)

  plot2 <- pub_bias_plot(plot, MA2_1, MA4, col = c(pink_red, dark_green)) +
    georgia_theme +
    scale_fill_manual(values = light_green) +
    scale_colour_manual(values = dark_green)

  fig_save_name <- paste0("figures/", name, ".png")

  png(fig_save_name, width = 3508, height = 2480, res = 300) # A4 landscape in inches (height reduced from 2480 to leave room for note)
  par(
    par(mar = c(4, 4, 5, 2)),
    par(oma = c(4, 0, 4, 0)),
    par(family = "Georgia")
  )

  # if using showtext

  print(plot2)

  dev.off()

  return(fig_save_name)
}
