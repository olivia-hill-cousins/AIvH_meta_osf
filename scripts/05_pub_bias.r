## I2
run_i2_test <- function(model,
                        table_filename = "i2_table.html",
                        plot_filename = "i2_plot.png") {
  #' Calculate I-squared values and variance distribution for multilevel meta-analysis models
  #'
  #' This function calculates values of \eqn{I^2} and the variance distribution for multilevel meta-analysis
  #' models fitted with \code{\link[metafor]{rma.mv}}.
  #'
  #'
  #' @usage mlm.variance.distribution(x)
  #'
  #' @param x An object of class \code{rma.mv}. Must be a multilevel model with two random effects (three-level meta-analysis model).
  #'
  #' @details This function estimates the distribution of variance in a three-level meta-analysis
  #' model (fitted with the \code{\link[metafor]{rma.mv}} function). The share of variance attributable to
  #' sampling error, within and between-cluster heterogeneity is calculated,
  #' and an estimate of \eqn{I^2} (total and for Level 2 and Level 3) is provided. The function uses the formula by
  #' Cheung (2014) to estimate the variance proportions attributable to each model component and to derive the \eqn{I^2} estimates.
  #'
  #'
  #' @references
  #'
  #' Harrer, M., Cuijpers, P., Furukawa, T.A, & Ebert, D. D. (2019).
  #' \emph{Doing Meta-Analysis in R: A Hands-on Guide}. DOI: 10.5281/zenodo.2551803. \href{https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/mlma.html}{Chapter 12}.
  #'
  #' Cheung, M. W. L. (2014). Modeling dependent effect sizes with three-level meta-analyses: a structural equation modeling approach. \emph{Psychological Methods, 19}(2), 211.
  #'
  #' @author Mathias Harrer & David Daniel Ebert
  #'
  #' @aliases var.comp
  #'
  #' @import ggplot2
  #' @importFrom stats model.matrix
  #'
  #' @return Returns a data frame containing the results. A plot summarizing the variance distribution and \eqn{I^2} values can be generated using \code{plot}.
  #'
  #' @export mlm.variance.distribution
  #' @export var.comp
  #'
  #' @examples
  #' # Use dat.konstantopoulos2011 from the "metafor" package
  #' library(metafor)
  #'
  #' # Build Multilevel Model (Three Levels)
  #' m = rma.mv(yi, vi, random = ~ 1 | district/school, data=dat.konstantopoulos2011)
  #'
  #' # Calculate Variance Distribution
  #' mlm.variance.distribution(m)
  #'
  #' # Use alias 'var.comp' and 'Chernobyl' data set
  #' data("Chernobyl")
  #' m2 = rma.mv(yi = z, V = var.z, data = Chernobyl, random = ~ 1 | author/es.id)
  #' res = var.comp(m2)
  #'
  #' # Print results
  #' res
  #'
  #' # Generate plot
  #' plot(res)
  
  
  mlm.variance.distribution <- var.comp <- function(x) {
    m <- x
    
    # Check class
    if (!(class(m)[1] %in% c("rma.mv", "rma"))) {
      stop("x must be of class 'rma.mv'.")
    }
    
    # Check for three level model
    if (m$sigma2s != 2) {
      stop("The model you provided does not seem to be a three-level model. This function can only be used for three-level models.")
    }
    
    # Check for right specification (nested model)
    if (sum(grepl("/", as.character(m$random[[1]]))) < 1) {
      stop("Model must contain nested random effects. Did you use the '~ 1 | cluster/effect-within-cluster' notation in 'random'? See ?metafor::rma.mv for more details.")
    }
    
    # Get variance diagonal and calculate total variance
    n <- m$k.eff
    vector.inv.var <- 1 / (diag(m$V))
    sum.inv.var <- sum(vector.inv.var)
    sum.sq.inv.var <- (sum.inv.var)^2
    vector.inv.var.sq <- 1 / (diag(m$V)^2)
    sum.inv.var.sq <- sum(vector.inv.var.sq)
    num <- (n - 1) * sum.inv.var
    den <- sum.sq.inv.var - sum.inv.var.sq
    est.samp.var <- num / den
    
    # Calculate variance proportions
    level1 <- ((est.samp.var) / (m$sigma2[1] + m$sigma2[2] + est.samp.var) * 100)
    level2 <- ((m$sigma2[2]) / (m$sigma2[1] + m$sigma2[2] + est.samp.var) * 100)
    level3 <- ((m$sigma2[1]) / (m$sigma2[1] + m$sigma2[2] + est.samp.var) * 100)
    
    # Prepare df for return
    Level <- c("Level 1", "Level 2", "Level 3")
    Variance <- c(level1, level2, level3)
    df.res <- data.frame(Variance)
    colnames(df.res) <- c("% of total variance")
    rownames(df.res) <- Level
    I2 <- c("---", round(Variance[2:3], 2))
    df.res <- as.data.frame(cbind(df.res, I2))
    
    totalI2 <- Variance[2] + Variance[3]
    
    
    # Generate plot
    df1 <- data.frame(
      "Level" = c("Sampling Error", "Total Heterogeneity"),
      "Variance" = c(df.res[1, 1], df.res[2, 1] + df.res[3, 1]),
      "Type" = rep(1, 2)
    )
    
    df2 <- data.frame(
      "Level" = rownames(df.res),
      "Variance" = df.res[, 1],
      "Type" = rep(2, 3)
    )
    
    df <- as.data.frame(rbind(df1, df2))
    
    
    g <- ggplot(df, aes(fill = Level, y = Variance, x = as.factor(Type))) +
      coord_cartesian(ylim = c(0, 1), clip = "off") +
      geom_bar(stat = "identity", position = "fill", width = 1, color = "black") +
      scale_y_continuous(labels = scales::percent) +
      theme(
        axis.title.x = element_blank(),
        axis.text.y = element_text(color = "black"),
        axis.line.y = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_line(lineend = "round"),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.background = element_rect(
          linetype = "solid",
          colour = "black"
        ),
        legend.title = element_blank(),
        legend.key.size = unit(0.75, "cm"),
        axis.ticks.length = unit(.25, "cm"),
        plot.margin = unit(c(1, 3, 1, 1), "lines")
      ) +
      scale_fill_manual(values = c(
        "darkseagreen3", "deepskyblue3", "darkseagreen2",
        "deepskyblue1", "deepskyblue2"
      )) +
      
      # Add Annotation
      
      # Total Variance
      annotate("text",
               x = 1.5, y = 1.05,
               label = paste(
                 "Total Variance:",
                 round(m$sigma2[1] + m$sigma2[2] + est.samp.var, 3)
               )
      ) +
      
      # Sampling Error
      annotate("text",
               x = 1, y = (df[1, 2] / 2 + df[2, 2]) / 100,
               label = paste("Sampling Error Variance: \n", round(est.samp.var, 3)), size = 3
      ) +
      
      # Total I2
      annotate("text",
               x = 1, y = ((df[2, 2]) / 100) / 2 - 0.02,
               label = bquote("Total" ~ italic(I)^2 * ":" ~ .(round(df[2, 2], 2)) * "%"), size = 3
      ) +
      annotate("text",
               x = 1, y = ((df[2, 2]) / 100) / 2 + 0.05,
               label = paste("Variance not attributable \n to sampling error: \n", round(m$sigma2[1] + m$sigma2[2], 3)), size = 3
      ) +
      
      # Level 1
      annotate("text", x = 2, y = (df[1, 2] / 2 + df[2, 2]) / 100, label = paste("Level 1: \n",
                                                                                 round(df$Variance[3], 2), "%",
                                                                                 sep = ""
      ), size = 3) +
      
      # Level 2
      annotate("text",
               x = 2, y = (df[5, 2] + (df[4, 2] / 2)) / 100,
               label = bquote(italic(I)[Level2]^2 * ":" ~ .(round(df[4, 2], 2)) * "%"), size = 3
      ) +
      
      # Level 3
      annotate("text",
               x = 2, y = (df[5, 2] / 2) / 100,
               label = bquote(italic(I)[Level3]^2 * ":" ~ .(round(df[5, 2], 2)) * "%"), size = 3
      )
    
    returnlist <- list(
      results = df.res,
      totalI2 = totalI2,
      plot = g
    )
    class(returnlist) <- c("mlm.variance.distribution", "list")
    
    invisible(returnlist)
    
    returnlist
  }
  
  mlm.variance.distribution <- var.comp <- function(x) {
    m <- x
    
    # Check class
    if (!(class(m)[1] %in% c("rma.mv", "rma"))) {
      stop("x must be of class 'rma.mv'.")
    }
    
    # Check for three level model
    if (m$sigma2s != 2) {
      stop("The model you provided does not seem to be a three-level model. This function can only be used for three-level models.")
    }
    
    # Check for right specification (nested model)
    if (sum(grepl("/", as.character(m$random[[1]]))) < 1) {
      stop("Model must contain nested random effects. Did you use the '~ 1 | cluster/effect-within-cluster' notation in 'random'? See ?metafor::rma.mv for more details.")
    }
    
    # Get variance diagonal and calculate total variance
    n <- m$k.eff
    vector.inv.var <- 1 / (diag(m$V))
    sum.inv.var <- sum(vector.inv.var)
    sum.sq.inv.var <- (sum.inv.var)^2
    vector.inv.var.sq <- 1 / (diag(m$V)^2)
    sum.inv.var.sq <- sum(vector.inv.var.sq)
    num <- (n - 1) * sum.inv.var
    den <- sum.sq.inv.var - sum.inv.var.sq
    est.samp.var <- num / den
    
    # Calculate variance proportions
    level1 <- ((est.samp.var) / (m$sigma2[1] + m$sigma2[2] + est.samp.var) * 100)
    level2 <- ((m$sigma2[2]) / (m$sigma2[1] + m$sigma2[2] + est.samp.var) * 100)
    level3 <- ((m$sigma2[1]) / (m$sigma2[1] + m$sigma2[2] + est.samp.var) * 100)
    
    # Prepare df for return
    Level <- c("Level 1", "Level 2", "Level 3")
    Variance <- c(level1, level2, level3)
    df.res <- data.frame(Variance)
    colnames(df.res) <- c("% of total variance")
    rownames(df.res) <- Level
    I2 <- c("---", round(Variance[2:3], 2))
    df.res <- as.data.frame(cbind(df.res, I2))
    
    totalI2 <- Variance[2] + Variance[3]
    
    # Generate plot
    df1 <- data.frame(
      "Level" = c("Sampling Error", "Total Heterogeneity"),
      "Variance" = c(df.res[1, 1], df.res[2, 1] + df.res[3, 1]),
      "Type" = rep(1, 2)
    )
    
    df2 <- data.frame(
      "Level" = rownames(df.res),
      "Variance" = df.res[, 1],
      "Type" = rep(2, 3)
    )
    
    df <- as.data.frame(rbind(df1, df2))
    
    g <- ggplot(df, aes(fill = Level, y = Variance, x = as.factor(Type))) +
      coord_cartesian(ylim = c(0, 1), clip = "off") +
      geom_bar(stat = "identity", position = "fill", width = 1, color = "black") +
      scale_y_continuous(labels = scales::percent) +
      theme(
        axis.title.x = element_blank(),
        axis.text.y = element_text(color = "black"),
        axis.line.y = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_line(lineend = "round"),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.background = element_rect(
          linetype = "solid",
          colour = "black"
        ),
        legend.title = element_blank(),
        legend.key.size = unit(0.75, "cm"),
        axis.ticks.length = unit(.25, "cm"),
        plot.margin = unit(c(1, 3, 1, 1), "lines")
      ) +
      scale_fill_manual(values = c(
        "darkseagreen3", "deepskyblue3", "darkseagreen2",
        "deepskyblue1", "deepskyblue2"
      )) +
      
      # Add Annotation ----
    
    # Total Variance
    annotate("text",
             x = 1.5, y = 1.05,
             label = paste(
               "Total Variance:",
               round(m$sigma2[1] + m$sigma2[2] + est.samp.var, 3)
             )
    ) +
      
      # Sampling Error
      annotate("text",
               x = 1, y = (df[1, 2] / 2 + df[2, 2]) / 100,
               label = paste("Sampling Error Variance:\n", round(est.samp.var, 3)), size = 3.5
      ) +
      
      # Total I2
      geom_text(
        x = 1, y = ((df[2, 2]) / 100) / 2 - 0.02,
        label = paste0('"Total"~italic(I)^2*":"~"', round(df[2, 2], 2), '"~"%"'),
        parse = TRUE, size = 4
      ) +
      annotate("text",
               x = 1, y = ((df[2, 2]) / 100) / 2 + 0.05,
               label = paste("Variance not attributable \n to sampling error: \n", round(m$sigma2[1] + m$sigma2[2], 3)), size = 3.5
      ) +
      
      # Level 1
      annotate("text",
               x = 2, y = (df[1, 2] / 2 + df[2, 2]) / 100,
               label = paste("Level 1:\n", round(df$Variance[3], 2), "%"), size = 3.5
      ) +
      
      # Level 2
      geom_text(
        x = 2, y = (df[5, 2] + (df[4, 2] / 2)) / 100,
        label = paste0('italic(I)[Level2]^2*":"~"', round(df[4, 2], 2), '"~"%"'),
        parse = TRUE, size = 3.5
      ) +
      
      # Level 3
      geom_text(
        x = 2, y = (df[5, 2] / 2) / 100,
        label = paste0('italic(I)[Level3]^2*":"~"', round(df[5, 2], 2), '"~"%"'),
        parse = TRUE, size = 3.5
      )
    
    returnlist <- list(
      results = df.res,
      totalI2 = totalI2,
      plot = g
    )
    class(returnlist) <- c("mlm.variance.distribution", "list")
    
    invisible(returnlist)
    
    returnlist
  }
  
  
  # Compute variance distribution
  i2 <- var.comp(model)
  
  # Build table data
  i2.res <- as.data.frame(i2$results)
  i2.res <- i2.res %>%
    dplyr::rename(V = "% of total variance")
  
  levels <- c("Level 1", "Level 2", "Level 3")
  i2.res <- cbind(levels, i2.res)
  
  one_row <- data.frame(
    levels = "Total:",
    V = "",
    I2 = i2$totalI2
  )
  
  i2.res <- rbind(i2.res, one_row)
  
  # Build kable table object
  kable_tbl <- knitr::kable(
    i2.res,
    booktabs = TRUE,
    col.names = c("σ", "% of Total V", "I²"),
    digits = 3,
    row.names = FALSE
  ) %>%
    kableExtra::row_spec(row = 0, align = "c") %>%
    kableExtra::kable_styling(full_width = TRUE) %>%
    kableExtra::row_spec(nrow(i2.res), hline_after = TRUE) %>%
    kableExtra::footnote(
      general_title = "*",
      general = "p<0.05; ** p<0.01; *** p<0.001",
      threeparttable = TRUE,
      footnote_as_chunk = TRUE
    )
  
  # Customise plot
  colouring <- c(
    "#A6CEE3", "#1F78B4", "#B2DF8A",
    "#FB9A99", "#CAB2D6", "#6A3D9A"
  )
  
  p_custom <- i2$plot +
    scale_color_manual(values = colouring) +
    scale_fill_manual(values = colouring) +
    theme_minimal(base_family = "Georgia") +
    theme(
      text = element_text(family = "Georgia"),
      axis.title.y = element_text(family = "Georgia"),
      axis.title.x = element_blank(),
      axis.text = element_text(family = "Georgia"),
      legend.text = element_text(family = "Georgia"),
      legend.title = element_text(family = "Georgia")
    )
  
  # Save table
  dir.create("tables", showWarnings = FALSE, recursive = TRUE)
  table_path <- file.path("tables", table_filename)
  kableExtra::save_kable(kable_tbl, table_path)
  
  # Save plot
  dir.create("figures", showWarnings = FALSE, recursive = TRUE)
  plot_path <- file.path("figures", plot_filename)
  ggplot2::ggsave(
    filename = plot_path,
    plot = p_custom,
    width = 9,
    height = 10,
    dpi = 300
  )
  
  # Return everything
  list(
    raw_table = i2.res,
    kable_table = kable_tbl,
    plot = p_custom,
    table_file = table_path,
    plot_file = plot_path
  )
}

make_orchard_pub_bias_plot <- function(model, i2_data, name) {
  colours <- brewer.pal(12, "Paired")
  
  light_green <- "#B2DF8A"
  dark_green <- "#33A02C"
  pink_red <- "#FB9A99"
  red <- "#E31A1C"
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
  I2 <- as.numeric(i2_data$raw_table$I2[[4]])
  orchard <- orchaRd::orchard_plot(model, mod = "1", group = "participant_id", xlab = expression("Hedge's " * italic(g)), cb = TRUE) +
    annotate(
      geom = "text", x = 0.80, y = 1, label = paste0("italic(I)^{2} == ", round(I2, 2), "*\"%\""),
      color = "black", parse = TRUE, size = 5
    ) +
    georgia_theme +
    scale_fill_manual(values = light_green) +
    scale_colour_manual(values = dark_green)
  
  fig_save_name <- paste0("figures/", name, ".png")
  
  png(fig_save_name, width = 3508, height = 2480, res = 300) # A4 landscape in inches
  par(
    par(mar = c(4, 4, 5, 2)),
    par(oma = c(2, 0, 2, 0)),
    par(family = "Georgia")
  )
  
  # if using showtext
  
  print(orchard)
  
  dev.off()
  
  return(fig_save_name)
}

## funnel plots
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

## egger's regression equivalent for multilevel
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
