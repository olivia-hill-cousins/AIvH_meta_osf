make_profile_plot <- function(
  mod_article_pid,
  mod_study_pid,
  mod_pid_efid,
  file,
  fig_title = "Profile Likelihood Plots"
) {
  png(file, width = 2480, height = 3508, res = 300)

  par(
    mfrow = c(3, 2),
    oma = c(1, 2, 5, 0.5),
    mar = c(4, 5, 2.5, 1.5),
    family = "Georgia"
  )

  ## -------------------------
  ## Row 1: Article ID / PIDs
  ## -------------------------
  profile(mod_article_pid, sigma2 = 1, ylab = "Restricted logLik", main = "")
  title(bquote(bolditalic(sigma["(" * .("Article ID") * ")"]^2)), line = 0.5, cex.main = 1.25)
  mtext("Article ID / PIDs", side = 2, line = 5, cex = 0.8, font = 2, family = "Georgia")

  profile(mod_article_pid, sigma2 = 2, ylab = "Restricted logLik", main = "")
  title(bquote(bolditalic(sigma["(" * .("Article ID / PIDs") * ")"]^2)), line = 0.5, cex.main = 1.25)

  mtext("Figure X",
    side = 3, line = 4, at = -11, adj = 1,
    cex = 1.2, font = 2, family = "Georgia"
  )
  mtext(fig_title,
    side = 3, line = 2, at = 0, adj = 0.75,
    cex = 1, font = 3, family = "Georgia"
  )

  ## -------------------------
  ## Row 2: Study ID / PIDs
  ## -------------------------
  profile(mod_study_pid, sigma2 = 1, ylab = "Restricted logLik", main = "")
  title(bquote(bolditalic(sigma["(" * .("Study ID") * ")"]^2)), line = 0.5, cex.main = 1.25)
  mtext("Study ID / PIDs", side = 2, line = 5, cex = 0.8, font = 2, family = "Georgia")

  profile(mod_study_pid, sigma2 = 2, ylab = "Restricted logLik", main = "")
  title(bquote(bolditalic(sigma["(" * .("Study ID / PIDs") * ")"]^2)), line = 0.5, cex.main = 1.25)

  ## -------------------------
  ## Row 3: PIDs / EFIDs
  ## -------------------------
  profile(mod_pid_efid, sigma2 = 1, ylab = "Restricted logLik", main = "")
  title(bquote(bolditalic(sigma["(" * .("PIDs") * ")"]^2)), line = 0.5, cex.main = 1.25)
  mtext("PIDs / EFIDs", side = 2, line = 5, cex = 0.8, font = 2, family = "Georgia")

  profile(mod_pid_efid, sigma2 = 2, ylab = "Restricted logLik", main = "")
  title(bquote(bolditalic(sigma["(" * .("PIDs / EFIDs") * ")"]^2)), line = 0.5, cex.main = 1.25)

  dev.off()

  return(file)
}
