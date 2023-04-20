prevdebiasr_installed <- require("prevdebiasr")
if (!prevdebiasr_installed) {
  devtools_installed <- require("devtools")
  if (!devtools_installed) {
    install.packages("devtools")
  }
  devtools::install_github("alan-turing-institute/prevdebiasr@main")
}
control <- get_control_parameters()

#############################################################
# Source all functions, get global parameters
fn_source_out <- sapply(list.files("scripts/functions", full.names = T), source)

##############################################
# Import weekly Pillar 2 and REACT
force_reimport_data <- FALSE
data_file <- file.path("output", "d_age_weekly.RDS")
if (!file.exists(data_file) | force_reimport_data) {
  data_import <- import_weekly_age_stratified_data(control)
  d_fine <- data_import$d_fine
  d_coarse <- data_import$d_coarse
  dir.create("output", showWarnings = FALSE)
  saveRDS(d_fine, file = file.path("output", "d_fine.RDS"))
  saveRDS(d_coarse, file = file.path("output", "d_coarse.RDS"))
} else {
  d_fine <- readRDS(file = file.path("output", "d_fine.RDS"))
  d_coarse <- readRDS(file = file.path("output", "d_coarse.RDS"))
}

utla_unique <- na.omit(unique(d_fine$location))
age_grp_unique <- unique(d_fine$age_bin)




all_results <- data.frame()
for (utla_to_debias in utla_unique) {
  print(utla_to_debias)
  for (age_grp_to_debias in age_grp_unique) {
    phe_region_of_utla_to_debias <- d_fine[match(utla_to_debias, d_fine$location), "phe_region"]
    d_coarse_curr <- d_coarse[d_coarse$location == phe_region_of_utla_to_debias & d_coarse$age_bin == age_grp_to_debias, ]
    d_fine_curr <- d_fine[d_fine$location == utla_to_debias & d_fine$age_bin == age_grp_to_debias, ]

    ##################################
    # Estimate ascertainment parameter, delta
    delta_out <- prevdebiasr::specify_delta_prior(test_df = d_coarse_curr,
                                                  control = control,
                                                  imperfect = FALSE)

    ##################################
    # Add delta to d_fine_curr
    d_fine_curr[, c("delta_prior_mean", "delta_prior_sd")] <- delta_out

    ##################################
    # Calculate prevalence using delta and Pillar 1+2 at local level
    prev_out <- prevdebiasr::local_prevalence(
      test_df = d_fine_curr,
      control = get_control_parameters(),
      imperfect = FALSE,
      type = "PCR_positive"
    )

    ##################################
    # Calculate logit moments
    logit_moments_out <- as.data.frame(t(apply(X = prev_out$log_lik,
                                               MARGIN = 1,
                                               FUN = calculate_logit_moments,
                                               values = control$I_seq / d_fine_curr$M[1])))
    names(logit_moments_out) <- c("mean_logit_pi", "sd_logit_pi")
    results_curr <- cbind(d_fine_curr, logit_moments_out)
    all_results <- rbind(all_results, results_curr)
  }
}


########################################################################
# Plot delta and prevalence for a single UTLA
utla_to_plot <- utla_unique[1]
res_plot <- all_results[all_results$location == utla_to_plot, ]
phe_region_to_plot <- res_plot$phe_region[1]
res_plot <- res_plot[order(res_plot$age_bin, res_plot$mid_week), ]
n_week <- length(unique(res_plot$mid_week))
n_age <- length(age_grp_unique)
delta_mat_mn <- matrix(res_plot$delta_prior_mean, n_week, n_age)
delta_mat_sd <- matrix(res_plot$delta_prior_sd, n_week, n_age)
logit_pi_mat_mn <- matrix(res_plot$mean_logit_pi, n_week, n_age)
logit_pi_mat_sd <- matrix(res_plot$sd_logit_pi, n_week, n_age)

par(mfrow = c(2, 1), mar = c(1, 2, 1, 2), oma = c(5, 1, 1, 1))

##################################
# Plot ascertainment parameter, delta
n_week <- nrow(delta_out)
plot(x = 1:n_week,
     y = delta_out$delta_prior_mean,
     ty = "n",
     xaxt = "n",
     xlab = "",
     ylab = "Ascertainment parameter, delta",
     ylim = c(-1, 4),
     main = "Ascertainment parameter, delta")
legend(x = "topleft",
       legend = age_grp_unique,
       col = 1:n_age,
       lty = 1,
       lwd = 3,
       cex = .6,
       title = phe_region_to_plot)
for (age_curr in 1:n_age) {
  for (i in c(-1, 0, 1)) {
    delta_curr <- delta_mat_mn[, age_curr] + 2 * i * delta_mat_sd[, age_curr]
    lines(x = 1:n_week,
          y = delta_curr,
          col = age_curr,
          lwd = c(1, 3, 1)[i + 2],
          lty = c(3, 1, 3)[i + 2])
  }
}

##################################
# Plot prevalence proportion
plot(x = 1:n_week,
     y = boot::inv.logit(logit_moments_out$mean),
     ty = "n",
     xaxt = "n",
     xlab = "",
     ylab = "Prevalence proportion",
     ylim = c(0, .1),
     main = "Estimated cross-sectional prevalence proportion")
legend(x = "topleft",
       legend = age_grp_unique,
       col = 1:n_age,
       lty = 1,
       lwd = 3,
       cex = .6,
       title = utla_to_plot)
for (age_curr in 1:n_age) {
  for (i in c(-1, 0, 1)) {
    pi_curr <- boot::inv.logit(logit_pi_mat_mn[, age_curr] +
                                   2 * i * logit_pi_mat_sd[, age_curr])
    print(pi_curr)
    lines(x = 1:n_week,
          y = pi_curr,
          col = age_curr,
          lwd = c(1, 3, 1)[i + 2],
          lty = c(3, 1, 3)[i + 2])
  }
}
axis(side = 1,
     at = 1:n_week,
     labels = d_fine_curr$mid_week,
     las = 2)


