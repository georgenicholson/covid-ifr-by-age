##################################
# Function to transform to prevalence proportion on logit scale and
# calculate moment-matched Gaussian approximation to likelihood as function of logit(prevalence)
calculate_logit_moments <- function (x, values) {
  if (any(values < 0)) {
    stop("Proportions must be non-negative")
  }
  if (any(x > -Inf & values > 1)) {
    stop("Log likelihood must be -Inf when prevalence I exceeds pop size M")
  }
  logit_values <- boot::logit(pmin(values, 1))
  norm_prob <- exp(x) / sum(exp(x))
  norm_prob_dezeroed <- norm_prob
  norm_prob_dezeroed[1] <- 0
  norm_prob_dezeroed[2] <- norm_prob[1] + norm_prob[2]
  mom_match1 <- sum(norm_prob_dezeroed * logit_values, na.rm = TRUE)
  mom_match2 <- sum(norm_prob_dezeroed * (logit_values^2),
                    na.rm = TRUE)
  c(mean = mom_match1, sd = sqrt(mom_match2 - (mom_match1^2)))
}

