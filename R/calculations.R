# =============================================================================
# Calculation engine for the AssuRance Shiny app.
#
# Shiny automatically sources every file in R/ before app.R runs. Everything
# here is plain R (no Shiny), so it can be tested on its own:
#   Rscript tests/test_calculations.R
#
# Contents
#   1. Mapping our inputs to bayesassurance's parameterisation
#   2. Simulated assurance  (bayesassurance::bayes_sim_unbalanced)  <- engine
#   3. Exact closed-form assurance (validation overlay, sensitivity, finder)
#   4. Frequentist power (two-sample t-test)
#   5. Helpers: sample-size finder, assurance ceiling, run-time estimate
# =============================================================================


# -----------------------------------------------------------------------------
# THE STATISTICAL MODEL
# -----------------------------------------------------------------------------
#   y_Ti ~ N(mu_T, sigma^2),  i = 1..n_t   (treatment arm)
#   y_Ci ~ N(mu_C, sigma^2),  i = 1..n_c   (control arm)
#   Delta = mu_T - mu_C  (the treatment effect);  sigma is known.
#
#   Design prior   (what we believe; used to SIMULATE the true effect):
#       Delta ~ N(design_mean, design_sd^2)
#   Analysis prior (what the final analysis will use):
#       Delta ~ N(analysis_mean, analysis_sd^2)
#
#   Success rule, with margin m >= 0 (the clinically meaningful difference;
#   m = 0 means "any effect"):
#       alt = "greater"   : P(Delta >  m | data) > 1 - alpha
#       alt = "less"      : P(Delta < -m | data) > 1 - alpha
#       alt = "two.sided" : P(Delta > 0 | data) > 1 - alpha/2  OR
#                           P(Delta < 0 | data) > 1 - alpha/2   (m forced to 0)
#   Assurance = P(success), averaging over the design prior AND the data.
# -----------------------------------------------------------------------------


# =============================================================================
# 1. MAPPING TO bayesassurance's PARAMETERISATION  (bayesassurance 0.1.0)
# =============================================================================
# bayes_sim / bayes_sim_unbalanced use the linear model
#     y = Xn %*% beta + e,          e ~ N(0, sigsq * Vn)
#     design prior    beta ~ N(mu_beta_d, sigsq * Vbeta_d)
#     analysis prior  beta ~ N(mu_beta_a, sigsq * solve(Vbeta_a_inv))
# and judge success on u' beta compared with a constant C.
#
# KEY GOTCHA: every prior (co)variance is expressed RELATIVE TO sigsq. A prior
# SD of tau on the outcome scale becomes a Vbeta entry of tau^2 / sigsq and a
# precision entry of sigsq / tau^2.
#
# Our mapping:
#   Xn   : left NULL, so the package builds gen_Xn(c(n_t, n_c)), a "cell
#          means" design. Rows 1..n_t are the treatment arm (column 1), the
#          remaining n_c rows are control (column 2). So beta = (mu_T, mu_C).
#   Vn   : NULL -> identity (independent observations).
#   sigsq = sigma^2.
#   u = c(1, -1)  ->  u' beta = mu_T - mu_C = Delta.
#   C    : +m for "greater", -m for "less", 0 for "two.sided" (see rule above).
#
#   Design prior on beta = (mu_T, mu_C):
#       mu_beta_d = c(design_mean, 0)
#       Vbeta_d   = diag(c(design_sd^2 / sigma^2, 0))
#     i.e. mu_C is fixed at 0 and mu_T carries the effect, so
#     Delta ~ N(design_mean, design_sd^2) exactly. Fixing mu_C loses nothing
#     because the analysis (below) is invariant to the common level.
#
#   Analysis prior on beta = (mu_T, mu_C):
#       mu_beta_a   = c(analysis_mean, 0)
#       Vbeta_a_inv = (sigma^2 / analysis_sd^2) * [ 1 -1 ; -1 1 ]
#     This precision matrix puts N(analysis_mean, analysis_sd^2) on Delta and
#     a FLAT prior on the common level, because
#       (beta - mu)' [1 -1; -1 1] (beta - mu) = (Delta - analysis_mean)^2.
#     It is singular, but the package only uses Vbeta_a_inv + t(Xn) %*% Xn,
#     which is positive definite.
#
# Why bayes_sim_unbalanced() rather than bayes_sim()?
#   * It supports unequal arm sizes (allocation ratio != 1).
#   * Called with scalar n1/n2 it still returns a numeric table (bayes_sim
#     returns a rounded character string for a scalar n), which lets the app
#     evaluate one sample size at a time and show real progress.
#   * With n1 == n2 it performs exactly the same computation as bayes_sim.
#   Do NOT pass your own Xn: the package would reuse that one matrix for
#   every sample size.
#
# Why not the package's closed form assurance_nd_na()? It is a one-sample
# formula whose analysis prior mean is tied to the null value, and it
# disagrees with the exact posterior-probability assurance whenever the
# analysis prior is informative. Section 3 contains a correct closed form,
# which the tests show agrees with the simulation.
# =============================================================================
package_args <- function(design_mean, design_sd, analysis_mean, analysis_sd,
                         sigma, alt, margin = 0) {
  sigsq <- sigma^2
  list(
    u           = c(1, -1),
    C           = switch(alt, greater = margin, less = -margin, two.sided = 0),
    sigsq       = sigsq,
    mu_beta_d   = c(design_mean, 0),
    Vbeta_d     = diag(c(design_sd^2 / sigsq, 0)),
    mu_beta_a   = c(analysis_mean, 0),
    Vbeta_a_inv = (sigsq / analysis_sd^2) * matrix(c(1, -1, -1, 1), nrow = 2),
    alt         = alt
  )
}


# =============================================================================
# 2. SIMULATED ASSURANCE (bayesassurance engine)
# =============================================================================
# n_t, n_c : equal-length vectors of arm sizes.
# progress : optional function(i, total) called after each sample size.
# Returns a numeric vector of assurance estimates (one per sample size).
sim_assurance <- function(n_t, n_c, design_mean, design_sd,
                          analysis_mean, analysis_sd, sigma, alpha, alt,
                          margin = 0, mc_iter = 2000, progress = NULL) {
  a <- package_args(design_mean, design_sd, analysis_mean, analysis_sd,
                    sigma, alt, margin)

  # The package draws a pbapply progress bar and print()s every (n1, n2)
  # pair to the console; silence both.
  old_pb <- pbapply::pboptions(type = "none")
  on.exit(pbapply::pboptions(old_pb), add = TRUE)

  out <- numeric(length(n_t))
  for (i in seq_along(n_t)) {
    res <- NULL
    utils::capture.output(
      res <- suppressWarnings(bayesassurance::bayes_sim_unbalanced(
        n1 = n_t[i], n2 = n_c[i], repeats = 1,
        u = a$u, C = a$C, Xn = NULL, Vn = NULL,
        Vbeta_d = a$Vbeta_d, Vbeta_a_inv = a$Vbeta_a_inv, sigsq = a$sigsq,
        mu_beta_d = a$mu_beta_d, mu_beta_a = a$mu_beta_a,
        alt = a$alt, alpha = alpha, mc_iter = mc_iter,
        surface_plot = FALSE))
    )
    # assurance_table columns: n1, n2, assurance
    out[i] <- res$assurance_table[1, 3]
    if (!is.null(progress)) progress(i, length(n_t))
  }
  out
}

# Monte Carlo standard error of a simulated probability.
mc_se <- function(p, mc_iter) sqrt(p * (1 - p) / mc_iter)


# =============================================================================
# 3. EXACT CLOSED-FORM ASSURANCE
# =============================================================================
# With known sigma everything is normal, so assurance has a closed form.
# The observed difference in means d has variance se2 = sigma^2 (1/n_t + 1/n_c).
# Under the analysis prior the posterior for Delta is normal with
#     post_var  = 1 / (1/analysis_sd^2 + 1/se2)
#     post_mean = post_var * (analysis_mean/analysis_sd^2 + d/se2)
# "P(Delta > C | d) > 1 - a"  <=>  post_mean > C + z_{1-a} sqrt(post_var)
#                             <=>  d > se2 * ((C + z sqrt(post_var))/post_var
#                                             - analysis_mean/analysis_sd^2)
# (and symmetrically for "less"). So success is simply d above an upper
# cut-off and/or below a lower cut-off. Everything below builds on this.
success_cutoffs <- function(n_t, n_c, analysis_mean, analysis_sd, sigma,
                            alpha, alt, margin = 0) {
  se2 <- sigma^2 * (1 / n_t + 1 / n_c)
  w   <- 1 / analysis_sd^2
  pv  <- 1 / (w + 1 / se2)
  sp  <- sqrt(pv)
  up_cut  <- function(C, z) se2 * ((C + z * sp) / pv - analysis_mean * w)
  low_cut <- function(C, z) se2 * ((C - z * sp) / pv - analysis_mean * w)
  inf <- rep(Inf, length(se2))
  switch(alt,
    greater   = list(se2 = se2, upper = up_cut(margin, qnorm(1 - alpha)),
                     lower = -inf),
    less      = list(se2 = se2, upper = inf,
                     lower = low_cut(-margin, qnorm(1 - alpha))),
    two.sided = list(se2 = se2, upper = up_cut(0, qnorm(1 - alpha / 2)),
                     lower = low_cut(0, qnorm(1 - alpha / 2))))
}

# P(success) when d ~ N(mean_d, sd_d^2), split into the part coming from the
# upper cut-off (a "positive" finding) and the lower cut-off ("negative").
success_prob <- function(cuts, mean_d, sd_d) {
  pos <- pnorm((mean_d - cuts$upper) / sd_d)
  neg <- pnorm((cuts$lower - mean_d) / sd_d)
  list(total = pos + neg, positive = pos, negative = neg)
}

# Exact assurance: marginally over the design prior, d ~ N(design_mean,
# design_sd^2 + se2). Vectorised over n_t / n_c.
exact_assurance <- function(n_t, n_c, design_mean, design_sd,
                            analysis_mean, analysis_sd, sigma, alpha, alt,
                            margin = 0, parts = FALSE) {
  cuts <- success_cutoffs(n_t, n_c, analysis_mean, analysis_sd, sigma,
                          alpha, alt, margin)
  p <- success_prob(cuts, design_mean, sqrt(design_sd^2 + cuts$se2))
  if (parts) p else p$total
}

# Probability that the Bayesian analysis declares success if the true effect
# were exactly `delta` (a "power curve" for the Bayesian decision rule).
# Assurance is this curve averaged over the design prior.
bayes_success_given_delta <- function(delta, n_t, n_c, analysis_mean,
                                      analysis_sd, sigma, alpha, alt,
                                      margin = 0) {
  cuts <- success_cutoffs(n_t, n_c, analysis_mean, analysis_sd, sigma,
                          alpha, alt, margin)
  success_prob(cuts, delta, sqrt(cuts$se2))$total
}


# =============================================================================
# 4. FREQUENTIST POWER (two-sample t-test, pooled SD)
# =============================================================================
# The design prior MEAN is treated as the exact, known true effect.
# This is the standard noncentral-t power formula, identical to
# stats::power.t.test(type = "two.sample") when n_t == n_c and margin == 0
# (checked in the tests), extended to unequal arms and to a margin:
#   greater   : reject H0: Delta <= m    if t > t_{1-alpha}
#   less      : reject H0: Delta >= -m   if t < -t_{1-alpha}
#   two.sided : reject H0: Delta == 0    if |t| > t_{1-alpha/2}; both tails
#               count (power.t.test's strict = TRUE), matching the Bayesian
#               two-sided rule.
freq_power <- function(n_t, n_c, delta, sigma, alpha, alt, margin = 0) {
  df <- n_t + n_c - 2
  se <- sigma * sqrt(1 / n_t + 1 / n_c)
  switch(alt,
    greater = {
      q <- qt(1 - alpha, df)
      pt(q, df, ncp = (delta - margin) / se, lower.tail = FALSE)
    },
    less = {
      q <- qt(1 - alpha, df)
      pt(-q, df, ncp = (delta + margin) / se)
    },
    two.sided = {
      q <- qt(1 - alpha / 2, df)
      pt(q, df, ncp = delta / se, lower.tail = FALSE) +
        pt(-q, df, ncp = delta / se)
    })
}


# =============================================================================
# 5. HELPERS
# =============================================================================

# Treatment-arm size for a given control-arm size and allocation ratio
# (treatment : control). Ratio 1 gives equal arms.
treatment_n <- function(n_c, ratio) pmax(2, round(ratio * n_c))

# Number to enrol so that `n` remain after a dropout proportion `dropout`.
enrolled_n <- function(n, dropout) ceiling(n / (1 - dropout) - 1e-9)

# Smallest control-arm size (2..n_limit) at which `fun(n_t, n_c)` reaches
# `target`. Returns NA if never reached.
smallest_n <- function(fun, ratio, target, n_limit = 20000) {
  n_c <- 2:n_limit
  vals <- fun(treatment_n(n_c, ratio), n_c)
  hit <- which(vals >= target)
  if (length(hit) == 0) NA_integer_ else n_c[hit[1]]
}

# Assurance ceiling: the limit of assurance as the sample size grows without
# bound. Once the data swamp the analysis prior, the trial succeeds exactly
# when the true effect lies beyond the success threshold, so the ceiling is
# the design-prior probability of that event.
assurance_ceiling <- function(design_mean, design_sd, alt, margin = 0) {
  switch(alt,
    greater   = 1 - pnorm((margin - design_mean) / design_sd),
    less      = pnorm((-margin - design_mean) / design_sd),
    two.sided = 1)
}

# Rough run-time estimate (seconds) for the simulation. Each simulated trial
# costs about (n_t + n_c)^2 operations in the package's R loop.
# Calibrated on a typical laptop; hosted servers can be 1.5-3x slower.
estimate_seconds <- function(n_t, n_c, mc_iter) {
  N <- n_t + n_c
  mc_iter * sum(4.5e-5 + 4.5e-9 * N^2)
}
