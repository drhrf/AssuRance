# =============================================================================
# Core calculation engine for the AssuRance Shiny app.
#
# Shiny automatically sources every file in R/ before app.R runs. Everything
# here is plain R (no Shiny), so it can be tested on its own:
#   Rscript tests/test_calculations.R
#
# This file is outcome-agnostic. It works with an effect `theta` on a
# "working scale" on which the estimate is approximately normal:
#   continuous outcome          theta = difference in means
#   ANCOVA                      theta = baseline-adjusted difference in means
#   binary, odds ratio          theta = log odds ratio
#   binary, risk ratio          theta = log risk ratio
#   binary, risk difference     theta = risk difference (proportion scale)
#   time to event               theta = log hazard ratio
# R/models.R supplies, for each outcome type, the variance of the estimate
# and the simulators; this file turns those into assurance, power, sample
# sizes and so on.
#
# Contents
#   1. The Bayesian success rule and its cut-offs
#   2. Exact assurance (closed form, or numerical integration)
#   3. The bayesassurance engine for continuous outcomes
#   4. Frequentist power
#   5. Helpers: sample-size finder, ceiling, chunked simulation, RNG
# =============================================================================


# -----------------------------------------------------------------------------
# THE BAYESIAN SET-UP (all outcome types)
# -----------------------------------------------------------------------------
#   Design prior   (what we believe; used to SIMULATE the true effect):
#       theta ~ N(m_d, s_d^2)
#   Analysis prior (what the final analysis will use):
#       theta ~ N(m_a, s_a^2)
#   Data summarised by the estimate  d ~ N(theta, v),  where v depends on the
#   sample sizes (and, for binary and survival outcomes, on theta itself).
#
#   Success rule, with threshold C on the working scale:
#       alt = "greater"   : P(theta > C | data) > 1 - alpha
#       alt = "less"      : P(theta < C | data) > 1 - alpha
#       alt = "two.sided" : P(theta > 0 | data) > 1 - alpha/2  OR
#                           P(theta < 0 | data) > 1 - alpha/2   (C = 0)
#   C = 0 is ordinary superiority. C beyond 0 in the beneficial direction
#   demands a clinically meaningful effect; C on the harmful side gives a
#   non-inferiority design (e.g. "less" with C = log(1.3) for a hazard ratio).
#
#   Assurance = P(success), averaging over the design prior AND the data.
# -----------------------------------------------------------------------------


# =============================================================================
# 1. SUCCESS CUT-OFFS
# =============================================================================
# Under the analysis prior the posterior for theta is normal with
#     post_var  = 1 / (1/s_a^2 + 1/v)
#     post_mean = post_var * (m_a/s_a^2 + d/v)
# "P(theta > C | d) > 1 - a"  <=>  post_mean > C + z_{1-a} sqrt(post_var)
#                             <=>  d > v * ((C + z sqrt(post_var))/post_var - m_a/s_a^2)
# (and symmetrically for "less"). So success is simply d above an upper
# cut-off and/or below a lower cut-off. Vectorised over v.
cutoffs_from_se2 <- function(se2, analysis_mean, analysis_sd, alpha, alt,
                             C = 0) {
  w   <- 1 / analysis_sd^2
  pv  <- 1 / (w + 1 / se2)
  sp  <- sqrt(pv)
  up_cut  <- function(C, z) se2 * ((C + z * sp) / pv - analysis_mean * w)
  low_cut <- function(C, z) se2 * ((C - z * sp) / pv - analysis_mean * w)
  inf <- rep(Inf, length(se2))
  switch(alt,
    greater   = list(se2 = se2, upper = up_cut(C, qnorm(1 - alpha)),
                     lower = -inf),
    less      = list(se2 = se2, upper = inf,
                     lower = low_cut(C, qnorm(1 - alpha))),
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

# Posterior-based success for simulated trials (vectors of estimates `est`
# with variances `v`). Used by the built-in simulators in R/models.R.
bayes_decision <- function(est, v, analysis_mean, analysis_sd, alpha, alt,
                           C = 0) {
  w  <- 1 / analysis_sd^2
  pv <- 1 / (w + 1 / v)
  pm <- pv * (analysis_mean * w + est / v)
  switch(alt,
    greater   = (pm - C) / sqrt(pv) > qnorm(1 - alpha),
    less      = (pm - C) / sqrt(pv) < -qnorm(1 - alpha),
    two.sided = abs(pm) / sqrt(pv) > qnorm(1 - alpha / 2))
}


# =============================================================================
# 2. EXACT ASSURANCE
# =============================================================================
# (a) Constant variance v (continuous outcomes): marginally over the design
#     prior, d ~ N(m_d, s_d^2 + v), which gives a closed form.
assurance_const_var <- function(se2, design_mean, design_sd, analysis_mean,
                                analysis_sd, alpha, alt, C = 0,
                                parts = FALSE) {
  cuts <- cutoffs_from_se2(se2, analysis_mean, analysis_sd, alpha, alt, C)
  p <- success_prob(cuts, design_mean, sqrt(design_sd^2 + cuts$se2))
  if (parts) p else p$total
}

# (b) Variance that depends on the true effect (binary, survival): average
#     P(success | theta) over the design prior by numerical integration on a
#     fine grid (1,601 points spanning +/- 8 SD, trapezoid weights).
#     se2_fun(theta, n_t, n_c) must be vectorised over equal-length vectors.
QUAD_Z <- seq(-8, 8, length.out = 1601)
QUAD_W <- local({ w <- dnorm(QUAD_Z); w[c(1, length(w))] <- w[c(1, length(w))] / 2; w / sum(w) })

assurance_quad <- function(se2_fun, n_t, n_c, design_mean, design_sd,
                           analysis_mean, analysis_sd, alpha, alt, C = 0,
                           parts = FALSE) {
  L <- length(n_c); K <- length(QUAD_Z)
  theta <- design_mean + design_sd * QUAD_Z
  th  <- rep(theta, times = L)
  nt  <- rep(n_t, each = K); nc <- rep(n_c, each = K)
  se2 <- se2_fun(th, nt, nc)
  cuts <- cutoffs_from_se2(se2, analysis_mean, analysis_sd, alpha, alt, C)
  p <- success_prob(cuts, th, sqrt(se2))
  agg <- function(x) as.vector(QUAD_W %*% matrix(x, nrow = K))
  out <- list(total = agg(p$total), positive = agg(p$positive),
              negative = agg(p$negative))
  if (parts) out else out$total
}

# Probability of success if the true effect were exactly theta
# (a "power curve" for the Bayesian decision rule).
cond_success <- function(theta, se2, analysis_mean, analysis_sd, alpha, alt,
                         C = 0) {
  cuts <- cutoffs_from_se2(se2, analysis_mean, analysis_sd, alpha, alt, C)
  success_prob(cuts, theta, sqrt(se2))$total
}

# Convenience wrappers for the plain continuous case (known SD sigma).
exact_assurance <- function(n_t, n_c, design_mean, design_sd, analysis_mean,
                            analysis_sd, sigma, alpha, alt, C = 0,
                            parts = FALSE) {
  assurance_const_var(sigma^2 * (1 / n_t + 1 / n_c), design_mean, design_sd,
                      analysis_mean, analysis_sd, alpha, alt, C, parts)
}

bayes_success_given_delta <- function(delta, n_t, n_c, analysis_mean,
                                      analysis_sd, sigma, alpha, alt, C = 0) {
  cond_success(delta, sigma^2 * (1 / n_t + 1 / n_c), analysis_mean,
               analysis_sd, alpha, alt, C)
}


# =============================================================================
# 3. THE bayesassurance ENGINE (continuous outcomes)
# =============================================================================
# bayes_sim / bayes_sim_unbalanced (bayesassurance 0.1.0) use the linear model
#     y = Xn %*% beta + e,          e ~ N(0, sigsq * Vn)
#     design prior    beta ~ N(mu_beta_d, sigsq * Vbeta_d)
#     analysis prior  beta ~ N(mu_beta_a, sigsq * solve(Vbeta_a_inv))
# and judge success on u' beta compared with a constant C, exactly as in the
# rule above.
#
# KEY GOTCHA: every prior (co)variance is expressed RELATIVE TO sigsq. A prior
# SD of tau on the outcome scale becomes a Vbeta entry of tau^2 / sigsq and a
# precision entry of sigsq / tau^2.
#
# Mapping for the two-arm comparison:
#   Xn   : "cell means" design gen_Xn(c(n_t, n_c)): rows 1..n_t are the
#          treatment arm (column 1), the remaining n_c rows control (column 2),
#          so beta = (mu_T, mu_C). For ANCOVA a third column holds the
#          standardised baseline value x and beta = (mu_T, mu_C, gamma).
#   Vn   : NULL -> identity (independent observations).
#   sigsq = sigma^2  (ANCOVA: the residual variance sigma^2 (1 - rho^2)).
#   u = c(1, -1[, 0])  ->  u' beta = mu_T - mu_C = Delta.
#   C    : the success threshold (0 for two-sided).
#
#   Design prior:  mu_beta_d = c(m_d, 0[, gamma]);
#                  Vbeta_d   = diag(c(s_d^2 / sigsq, 0[, 0]))
#     mu_C is fixed at 0 and mu_T carries the effect, so Delta ~ N(m_d, s_d^2)
#     exactly. For ANCOVA the true slope is gamma = rho * sigma (outcome SD
#     sigma, correlation rho with the standardised baseline).
#   Analysis prior: mu_beta_a = c(m_a, 0[, 0]);
#                   Vbeta_a_inv = (sigsq / s_a^2) * [1 -1 (0); -1 1 (0); (0 0 0)]
#     which puts N(m_a, s_a^2) on Delta and FLAT priors on the common level
#     (and on the baseline slope). It is singular, but the package only uses
#     Vbeta_a_inv + t(Xn) %*% Xn, which is positive definite.
#
# Package quirks worked around here:
#   * With a user-supplied Xn the package reuses that single matrix for every
#     sample size, so we call it once per (n_t, n_c) pair and build Xn
#     ourselves only for ANCOVA.
#   * For a scalar n, bayes_sim returns a rounded string; bayes_sim_unbalanced
#     returns a numeric table, so we use the latter throughout.
#   * It draws a progress bar and print()s every (n1, n2); both are silenced.
#   * Its analysis step reads only the first p rows of Vn, which is correct
#     only when Vn is the identity; we always leave Vn = NULL.
package_args <- function(design_mean, design_sd, analysis_mean, analysis_sd,
                         sigsq, alt, C = 0, gamma = NULL) {
  k <- if (is.null(gamma)) 2 else 3
  pad <- function(x) c(x, rep(0, k - length(x)))
  prec <- matrix(0, k, k); prec[1:2, 1:2] <- matrix(c(1, -1, -1, 1), 2)
  list(
    u           = pad(c(1, -1)),
    C           = if (alt == "two.sided") 0 else C,
    sigsq       = sigsq,
    mu_beta_d   = if (is.null(gamma)) c(design_mean, 0) else c(design_mean, 0, gamma),
    Vbeta_d     = diag(pad(design_sd^2 / sigsq), k),
    mu_beta_a   = pad(c(analysis_mean, 0)),
    Vbeta_a_inv = (sigsq / analysis_sd^2) * prec,
    alt         = alt
  )
}

# Simulated assurance with bayesassurance::bayes_sim_unbalanced.
# n_t, n_c : equal-length vectors of arm sizes.
# ancova_rho : NULL for a plain comparison of means; otherwise the correlation
#   between baseline and outcome, and a baseline covariate is simulated.
sim_assurance <- function(n_t, n_c, design_mean, design_sd,
                          analysis_mean, analysis_sd, sigma, alpha, alt,
                          C = 0, mc_iter = 2000, ancova_rho = NULL) {
  ancova <- !is.null(ancova_rho)
  sigsq <- if (ancova) sigma^2 * (1 - ancova_rho^2) else sigma^2
  gamma <- if (ancova) ancova_rho * sigma else NULL
  a <- package_args(design_mean, design_sd, analysis_mean, analysis_sd,
                    sigsq, alt, C, gamma)

  old_pb <- pbapply::pboptions(type = "none")
  on.exit(pbapply::pboptions(old_pb), add = TRUE)

  out <- numeric(length(n_t))
  for (i in seq_along(n_t)) {
    Xn <- NULL
    if (ancova) {
      # standardised baseline values, drawn from the run's random stream
      Xn <- cbind(bayesassurance::gen_Xn(c(n_t[i], n_c[i])),
                  stats::rnorm(n_t[i] + n_c[i]))
    }
    res <- NULL
    utils::capture.output(
      res <- suppressWarnings(bayesassurance::bayes_sim_unbalanced(
        n1 = n_t[i], n2 = n_c[i], repeats = 1,
        u = a$u, C = a$C, Xn = Xn, Vn = NULL,
        Vbeta_d = a$Vbeta_d, Vbeta_a_inv = a$Vbeta_a_inv, sigsq = a$sigsq,
        mu_beta_d = a$mu_beta_d, mu_beta_a = a$mu_beta_a,
        alt = a$alt, alpha = alpha, mc_iter = mc_iter,
        surface_plot = FALSE))
    )
    # assurance_table columns: n1, n2, assurance
    out[i] <- res$assurance_table[1, 3]
  }
  out
}

# Monte Carlo standard error of a simulated probability.
mc_se <- function(p, mc_iter) sqrt(p * (1 - p) / mc_iter)


# =============================================================================
# 4. FREQUENTIST POWER
# =============================================================================
# The design prior's central value is treated as the exact, known effect.
#
# (a) t-test (continuous outcomes): noncentral t, identical to
#     stats::power.t.test(type = "two.sample") when n_t == n_c and C == 0,
#     extended to unequal arms, a threshold C and a variance inflation factor
#     `vif` / reduced degrees of freedom `df_loss` (used for ANCOVA).
#   greater   : reject H0: Delta <= C  if t > t_{1-alpha}
#   less      : reject H0: Delta >= C  if t < -t_{1-alpha}
#   two.sided : reject H0: Delta == 0  if |t| > t_{1-alpha/2}; both tails count
#               (power.t.test's strict = TRUE), matching the Bayesian rule.
freq_power <- function(n_t, n_c, delta, sigma, alpha, alt, C = 0,
                       vif = 1, df_loss = 0) {
  df <- n_t + n_c - 2 - df_loss
  se <- sigma * sqrt((1 / n_t + 1 / n_c) * vif)
  switch(alt,
    greater = pt(qt(1 - alpha, df), df, ncp = (delta - C) / se,
                 lower.tail = FALSE),
    less    = pt(-qt(1 - alpha, df), df, ncp = (delta - C) / se),
    two.sided = {
      q <- qt(1 - alpha / 2, df)
      pt(q, df, ncp = delta / se, lower.tail = FALSE) +
        pt(-q, df, ncp = delta / se)
    })
}

# (b) Wald z-test on the working scale (binary and survival outcomes), with
#     the standard error evaluated at the assumed true effect.
freq_power_z <- function(theta, se, alpha, alt, C = 0) {
  switch(alt,
    greater   = pnorm((theta - C) / se - qnorm(1 - alpha)),
    less      = pnorm((C - theta) / se - qnorm(1 - alpha)),
    two.sided = pnorm(theta / se - qnorm(1 - alpha / 2)) +
                pnorm(-theta / se - qnorm(1 - alpha / 2)))
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
# `target`. Returns NA if never reached. Searches a log-spaced grid first and
# then every integer in the bracket where the target is first reached, so it
# is fast even when `fun` uses numerical integration.
smallest_n <- function(fun, ratio, target, n_limit = 20000) {
  grid <- unique(round(exp(seq(log(2), log(n_limit), length.out = 300))))
  vals <- fun(treatment_n(grid, ratio), grid)
  hit <- which(vals >= target)
  if (length(hit) == 0) return(NA_integer_)
  j <- hit[1]
  if (j == 1) return(as.integer(grid[1]))
  cand <- (grid[j - 1] + 1):grid[j]
  v2 <- fun(treatment_n(cand, ratio), cand)
  as.integer(cand[which(v2 >= target)[1]])
}

# Assurance ceiling: the limit of assurance as the sample size grows without
# bound. Once the data swamp the analysis prior, the trial succeeds exactly
# when the true effect lies beyond the success threshold, so the ceiling is
# the design-prior probability of that event.
assurance_ceiling <- function(design_mean, design_sd, alt, C = 0) {
  switch(alt,
    greater   = 1 - pnorm((C - design_mean) / design_sd),
    less      = pnorm((C - design_mean) / design_sd),
    two.sided = 1)
}

# Rough run-time (seconds per simulated trial) for the bayesassurance engine.
# Each simulated trial costs about (n_t + n_c)^2 operations in the package's R
# loop. Calibrated on a typical laptop; hosted servers can be 1.5-3x slower.
package_seconds_per_iter <- function(n_t, n_c) {
  N <- n_t + n_c
  4.5e-5 + 4.5e-9 * N^2
}

# Split a simulation into small chunks so the app can stay responsive (and be
# stopped) while it runs. Each chunk simulates `iters` trials at sample size
# number `i`; chunks for the same i add up to mc_iter, so combining them
# (sum of successes / mc_iter) gives exactly the same estimator as one big run.
# per_iter  : estimated seconds per simulated trial, one per sample size.
# max_chunk : optional cap on trials per chunk (limits memory), one per size.
# Chunk sizes depend only on the inputs, never on timing, so a fixed seed
# still gives reproducible results.
sim_chunk_plan <- function(per_iter, mc_iter, chunk_seconds = 0.4,
                           min_chunk = 25, max_chunk = Inf) {
  max_chunk <- rep_len(max_chunk, length(per_iter))
  size <- pmin(mc_iter, max_chunk, pmax(min_chunk, floor(chunk_seconds / per_iter)))
  do.call(rbind, lapply(seq_along(per_iter), function(i) {
    k <- ceiling(mc_iter / size[i])
    iters <- rep(size[i], k)
    iters[k] <- mc_iter - size[i] * (k - 1)
    data.frame(i = i, iters = iters)
  }))
}

# Run fun() with R's random-number generator set to `state` (a saved
# .Random.seed; NULL = leave it as is). Returns the value and the generator's
# state afterwards, and restores the global generator, so a chunked run keeps
# its own reproducible stream of random numbers.
with_seed_state <- function(state, fun) {
  genv <- globalenv()
  had <- exists(".Random.seed", envir = genv, inherits = FALSE)
  old <- if (had) get(".Random.seed", envir = genv)
  on.exit({
    if (had) assign(".Random.seed", old, envir = genv)
    else if (exists(".Random.seed", envir = genv, inherits = FALSE))
      rm(".Random.seed", envir = genv)
  })
  if (!is.null(state)) assign(".Random.seed", state, envir = genv)
  value <- fun()
  list(value = value, state = get(".Random.seed", envir = genv))
}

# "12 s" / "3.5 min"
format_secs <- function(s) {
  if (s < 60) paste0(max(1, round(s)), " s") else paste0(round(s / 60, 1), " min")
}
