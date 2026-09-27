# =============================================================================
# Calculation helpers for the AssuRance Shiny app.
#
# Shiny automatically sources every file in the R/ directory before app.R runs,
# so these functions are available to the server. They are kept separate from
# the UI so they can be tested on their own (see tests/test_calculations.R).
# =============================================================================


# -----------------------------------------------------------------------------
# Bayesian assurance via bayesassurance::bayes_sim()
# -----------------------------------------------------------------------------
#
# THE STATISTICAL MODEL WE WANT
# -----------------------------
#   y_Ti ~ N(mu_T, sigma^2),  i = 1..n   (treatment arm)
#   y_Ci ~ N(mu_C, sigma^2),  i = 1..n   (control arm)
#   Delta = mu_T - mu_C                  (the treatment effect)
#   sigma is known.
#
#   Design prior   (used to SIMULATE a plausible true effect):
#       Delta ~ N(design_mean,   design_sd^2)
#   Analysis prior (used to ANALYSE each simulated trial):
#       Delta ~ N(analysis_mean, analysis_sd^2)
#
#   A simulated trial is a "success" when the posterior (under the analysis
#   prior) is convincing:
#       one-sided, greater : P(Delta > 0 | data) > 1 - alpha
#       one-sided, less    : P(Delta < 0 | data) > 1 - alpha
#       two-sided          : either posterior tail probability > 1 - alpha/2
#   Assurance = probability of success, averaged over the design prior.
#
# HOW bayes_sim() PARAMETERISES THINGS  (bayesassurance 0.1.0)
# -------------------------------------
#   Linear model  y = Xn %*% beta + e,  e ~ N(0, sigsq * Vn)
#   Design prior   beta ~ N(mu_beta_d, sigsq * Vbeta_d)
#   Analysis prior beta ~ N(mu_beta_a, sigsq * solve(Vbeta_a_inv))
#   Success is judged on the linear combination  u' beta  versus a constant C.
#
#   KEY GOTCHA: every prior (co)variance in bayes_sim is expressed RELATIVE TO
#   sigsq. A prior SD of tau on the raw outcome scale therefore corresponds to
#   a Vbeta entry of tau^2 / sigsq, and a precision entry of sigsq / tau^2.
#
# OUR MAPPING
# -----------
#   Xn  : left NULL with p = 2, so bayes_sim builds gen_Xn(c(n, n)), a
#         "cell means" design: the first n rows are the treatment arm
#         (column 1) and the next n rows are the control arm (column 2).
#         Therefore beta = (mu_T, mu_C).
#         NOTE: we deliberately do NOT pass our own Xn. If Xn is supplied,
#         bayes_sim reuses that single matrix for every n in the vector, so
#         the sample size would silently stop varying.
#   Vn  : NULL -> identity (independent observations with variance sigsq).
#   sigsq = sigma^2 (the known outcome variance).
#   u = c(1, -1), C = 0  ->  success is judged on  u' beta = mu_T - mu_C = Delta
#                            versus 0 (no difference).
#
#   Design prior, mapped to beta = (mu_T, mu_C):
#       Only Delta matters, so we fix mu_C at 0 (with zero variance) and let
#       mu_T carry the effect:
#           mu_beta_d = c(design_mean, 0)
#           Vbeta_d   = diag(c(design_sd^2 / sigma^2, 0))
#       This gives Delta = mu_T - mu_C ~ N(design_mean, design_sd^2) exactly.
#       (Fixing mu_C at 0 loses nothing: the analysis below is invariant to
#       the common level of the two arms.)
#
#   Analysis prior, mapped to beta = (mu_T, mu_C):
#       We want an informative N(analysis_mean, analysis_sd^2) prior on Delta
#       and a flat (non-informative) prior on the common level of the arms.
#       That is the (rank-1, improper-in-one-direction) precision matrix
#           Vbeta_a_inv = (sigma^2 / analysis_sd^2) * [ 1 -1 ; -1  1 ]
#       because (beta - mu)' [1 -1; -1 1] (beta - mu) = (Delta - analysis_mean)^2
#       when mu_beta_a = c(analysis_mean, 0). bayes_sim only ever uses
#       Vbeta_a_inv + t(Xn) %*% Xn, which is positive definite, so the
#       singular prior precision is fine.
#           mu_beta_a = c(analysis_mean, 0)
#
#   alt   : "greater", "less", or "two.sided" (passed straight through).
#   alpha : significance threshold (passed straight through).
#
#   This mapping was checked against the exact closed-form normal-normal
#   assurance (see tests/test_calculations.R).
#
# WHY NOT bayesassurance::assurance_nd_na() (the package's closed form)?
#   It is a one-sample formula whose analysis prior mean is tied to the null
#   value theta_0, so an analysis prior mean different from the null cannot be
#   expressed. It also disagrees with the exact posterior-probability
#   assurance (and with bayes_sim) whenever the analysis prior is
#   informative (n_a not ~ 0). It only matches when the analysis prior is
#   essentially flat, so we use the simulation-based bayes_sim() instead.
# -----------------------------------------------------------------------------
compute_bayes_assurance <- function(n, design_mean, design_sd,
                                    analysis_mean, analysis_sd,
                                    sigma, alpha, alt, mc_iter) {
  sigsq <- sigma^2

  mu_beta_d   <- c(design_mean, 0)
  Vbeta_d     <- diag(c(design_sd^2 / sigsq, 0))

  mu_beta_a   <- c(analysis_mean, 0)
  Vbeta_a_inv <- (sigsq / analysis_sd^2) * matrix(c(1, -1, -1, 1), nrow = 2)

  # bayes_sim prints a pbapply progress bar to the console; silence it.
  old_pb <- pbapply::pboptions(type = "none")
  on.exit(pbapply::pboptions(old_pb), add = TRUE)

  # bayes_sim returns a rounded character string (not a table) when n is a
  # scalar, so a single sample size is evaluated twice and the first row kept.
  n_call <- if (length(n) == 1) c(n, n) else n

  # suppressWarnings: bayes_sim builds a ggplot internally that emits a
  # harmless "Ignoring unknown parameters" warning with recent ggplot2.
  res <- suppressWarnings(
    bayesassurance::bayes_sim(
      n = n_call, p = 2,
      u = c(1, -1), C = 0,
      Xn = NULL, Vn = NULL,
      Vbeta_d = Vbeta_d, Vbeta_a_inv = Vbeta_a_inv,
      sigsq = sigsq,
      mu_beta_d = mu_beta_d, mu_beta_a = mu_beta_a,
      alt = alt, alpha = alpha, mc_iter = mc_iter
    )
  )

  # Column 2 of the returned table is "Assurance" (column 1 is n).
  res$assurance_table[seq_along(n), 2]
}


# -----------------------------------------------------------------------------
# Frequentist power via stats::power.t.test()
# -----------------------------------------------------------------------------
# Treats the design prior MEAN as the exact, known true effect size (no
# uncertainty). Two-sample t-test with n per group and pooled SD sigma.
#
#   alt = "greater"   : one-sided test of treatment > control, delta = design_mean
#   alt = "less"      : one-sided test of treatment < control. power.t.test
#                       only tests in the positive direction, so the sign is
#                       flipped: delta = -design_mean.
#   alt = "two.sided" : strict = TRUE counts rejections in BOTH tails, which
#                       matches the two-sided Bayesian success rule above.
# -----------------------------------------------------------------------------
compute_freq_power <- function(n, design_mean, sigma, alpha, alt) {
  delta <- switch(alt,
                  greater   = design_mean,
                  less      = -design_mean,
                  two.sided = design_mean)
  alternative <- if (alt == "two.sided") "two.sided" else "one.sided"

  vapply(n, function(ni) {
    stats::power.t.test(n = ni, delta = delta, sd = sigma,
                        sig.level = alpha, type = "two.sample",
                        alternative = alternative, strict = TRUE)$power
  }, numeric(1))
}


# -----------------------------------------------------------------------------
# Plain-language summary for the largest sample size evaluated
# -----------------------------------------------------------------------------
fmt_pct <- function(p) paste0(formatC(100 * p, format = "f", digits = 1), "%")

build_summary <- function(results, design_mean, alt) {
  last <- results[nrow(results), ]
  n_max <- last$n
  assur <- last$assurance
  pwr   <- last$power

  direction <- switch(alt,
    greater   = "a benefit of treatment over control",
    less      = "a reduction with treatment relative to control",
    two.sided = "a difference between treatment and control")

  p1 <- paste0(
    "With ", n_max, " participants per group (", 2 * n_max, " in total), ",
    "and given your assumptions, there is an estimated ", fmt_pct(assur),
    " probability that this study design will successfully detect ",
    direction, ", accounting for your uncertainty about the true effect size. ",
    "This is the Bayesian assurance: it averages the chance of success over ",
    "the full range of effect sizes you consider plausible, rather than ",
    "betting on a single value."
  )

  p2 <- paste0(
    "By contrast, under a traditional frequentist framework that assumes the ",
    "effect size is known exactly to be ", signif(design_mean, 4),
    ", the study has ", fmt_pct(pwr), " power to detect this effect ",
    "at the same sample size. This figure ignores any uncertainty about ",
    "whether the true effect really is that size."
  )

  gap <- pwr - assur
  p3 <- NULL
  if (abs(gap) > 0.15) {
    p3 <- paste0(
      "Note: these two numbers differ by ", round(100 * abs(gap)),
      " percentage points. The difference reflects the extra uncertainty ",
      "about the true effect size that the Bayesian approach takes into ",
      "account (and, if you chose a different analysis prior, the influence ",
      "of that prior). This gap is worth considering when judging whether ",
      "the study is feasible: ",
      if (gap > 0) {
        "the traditional power calculation may be painting an optimistic picture."
      } else {
        "here the traditional power calculation is the more pessimistic of the two."
      }
    )
  }

  c(p1, p2, p3)
}
