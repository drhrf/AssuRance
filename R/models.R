# =============================================================================
# Outcome-type models.
#
# build_model(p) turns the validated inputs `p` (see params() in app.R) into a
# "model": everything the rest of the app needs to know about one outcome
# type, on the working scale used by R/calculations.R.
#
#   Outcome type        working scale theta     variance of the estimate
#   ------------------  ----------------------  ------------------------------
#   cont    means       mean difference         sigma^2 (1/n_t + 1/n_c)
#   ancova  adj. means  adjusted mean diff.     sigma^2 (1 - rho^2) (1/n_t + 1/n_c) * vif
#   binary  OR          log odds ratio          1/(n_t p_t q_t) + 1/(n_c p_c q_c)
#   binary  RR          log risk ratio          q_t/(n_t p_t) + q_c/(n_c p_c)
#   binary  RD          risk difference         p_t q_t / n_t + p_c q_c / n_c
#   surv    HR          log hazard ratio        1/E[D_t] + 1/E[D_c]  (events)
#
# For binary and survival outcomes the variance depends on the true effect
# (through p_t or the treatment-arm event count), so exact assurance is
# computed by numerical integration over the design prior.
#
# Simulation engines
#   cont, ancova : bayesassurance::bayes_sim_unbalanced (R/calculations.R)
#   binary       : built-in: binomial counts are simulated for each trial and
#                  analysed with a normal approximation on the working scale.
#                  (bayesassurance::bayes_sim_betabin is not used: it draws the
#                  true proportions ONCE per call, so it does not average over
#                  the design prior, and it cannot use separate design and
#                  analysis priors.)
#   surv         : built-in: patient-level exponential survival times with
#                  uniform accrual, administrative censoring and loss to
#                  follow-up; analysed with the exponential-model log hazard
#                  ratio (log of the ratio of event rates, variance
#                  1/D_t + 1/D_c). bayesassurance has no survival functions.
# =============================================================================

# ---- Small helpers -------------------------------------------------------------
scale_family <- function(otype, measure) {
  if (otype %in% c("cont", "ancova")) "additive"
  else if (otype == "binary" && identical(measure, "rd")) "rd"
  else "ratio"
}

# A 95% range (lo, hi) on the ratio scale -> normal prior on the log scale.
ratio_range_to_prior <- function(lo, hi) {
  list(mean = (log(lo) + log(hi)) / 2, sd = (log(hi) - log(lo)) / (2 * qnorm(0.975)))
}

# Probability that a patient has an observed event, for an exponential event
# hazard `lambda`, loss-to-follow-up hazard `eta`, uniform accrual over
# [0, accrual] and analysis at accrual + followup.
surv_event_prob <- function(lambda, eta, accrual, followup) {
  k <- lambda + eta
  frac <- lambda / k
  if (accrual <= 0) return(frac * (1 - exp(-k * followup)))
  frac * (1 - (exp(-k * followup) - exp(-k * (accrual + followup))) / (k * accrual))
}

# ---- The model builder ------------------------------------------------------------
build_model <- function(p) {
  otype   <- p$otype
  measure <- if (otype == "binary") p$measure else NA
  fam     <- scale_family(otype, measure)

  # -- priors and threshold on the working scale --------------------------------
  if (fam == "additive") {
    m_d <- p$design_mean;  s_d <- p$design_sd
    m_a <- p$analysis_mean; s_a <- p$analysis_sd
    C   <- p$threshold
  } else if (fam == "rd") {
    m_d <- p$rd_design_mean / 100;  s_d <- p$rd_design_sd / 100
    m_a <- p$rd_analysis_mean / 100; s_a <- p$rd_analysis_sd / 100
    C   <- p$rd_threshold / 100
  } else {
    d <- ratio_range_to_prior(p$ratio_design_lo, p$ratio_design_hi)
    a <- ratio_range_to_prior(p$ratio_analysis_lo, p$ratio_analysis_hi)
    m_d <- d$mean; s_d <- d$sd; m_a <- a$mean; s_a <- a$sd
    C   <- log(p$ratio_threshold)
  }
  if (isTRUE(p$same_prior)) { m_a <- m_d; s_a <- s_d }
  if (p$alt == "two.sided") C <- 0

  # -- display helpers --------------------------------------------------------------
  nat <- switch(fam, additive = identity, rd = function(x) 100 * x, ratio = exp)
  fmt_eff <- switch(fam,
    additive = function(x) format(signif(x, 3), trim = TRUE),
    rd       = function(x) paste0(formatC(100 * x, format = "f", digits = 1), " percentage points"),
    ratio    = function(x) formatC(exp(x), format = "f", digits = 2))

  key <- if (otype == "binary") paste0("bin_", measure) else otype
  lab <- switch(key,
    cont   = list(short = "difference in means", thing = "the outcome",
                  axis = "True difference in means (treatment minus control)",
                  type = "Continuous"),
    ancova = list(short = "adjusted difference in means", thing = "the outcome",
                  axis = "True baseline-adjusted difference in means",
                  type = "Continuous, baseline-adjusted"),
    bin_or = list(short = "odds ratio", thing = "the odds of the event",
                  axis = "True odds ratio (treatment vs control, log scale)",
                  type = "Binary, odds ratio"),
    bin_rr = list(short = "risk ratio", thing = "the risk of the event",
                  axis = "True risk ratio (treatment vs control, log scale)",
                  type = "Binary, risk ratio"),
    bin_rd = list(short = "risk difference", thing = "the risk of the event",
                  axis = "True risk difference (percentage points)",
                  type = "Binary, risk difference"),
    surv   = list(short = "hazard ratio", thing = "the hazard (event rate)",
                  axis = "True hazard ratio (treatment vs control, log scale)",
                  type = "Time to event"))

  m <- list(key = key, otype = otype, measure = measure, family = fam,
            m_d = m_d, s_d = s_d, m_a = m_a, s_a = s_a, C = C,
            alt = p$alt, alpha = p$alpha, nat = nat, fmt_eff = fmt_eff,
            labels = lab, log_axis = fam == "ratio", warnings = character(0),
            events = NULL)

  # -- does the analysis prior alone already meet the success rule? -------------------
  # If so the "trial" succeeds even without data (typically when an optimistic
  # design prior is reused for the analysis), and assurance is meaningless.
  pa_above <- 1 - pnorm((C - m_a) / s_a)
  prior_prob <- switch(p$alt, greater = pa_above, less = 1 - pa_above,
                       two.sided = max(pa_above, 1 - pa_above))
  need_prob <- if (p$alt == "two.sided") 1 - p$alpha / 2 else 1 - p$alpha
  m$prior_only_success <- prior_prob > need_prob
  if (m$prior_only_success) {
    m$warnings <- c(m$warnings, paste0(
      "Your analysis prior on its own already meets the success rule: it gives a ",
      fmt_pct(prior_prob), " probability to the tested effect, above the ",
      fmt_pct(need_prob), " required. The analysis would declare success even ",
      "with almost no data, so assurance and the sample-size finder are not ",
      "meaningful. Use a sceptical or vaguer analysis prior (untick 'Analyse ",
      "with the same prior')."))
  }

  # -- outcome-specific parts ---------------------------------------------------------
  if (otype == "cont") {
    sigma <- p$sigma
    m$const_var <- TRUE
    m$se2 <- function(theta, n_t, n_c) sigma^2 * (1 / n_t + 1 / n_c)
    m$power_at <- function(theta, n_t, n_c)
      freq_power(n_t, n_c, theta, sigma, m$alpha, m$alt, m$C)
    m$sim <- function(mm, n_t, n_c, iters)
      sim_assurance(n_t, n_c, mm$m_d, mm$s_d, mm$m_a, mm$s_a, sigma,
                    mm$alpha, mm$alt, mm$C, iters)
    m$per_iter <- package_seconds_per_iter
    m$max_chunk <- function(n_t, n_c) Inf
    m$engine_label <- "bayesassurance::bayes_sim_unbalanced"
    m$power_label <- "two-sample t-test"
    m$describe <- paste0("Continuous, SD ", fmt_num(sigma))
    m$nuisance <- list(label = "Outcome standard deviation",
                       values = seq(sigma / 2, sigma * 2, length.out = 40),
                       current = sigma, set = function(q, v) { q$sigma <- v; q })

  } else if (otype == "ancova") {
    sigma <- p$sigma; rho <- p$rho
    vif <- function(n_t, n_c) 1 + 1 / pmax(n_t + n_c - 4, 1)
    m$const_var <- TRUE
    m$se2 <- function(theta, n_t, n_c)
      sigma^2 * (1 - rho^2) * (1 / n_t + 1 / n_c) * vif(n_t, n_c)
    m$power_at <- function(theta, n_t, n_c)
      freq_power(n_t, n_c, theta, sigma * sqrt(1 - rho^2), m$alpha, m$alt,
                 m$C, vif = vif(n_t, n_c), df_loss = 1)
    m$sim <- function(mm, n_t, n_c, iters)
      sim_assurance(n_t, n_c, mm$m_d, mm$s_d, mm$m_a, mm$s_a, sigma,
                    mm$alpha, mm$alt, mm$C, iters, ancova_rho = rho)
    m$per_iter <- package_seconds_per_iter
    m$max_chunk <- function(n_t, n_c) Inf
    m$engine_label <- "bayesassurance::bayes_sim_unbalanced (with a simulated baseline covariate)"
    m$power_label <- "ANCOVA t-test"
    m$describe <- paste0("ANCOVA, SD ", fmt_num(sigma), ", rho ", fmt_num(rho))
    m$nuisance <- list(label = "Baseline-outcome correlation (rho)",
                       values = seq(0, 0.9, length.out = 40),
                       current = rho, set = function(q, v) { q$rho <- v; q })

  } else if (otype == "binary") {
    pc <- p$p_control
    p_t <- switch(measure,
      or = function(theta) plogis(qlogis(pc) + theta),
      rr = function(theta) pmin(pc * exp(theta), 0.999),
      rd = function(theta) pmin(pmax(pc + theta, 0.001), 0.999))
    unit_var <- switch(measure,
      or = function(q) 1 / (q * (1 - q)),
      rr = function(q) (1 - q) / q,
      rd = function(q) q * (1 - q))
    m$const_var <- FALSE
    m$p_t <- p_t
    m$se2 <- function(theta, n_t, n_c) unit_var(p_t(theta)) / n_t + unit_var(pc) / n_c
    m$power_at <- function(theta, n_t, n_c)
      freq_power_z(theta, sqrt(m$se2(theta, n_t, n_c)), m$alpha, m$alt, m$C)
    m$events <- function(theta, n_t, n_c) n_t * p_t(theta) + n_c * pc
    m$sim <- function(mm, n_t, n_c, iters) {
      theta <- stats::rnorm(iters, mm$m_d, mm$s_d)
      xt <- stats::rbinom(iters, n_t, p_t(theta))
      xc <- stats::rbinom(iters, n_c, pc)
      if (measure == "or") {
        cc <- 0.5 * (xt == 0 | xt == n_t | xc == 0 | xc == n_c)
        a <- xt + cc; b <- n_t - xt + cc; c <- xc + cc; d <- n_c - xc + cc
        est <- log(a * d / (b * c)); v <- 1 / a + 1 / b + 1 / c + 1 / d
      } else if (measure == "rr") {
        cc <- 0.5 * (xt == 0 | xc == 0)
        a <- xt + cc; c <- xc + cc; nt2 <- n_t + cc; nc2 <- n_c + cc
        est <- log((a / nt2) / (c / nc2))
        v <- pmax(1 / a - 1 / nt2 + 1 / c - 1 / nc2, 1e-8)
      } else {
        est <- xt / n_t - xc / n_c
        qt_ <- (xt + 0.5) / (n_t + 1); qc_ <- (xc + 0.5) / (n_c + 1)
        v <- qt_ * (1 - qt_) / n_t + qc_ * (1 - qc_) / n_c
      }
      mean(bayes_decision(est, v, mm$m_a, mm$s_a, mm$alpha, mm$alt, mm$C))
    }
    m$per_iter <- function(n_t, n_c) rep(3e-6, length(n_t))
    m$max_chunk <- function(n_t, n_c) Inf
    m$engine_label <- "built-in simulation of binomial trial data"
    m$power_label <- paste0("Wald z-test on the ", c(or = "log odds ratio",
      rr = "log risk ratio", rd = "risk difference")[[measure]])
    m$describe <- paste0("Binary (", toupper(measure), "), control ",
                         fmt_num(100 * pc), "%")
    lo <- max(1, 100 * pc / 2.5); hi <- min(99, 100 * pc * 2)
    m$nuisance <- list(label = "Control-group event rate (%)",
                       values = seq(lo, hi, length.out = 40),
                       current = 100 * pc,
                       set = function(q, v) { q$p_control <- v / 100; q })
    # Parts of the design prior that give impossible probabilities are
    # truncated for RR and RD; warn if that matters.
    beyond <- switch(measure,
      or = 0,
      rr = 1 - pnorm((log(0.999 / pc) - m_d) / s_d),
      rd = pnorm((0.001 - pc - m_d) / s_d) + 1 - pnorm((0.999 - pc - m_d) / s_d))
    if (beyond > 0.01) {
      m$warnings <- c(m$warnings, paste0(
        "About ", fmt_pct(beyond), " of your design prior implies a treatment-",
        "group event rate outside 0-100%. Those values are truncated, which makes ",
        "the results approximate. Consider a narrower prior or the odds ratio."))
    }

  } else if (otype == "surv") {
    lam_c <- log(2) / p$surv_median
    A <- p$accrual; F <- p$followup
    eta <- if (p$loss > 0) -log(1 - p$loss) / (F + A / 2) else 0
    pev <- function(lambda) surv_event_prob(lambda, eta, A, F)
    pev_c <- pev(lam_c)
    m$const_var <- FALSE
    m$pev <- pev; m$pev_c <- pev_c
    m$se2 <- function(theta, n_t, n_c)
      1 / (n_t * pev(lam_c * exp(theta))) + 1 / (n_c * pev_c)
    m$power_at <- function(theta, n_t, n_c)
      freq_power_z(theta, sqrt(m$se2(theta, n_t, n_c)), m$alpha, m$alt, m$C)
    m$events <- function(theta, n_t, n_c) n_t * pev(lam_c * exp(theta)) + n_c * pev_c
    sim_arm <- function(n, rate, iters) {
      cells <- iters * n
      entry <- if (A > 0) stats::runif(cells, 0, A) else numeric(cells)
      admin <- A + F - entry
      tt <- stats::rexp(cells, rate = rep(rate, times = n))
      lt <- if (eta > 0) stats::rexp(cells, eta) else rep(Inf, cells)
      cens <- pmin(lt, admin)
      obs <- matrix(pmin(tt, cens), nrow = iters)
      ev  <- matrix(tt <= cens, nrow = iters)
      list(d = rowSums(ev), E = rowSums(obs))
    }
    m$sim <- function(mm, n_t, n_c, iters) {
      theta <- stats::rnorm(iters, mm$m_d, mm$s_d)
      tr <- sim_arm(n_t, lam_c * exp(theta), iters)
      co <- sim_arm(n_c, rep(lam_c, iters), iters)
      cc <- 0.5 * (tr$d == 0 | co$d == 0)
      dt <- tr$d + cc; dc <- co$d + cc
      est <- log(dt / tr$E) - log(dc / co$E)
      mean(bayes_decision(est, 1 / dt + 1 / dc, mm$m_a, mm$s_a, mm$alpha,
                          mm$alt, mm$C))
    }
    m$per_iter <- function(n_t, n_c) 2e-6 + 3e-7 * (n_t + n_c)
    m$max_chunk <- function(n_t, n_c) pmax(10, floor(4e5 / (n_t + n_c)))
    m$engine_label <- "built-in simulation of patient-level survival data"
    m$power_label <- "Wald z-test on the log hazard ratio (exponential model)"
    tu <- if (nzchar(p$time_unit)) p$time_unit else "time units"
    m$time_unit <- tu
    m$describe <- paste0("Survival, control median ", fmt_num(p$surv_median), " ", tu)
    m$nuisance <- list(label = paste0("Control-group median survival (", tu, ")"),
                       values = seq(p$surv_median / 2, p$surv_median * 2, length.out = 40),
                       current = p$surv_median,
                       set = function(q, v) { q$surv_median <- v; q })
  }
  m
}

# ---- Generic operations on a model ------------------------------------------------
model_exact <- function(m, n_t, n_c, parts = FALSE) {
  if (m$const_var) {
    assurance_const_var(m$se2(NA, n_t, n_c), m$m_d, m$s_d, m$m_a, m$s_a,
                        m$alpha, m$alt, m$C, parts)
  } else {
    assurance_quad(m$se2, n_t, n_c, m$m_d, m$s_d, m$m_a, m$s_a, m$alpha,
                   m$alt, m$C, parts)
  }
}

model_cond <- function(m, theta, n_t, n_c) {
  se2 <- rep_len(m$se2(theta, n_t, n_c), length(theta))
  cond_success(theta, se2, m$m_a, m$s_a, m$alpha, m$alt, m$C)
}

model_power <- function(m, n_t, n_c, theta = m$m_d) m$power_at(theta, n_t, n_c)

model_ceiling <- function(m) assurance_ceiling(m$m_d, m$s_d, m$alt, m$C)

model_with_priors <- function(m, m_d = m$m_d, s_d = m$s_d, m_a = m$m_a,
                              s_a = m$s_a) {
  m$m_d <- m_d; m$s_d <- s_d; m$m_a <- m_a; m$s_a <- s_a
  m
}

# Words used in the plain-language summary.
model_goal <- function(m) {
  if (m$alt == "two.sided") {
    return(paste0("a difference between treatment and control in ", m$labels$thing))
  }
  null_C <- abs(m$C) < 1e-12
  if (null_C) {
    paste0("that treatment ", if (m$alt == "greater") "increases " else "reduces ",
           m$labels$thing)
  } else {
    paste0("that the ", m$labels$short, " is ",
           if (m$alt == "greater") "above " else "below ", m$fmt_eff(m$C))
  }
}
