# =============================================================================
# Plain-language summary for non-statisticians, and short text descriptions
# of the inputs (used by the report and the scenario table).
# =============================================================================

fmt_pct <- function(p, digits = 1) {
  paste0(formatC(100 * p, format = "f", digits = digits), "%")
}

fmt_num <- function(x) format(signif(x, 4), big.mark = ",", scientific = FALSE,
                              trim = TRUE)

# Describe the arm sizes, e.g. "100 participants per group (200 in total)".
describe_arms <- function(n_t, n_c) {
  if (n_t == n_c) {
    paste0(fmt_num(n_c), " participants per group (", fmt_num(2 * n_c),
           " in total)")
  } else {
    paste0(fmt_num(n_t), " participants in the treatment group and ",
           fmt_num(n_c), " in the control group (", fmt_num(n_t + n_c),
           " in total)")
  }
}

# Expected number of events at the design prior's centre (binary, survival).
expected_events <- function(m, n_t, n_c) {
  if (is.null(m$events)) NA_real_ else m$events(m$m_d, n_t, n_c)
}

# Prior as text on the natural scale.
describe_prior <- function(m, which = c("design", "analysis")) {
  which <- match.arg(which)
  mu <- if (which == "design") m$m_d else m$m_a
  s  <- if (which == "design") m$s_d else m$s_a
  z <- qnorm(0.975)
  switch(m$family,
    additive = paste0("mean ", fmt_num(mu), ", SD ", fmt_num(s)),
    rd       = paste0("mean ", fmt_num(100 * mu), " points, SD ", fmt_num(100 * s), " points"),
    ratio    = paste0(m$labels$short, " ", formatC(exp(mu), format = "f", digits = 2),
                      " (95% range ", formatC(exp(mu - z * s), format = "f", digits = 2),
                      " to ", formatC(exp(mu + z * s), format = "f", digits = 2), ")"))
}

describe_test <- function(m) {
  base <- c(two.sided = "two-sided", greater = "one-sided, treatment > control",
            less = "one-sided, treatment < control")[[m$alt]]
  if (m$alt != "two.sided" && abs(m$C) > 1e-12) {
    base <- paste0(base, ", threshold ", m$fmt_eff(m$C))
  }
  paste0(base, ", alpha = ", m$alpha)
}

# res : list produced by the server's finish_run() (see app.R).
# Returns a character vector of paragraphs.
build_summary <- function(res) {
  p   <- res$p
  m   <- res$model
  tab <- res$table
  last <- tab[nrow(tab), ]
  assur <- last$assurance
  pwr   <- last$power
  goal  <- model_goal(m)
  ev_txt <- function(n_t, n_c) {
    ev <- expected_events(m, n_t, n_c)
    if (is.na(ev)) "" else paste0(", with about ", fmt_num(round(ev)),
                                  " events expected")
  }

  out <- character(0)

  # 1. Bayesian assurance at the largest sample size ------------------------
  out <- c(out, paste0(
    "With ", describe_arms(last$n_t, last$n_c), ev_txt(last$n_t, last$n_c),
    ", and given your assumptions, there is an estimated ", fmt_pct(assur),
    " probability that this study design will successfully show ", goal,
    ", accounting for your uncertainty about the true effect size. This is ",
    "the Bayesian assurance: it averages the chance of success over the whole ",
    "range of effect sizes you consider plausible, rather than betting on one ",
    "value."))

  # 2. Frequentist power at the same sample size ----------------------------
  out <- c(out, paste0(
    "By contrast, under a traditional frequentist framework that assumes the ",
    m$labels$short, " is known exactly to be ", m$fmt_eff(m$m_d),
    ", the study has ", fmt_pct(pwr), " power to detect this effect at the ",
    "same sample size (", m$power_label, "). That figure takes no account of ",
    "the possibility that the true effect is smaller (or larger) than ",
    "assumed."))

  # 3. Flag a large gap -----------------------------------------------------
  gap <- pwr - assur
  if (abs(gap) > 0.15) {
    out <- c(out, paste0(
      "Note: the two numbers differ by ", round(100 * abs(gap)),
      " percentage points. The difference reflects the extra uncertainty ",
      "about the true effect size that the Bayesian approach takes into ",
      "account", if (!p$same_prior) " (and the influence of the analysis prior you chose)" else "",
      ". This gap is worth considering when judging whether the study is ",
      "feasible: ",
      if (gap > 0) "the traditional power calculation may be painting an optimistic picture."
      else "here the traditional power calculation is the more pessimistic of the two."))
  }

  # 4. Sample size needed for the target ------------------------------------
  mt <- res$metrics
  tgt <- fmt_pct(p$target, 0)
  if (is.na(mt$n_assur)) {
    if (mt$ceiling < p$target) {
      out <- c(out, paste0(
        "Your target of ", tgt, " assurance cannot be reached at any sample ",
        "size. Even with unlimited participants, assurance levels off at ",
        "about ", fmt_pct(mt$ceiling), ", because your design prior gives a ",
        fmt_pct(1 - mt$ceiling), " chance that the true ", m$labels$short,
        " is not beyond the success threshold (",
        m$fmt_eff(m$C), "). No trial can reliably show an effect that is not ",
        "there; more participants will not fix this."))
    } else {
      out <- c(out, paste0(
        "Your target of ", tgt, " assurance is not reached below 20,000 ",
        "participants per group."))
    }
  } else {
    n_t <- treatment_n(mt$n_assur, p$ratio)
    txt <- paste0("To reach ", tgt, " assurance you would need about ",
                  describe_arms(n_t, mt$n_assur), ev_txt(n_t, mt$n_assur))
    if (p$dropout > 0) {
      txt <- paste0(txt, "; allowing for ", fmt_pct(p$dropout, 0),
                    " dropout, plan to enrol about ",
                    fmt_num(enrolled_n(n_t, p$dropout) +
                              enrolled_n(mt$n_assur, p$dropout)), " in total")
    }
    txt <- paste0(txt, ".")
    if (!is.na(mt$n_power)) {
      nt_p <- treatment_n(mt$n_power, p$ratio)
      txt <- paste0(txt, " A traditional calculation would instead ask for ",
                    describe_arms(nt_p, mt$n_power), ev_txt(nt_p, mt$n_power),
                    if (is.null(m$events)) "" else ",", " to reach ", tgt, " power.")
    }
    out <- c(out, paste0(txt, " (These sample sizes use the exact formula, ",
                         "so they are free of simulation noise.)"))
  }

  # 5. One-sided ceiling, when it is informative ------------------------------
  if (m$alt != "two.sided" && mt$ceiling < 0.99 && !is.na(mt$n_assur)) {
    out <- c(out, paste0(
      "Keep in mind that assurance can never exceed about ",
      fmt_pct(mt$ceiling), " for this design, however large the trial: that ",
      "is the probability, under your design prior, that the true effect is ",
      "beyond the success threshold at all."))
  }

  # 6. Two-sided: success in the unexpected direction -------------------------
  if (m$alt == "two.sided" && mt$neg_share > 0.01) {
    out <- c(out, paste0(
      "With a two-sided test, 'success' also includes finding a clear effect ",
      "in the opposite direction to the one you expect. About ",
      fmt_pct(mt$neg_share), " of the ", fmt_pct(assur), " assurance comes ",
      "from such findings, which you may not regard as a success."))
  }

  # 7. Few events: approximations are less reliable -----------------------------
  if (!is.null(m$events)) {
    ev_min <- expected_events(m, tab$n_t[1], tab$n_c[1])
    if (!is.na(ev_min) && ev_min < 30) {
      out <- c(out, paste0(
        "At the smallest sample sizes only about ", fmt_num(round(ev_min)),
        " events are expected. With so few events the normal approximations ",
        "behind the exact formula and the frequentist power are rough; the ",
        "simulation engine, which analyses the simulated counts directly, is ",
        "more trustworthy there."))
    }
  }

  out
}

# Plain-text description of every input, for the downloadable report.
describe_inputs <- function(p, m) {
  lines <- c(sprintf("  Outcome:         %s", m$describe))
  if (p$otype == "surv") {
    lines <- c(lines, sprintf("  Survival design: recruitment %s, extra follow-up %s %s, %s%% lost to follow-up",
                              fmt_num(p$accrual), fmt_num(p$followup), m$time_unit,
                              fmt_num(100 * p$loss)))
  }
  c(lines,
    sprintf("  Design prior:    %s", describe_prior(m, "design")),
    sprintf("  Analysis prior:  %s%s", describe_prior(m, "analysis"),
            if (p$same_prior) "  [same as design prior]" else ""),
    sprintf("  Test:            %s", describe_test(m)),
    sprintf("  Allocation:      %s : 1 (treatment : control)", p$ratio),
    if (p$otype != "surv") sprintf("  Dropout:         %s%%", 100 * p$dropout),
    sprintf("  Target:          %s", fmt_pct(p$target, 0)),
    sprintf("  Engine:          %s", if (p$engine == "sim")
      paste0(m$engine_label, ", ", p$mc_iter, " trials per n, seed ", p$seed)
      else "exact formula"),
    sprintf("  Power:           %s", m$power_label))
}
