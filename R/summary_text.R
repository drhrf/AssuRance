# =============================================================================
# Plain-language summary for non-statisticians.
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

describe_goal <- function(alt, margin) {
  if (margin > 0) {
    switch(alt,
      greater = paste0("a benefit of treatment larger than ", fmt_num(margin)),
      less    = paste0("a reduction with treatment larger than ",
                       fmt_num(margin)))
  } else {
    switch(alt,
      greater   = "a benefit of treatment over control",
      less      = "a reduction with treatment relative to control",
      two.sided = "a difference between treatment and control")
  }
}

# res : list produced by the server's results() reactive (see app.R).
# Returns a character vector of paragraphs.
build_summary <- function(res) {
  p   <- res$p
  tab <- res$table
  last <- tab[nrow(tab), ]
  assur <- last$assurance
  pwr   <- last$power
  goal  <- describe_goal(p$alt, p$margin)

  out <- character(0)

  # 1. Bayesian assurance at the largest sample size ------------------------
  out <- c(out, paste0(
    "With ", describe_arms(last$n_t, last$n_c), ", and given your ",
    "assumptions, there is an estimated ", fmt_pct(assur), " probability ",
    "that this study design will successfully detect ", goal, ", accounting ",
    "for your uncertainty about the true effect size. This is the Bayesian ",
    "assurance: it averages the chance of success over the whole range of ",
    "effect sizes you consider plausible, rather than betting on one value."))

  # 2. Frequentist power at the same sample size ----------------------------
  out <- c(out, paste0(
    "By contrast, under a traditional frequentist framework that assumes the ",
    "effect size is known exactly to be ", fmt_num(p$design_mean),
    ", the study has ", fmt_pct(pwr), " power to detect this effect at the ",
    "same sample size. That figure takes no account of the possibility that ",
    "the true effect is smaller (or larger) than assumed."))

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
  m <- res$metrics
  tgt <- fmt_pct(p$target, 0)
  if (is.na(m$n_assur)) {
    if (m$ceiling < p$target) {
      out <- c(out, paste0(
        "Your target of ", tgt, " assurance cannot be reached at any sample ",
        "size. Even with unlimited participants, assurance levels off at ",
        "about ", fmt_pct(m$ceiling), ", because your design prior gives a ",
        fmt_pct(1 - m$ceiling), " chance that the true effect is ",
        if (p$margin > 0) "smaller than the clinically meaningful difference"
        else "in the opposite direction",
        ". No trial can reliably detect an effect that is not there; more ",
        "participants will not fix this."))
    } else {
      out <- c(out, paste0(
        "Your target of ", tgt, " assurance is not reached below 20,000 ",
        "participants per group."))
    }
  } else {
    n_t <- treatment_n(m$n_assur, p$ratio)
    txt <- paste0(
      "To reach ", tgt, " assurance you would need about ",
      describe_arms(n_t, m$n_assur))
    if (p$dropout > 0) {
      txt <- paste0(txt, " who complete the study; allowing for ",
                    fmt_pct(p$dropout, 0),
                    " dropout, plan to enrol about ",
                    fmt_num(enrolled_n(n_t, p$dropout) +
                              enrolled_n(m$n_assur, p$dropout)), " in total")
    }
    txt <- paste0(txt, ".")
    if (!is.na(m$n_power)) {
      txt <- paste0(txt, " A traditional calculation would instead ask for ",
                    describe_arms(treatment_n(m$n_power, p$ratio), m$n_power),
                    " to reach ", tgt, " power.")
    }
    out <- c(out, paste0(txt, " (These sample sizes use the exact formula, ",
                         "so they are free of simulation noise.)"))
  }

  # 5. One-sided ceiling, when it is informative ------------------------------
  if (p$alt != "two.sided" && m$ceiling < 0.99 && !is.na(m$n_assur)) {
    out <- c(out, paste0(
      "Keep in mind that assurance can never exceed about ",
      fmt_pct(m$ceiling), " for this design, however large the trial: that ",
      "is the probability, under your design prior, that the true effect is ",
      "large enough to count as a success at all."))
  }

  # 6. Two-sided: success in the unexpected direction -------------------------
  if (p$alt == "two.sided" && m$neg_share > 0.01) {
    out <- c(out, paste0(
      "With a two-sided test, 'success' also includes finding a clear effect ",
      "in the opposite direction to the one you expect. About ",
      fmt_pct(m$neg_share), " of the ", fmt_pct(assur), " assurance comes ",
      "from such findings, which you may not regard as a success."))
  }

  out
}
