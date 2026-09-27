# Checks for R/detectability.R. Run from the project root:
#   Rscript tests/test_detectability.R

suppressMessages(library(bayesassurance))
for (f in c("R/i18n.R", "R/calculations.R", "R/models.R", "R/summary_text.R", "R/detectability.R")) source(f)

ok <- TRUE
check <- function(desc, cond) {
  cat(sprintf("%-72s %s\n", desc, if (isTRUE(cond)) "OK" else "FAIL"))
  ok <<- ok && isTRUE(cond)
}

base <- list(
  otype = "cont", measure = "or", sigma = 10, rho = 0.5, p_control = 0.3,
  surv_median = 12, time_unit = "months", accrual = 12, followup = 12, loss = 0.05,
  design_mean = 5, design_sd = 3, analysis_mean = 0, analysis_sd = 10,
  rd_design_mean = -10, rd_design_sd = 5, rd_analysis_mean = 0, rd_analysis_sd = 20,
  ratio_design_lo = 0.5, ratio_design_hi = 1.0, ratio_analysis_lo = 0.5, ratio_analysis_hi = 2,
  same_prior = FALSE, alt = "greater", alpha = 0.025, threshold = 0,
  rd_threshold = 0, ratio_threshold = 1, ratio = 1)
model <- function(...) build_model(modifyList(base, list(...)))

# --- 1. Bookkeeping: the outcomes partition all trials and match assurance -----------
for (a in list(list(otype = "cont"), list(otype = "cont", threshold = 2),
               list(otype = "binary", measure = "or", alt = "less"),
               list(otype = "surv", alt = "less", ratio_threshold = 0.9),
               list(otype = "ancova", alt = "two.sided"), list(otype = "surv", alt = "two.sided"))) {
  m <- do.call(model, a)
  d <- detectability_one(m, 150, 150)
  tot <- d$detected + d$missed + d$false_success + d$wrong_dir + d$correct_no
  lab <- paste(names(a), unlist(a), sep = "=", collapse = ", ")
  check(paste("outcomes add up to 1 and match exact assurance:", lab),
        abs(tot - 1) < 1e-9 && abs(d$assurance - model_exact(m, 150, 150)) < 1e-9)
  if (m$alt != "two.sided") {
    check(paste("P(real) equals the assurance ceiling:", lab), abs(d$p_real - model_ceiling(m)) < 1e-4)
  }
  check(paste("information measures are coherent:", lab),
        d$mi_truth >= -1e-12 && d$mi_truth <= min(d$h_truth, d$h_outcome) + 1e-9 &&
          d$h_outcome <= log2(if (m$alt == "two.sided") 3 else 2) + 1e-9)
}

# --- 2. Limits ---------------------------------------------------------------------------
m <- model(otype = "surv", alt = "less", ratio_threshold = 1)
dd <- detectability(m, c(20, 200, 2000, 1e6), c(20, 200, 2000, 1e6))
check("detection if real increases with n and approaches 100%",
      all(diff(dd$detect) > 0) && dd$detect[4] > 0.99)
check("with a huge trial nearly all doubt about the effect being real is resolved",
      dd$mi_share[4] > 0.9)
mt <- model(otype = "cont", alt = "two.sided")
d2 <- detectability(mt, c(10, 1000), c(10, 1000))
check("two-sided: wrong-direction successes shrink as n grows", d2$wrong_dir[2] < d2$wrong_dir[1])
check("continuous: uncertainty removed = s_d^2 / (s_d^2 + v)",
      abs(detectability_one(mt, 50, 50)$var_removed - 9 / (9 + 2 * 100 / 50)) < 1e-12)

# --- 3. Direct simulation of the continuous case ------------------------------------------
# Draw true effects from the design prior, simulate the estimate, apply the
# Bayesian decision, and compare with the exact detectability measures.
set.seed(5)
mc <- model(otype = "cont", alt = "greater", threshold = 1)
n <- 60; v <- 2 * 100 / n; K <- 4e5
theta <- rnorm(K, mc$m_d, mc$s_d)
est <- rnorm(K, theta, sqrt(v))
succ <- bayes_decision(est, v, mc$m_a, mc$s_a, mc$alpha, mc$alt, mc$C)
real <- theta > mc$C
ex <- detectability_one(mc, n, n)
check("simulated detection-if-real matches the exact value",
      abs(mean(succ[real]) - ex$detect) < 0.004)
check("simulated false-success share matches", abs(mean(succ & !real) - ex$false_success) < 0.002)
tab <- table(real, succ) / K
check("simulated mutual information matches", abs(mutual_info(unclass(tab)) - ex$mi_truth) < 0.005)
# posterior variance of theta given the estimate (design prior as belief)
fit_var <- var(theta - predict(lm(theta ~ est)))
check("simulated posterior variance matches the uncertainty removed",
      abs((1 - fit_var / mc$s_d^2) - ex$var_removed) < 0.005)

# --- 4. Text ---------------------------------------------------------------------------------
dr <- detectability_one(m, 300, 300); dr$n_t <- 300; dr$n_c <- 300
p <- modifyList(base, list(otype = "surv", alt = "less", target = 0.8))
en <- with_lang("en", c(detect_paragraphs(dr, m, p, 400), describe_detectability(dr, m)))
pt <- with_lang("pt", c(detect_paragraphs(dr, m, p, 400), describe_detectability(dr, m)))
check("detectability text builds in both languages",
      any(grepl("detection if real", en)) && any(grepl("detec\u00E7\u00E3o se real", pt)) &&
        any(grepl("bits", pt)) && !any(grepl("NA", c(en, pt))))

if (!ok) stop("Some checks failed.")
cat("All checks passed.\n")
