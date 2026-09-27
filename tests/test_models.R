# Checks for the outcome-type models (R/models.R). Run from the project root:
#   Rscript tests/test_models.R
# Takes about half a minute.

suppressMessages(library(bayesassurance))
for (f in c("R/calculations.R", "R/models.R", "R/summary_text.R")) source(f)

ok <- TRUE
check <- function(desc, cond) {
  cat(sprintf("%-70s %s\n", desc, if (isTRUE(cond)) "OK" else "FAIL"))
  ok <<- ok && isTRUE(cond)
}

# Inputs as produced by config() in app.R (percentages already converted).
base <- list(
  otype = "cont", measure = "or", sigma = 10, rho = 0.5, p_control = 0.3,
  surv_median = 12, time_unit = "months", accrual = 12, followup = 12, loss = 0.05,
  design_mean = 5, design_sd = 3, analysis_mean = 0, analysis_sd = 10,
  rd_design_mean = -10, rd_design_sd = 5, rd_analysis_mean = 0, rd_analysis_sd = 20,
  ratio_design_lo = 0.5, ratio_design_hi = 1.0, ratio_analysis_lo = 0.5, ratio_analysis_hi = 2,
  same_prior = FALSE, alt = "two.sided", alpha = 0.05, threshold = 0,
  rd_threshold = 0, ratio_threshold = 1, ratio = 1, dropout = 0, target = 0.8,
  engine = "exact", mc_iter = 2000, seed = 1)
model <- function(...) build_model(modifyList(base, list(...)))

# --- 1. Simulation agrees with the exact calculation, every outcome type -----
set.seed(11)
cases <- list(
  list(args = list(otype = "cont"), nt = c(30, 100), nc = c(30, 100), mc = 8000),
  list(args = list(otype = "ancova"), nt = c(30, 100), nc = c(30, 100), mc = 8000),
  list(args = list(otype = "ancova", alt = "greater", threshold = 1), nt = c(40, 120), nc = c(20, 60), mc = 8000),
  list(args = list(otype = "binary", measure = "or"), nt = c(200, 600), nc = c(200, 600), mc = 20000),
  list(args = list(otype = "binary", measure = "rr", alt = "less"), nt = c(200, 600), nc = c(200, 600), mc = 20000),
  list(args = list(otype = "binary", measure = "rd", alt = "less", rd_threshold = -2), nt = c(150, 500), nc = c(150, 500), mc = 20000),
  list(args = list(otype = "binary", measure = "or", same_prior = TRUE), nt = c(200, 400), nc = c(100, 200), mc = 20000),
  list(args = list(otype = "surv", alt = "less"), nt = c(150, 400), nc = c(150, 400), mc = 10000),
  list(args = list(otype = "surv", alt = "less", ratio_threshold = 1.3), nt = c(100, 300), nc = c(100, 300), mc = 10000),
  list(args = list(otype = "surv", accrual = 0, loss = 0), nt = c(200, 300), nc = c(100, 150), mc = 10000)
)
for (cs in cases) {
  m <- do.call(model, cs$args)
  sim <- vapply(seq_along(cs$nt), function(i) m$sim(m, cs$nt[i], cs$nc[i], cs$mc), 0)
  ex <- model_exact(m, cs$nt, cs$nc)
  tol <- 4 * 0.5 / sqrt(cs$mc) + 0.005   # Monte Carlo + approximation error
  check(sprintf("sim vs exact: %s", paste(names(cs$args), unlist(cs$args), sep = "=", collapse = ", ")),
        all(abs(sim - ex) < tol))
}

# --- 2. Building blocks --------------------------------------------------------------
# Survival event probability against a direct Monte Carlo of one arm.
set.seed(3)
lam <- log(2) / 12; eta <- -log(1 - 0.05) / (12 + 6); n <- 4e5
entry <- runif(n, 0, 12); tt <- rexp(n, lam); lt <- rexp(n, eta)
check("survival event probability matches direct simulation",
      abs(mean(tt <= pmin(lt, 24 - entry)) - surv_event_prob(lam, eta, 12, 12)) < 0.003)
check("survival event probability without accrual or loss = 1 - exp(-lambda F)",
      abs(surv_event_prob(lam, 0, 0, 12) - (1 - exp(-lam * 12))) < 1e-12)
ms <- model(otype = "surv")
d_tot <- ms$events(0, 300, 300)
check("log-HR variance close to Schoenfeld's 4/D for HR = 1",
      abs(ms$se2(0, 300, 300) - 4 / d_tot) < 1e-12)
check("ratio range maps to log-normal prior",
      all(abs(unlist(ratio_range_to_prior(0.5, 2)) - c(0, log(4) / (2 * qnorm(0.975)))) < 1e-12))
mo <- model(otype = "binary", measure = "or")
check("binary OR: treatment rate at OR = 1 equals control rate", abs(mo$p_t(0) - 0.3) < 1e-12)
check("binary OR: variance formula", abs(mo$se2(log(0.7), 100, 100) -
  (1 / (100 * mo$p_t(log(0.7)) * (1 - mo$p_t(log(0.7)))) + 1 / (100 * 0.3 * 0.7))) < 1e-12)
check("binary RD: prior outside 0-100% triggers a warning",
      length(model(otype = "binary", measure = "rd", p_control = 0.05,
                   rd_design_mean = -4, rd_design_sd = 5)$warnings) == 1 &&
        length(model(otype = "binary", measure = "rd")$warnings) == 0)
ma <- model(otype = "ancova", rho = 0)
mc_ <- model(otype = "cont")
check("ANCOVA with rho = 0 differs from the t-test only by the small vif",
      abs(ma$se2(NA, 100, 100) / mc_$se2(NA, 100, 100) - (1 + 1 / 196)) < 1e-12)

# --- 3. Model-level quantities ---------------------------------------------------------
m1 <- model(otype = "surv", alt = "less", ratio_threshold = 0.9)
check("ratio threshold is on the log scale", abs(m1$C - log(0.9)) < 1e-12)
check("ceiling = design-prior probability beyond the threshold",
      abs(model_ceiling(m1) - pnorm((log(0.9) - m1$m_d) / m1$s_d)) < 1e-12)
check("exact assurance approaches the ceiling for huge n",
      abs(model_exact(m1, 1e6, 1e6) - model_ceiling(m1)) < 0.005)
check("conditional success averaged over the design prior = assurance",
      abs(sum(QUAD_W * model_cond(m1, m1$m_d + m1$s_d * QUAD_Z, 200, 200)) -
          model_exact(m1, 200, 200)) < 1e-10)
mr <- model(otype = "binary", measure = "or")
nn <- smallest_n(function(nt, nc) model_exact(mr, nt, nc), 1, 0.5)
check("smallest_n with numerical integration finds the first crossing",
      model_exact(mr, nn, nn) >= 0.5 && model_exact(mr, nn - 1, nn - 1) < 0.5)
check("non-inferiority threshold gives higher power than superiority",
      model_power(model(otype = "surv", alt = "less", ratio_threshold = 1.3), 200, 200) >
        model_power(model(otype = "surv", alt = "less"), 200, 200))
check("goal text: superiority",
      model_goal(model(otype = "surv", alt = "less")) == "that treatment reduces the hazard (event rate)")
check("goal text: threshold",
      model_goal(m1) == "that the hazard ratio is below 0.90")

check("warning when the analysis prior alone meets the success rule",
      isTRUE(model(otype = "surv", alt = "less", same_prior = TRUE)$prior_only_success) &&
        !isTRUE(model(otype = "surv", alt = "less")$prior_only_success) &&
        length(model(otype = "surv", alt = "less", same_prior = TRUE)$warnings) == 1)

# --- 4. Summary and descriptions run for every outcome type ---------------------------
for (a in list(list(otype = "cont"), list(otype = "ancova"),
               list(otype = "binary", measure = "rd"), list(otype = "surv", alt = "less"))) {
  p <- modifyList(base, a); p$n_c <- c(20L, 200L); p$n_t <- p$n_c
  m <- build_model(p)
  res <- list(p = p, model = m,
              table = data.frame(n_t = p$n_t, n_c = p$n_c,
                                 assurance = model_exact(m, p$n_t, p$n_c),
                                 power = model_power(m, p$n_t, p$n_c)),
              metrics = list(n_assur = smallest_n(function(nt, nc) model_exact(m, nt, nc), 1, 0.8),
                             n_power = smallest_n(function(nt, nc) model_power(m, nt, nc), 1, 0.8),
                             ceiling = model_ceiling(m), neg_share = 0.02))
  txt <- tryCatch(c(build_summary(res), describe_inputs(p, m)), error = function(e) NULL)
  check(paste("summary and report text build for", a$otype),
        !is.null(txt) && length(txt) > 5 && !any(grepl("NA", txt[1:2])))
}

if (!ok) stop("Some checks failed.")
cat("All checks passed.\n")
