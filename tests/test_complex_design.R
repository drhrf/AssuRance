# Checks for R/complex_design.R (rough extrapolation to complex designs).
# Run from the project root:   Rscript tests/test_complex_design.R

suppressMessages(library(bayesassurance))
invisible(Sys.setlocale("LC_CTYPE", "C.UTF-8"))
for (f in c("R/i18n.R", "R/calculations.R", "R/models.R", "R/summary_text.R",
            "R/detectability.R", "R/complex_design.R")) source(f)

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
  same_prior = FALSE, alt = "two.sided", alpha = 0.05, threshold = 0,
  rd_threshold = 0, ratio_threshold = 1, ratio = 1, dropout = 0, target = 0.8)
no_cx <- list(features = character(0), target = 0.8, alpha = NA, m = 20, icc = 0.05, cv = 0,
              k = 3, rep_rho = 0.5, x_rho = 0.6, arms = 3, mult = "bonferroni", J = 2,
              jmode = "any", looks = 3, bound = "obf", nonadh = 10, contam = 5)
base_n <- function(p, m) list(
  power = smallest_n(function(nt, nc) model_power(m, nt, nc), p$ratio, p$target),
  assurance = smallest_n(function(nt, nc) model_exact(m, nt, nc), p$ratio, p$target),
  detect = smallest_n(function(nt, nc) detect_rate(m, nt, nc), p$ratio, p$target))
run <- function(pl = list(), cl = list()) {
  p <- modifyList(base, pl); m <- build_model(p)
  cx_extrapolate(p, m, modifyList(no_cx, cl), base_n(p, m))
}

# --- 1. Formulas ---------------------------------------------------------------------
check("cluster design effect 1 + (m - 1) ICC, and the CV version",
      abs(cluster_de(20, 0.05) - 1.95) < 1e-12 && abs(cluster_de(20, 0.05, 0.4) - (1 + (1.16 * 20 - 1) * 0.05)) < 1e-12)
check("repeated measures: one measurement changes nothing; k = 3, rho = 0.5 gives 2/3",
      repeated_de(1, 0.7) == 1 && abs(repeated_de(3, 0.5) - 2 / 3) < 1e-12)
check("crossover with rho = 0 needs half the participants per 'arm'", crossover_de(0) == 0.5)
check("alpha splitting: Bonferroni and Sidak",
      split_alpha(0.05, 2) == 0.025 && abs(split_alpha(0.05, 2, "sidak") - (1 - sqrt(0.95))) < 1e-12)
# Published values (Jennison & Turnbull 2000, two-sided alpha 0.05)
jt <- rbind(c(2, 0.8, 1.110, 1.008), c(3, 0.8, 1.166, 1.017), c(4, 0.9, 1.183, 1.022), c(5, 0.9, 1.207, 1.026))
check("group sequential inflation factors match the published tables (+/- 0.003)",
      all(apply(jt, 1, function(r) abs(gs_inflation(r[1], "pocock", 0.05, r[2]) - r[3]) < 0.003 &&
                                   abs(gs_inflation(r[1], "obf", 0.05, r[2]) - r[4]) < 0.003)))
check("inflation factors: Pocock > O'Brien-Fleming, growing with the number of looks",
      gs_inflation(3, "pocock", 0.05, 0.9) > gs_inflation(3, "obf", 0.05, 0.9) &&
        gs_inflation(5, "pocock", 0.05, 0.9) > gs_inflation(2, "pocock", 0.05, 0.9) && gs_inflation(1, "obf", 0.05, 0.9) == 1)

# --- 2. The extrapolation ----------------------------------------------------------------
for (pl in list(list(otype = "cont"), list(otype = "binary", measure = "or"),
                list(otype = "surv", alt = "less"), list(otype = "ancova", ratio = 2))) {
  r <- run(pl)
  p <- modifyList(base, pl); b <- base_n(p, build_model(p))
  check(paste("nothing selected: the complex design equals the simple one,", pl$otype),
        all(vapply(names(b), function(k) identical(as.numeric(r$crit[[k]]$final$n_c), as.numeric(b[[k]])), logical(1))))
}
n_icc <- vapply(c(0, 0.02, 0.05, 0.1), function(i) run(cl = list(features = "cluster", icc = i))$crit$power$final$n_c, 0)
check("sample size grows with the ICC; ICC = 0 changes nothing", all(diff(n_icc) > 0) && n_icc[1] == run()$crit$power$final$n_c)
r_gs <- vapply(2:5, function(K) run(cl = list(features = "interim", looks = K, bound = "pocock"))$crit$power$final$n_c, 0)
check("sample size grows with the number of interim analyses", all(diff(r_gs) >= 0) && r_gs[4] > r_gs[1])
r_t <- run(cl = list(target = 0.9))
check("a stricter target is recalculated, not multiplied",
      r_t$crit$power$final$n_c == smallest_n(function(nt, nc) model_power(build_model(base), nt, nc), 1, 0.9))
r_all <- run(cl = list(features = "endpoints", jmode = "all", J = 2, target = 0.9))
check("co-primary endpoints raise the target per endpoint to target^(1/J)",
      abs(r_all$target[["final"]] - sqrt(0.9)) < 1e-12 && r_all$alpha[["final"]] == 0.05)
r_any <- run(cl = list(features = "endpoints", jmode = "any", J = 2))
check("'any endpoint' splits alpha", r_any$alpha[["final"]] == 0.025)
r_arm <- run(cl = list(features = "multiarm", arms = 4))
f <- r_arm$crit$power$final
check("several arms: alpha split over k - 1 comparisons and totals over k arms",
      abs(r_arm$alpha[["final"]] - 0.05 / 3) < 1e-12 && f$total == 4 * f$n_c)
r_ad <- run(cl = list(features = "adherence", nonadh = 10, contam = 5))
check("non-adherence: power sample size close to the classic n / d^2 rule",
      abs(r_ad$crit$power$final$n_c / run()$crit$power$final$n_c - 1 / 0.85^2) < 0.08)
r_cl <- run(cl = list(features = "cluster", m = 10), pl = list(dropout = 0.1))
f <- r_cl$crit$power$final
check("clusters are counted from the numbers to enrol", f$clusters == 2 * ceiling(enrolled_n(f$n_c, 0.1) / 10))
r_un <- run(pl = list(alt = "greater", alpha = 0.025, threshold = 4), cl = list(target = 0.95))
check("unreachable assurance target: no result, and the text still builds",
      is.null(r_un$crit$assurance$final) &&
        all(nzchar(with_lang("en", cx_headline(r_un, modifyList(base, list(alt = "greater", threshold = 4)),
                                             base_n(modifyList(base, list(alt = "greater", threshold = 4, alpha = 0.025)),
                                                    build_model(modifyList(base, list(alt = "greater", threshold = 4, alpha = 0.025)))))))))
r_ig <- run(pl = list(otype = "surv", alt = "less"), cl = list(features = c("crossover", "repeated", "cluster")))
check("features that do not apply to the outcome type are ignored and reported",
      setequal(r_ig$ignored, c("crossover", "repeated")) && identical(r_ig$used, "cluster"))
check("input checks: defaults pass, bad values are caught",
      is.null(cx_check(modifyList(no_cx, list(features = CX_FEATURES)))) &&
        !is.null(cx_check(modifyList(no_cx, list(features = "cluster", icc = 1.2)))) &&
        !is.null(cx_check(modifyList(no_cx, list(features = "interim", looks = 7)))) &&
        !is.null(cx_check(modifyList(no_cx, list(features = "adherence", nonadh = 60, contam = 40)))))

# --- 3. Simulation checks: the extrapolated size gives the target power ----------------
set.seed(11)
nsim <- 4000
# (a) cluster randomisation, analysed with a t-test on the cluster means
r <- run(cl = list(features = "cluster", m = 10, icc = 0.05, target = 0.9))
k <- ceiling(r$crit$power$final$n_c / 10)          # clusters per arm
rej <- replicate(nsim, {
  arm <- function(delta) {
    u <- rnorm(k, 0, sqrt(0.05) * 10)
    rowMeans(matrix(rnorm(k * 10, delta + rep(u, each = 10), sqrt(0.95) * 10), nrow = k, byrow = TRUE))
  }
  t.test(arm(5), arm(0), var.equal = TRUE)$p.value < 0.05
})
check(sprintf("cluster trial: simulated power %.3f at the extrapolated size (target 0.9)", mean(rej)),
      abs(mean(rej) - 0.9) < 0.03)
# (b) 2x2 crossover, analysed with a paired t-test (no period effect)
r <- run(cl = list(features = "crossover", x_rho = 0.6, target = 0.9))
N <- r$crit$power$final$n_c + r$crit$power$final$n_t
rej <- replicate(nsim, {
  z <- MASS::mvrnorm(N, c(5, 0), 100 * matrix(c(1, 0.6, 0.6, 1), 2))
  t.test(z[, 1], z[, 2], paired = TRUE)$p.value < 0.05
})
check(sprintf("crossover: simulated power %.3f at the extrapolated size (target 0.9)", mean(rej)),
      abs(mean(rej) - 0.9) < 0.03)
# (c) non-adherence and contamination, intention-to-treat t-test
r <- run(cl = list(features = "adherence", nonadh = 15, contam = 10, target = 0.9))
n <- r$crit$power$final$n_c
rej <- replicate(nsim, {
  yt <- rnorm(n, 5 * (runif(n) > 0.15), 10); yc <- rnorm(n, 5 * (runif(n) < 0.10), 10)
  t.test(yt, yc, var.equal = TRUE)$p.value < 0.05
})
check(sprintf("non-adherence: simulated power %.3f at the extrapolated size (target 0.9)", mean(rej)),
      abs(mean(rej) - 0.9) < 0.03)

# --- 4. Text in both languages, for every outcome type ---------------------------------
all_cx <- modifyList(no_cx, list(features = CX_FEATURES, target = 0.9, alpha = 0.01))
for (pl in list(list(otype = "cont"), list(otype = "ancova"), list(otype = "binary", measure = "rd"),
                list(otype = "surv", alt = "less"))) {
  p <- modifyList(base, pl); m <- build_model(p); b <- base_n(p, m)
  r <- cx_extrapolate(p, m, all_cx, b)
  txt <- lapply(c("en", "pt"), function(lg) with_lang(lg, c(cx_headline(r, p, b), cx_notes(r, p), cx_report_lines(r, p, b),
                                                          unlist(cx_result_table(r, p, b)), unlist(cx_factor_table(r, p)))))
  check(paste("text builds in both languages,", pl$otype),
        length(txt[[1]]) > 10 && any(grepl("Complex design", txt[[1]])) && any(grepl("extrapola\u00E7\u00E3o", txt[[2]])) &&
          !any(grepl("participants", txt[[2]])))
}

if (!ok) stop("Some checks failed.")
cat("All checks passed.\n")
