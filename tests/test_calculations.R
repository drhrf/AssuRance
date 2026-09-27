# Checks for R/calculations.R. Run from the project root:
#   Rscript tests/test_calculations.R
# Takes about a minute (the simulation checks use 20,000 trials each).

suppressMessages(library(bayesassurance))
source("R/calculations.R")

ok <- TRUE
check <- function(desc, cond) {
  cat(sprintf("%-68s %s\n", desc, if (cond) "OK" else "FAIL"))
  ok <<- ok && cond
}

# --- 1. Package simulation agrees with the exact closed form ---------------
# This is the key check that the prior/parameter mapping into
# bayesassurance::bayes_sim_unbalanced() is right.
mc  <- 20000
tol <- 4 * 0.5 / sqrt(mc)   # generous Monte Carlo tolerance (~0.014)
cases <- list(
  list(nt = c(20, 80, 200), nc = c(20, 80, 200), md = 5,  td = 3, ma = 0,  ta = 4,   s = 10, a = 0.05,  alt = "greater",   m = 0),
  list(nt = c(20, 80, 200), nc = c(20, 80, 200), md = 5,  td = 3, ma = 5,  ta = 3,   s = 10, a = 0.05,  alt = "two.sided", m = 0),
  list(nt = c(20, 80, 200), nc = c(20, 80, 200), md = -2, td = 1, ma = 0,  ta = 1e3, s = 6,  a = 0.025, alt = "less",      m = 0),
  list(nt = c(40, 120),     nc = c(20, 60),      md = 5,  td = 3, ma = 0,  ta = 4,   s = 10, a = 0.05,  alt = "greater",   m = 2),
  list(nt = c(15, 45),      nc = c(30, 90),      md = -3, td = 3, ma = -1, ta = 5,   s = 8,  a = 0.05,  alt = "less",      m = 1),
  list(nt = c(30, 90),      nc = c(20, 60),      md = 1,  td = 2, ma = -1, ta = 2,   s = 5,  a = 0.1,   alt = "two.sided", m = 0)
)
set.seed(1)
for (cs in cases) {
  sim <- with(cs, sim_assurance(nt, nc, md, td, ma, ta, s, a, alt, m, mc))
  ex  <- with(cs, exact_assurance(nt, nc, md, td, ma, ta, s, a, alt, m))
  check(sprintf("sim vs exact: %-9s md=%g ma=%g ta=%g margin=%g T/C=%s",
                cs$alt, cs$md, cs$ma, cs$ta, cs$m,
                paste(cs$nt[1], cs$nc[1], sep = "/")),
        all(abs(sim - ex) < tol))
}

# --- 2. Frequentist power equals stats::power.t.test ----------------------
for (alt in c("greater", "two.sided")) {
  mine <- freq_power(50, 50, 5, 10, 0.05, alt)
  ref  <- power.t.test(n = 50, delta = 5, sd = 10, sig.level = 0.05,
                       alternative = if (alt == "greater") "one.sided" else "two.sided",
                       strict = TRUE)$power
  check(paste("freq_power matches power.t.test,", alt), abs(mine - ref) < 1e-10)
}
check("freq_power 'less' mirrors 'greater'",
      abs(freq_power(40, 40, -5, 10, 0.05, "less") -
          freq_power(40, 40, 5, 10, 0.05, "greater")) < 1e-12)
check("freq_power with margin = shifted effect",
      abs(freq_power(40, 40, 5, 10, 0.05, "greater", margin = 2) -
          freq_power(40, 40, 3, 10, 0.05, "greater")) < 1e-12)

# --- 3. Exact assurance: limits -----------------------------------------------
# A near-point design prior and vague analysis prior gives the z-test power.
z_pow <- pnorm(sqrt(50 / 2) * 5 / 10 - qnorm(0.975)) +
         pnorm(-sqrt(50 / 2) * 5 / 10 - qnorm(0.975))
check("point design prior + flat analysis prior = z-test power",
      abs(exact_assurance(50, 50, 5, 1e-6, 0, 1e6, 10, 0.05, "two.sided") - z_pow) < 1e-6)
check("assurance approaches its ceiling for huge n (one-sided, margin)",
      abs(exact_assurance(1e7, 1e7, 5, 3, 0, 4, 10, 0.05, "greater", 2) -
          assurance_ceiling(5, 3, "greater", 2)) < 1e-3)
parts <- exact_assurance(100, 100, 5, 3, 0, 4, 10, 0.05, "two.sided", parts = TRUE)
check("two-sided parts add up", abs(parts$positive + parts$negative - parts$total) < 1e-12)
check("conditional success averaged over design prior = assurance",
      abs(integrate(function(d) bayes_success_given_delta(d, 60, 60, 0, 4, 10, 0.05, "greater") *
                      dnorm(d, 5, 3), -Inf, Inf)$value -
          exact_assurance(60, 60, 5, 3, 0, 4, 10, 0.05, "greater")) < 1e-6)

# --- 4. Helpers ------------------------------------------------------------------
check("smallest_n finds the power.t.test sample size",
      smallest_n(function(nt, nc) freq_power(nt, nc, 5, 10, 0.05, "two.sided"), 1, 0.8) ==
        ceiling(power.t.test(delta = 5, sd = 10, power = 0.8)$n))
check("smallest_n returns NA above the ceiling",
      is.na(smallest_n(function(nt, nc) exact_assurance(nt, nc, 5, 3, 0, 4, 10, 0.05, "greater"),
                       1, 0.99)))
check("enrolled_n inflates for dropout", enrolled_n(80, 0.2) == 100 && enrolled_n(81, 0.2) == 102)
check("treatment_n applies the ratio", identical(treatment_n(c(10, 25), 2), c(20, 50)))

if (!ok) stop("Some checks failed.")
cat("All checks passed.\n")
