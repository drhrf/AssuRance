# Checks for R/calculations.R. Run from the project root:
#   Rscript tests/test_calculations.R
# Takes about a minute (the simulation checks use 20,000 trials each).
# Outcome-type models are checked in tests/test_models.R.

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
  list(nt = c(15, 45),      nc = c(30, 90),      md = -3, td = 3, ma = -1, ta = 5,   s = 8,  a = 0.05,  alt = "less",      m = -1),
  list(nt = c(30, 90),      nc = c(20, 60),      md = 1,  td = 2, ma = -1, ta = 2,   s = 5,  a = 0.1,   alt = "two.sided", m = 0)
)
set.seed(1)
for (cs in cases) {
  sim <- with(cs, sim_assurance(nt, nc, md, td, ma, ta, s, a, alt, m, mc))
  ex  <- with(cs, exact_assurance(nt, nc, md, td, ma, ta, s, a, alt, m))
  check(sprintf("sim vs exact: %-9s md=%g ma=%g ta=%g C=%g T/C=%s",
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
check("freq_power with threshold C = shifted effect",
      abs(freq_power(40, 40, 5, 10, 0.05, "greater", C = 2) -
          freq_power(40, 40, 3, 10, 0.05, "greater")) < 1e-12)

# --- 3. Exact assurance: limits -----------------------------------------------
# A near-point design prior and vague analysis prior gives the z-test power.
z_pow <- pnorm(sqrt(50 / 2) * 5 / 10 - qnorm(0.975)) +
         pnorm(-sqrt(50 / 2) * 5 / 10 - qnorm(0.975))
check("point design prior + flat analysis prior = z-test power",
      abs(exact_assurance(50, 50, 5, 1e-6, 0, 1e6, 10, 0.05, "two.sided") - z_pow) < 1e-6)
check("assurance approaches its ceiling for huge n (one-sided, threshold)",
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

check("chunk plan respects max_chunk",
      all(sim_chunk_plan(c(1e-6, 1e-6), 1000, max_chunk = c(100, Inf))$iters[1:10] == 100))
check("freq_power_z matches the z-test formula",
      abs(freq_power_z(0.5, 0.2, 0.05, "two.sided") -
          (pnorm(0.5 / 0.2 - qnorm(0.975)) + pnorm(-0.5 / 0.2 - qnorm(0.975)))) < 1e-12)
check("assurance_quad equals the closed form when the variance is constant",
      all(abs(assurance_quad(function(th, nt, nc) 2 * 100 / nt,
                             c(30, 300), c(30, 300), 5, 3, 0, 4, 0.05, "greater") -
              exact_assurance(c(30, 300), c(30, 300), 5, 3, 0, 4, 10, 0.05, "greater")) < 2e-3))

# --- 5. Chunked (stoppable) simulation, as run by the app ----------------------
plan <- sim_chunk_plan(package_seconds_per_iter(c(20, 300, 800), c(20, 300, 800)), 3000)
check("chunk plan: iterations add up to mc_iter for every sample size",
      all(tapply(plan$iters, plan$i, sum) == 3000))
check("chunk plan: large sample sizes are split into several chunks",
      sum(plan$i == 3) > 1 && sum(plan$i == 1) == 1)

# Mimic the app: run the chunks one by one with a private RNG stream, while
# other code draws random numbers in between.
run_chunked <- function(n_t, n_c, mc, seed, meddle = FALSE) {
  plan <- sim_chunk_plan(package_seconds_per_iter(n_t, n_c), mc, chunk_seconds = 0.05)
  rng <- with_seed_state(NULL, function() { set.seed(seed); NULL })$state
  succ <- numeric(length(n_t))
  for (r in seq_len(nrow(plan))) {
    if (meddle) runif(3)   # e.g. a plot being drawn on another tab
    step <- with_seed_state(rng, function()
      sim_assurance(n_t[plan$i[r]], n_c[plan$i[r]], 5, 3, 0, 4, 10, 0.05,
                    "greater", 0, plan$iters[r]))
    rng <- step$state
    succ[plan$i[r]] <- succ[plan$i[r]] + step$value * plan$iters[r]
  }
  succ / mc
}
a1 <- run_chunked(c(30, 150), c(30, 150), 4000, seed = 7)
a2 <- run_chunked(c(30, 150), c(30, 150), 4000, seed = 7, meddle = TRUE)
check("chunked run is reproducible and unaffected by other RNG use", identical(a1, a2))
check("chunked run agrees with the exact assurance",
      all(abs(a1 - exact_assurance(c(30, 150), c(30, 150), 5, 3, 0, 4, 10, 0.05,
                                   "greater")) < 4 * 0.5 / sqrt(4000)))
set.seed(99); before <- runif(1); set.seed(99)
invisible(with_seed_state(NULL, function() { set.seed(1); runif(5) }))
check("with_seed_state restores the global random-number stream", runif(1) == before)

if (!ok) stop("Some checks failed.")
cat("All checks passed.\n")
