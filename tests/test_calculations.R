# Checks that the bayes_sim() parameter mapping in R/calculations.R reproduces
# the exact (closed-form) normal-normal assurance for a two-arm trial with
# known variance. Run from the project root:  Rscript tests/test_calculations.R

source("R/calculations.R")

# Exact assurance. With n per arm, the observed difference in means d has
# standard error^2 se2 = 2 sigma^2 / n. Under the analysis prior
# N(ma, ta^2), the posterior for Delta is normal with
#   precision = 1/ta^2 + 1/se2,  mean = post_var * (ma/ta^2 + d/se2).
# Under the design prior N(md, td^2), marginally d ~ N(md, td^2 + se2).
exact_assurance <- function(n, md, td, ma, ta, sigma, alpha, alt) {
  se2 <- 2 * sigma^2 / n
  post_var <- 1 / (1 / ta^2 + 1 / se2)
  # P(Delta > 0 | d) > 1 - a  <=>  post_mean / sqrt(post_var) > qnorm(1 - a)
  # <=> d > (qnorm(1 - a) * sqrt(post_var) / post_var - ma / ta^2) * se2
  thresh <- function(a) (qnorm(1 - a) / sqrt(post_var) - ma / ta^2) * se2
  # P(Delta < 0 | d) > 1 - a  <=>  d < (-qnorm(1 - a) / sqrt(post_var) - ma / ta^2) * se2
  lower  <- function(a) (-qnorm(1 - a) / sqrt(post_var) - ma / ta^2) * se2
  sd_d <- sqrt(td^2 + se2)
  switch(alt,
    greater   = 1 - pnorm((thresh(alpha) - md) / sd_d),
    less      = pnorm((lower(alpha) - md) / sd_d),
    two.sided = 1 - pnorm((thresh(alpha / 2) - md) / sd_d) +
                pnorm((lower(alpha / 2) - md) / sd_d))
}

n <- c(20, 80, 200)
mc <- 20000
tol <- 4 * 0.5 / sqrt(mc)   # generous Monte Carlo tolerance (~0.014)
cases <- list(
  list(md = 5,  td = 3,  ma = 0,  ta = 4,   sigma = 10, alpha = 0.05,  alt = "greater"),
  list(md = 5,  td = 3,  ma = 5,  ta = 3,   sigma = 10, alpha = 0.05,  alt = "two.sided"),
  list(md = -2, td = 1,  ma = 0,  ta = 1e3, sigma = 6,  alpha = 0.025, alt = "less"),
  list(md = 1,  td = 2,  ma = -1, ta = 2,   sigma = 5,  alpha = 0.1,   alt = "two.sided")
)

set.seed(1)
ok <- TRUE
for (cs in cases) {
  sim <- with(cs, compute_bayes_assurance(n, md, td, ma, ta, sigma, alpha, alt, mc))
  ex  <- with(cs, exact_assurance(n, md, td, ma, ta, sigma, alpha, alt))
  pass <- all(abs(sim - ex) < tol)
  ok <- ok && pass
  cat(sprintf("%-9s md=%5.1f td=%4.1f ma=%4.1f ta=%6.1f | sim: %s | exact: %s | %s\n",
              cs$alt, cs$md, cs$td, cs$ma, cs$ta,
              paste(sprintf("%.3f", sim), collapse = " "),
              paste(sprintf("%.3f", ex), collapse = " "),
              if (pass) "OK" else "FAIL"))
}

# Frequentist power: sign handling for the "less" direction and the
# two-sided strict option.
stopifnot(
  abs(compute_freq_power(50, 5, 10, 0.05, "greater") -
      power.t.test(n = 50, delta = 5, sd = 10, alternative = "one.sided")$power) < 1e-12,
  abs(compute_freq_power(50, -5, 10, 0.05, "less") -
      compute_freq_power(50, 5, 10, 0.05, "greater")) < 1e-12,
  abs(compute_freq_power(50, -5, 10, 0.05, "two.sided") -
      compute_freq_power(50, 5, 10, 0.05, "two.sided")) < 1e-12,
  # single sample size path through bayes_sim
  length(compute_bayes_assurance(30, 5, 3, 0, 4, 10, 0.05, "greater", 200)) == 1
)

if (!ok) stop("Simulation and exact assurance disagree beyond Monte Carlo tolerance.")
cat("All checks passed.\n")
