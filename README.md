# AssuRance

A small R Shiny app that compares **Bayesian assurance** (the probability a trial
succeeds, averaged over your uncertainty about the true effect) with
**frequentist power** (which assumes the effect is known exactly). It's for a
two-arm trial with a continuous, normally distributed outcome and a known SD.

## Install and run

```r
install.packages(c("shiny", "ggplot2", "bayesassurance"))
# or, for the development version of bayesassurance:
# remotes::install_github("jpan928/bayesassurance_rpackage")

shiny::runApp()   # from this directory
```

## Files

- `app.R`: the user interface and server logic.
- `R/calculations.R`: the assurance, power and summary-text functions. Shiny
  loads this file automatically. It documents in detail how the design and
  analysis priors map onto the arguments of `bayesassurance::bayes_sim()`.
- `tests/test_calculations.R`: checks the `bayes_sim()` mapping against the
  exact normal-normal assurance formula. Run it with
  `Rscript tests/test_calculations.R` (takes about a minute).

## Method notes

- **Assurance** comes from `bayesassurance::bayes_sim()` (Monte Carlo), using a
  cell-means design with `u = c(1, -1)` and `C = 0`. A simulated trial counts
  as a success when the posterior probability of an effect in the tested
  direction is greater than `1 - alpha` (`1 - alpha/2` per tail when the test
  is two-sided).
- The package's closed-form `assurance_nd_na()` is **not** used. It ties the
  analysis prior mean to the null value, and it disagrees with the exact
  posterior-probability assurance whenever the analysis prior is informative.
- **Power** comes from `stats::power.t.test()` (two-sample t-test, with
  `strict = TRUE` for two-sided tests). It uses the design prior mean as the
  true effect.
